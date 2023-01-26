/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
This is the package that provides I/O capabilities to Nyx, including marshalling and unmarshalling to/from common file formats.
*/
package io

import (
	"bytes"         // https://pkg.go.dev/bytes
	"encoding/json" // https://pkg.go.dev/encoding/json
	"fmt"           // https://pkg.go.dev/fmt
	"io/ioutil"     // https://pkg.go.dev/io/ioutil
	"net/http"      // https://pkg.go.dev/net/http
	"net/url"       // https://pkg.go.dev/net/url
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"reflect"       // https://pkg.go.dev/reflect
	"strings"       // https://pkg.go.dev/strings

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus
	yaml "gopkg.in/yaml.v3"          // https://pkg.go.dev/gopkg.in/yaml.v3

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

/*
Returns true if and only if the given path ends with a recognized YAML extension, otherwise false.
*/
func isYAML(path string) bool {
	lowerPath := strings.ToLower(path)
	if strings.HasSuffix(lowerPath, ".yaml") || strings.HasSuffix(lowerPath, ".yml") {
		return true
	} else {
		return false
	}
}

/*
Unmarshals the content of the given file to an object of the given type.

Arguments are as follows:

  - path the file to load from.
    The file path must end with one of the supported extensions: json, yaml, yml (or JSON is used by default).
  - target the pointer to the object to load the data into. It must be a pointer

Errors can be:

- DataAccessError in case of any error due to data access
- IllegalArgumentError if the given file path does not contain a supported extension or the given target is nil or not a pointer
*/
func LoadFromFile(path string, target any) error {
	if target == nil {
		return &errs.IllegalPropertyError{Message: "target can't be nil"}
	} else if reflect.ValueOf(target).Kind() != reflect.Ptr {
		return &errs.IllegalPropertyError{Message: fmt.Sprintf("target must be a pointer, '%v' was passed", reflect.ValueOf(target).Kind())}
	}
	log.Tracef("reading contents from file '%v'", path)

	content, err := ioutil.ReadFile(path)
	if err != nil {
		return &errs.DataAccessError{Message: fmt.Sprintf("unable to read file '%s'", path), Cause: err}
	}

	if isYAML(path) {
		log.Tracef("unmarshalling object as YAML from file '%v' to type '%v'", path, reflect.ValueOf(target).Kind())

		err = yaml.Unmarshal(content, target)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to unmarshal content from file '%s'", path), Cause: err}
		}
	} else {
		log.Tracef("unmarshalling object as JSON from file '%v' to type '%v'", path, reflect.ValueOf(target).Kind())

		err = json.Unmarshal(content, target)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to unmarshal content from file '%s'", path), Cause: err}
		}
	}
	return nil
}

/*
Unmarshals the content of the given URL to an object of the given type.

Arguments are as follows:

  - path the URL to load from.
    The URL path must end with one of the supported extensions: json, yaml, yml (or JSON is used by default).
  - target the pointer to the object to load the data into. It must be a pointer

Errors can be:

- DataAccessError in case of any error due to data access
- IllegalArgumentError if the given file path does not contain a supported extension or the given target is nil or not a pointer
*/
func LoadFromURL(path url.URL, target any) error {
	if target == nil {
		return &errs.IllegalPropertyError{Message: "target can't be nil"}
	} else if reflect.ValueOf(target).Kind() != reflect.Ptr {
		return &errs.IllegalPropertyError{Message: fmt.Sprintf("target must be a pointer, '%v' was passed", reflect.ValueOf(target).Kind())}
	}
	log.Tracef("reading contents from URL '%v'", path.String())

	response, err := http.Get(path.String())
	if err != nil {
		return &errs.DataAccessError{Message: fmt.Sprintf("unable to read URL '%s'", path.String()), Cause: err}
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusOK {
		return &errs.DataAccessError{Message: fmt.Sprintf("unable to read URL '%s', server returned error %v", path.String(), response.StatusCode)}
	}
	content, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return &errs.DataAccessError{Message: fmt.Sprintf("unable to read URL '%s'", path.String()), Cause: err}
	}

	if isYAML(path.Path) {
		log.Tracef("unmarshalling object as YAML from URL '%v' to type '%v'", path, reflect.ValueOf(target).Kind())

		err = yaml.Unmarshal(content, target)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to unmarshal content from URL '%s'", path.String()), Cause: err}
		}
	} else {
		log.Tracef("unmarshalling object as JSON from URL '%v' to type '%v'", path, reflect.ValueOf(target).Kind())

		err = json.Unmarshal(content, target)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to unmarshal content from URL '%s'", path.String()), Cause: err}
		}
	}
	return nil
}

/*
Marshals the content of the given object to a file represented by the given path.

Arguments are as follows:

  - path the file to save to. If it's a relative path it will be considered relative to the current working directory.
    The file path must end with one of the supported extensions: json, yaml, yml (or JSON is used by default).
  - content the object to marshal.

Errors can be:

- DataAccessError in case of any error due to data access
- IllegalArgumentError if the given file path does not contain a supported extension
*/
func Save(path string, content any) error {
	var buffer bytes.Buffer
	var err error

	if path != filepath.Base(path) {
		// the given path contains directories
		err = os.MkdirAll(filepath.Dir(path), 0755)
		if err != nil && !os.IsExist(err) {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to create directory '%v'", filepath.Dir(path)), Cause: err}
		}
	}

	if isYAML(path) {
		log.Tracef("marshalling object of type '%T' as YAML to file '%s' ", content, path)

		encoder := yaml.NewEncoder(&buffer)
		// Here we should also be able to instruct the marshaller to use double quotes for strings.
		// Missing quotes is not that important, nor it breaks compatibility, but it would be clearer and also consistent
		// with the Java implementation.
		encoder.SetIndent(2)
		encoder.Encode(content)

		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to marshal content '%v'", content), Cause: err}
		}
	} else {
		log.Tracef("marshalling object of type '%T' as JSON to file '%s' ", content, path)

		encoder := json.NewEncoder(&buffer)
		encoder.SetEscapeHTML(false)
		encoder.SetIndent("", "  ")
		encoder.Encode(content)

		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to marshal content '%v'", content), Cause: err}
		}
	}

	err = ioutil.WriteFile(path, buffer.Bytes(), 0644)
	if err != nil {
		return &errs.DataAccessError{Message: fmt.Sprintf("unable to write file '%s'", path), Cause: err}
	}
	return nil
}
