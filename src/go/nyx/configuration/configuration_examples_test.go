//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

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

package configuration

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	io "github.com/mooltiverse/nyx/src/go/nyx/io"
)

const (
	/*
	 * The name of the environment variable used to pass the path of the
	 * extended JSON configuration file example to tests.
	 */
	EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "extendedJSONExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * medium JSON configuration file example to tests.
	 */
	MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "mediumJSONExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * simple JSON configuration file example to tests.
	 */
	SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "simpleJSONExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * simplest JSON configuration file example to tests.
	 */
	SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "simplestJSONExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * extended YAML configuration file example to tests.
	 */
	EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "extendedYAMLExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * medium YAML configuration file example to tests.
	 */
	MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "mediumYAMLExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * simple YAML configuration file example to tests.
	 */
	SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "simpleYAMLExampleConfigurationFile"

	/*
	 * The name of the environment variable used to pass the path of the
	 * simplest YAML configuration file example to tests.
	 */
	SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE = "simplestYAMLExampleConfigurationFile"
)

func TestConfigurationExamplesSaveAndLoadSimplestJSONExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simplest"+fmt.Sprintf("%p", t)+".json"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------ Simplest JSON configuration ------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadSimpleJSONExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simple"+fmt.Sprintf("%p", t)+".json"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(SIMPLE_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------- Simple JSON configuration -------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadMediumJSONExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "medium"+fmt.Sprintf("%p", t)+".json"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(MEDIUM_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------- Medium JSON configuration -------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadExtendedJSONExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "extended"+fmt.Sprintf("%p", t)+".json"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------ Extended JSON configuration ------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadSimplestYAMLExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simplest"+fmt.Sprintf("%p", t)+".yaml"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------ Simplest YAML configuration ------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadSimpleYAMLExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simple"+fmt.Sprintf("%p", t)+".yaml"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(SIMPLE_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------- Simple YAML configuration -------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadMediumYAMLExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "medium"+fmt.Sprintf("%p", t)+".yaml"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(MEDIUM_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------- Medium YAML configuration -------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}

func TestConfigurationExamplesSaveAndLoadExtendedYAMLExample(t *testing.T) {
	assert.NotEmpty(t, os.Getenv(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	exampleFile, err := os.Open(os.Getenv(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "extended"+fmt.Sprintf("%p", t)+".yaml"))
	assert.NoError(t, err)

	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(exampleFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), simpleConfigurationLayer)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// recreate the file objects to have a clean state on locks, buffers etc
	exampleFile, err = os.Open(os.Getenv(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE))
	assert.NoError(t, err)
	savedFile, err = os.OpenFile(savedFile.Name(), os.O_RDWR, 0755)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Printf("------ Extended YAML configuration ------")
	//fmt.Println()
	//fmt.Printf("Loading from: " + exampleFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	content, err := os.ReadFile(exampleFile.Name())
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	//fmt.Printf("Saving to: " + savedFile.Name())
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
	_, err = savedFile.Write(content)
	assert.NoError(t, err)
	//fmt.Printf(string(content))
	//fmt.Println()
	//fmt.Printf("-----------------------------------------")
	//fmt.Println()
}
