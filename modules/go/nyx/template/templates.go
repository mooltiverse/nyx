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
This is the templates package for Nyx, providing functions to render text templates.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package templates

import (
	"fmt"     // https://pkg.go.dev/fmt
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings

	raymond "github.com/aymerick/raymond" // https://pkg.go.dev/github.com/aymerick/raymond

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

const (
	// The opening delimiter of a template.
	OPENING_DELIMITER = "{{"

	// The closing delimiter of a template.
	CLOSING_DELIMITER = "}}"
)

/*
Returns true if the given buffer is a template, false otherwise.

The buffer is a template if it has the opening {{ and closing }} delimiters.

This method does not check for syntactic correctness of the template.

Arguments are as follows:

- buffer the buffer to test
*/
func IsTemplate(buffer string) bool {
	return strings.Contains(buffer, OPENING_DELIMITER) && strings.Contains(buffer, CLOSING_DELIMITER)
}

/*
Renders the given template using the given scope to fetch the values and writing the output to the given writer.

Standard functions are available in the rendering engine.

Arguments are as follows:

  - template the template
  - scope the object representing the value to use in rendering. If nil it won't be used.
    writer the object to write the output to

Errors can be:

- IOError: in case data cannot be read or accessed.
*/
func Render(template string, scope interface{}) (string, error) {
	registerHelpers() // register custom helpers
	output, err := raymond.Render(template, scope)

	if err != nil {
		return "", &errs.IOError{Message: fmt.Sprintf("unable to render the template using the given values"), Cause: err}
	}

	// The rendering engine doesn't cope well with nil values returned from the scope objects and renders them
	// as '%!s(*string=&lt;nil&gt;'. The enine doesn't expose any means to control its behavior when a nil is encountered
	// so the quick and dirty workaround here is to just replace all of those rubbish strings with an empty string
	output = strings.ReplaceAll(output, "%!s(*string=&lt;nil&gt;)", "")

	return output, nil

}

/*
Converts the given value to a boolean.

Returns true if the given value is not nil and not blank and it is a string representation of a true,
false in all other cases.
*/
func ToBoolean(value *string) bool {
	if value == nil {
		return false
	}
	// strconv.ParseBool() parses '1' as true, which does not comply with our contract (where evry number is false),
	// so let's check first
	_, err := strconv.ParseInt(*value, 10, 64)
	if err == nil {
		// it's an integer
		return false
	}

	res, err := strconv.ParseBool(*value)
	if err != nil {
		return false
	} else {
		return res
	}
}

/*
Converts the given value to an integer.

Returns the integer representation of the given string, when the given string contains a valid integer
representation, or 0 otherwise.
*/
func ToInteger(value *string) int64 {
	if value == nil {
		return 0
	}
	res, err := strconv.ParseInt(*value, 10, 64)
	if err != nil {
		return 0
	} else {
		return res
	}
}
