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

package version

import (
	"fmt"     // https://pkg.go.dev/fmt
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
)

const (
	// The range of allowed characters in string identifiers.
	ALLOWED_ALPHANUMERIC_CHARACTERS = "[0-9a-zA-Z-]"

	// The regexp pattern that can be used to validate characters in string identifiers.
	ALLOWED_ALPHANUMERIC_CHARACTERS_REGEXP_PATTERN = "^[0-9a-zA-Z-]+$"
)

/*
Validates the given value to make sure it's legal for an alphanumeric identifier.
This method returns the same value received as input if validation passes, otherwise an error is thrown.

Arguments are as follows:

- value the same value passed in input, if no errors are found.
*/
func validateStringIdentifier(value string) (string, error) {
	if "" == value {
		return value, fmt.Errorf("identifier value cannot be empty")
	}

	re := regexp2.MustCompile(ALLOWED_ALPHANUMERIC_CHARACTERS_REGEXP_PATTERN, 0)
	isMatch, _ := re.MatchString(value)
	if !isMatch {
		return value, fmt.Errorf("illegal value '%s' for string identifier. Illegal characters have been found. Allowed characters are limited to "+ALLOWED_ALPHANUMERIC_CHARACTERS+".", value)
	}

	return value, nil
}

/*
Validates the given value to make sure it's legal for a positive numeric identifier.
This method returns the same value received as input if validation passes, otherwise an error is thrown.

Arguments are as follows:

  - value the value to validate
  - allowNegative if false negative integers are not allowed so if the given value represents
    a negative integer an error is returned. If true this check is not performed.
*/
func validateIntegerIdentifier(value int, allowNegative bool) (int, error) {
	if !allowNegative && value < 0 {
		return value, fmt.Errorf("illegal value '%d' for integer identifier. Value cannot be negative.", value)
	}
	return value, nil
}

/*
Validates the given value to make sure it's legal for a positive numeric identifier.
This method returns the same value received as input if validation passes, otherwise an error is thrown.

Arguments are as follows:

  - value the value to validate
  - allowNegative if false negative integers are not allowed so if the given value represents
    a negative integer an error is returned. If true this check is not performed.
  - allowExtraCharacters if false this method also makes sure that no extra characters not affecting
    the acual value are present. Those may be a leading sign or leading zeroes. If true this check is not performed.
    When this flag is enabled this method makes sure that the string representation of converted integer returns the
    same as the input value, otherwise it means that some extra characters were present.
*/
func validateIntegerIdentifierFromString(value string, allowNegative bool, allowExtraCharacters bool) (int, error) {
	if "" == value {
		return 0, fmt.Errorf("identifier value cannot be empty")
	}

	intValue, err := strconv.Atoi(value)
	if err != nil {
		return 0, fmt.Errorf("illegal value '%s' for integer identifier. Not a valid Integer.", value)
	}

	// Check that the string representation of the parsed value is the same as the input value
	// they may differ if the input value had leading zeroes or signs
	if value != strconv.Itoa(intValue) {
		return 0, fmt.Errorf("illegal value '%s' for integer identifier. Value should be '%d'. Consider sanitizing the string before parsing.", value, intValue)
	}

	// Let the other overloaded version of the validate() method check if it's positive
	return validateIntegerIdentifier(intValue, allowNegative)
}

/*
Utility method that returns true if the given array is not nil, it's not empty
and all the elements in the given array are not nil, false otherwise.
*/
func hasValues(items []interface{}) bool {
	if items == nil || len(items) == 0 {
		return false
	}

	for _, item := range items {
		if item == nil {
			return false
		}
	}

	return true
}

/*
Returns a list of string identifiers representing the specified String value.
*/
func toStringIdentifiersWithSeparator(s string, separator string) ([]stringIdentifier, error) {
	if "" == s {
		return nil, fmt.Errorf("can't parse an empty string")
	}

	return toStringIdentifiers(strings.Split(s, separator))
}

/*
Returns a list of string identifiers representing the specified String values.
*/
func toStringIdentifiers(s []string) ([]stringIdentifier, error) {
	if len(s) == 0 {
		return nil, fmt.Errorf("can't parse an empty string array")
	}

	var identifiers []stringIdentifier
	for _, part := range s {
		si, err := newStringIdentifier(part)
		if err != nil {
			return nil, err
		}
		identifiers = append(identifiers, si)
	}
	return identifiers, nil
}

/*
Returns a list of integer identifiers representing the specified String value.
*/
func toIntegerIdentifiersWithSeparator(s string, separator string) ([]integerIdentifier, error) {
	if "" == s {
		return nil, fmt.Errorf("can't parse an empty string")
	}

	return toIntegerIdentifiers(strings.Split(s, separator))
}

/*
Returns a list of integer identifiers representing the specified String values.
*/
func toIntegerIdentifiers(s []string) ([]integerIdentifier, error) {
	if len(s) == 0 {
		return nil, fmt.Errorf("can't parse an empty string array")
	}

	var identifiers []integerIdentifier
	for _, part := range s {
		ii, err := newIntegerIdentifierFromString(part)
		if err != nil {
			return nil, err
		}
		identifiers = append(identifiers, ii)
	}
	return identifiers, nil
}

/*
Returns a list of identifiers representing the specified String value.
*/
func toIdentifiersWithSeparator(s string, separator string, integersPolicy UseIntegerIdentifiers) ([]identifier, error) {
	if "" == s {
		return nil, fmt.Errorf("can't parse an empty string")
	}

	return toIdentifiers(strings.Split(s, separator), integersPolicy)
}

/*
Returns a list of identifiers representing the specified String values.
*/
func toIdentifiers(s []string, integersPolicy UseIntegerIdentifiers) ([]identifier, error) {
	if len(s) == 0 {
		return nil, fmt.Errorf("can't parse an empty string array")
	}

	var identifiers []identifier
	for _, part := range s {
		switch integersPolicy {
		case ALWAYS:
			{
				ii, err := newIntegerIdentifierFromString(part)
				if err != nil {
					return nil, err
				}
				identifiers = append(identifiers, ii)
			}
		case NEVER:
			{
				si, err := newStringIdentifier(part)
				if err != nil {
					return nil, err
				}
				identifiers = append(identifiers, si)
			}
		case WHEN_POSSIBLE:
			{
				ii, err := newIntegerIdentifierFromString(part)
				if err != nil {
					// If the integer was not validated because it's a negative number we can't just use a string
					// but we need to let the error go.
					// If, instead, the string was not an integer representation, let's use a string
					_, err2 := strconv.Atoi(part)
					if err2 != nil {
						// this means it's a string that does not contain an integer, let's fall back to a string identifier
						si, err3 := newStringIdentifier(part)
						if err3 != nil {
							return nil, err3
						}
						identifiers = append(identifiers, si)
					} else {
						// this means it's an integer, whose falidation failed above, so we let the error go
						return nil, err
					}
				} else {
					identifiers = append(identifiers, ii)
				}
			}
		default:
			// this is never reached, but in case...
			panic("unknown UseIntegerIdentifiers. This means the switch/case statement needs to be updated")
		}
	}
	return identifiers, nil
}

/*
Returns an identifier representing the specified String value. If the integersPolicy
flag is ALWAYS or the given string represents an integer and the integersPolicy}
flag is WHEN_POSSIBLE then the returned identifier is an IntegerIdentifier,
otherwise it's a StringIdentifier.
*/
func toIdentifier(s string, integersPolicy UseIntegerIdentifiers) (identifier, error) {
	if "" == s {
		return nil, fmt.Errorf("can't parse an empty string")
	}

	switch integersPolicy {
	case ALWAYS:
		{
			ii, err := newIntegerIdentifierFromString(s)
			if err != nil {
				return nil, err
			}
			return ii, nil
		}
	case NEVER:
		{
			si, err := newStringIdentifier(s)
			if err != nil {
				return nil, err
			}
			return si, nil
		}
	case WHEN_POSSIBLE:
		{
			ii, err := newIntegerIdentifierFromString(s)
			if err != nil {
				// If the integer was not validated because it's a negative number we can't just use a string
				// but we need to let the error go.
				// If, instead, the string was not an integer representation, let's use a string
				_, err2 := strconv.Atoi(s)
				if err2 != nil {
					// this means it's a string that does not contain an integer, let's fall back to a string identifier
					si, err3 := newStringIdentifier(s)
					if err3 != nil {
						return nil, err3
					} else {
						return si, nil
					}
				} else {
					// this means it's an integer, whose falidation failed above, so we let the error go
					return nil, err
				}
			}
			return ii, nil
		}
	default:
		// this is never reached, but in case...
		panic("unknown UseIntegerIdentifiers. This means the switch/case statement needs to be updated")
	}

	// last resort, actually never reached
	return newStringIdentifier(s)
}
