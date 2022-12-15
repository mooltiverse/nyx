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
	"strings" // https://pkg.go.dev/strings
)

/*
A simple identifier holding a String value.

In Semantic Versioning 2.0.0 parlance, a string identifier can be
alphanumeric or even numeric, where leading zeroes are allowed. In other words, all non positive
integer identifiers in Semantic Versioning 2.0.0 are to be considered
as string identifiers, while positive integers are handled as IntegerIdentifier.
*/
type stringIdentifier struct {
	// Extend simpleIdentifier by composition
	simpleIdentifier

	// The value held by this identifier. This is redundant with the member from the nested simpleIdentifier
	// but it's typed so we can save methods with (up/down) casting.
	value string
}

/*
Builds the identifier with the given value.
*/
func newStringIdentifier(v string) (stringIdentifier, error) {
	validatedValue, err := validateStringIdentifier(v)
	if err != nil {
		return stringIdentifier{}, err
	}
	res := stringIdentifier{value: validatedValue}
	res.simpleIdentifier.value = validatedValue
	return res, nil
}

/*
Compares this object with the specified object for order.

Returns a negative integer, zero, or a positive integer as this object is less than,
equal to, or greater than the specified object.
*/
func (ii stringIdentifier) compareTo(o string) int {
	return strings.Compare(ii.value, o)
}

/*
Returns the underlying value held by this identifier.
*/
func (si stringIdentifier) getStringValue() string {
	return si.value
}
