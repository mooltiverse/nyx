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

/*
A simple identifier holding an integer value.

In Semantic Versioning 2.0.0 parlance, an integer identifier is
restricted to be a positive integer, with no leading zeroes, otherwise it's handled as a
stringIdentifier.
*/
type integerIdentifier struct {
	// Extend simpleIdentifier by composition
	simpleIdentifier

	// The value held by this identifier. This is redundant with the member from the nested simpleIdentifier
	// but it's typed so we can save methods with (up/down) casting.
	value int
}

/*
Builds the identifier with the given value.

Errors can be returned:

- if the given value is illegal for some reason, like including forbidden characters
*/
func newIntegerIdentifierFromInt(v int) (integerIdentifier, error) {
	validatedValue, err := validateIntegerIdentifier(v, false)
	if err != nil {
		return integerIdentifier{}, err
	}
	res := integerIdentifier{value: validatedValue}
	res.simpleIdentifier.value = validatedValue
	return res, nil
}

/*
Builds the identifier with the given value.

Errors can be returned:

- if the given value is illegal for some reason, like including forbidden characters.
*/
func newIntegerIdentifierFromString(s string) (integerIdentifier, error) {
	validatedValue, err := validateIntegerIdentifierFromString(s, false, false)
	if err != nil {
		return integerIdentifier{}, err
	}
	res := integerIdentifier{value: validatedValue}
	res.simpleIdentifier.value = validatedValue
	return res, nil
}

/*
Compares this object with the specified object for order.

Returns a negative integer, zero, or a positive integer as this object is less than,
equal to, or greater than the specified object.
*/
func (ii integerIdentifier) compareTo(o int) int {
	if ii.value < o {
		return o - ii.value
	} else if ii.value == o {
		return 0
	} else {
		return ii.value - o
	}
}

/*
Returns the underlying value held by this identifier.
*/
func (ii integerIdentifier) getIntValue() int {
	return ii.value
}
