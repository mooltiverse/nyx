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

package version

import (
	"math"    // https://pkg.go.dev/math
	"strconv" // https://pkg.go.dev/strconv
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

var (
	/*
		A fixture with valid structured data to test integer identifiers.
		Each returned argument is a string that must yield to a valid instance of an integer identifier.
	*/
	wellKnownConvertibleValidStringIdentifiers = []struct {
		value string
	}{
		// Strings with digits only
		{value: "0"},
		{value: "1"},
		{value: "123"},
		{value: "123456789"},
		{value: strconv.FormatInt(math.MaxInt64, 10)},
	}

	/*
		A fixture with valid structured data to test integer identifiers.
		Each returned argument is an integer that must yield to a valid instance of an integer identifier.
	*/
	wellKnownValidIntegerIdentifiers = []struct {
		value int
	}{
		// Strings with digits only
		{value: 0},
		{value: 1},
		{value: 123},
		{value: 123456789},
		{value: math.MaxInt64},
	}

	/*
		A fixture with valid structured data to test integer identifiers.
		Each returned argument is a string that must yield to an error
	*/
	wellKnownConvertibleInvalidStringIdentifiers = []struct {
		value string
	}{
		// Numbers with whitespaces
		{value: " 12"},
		{value: "34 "},
		{value: "5 6"},

		// Numbers with dots
		{value: ".12"},
		{value: "34."},
		{value: "5.6"},

		// Numbers with alphanumeric characters
		{value: "a12"},
		{value: "34b"},
		{value: "5c6"},

		// Strings with illegal characters
		{value: "|1"},
		{value: "2\\"},
		{value: "!3"},
		{value: "4\""},
		{value: "£5"},
		{value: "6$"},
		{value: "%7"},
		{value: "8&"},
		{value: "/9"},
		{value: "1("},
		{value: ")2"},
		{value: "3="},
		{value: "?4"},
		{value: "5'"},
		{value: "^6"},
		{value: "7["},
		{value: "]8"},
		{value: "9,"},
		{value: ";1"},
		{value: "2:"},
		{value: "_3"},
		{value: "4*"},
		{value: "+5"},
		{value: "6@"},
		{value: "#7"},
		{value: "8°"},
		{value: "§9"},

		// Negative numbers
		{value: "-12"},
		{value: "-34"},
		{value: "-56"},

		// Numbers with leading zeroes
		{value: "012"},
		{value: "034"},
		{value: "056"},

		// Real numbers,
		{value: "1.2"},
		{value: "123.345"},
		{value: "1,2"},
		{value: "123,345"},
	}

	/*
		A fixture with valid structured data to test integer identifiers.
		Each returned argument is a number that must yield to an error
	*/
	wellKnownInvalidIntegerIdentifiers = []struct {
		value int
	}{
		// Strings with digits only
		{value: -12},
		{value: -34},
		{value: -56},
	}
)

func TestIntegerIdentifierNewIntegerIdentifierFromString(t *testing.T) {
	for _, tc := range wellKnownConvertibleValidStringIdentifiers {
		t.Run(tc.value, func(t *testing.T) {
			_, err := newIntegerIdentifierFromString(tc.value)
			assert.NoError(t, err)
		})
	}

	for _, tc := range wellKnownConvertibleInvalidStringIdentifiers {
		t.Run(tc.value, func(t *testing.T) {
			_, err := newIntegerIdentifierFromString(tc.value)
			assert.Error(t, err)
		})
	}
}

func TestIntegerIdentifierNewIntegerIdentifierFromInteger(t *testing.T) {
	for _, tc := range wellKnownValidIntegerIdentifiers {
		t.Run(strconv.Itoa(tc.value), func(t *testing.T) {
			_, err := newIntegerIdentifierFromInt(tc.value)
			assert.NoError(t, err)
		})
	}

	for _, tc := range wellKnownInvalidIntegerIdentifiers {
		t.Run(strconv.Itoa(tc.value), func(t *testing.T) {
			_, err := newIntegerIdentifierFromInt(tc.value)
			assert.Error(t, err)
		})
	}
}
