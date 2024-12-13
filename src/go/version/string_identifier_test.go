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
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

var (
	/*
		A fixture with valid structured data to test string identifiers.
		Each returned argument is a string that must yield to a valid instance of an string identifier.
	*/
	wellKnownValidStringIdentifiers = []struct {
		value string
	}{
		// Strings with digits only
		{value: "0"},
		{value: "1"},
		{value: "123"},
		{value: "123456789"},

		// Strings with alphanumeric characters only
		{value: "a"},
		{value: "A"},
		{value: "aB"},
		{value: "bA"},
		{value: "alpha"},
		{value: "build"},
		{value: "snapshot"},
		{value: "BETA"},
		{value: "gAMma"},
		{value: "Delta"},

		// Strings with digits and alphanumeric characters
		{value: "a1"},
		{value: "2A"},
		{value: "a3B"},
		{value: "b45A"},
		{value: "alpha012"},
		{value: "34build"},

		// Strings with dashes only are allowed by spec
		{value: "-"},
		{value: "--"},
		{value: "---"},

		// Strings with mixture of characters, ingluding dash
		{value: "a-"},
		{value: "-A"},
		{value: "a-B"},
		{value: "1bA"},
		{value: "alpha2"},
		{value: "build-2"},
		{value: "3-snapshot"},
		{value: "BETA-GAMMA"},
		{value: "gAMma-3-alpha"},
		{value: "-Delta"},
	}

	/*
		A fixture with valid structured data to test string identifiers.
		Each returned argument is a string that must yield to an error
	*/
	wellKnownInvalidStringIdentifiers = []struct {
		value string
	}{
		// Strings with whitespaces
		{value: " aB"},
		{value: "bA "},
		{value: "a lp ha"},
		{value: "bui ld"},
		{value: "sn apsh ot"},
		{value: "BET A"},
		{value: "gA Mma"},
		{value: "Del ta"},

		// Strings with dots
		{value: "."},
		{value: ".a"},
		{value: "a."},
		{value: "a.a"},

		// Strings with illegal characters
		{value: "|"},
		{value: "\\"},
		{value: "!"},
		{value: "\""},
		{value: "£"},
		{value: "$"},
		{value: "%"},
		{value: "&"},
		{value: "/"},
		{value: "("},
		{value: ")"},
		{value: "="},
		{value: "?"},
		{value: "'"},
		{value: "^"},
		{value: "["},
		{value: "]"},
		{value: ","},
		{value: ";"},
		{value: ":"},
		{value: "_"},
		{value: "*"},
		{value: "+"},
		{value: "@"},
		{value: "#"},
		{value: "°"},
		{value: "§"},

		// Strings with accents
		{value: "ì"},
		{value: "è"},
		{value: "é"},
		{value: "ò"},

		// And mixtures
		{value: "òlpha"},
		{value: "béta"},
		{value: "status:"},
		{value: "good?"},
		{value: "!%/)?=(&)(£?"},
	}
)

func TestStringIdentifierNewStringIdentifier(t *testing.T) {
	for _, tc := range wellKnownValidStringIdentifiers {
		t.Run(tc.value, func(t *testing.T) {
			_, err := newStringIdentifier(tc.value)
			assert.NoError(t, err)
		})
	}

	for _, tc := range wellKnownInvalidStringIdentifiers {
		t.Run(tc.value, func(t *testing.T) {
			_, err := newStringIdentifier(tc.value)
			assert.Error(t, err)
		})
	}
}
