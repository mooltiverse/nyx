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

package entities

import (
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestProviderString(t *testing.T) {
	assert.Equal(t, "GITHUB", GITHUB.String())
	assert.Equal(t, "GITLAB", GITLAB.String())
}

func TestProviderValueOfProvider(t *testing.T) {
	provider, err := ValueOfProvider("GITHUB")
	assert.NoError(t, err)
	assert.Equal(t, GITHUB, provider)
	provider, err = ValueOfProvider("GITLAB")
	assert.NoError(t, err)
	assert.Equal(t, GITLAB, provider)
}
