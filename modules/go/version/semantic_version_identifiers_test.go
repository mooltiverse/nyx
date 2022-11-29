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
	"sort"    // https://pkg.go.dev/sort
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestSemanticVersionIdentifiersComparator(t *testing.T) {
	identifiers := []string{
		"alpha",
		"beta",
		"minor",
		"patch",
		"gamma",
		"gamma",
		"minor",
		"major",
		"theta",
		"patch",
		"major",
		"epsylon",
	}

	sort.Sort(semanticVersionIdentifiers(identifiers))
	assert.Equal(t, "major", identifiers[0])
	assert.Equal(t, "major", identifiers[1])
	assert.Equal(t, "minor", identifiers[2])
	assert.Equal(t, "minor", identifiers[3])
	assert.Equal(t, "patch", identifiers[4])
	assert.Equal(t, "patch", identifiers[5])
	assert.Equal(t, "alpha", identifiers[6])
	assert.Equal(t, "beta", identifiers[7])
	assert.Equal(t, "epsylon", identifiers[8])
	assert.Equal(t, "gamma", identifiers[9])
	assert.Equal(t, "gamma", identifiers[10])
	assert.Equal(t, "theta", identifiers[11])
}
