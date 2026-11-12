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
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
)

/*
Returns all the release types defined as presets, by name.
*/
func presetReleaseTypes() map[string]*ent.ReleaseType {
	return map[string]*ent.ReleaseType{
		"feature":     RELEASE_TYPES_FEATURE,
		"fix":         RELEASE_TYPES_FIX,
		"hotfix":      RELEASE_TYPES_HOTFIX,
		"integration": RELEASE_TYPES_INTEGRATION,
		"internal":    RELEASE_TYPES_INTERNAL,
		"mainline":    RELEASE_TYPES_MAINLINE,
		"maintenance": RELEASE_TYPES_MAINTENANCE,
		"maturity":    RELEASE_TYPES_MATURITY,
		"release":     RELEASE_TYPES_RELEASE,
	}
}

/*
Preset release types must use the default tag names, like the Java presets do, otherwise they never tag
(see https://github.com/mooltiverse/nyx/issues/443).
*/
func TestPresetsReleaseTypesUseDefaultGitTagNames(t *testing.T) {
	for name, releaseType := range presetReleaseTypes() {
		t.Run(name, func(t *testing.T) {
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_NAMES, releaseType.GetGitTagNames())
		})
	}
}

/*
The preset release types are shared by all the presets using them so they must be loaded through the presets too.
*/
func TestPresetsReleaseTypesUseDefaultGitTagNamesThroughPresets(t *testing.T) {
	for _, presetName := range []string{SIMPLE_NAME, EXTENDED_NAME} {
		t.Run(presetName, func(t *testing.T) {
			preset, err := PresetByName(presetName)
			assert.NoError(t, err)
			releaseTypes, err := preset.GetReleaseTypes()
			assert.NoError(t, err)
			for name, releaseType := range *releaseTypes.GetItems() {
				assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_NAMES, releaseType.GetGitTagNames(), "release type '%s'", name)
			}
		})
	}
}
