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
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

func TestPresetsPresetByName(t *testing.T) {
	simpleConfigurationLayer, err := PresetByName("")
	assert.Error(t, err)
	assert.Nil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName("notapreset")
	assert.Error(t, err)
	assert.Nil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName(SIMPLE_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName(EXTENDED_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)
}

func TestPresetsLoadSimplePresetDirectly(t *testing.T) {
	simpleConfigurationLayer, err := PresetByName(SIMPLE_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)

	changelog, err := simpleConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))

	commitMessageConventions, err := simpleConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	assert.Equal(t, 1, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 1, len(*commitMessageConventions.GetItems()))

	releaseTypes, err := simpleConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 2, len(*releaseTypes.GetItems()))

	services, err := simpleConfigurationLayer.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)

	assert.Equal(t, 0, len(*services))

	substitutions, err := simpleConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	assert.Nil(t, substitutions.GetEnabled())
	assert.Nil(t, substitutions.GetItems())
}

func TestPresetsLoadSimplePresetFromGlobalConfiguration(t *testing.T) {
	configuration, _ := NewConfiguration()
	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	simpleConfigurationLayer.SetPreset(utl.PointerToString(SIMPLE_NAME))
	var scl ConfigurationLayer = simpleConfigurationLayer
	configuration.WithPluginConfiguration(&scl)

	changelog, err := configuration.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))

	commitMessageConventions, err := configuration.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	assert.Equal(t, 1, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 1, len(*commitMessageConventions.GetItems()))

	releaseTypes, err := configuration.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 2, len(*releaseTypes.GetItems()))

	services, err := configuration.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)

	assert.Equal(t, 0, len(*services))

	substitutions, err := configuration.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	assert.Equal(t, 0, len(*substitutions.GetEnabled()))
	assert.Equal(t, 0, len(*substitutions.GetItems()))
}

func TestPresetsLoadExtendedPresetDirectly(t *testing.T) {
	simpleConfigurationLayer, err := PresetByName(EXTENDED_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)

	changelog, err := simpleConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Equal(t, 4, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))

	commitMessageConventions, err := simpleConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	assert.Equal(t, 2, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 3, len(*commitMessageConventions.GetItems()))

	releaseTypes, err := simpleConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 9, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 9, len(*releaseTypes.GetItems()))

	services, err := simpleConfigurationLayer.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)

	assert.Equal(t, 2, len(*services))
	assert.Equal(t, ent.GITHUB, *(*services)["github"].GetType())
	assert.Equal(t, 1, len(*(*services)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, ent.GITLAB, *(*services)["gitlab"].GetType())
	assert.Equal(t, 1, len(*(*services)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])

	substitutions, err := simpleConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	assert.Equal(t, 0, len(*substitutions.GetEnabled()))
	assert.Equal(t, 8, len(*substitutions.GetItems()))
}

func TestPresetsLoadExtendedPresetFromGlobalConfiguration(t *testing.T) {
	configuration, _ := NewConfiguration()
	simpleConfigurationLayer := NewSimpleConfigurationLayer()
	simpleConfigurationLayer.SetPreset(utl.PointerToString(EXTENDED_NAME))
	var scl ConfigurationLayer = simpleConfigurationLayer
	configuration.WithPluginConfiguration(&scl)

	changelog, err := configuration.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Equal(t, 4, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))

	commitMessageConventions, err := configuration.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	assert.Equal(t, 2, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 2, len(*commitMessageConventions.GetItems()))

	releaseTypes, err := configuration.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 9, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 9, len(*releaseTypes.GetItems()))

	services, err := configuration.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)

	assert.Equal(t, 2, len(*services))
	assert.Equal(t, ent.GITHUB, *(*services)["github"].GetType())
	assert.Equal(t, 1, len(*(*services)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, ent.GITLAB, *(*services)["gitlab"].GetType())
	assert.Equal(t, 1, len(*(*services)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])

	substitutions, err := configuration.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	assert.Equal(t, 0, len(*substitutions.GetEnabled()))
	assert.Equal(t, 0, len(*substitutions.GetItems()))
}
