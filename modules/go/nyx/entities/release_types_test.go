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

	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestReleaseTypesNewReleaseType(t *testing.T) {
	releaseTypes := NewReleaseTypes()

	// default constructor has its fields set to default values
	assert.NotNil(t, releaseTypes.GetEnabled())
	assert.NotNil(t, releaseTypes.GetItems())
	assert.NotNil(t, releaseTypes.GetPublicationServices())
	assert.Nil(t, releaseTypes.GetRemoteRepositories())
}

func TestReleaseTypeNewReleasesTypeWith(t *testing.T) {
	matchEnvironmentVariables := make(map[string]string)
	matchEnvironmentVariables["PATH"] = ".*"

	identifier1 := NewIdentifierWith(utl.PointerToString("alpha"), utl.PointerToString("any"), PointerToPosition(PRE_RELEASE))
	identifier2 := NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("123"), PointerToPosition(BUILD))
	identifiers := []*Identifier{identifier1, identifier2}

	releaseType := NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), utl.PointerToString("Release description"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{}, &identifiers, utl.PointerToString(""), &matchEnvironmentVariables, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))

	items := make(map[string]*ReleaseType)
	items["one"] = releaseType

	enabled := []*string{utl.PointerToString("one")}
	publicationServices := []*string{utl.PointerToString("aservice")}
	remoteRepositories := []*string{utl.PointerToString("arepo")}

	releaseTypes, err := NewReleaseTypesWith(&enabled, &publicationServices, &remoteRepositories, &items)
	assert.NoError(t, err)

	assert.Equal(t, &enabled, releaseTypes.GetEnabled())
	assert.Equal(t, &publicationServices, releaseTypes.GetPublicationServices())
	assert.Equal(t, &remoteRepositories, releaseTypes.GetRemoteRepositories())
	assert.Equal(t, &items, releaseTypes.GetItems())

	// also test error conditions when nil parameters are passed
	_, err = NewReleaseTypesWith(nil, &publicationServices, &remoteRepositories, &items)
	assert.NotNil(t, err)
	_, err = NewReleaseTypesWith(&enabled, nil, &remoteRepositories, &items)
	assert.NotNil(t, err)
	_, err = NewReleaseTypesWith(&enabled, &publicationServices, &remoteRepositories, nil)
	assert.NotNil(t, err)
}

func TestReleaseTypesGetEnabled(t *testing.T) {
	releaseTypes := NewReleaseTypes()

	enabled := []*string{utl.PointerToString("one")}
	err := releaseTypes.SetEnabled(&enabled)
	assert.NoError(t, err)
	assert.Equal(t, &enabled, releaseTypes.GetEnabled())

	// also test error conditions when nil parameters are passed
	err = releaseTypes.SetEnabled(nil)
	assert.NotNil(t, err)
}

func TestReleaseTypesGetPublicationServices(t *testing.T) {
	releaseTypes := NewReleaseTypes()

	publicationServices := []*string{utl.PointerToString("aservice")}
	err := releaseTypes.SetPublicationServices(&publicationServices)
	assert.NoError(t, err)
	assert.Equal(t, &publicationServices, releaseTypes.GetPublicationServices())

	// also test error conditions when nil parameters are passed
	err = releaseTypes.SetPublicationServices(nil)
	assert.NotNil(t, err)
}

func TestReleaseTypesGetRemoteRepositories(t *testing.T) {
	releaseTypes := NewReleaseTypes()

	remoteRepositories := []*string{utl.PointerToString("arepo")}
	err := releaseTypes.SetRemoteRepositories(&remoteRepositories)
	assert.NoError(t, err)
	assert.Equal(t, &remoteRepositories, releaseTypes.GetRemoteRepositories())
}

func TestReleaseTypesGetItems(t *testing.T) {
	releaseTypes := NewReleaseTypes()

	matchEnvironmentVariables := make(map[string]string)
	matchEnvironmentVariables["PATH"] = ".*"

	identifier1 := NewIdentifierWith(utl.PointerToString("alpha"), utl.PointerToString("any"), PointerToPosition(PRE_RELEASE))
	identifier2 := NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("123"), PointerToPosition(BUILD))
	identifiers := []*Identifier{identifier1, identifier2}

	releaseType := NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), utl.PointerToString("Release description"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{}, &identifiers, utl.PointerToString(""), &matchEnvironmentVariables, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))

	items := make(map[string]*ReleaseType)
	items["one"] = releaseType

	err := releaseTypes.SetItems(&items)
	assert.NoError(t, err)
	assert.Equal(t, &items, releaseTypes.GetItems())

	// also test error conditions when nil parameters are passed
	err = releaseTypes.SetItems(nil)
	assert.NotNil(t, err)
}
