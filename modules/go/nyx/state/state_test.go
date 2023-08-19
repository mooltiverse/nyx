//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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
package state

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strconv"       // https://pkg.go.dev/strconv
	"strings"       // https://pkg.go.dev/strings
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
	io "github.com/mooltiverse/nyx/modules/go/nyx/io"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestStateConstructor(t *testing.T) {
	// passing the nil argument must throw an error

	_, err := NewStateWith(nil)
	assert.Error(t, err)

	configuration, err := cnf.NewConfiguration()

	_, err = NewStateWith(configuration)
	assert.NoError(t, err)
}

func TestStateGetBranch(t *testing.T) {
	// make sure the branch is nil in the beginning (it's set only after the Infer task has run)
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)

	branch, err := state.GetBranch()
	assert.NoError(t, err)
	assert.Nil(t, branch)

	state.SetBranch(utl.PointerToString("abranch"))
	branch, err = state.GetBranch()
	assert.NoError(t, err)
	assert.Equal(t, "abranch", *branch)

	state.SetBranch(utl.PointerToString("anotherbranch"))
	branch, err = state.GetBranch()
	assert.NoError(t, err)
	assert.Equal(t, "anotherbranch", *branch)

}

func TestStateSetBranch(t *testing.T) {
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)

	state.SetBranch(utl.PointerToString("abranch"))
	branch, err := state.GetBranch()
	assert.NoError(t, err)
	assert.Equal(t, "abranch", *branch)
}

func TestStateGetBump(t *testing.T) {
	// make sure the bump is nil in the beginning (it's set only after the Infer task has run)
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)

	bump, err := state.GetBump()
	assert.Nil(t, bump)

	state.SetBump(utl.PointerToString("alpha"))
	bump, err = state.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "alpha", *bump)
}

func TestStateGetBumpOverrideByConfiguration(t *testing.T) {
	configuration, err := cnf.NewConfiguration()
	assert.NoError(t, err)
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetBump(utl.PointerToString("gamma"))
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)

	bump, err := state.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "gamma", *bump)
}

func TestStateSetBump(t *testing.T) {
	// make sure the bump is nil in the beginning (it's set only after the Infer task has run)
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)
	bump, err := state.GetBump()
	assert.NoError(t, err)
	assert.Nil(t, bump)

	state.SetBump(utl.PointerToString("alpha"))
	bump, err = state.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "alpha", *bump)
}

func TestStateSetBumpOverrideByConfiguration(t *testing.T) {
	configuration, err := cnf.NewConfiguration()
	assert.NoError(t, err)
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetBump(utl.PointerToString("gamma"))
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)

	bump, err := state.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "gamma", *bump)
	err = state.SetBump(utl.PointerToString("any value"))
	assert.Error(t, err)

}

func TestStateGetChangelog(t *testing.T) {
	// make sure the changelog is nil in the beginning (it's set only after the Infer task has run)
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)
	changelog, err := state.GetChangelog()
	assert.Nil(t, changelog)

	changelog1 := ent.NewChangelog()
	state.SetChangelog(changelog1)
	changelog2, err := state.GetChangelog()
	assert.NotNil(t, changelog2)
	assert.Equal(t, changelog1, changelog2)
}

func TestStateSetChangelog(t *testing.T) {
	// make sure the changelog is nil in the beginning (it's set only after the Infer task has run)
	configuration, err := cnf.NewConfiguration()
	state, err := NewStateWith(configuration)
	assert.NoError(t, err)
	changelog, err := state.GetChangelog()
	assert.Nil(t, changelog)

	changelog1 := ent.NewChangelog()
	state.SetChangelog(changelog1)
	changelog2, err := state.GetChangelog()
	assert.NotNil(t, changelog2)
	assert.Equal(t, changelog1, changelog2)
}

func TestStateGetConfiguration(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	assert.Equal(t, configuration, state.GetConfiguration())
}

func TestStateGetCoreVersion(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	configurationLayerMock.SetReleasePrefix(nil)
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	// try with no leniency or prefix
	state.SetVersion(utl.PointerToString("1.2.3"))
	coreVersion, err := state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("v1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("rel-1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-1"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	// try with leniency but no prefix
	configuration, _ = cnf.NewConfiguration()
	configurationLayerMock = cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))
	configurationLayerMock.SetReleasePrefix(nil)
	cl = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ = NewStateWith(configuration)

	state.SetVersion(utl.PointerToString("1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("v1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("rel-1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-1"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	// try with no leniency but a prefix
	configuration, _ = cnf.NewConfiguration()
	configurationLayerMock = cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	configurationLayerMock.SetReleasePrefix(utl.PointerToString("v"))
	cl = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ = NewStateWith(configuration)

	state.SetVersion(utl.PointerToString("1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("v1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("rel-1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-1"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	// try with leniency and a prefix
	configuration, _ = cnf.NewConfiguration()
	configurationLayerMock = cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))
	configurationLayerMock.SetReleasePrefix(utl.PointerToString("v"))
	cl = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ = NewStateWith(configuration)

	state.SetVersion(utl.PointerToString("1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("v1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("rel-1.2.3"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.True(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-1"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3-alpha+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)

	state.SetVersion(utl.PointerToString("1.2.3+build"))
	coreVersion, err = state.GetCoreVersion()
	assert.NoError(t, err)
	assert.False(t, coreVersion)
}

func TestStateGetDirectory(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	dir1, _ := configuration.GetDirectory()
	dir2, _ := state.GetDirectory()

	// make sure the state directory is the same from the configuration
	assert.Equal(t, *dir1, *dir2)
}

func TestStateGetInternals(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	// make sure the initial internals is never nil and empty
	internals, _ := state.GetInternals()
	assert.NotNil(t, internals)
	assert.Equal(t, 0, len(*internals))
}

func TestStateGetLatestVersion(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	// nomatter what we set, the latest version is always null until we set a version on the state
	latestVersion, err := state.GetLatestVersion()
	assert.NoError(t, err)
	assert.Nil(t, latestVersion)

	state.SetLatestVersion(utl.PointerToBoolean(true))
	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.Nil(t, latestVersion)

	state.SetLatestVersion(utl.PointerToBoolean(false))
	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.Nil(t, latestVersion)

	// try again, with a version set on the state
	state, _ = NewStateWith(configuration)
	state.SetVersion(utl.PointerToString("1.2.3"))

	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.Nil(t, latestVersion)

	state.SetLatestVersion(utl.PointerToBoolean(true))
	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.True(t, *latestVersion)

	state.SetLatestVersion(utl.PointerToBoolean(false))
	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.False(t, *latestVersion)

	state.SetLatestVersion(nil)
	latestVersion, err = state.GetLatestVersion()
	assert.NoError(t, err)
	assert.Nil(t, latestVersion)
}

func TestStateGetNewRelease(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	// inject a releaseType with the 'publish' flag to TRUE
	state.SetReleaseType(ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), nil, nil, nil, utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, &[]*string{}, nil, nil, nil, nil /*this is the 'publish' flag -> */, utl.PointerToString("true"), nil, nil, utl.PointerToBoolean(false)))
	state.SetVersion(utl.PointerToString("1.2.3"))
	releaseScope, _ := state.GetReleaseScope()
	releaseScope.SetPreviousVersion(utl.PointerToString("1.2.3"))
	newVersion, _ := state.GetNewVersion()
	newRelease, _ := state.GetNewRelease()
	assert.False(t, newVersion)
	assert.False(t, newRelease)

	releaseScope, _ = state.GetReleaseScope()
	releaseScope.SetPreviousVersion(utl.PointerToString("0.1.0"))
	newVersion, _ = state.GetNewVersion()
	newRelease, _ = state.GetNewRelease()
	assert.True(t, newVersion)
	assert.True(t, newRelease)

	// now replace the releaseType with the 'publish' flag to FALSE
	state.SetReleaseType(ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), nil, nil, nil, utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, &[]*string{}, nil, nil, nil, nil /*this is the 'publish' flag -> */, utl.PointerToString("false"), nil, nil, utl.PointerToBoolean(false)))

	releaseScope, _ = state.GetReleaseScope()
	releaseScope.SetPreviousVersion(utl.PointerToString("0.1.0"))
	newVersion, _ = state.GetNewVersion()
	newRelease, _ = state.GetNewRelease()
	assert.True(t, newVersion)
	assert.False(t, newRelease)
}

func TestStateGetNewVersion(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	releaseScope, _ := state.GetReleaseScope()
	newVersion, _ := state.GetNewVersion()
	state.SetVersion(utl.PointerToString("1.2.3"))
	releaseScope.SetPreviousVersion(utl.PointerToString("1.2.3"))
	assert.False(t, newVersion)

	releaseScope.SetPreviousVersion(utl.PointerToString("0.1.0"))
	newVersion, _ = state.GetNewVersion()
	assert.True(t, newVersion)
}

func TestStateGetReleaseAssets(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	ra := make([]ent.Attachment, 2)
	ra[0] = *ent.NewAttachmentWith(utl.PointerToString("asset.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("asset.txt"), utl.PointerToString("text/plain"))
	ra[1] = *ent.NewAttachmentWith(utl.PointerToString("asset.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("asset.bin"), utl.PointerToString("application/octet-stream"))

	releaseAssets, _ := state.GetReleaseAssets()
	assert.Equal(t, 0, len(*releaseAssets))

	state.SetReleaseAssets(&ra)
	releaseAssets, _ = state.GetReleaseAssets()
	assert.Equal(t, 2, len(*releaseAssets))
}

func TestStateGetReleaseScope(t *testing.T) {
	// make sure the release scope is initialized
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	releaseScope, _ := state.GetReleaseScope()
	assert.NotNil(t, releaseScope)
}

func TestStateGetReleaseType(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	// make sure the release type is nil until it has been explicitly set
	releaseType, _ := state.GetReleaseType()
	assert.Nil(t, releaseType)
	releaseType1 := ent.NewReleaseType()
	state.SetReleaseType(releaseType1)
	releaseType2, _ := state.GetReleaseType()
	assert.NotNil(t, releaseType2)
	assert.Equal(t, *releaseType1, *releaseType2)
}

func TestStateGetScheme(t *testing.T) {
	// make sure the scheme is the same from the configuration
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	scheme1, _ := configuration.GetScheme()
	scheme2, _ := state.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)
}

func TestStateGetTimestamp(t *testing.T) {
	// make sure the current timestamp is a fresh one
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	timestamp, _ := state.GetTimestamp()
	assert.True(t, time.Now().UnixMilli() >= *timestamp)
}

func TestStateTouchTimestamp(t *testing.T) {
	// make sure that when touching the timestamp the new value is updated
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	oldTimestamp, _ := state.GetTimestamp()
	for ok := true; ok; ok = *oldTimestamp == time.Now().UnixMilli() {
		// just do nothing and let at least 1 millisecond pass
	}

	touchTimestamp := state.TouchTimestamp()
	newTimestamp, _ := state.GetTimestamp()
	assert.Equal(t, *touchTimestamp, *newTimestamp)

	assert.NotEqual(t, *oldTimestamp, *newTimestamp)
}

func TestStateGetVersion(t *testing.T) {
	// make sure the version is nil in the beginning (it's set only after the Infer task has run)
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	version, _ := state.GetVersion()
	assert.Nil(t, version)

	state.SetVersion(utl.PointerToString("1.2.3"))
	version, _ = state.GetVersion()
	assert.Equal(t, "1.2.3", *version)

	state.SetVersion(utl.PointerToString("v1.2.3"))
	version, _ = state.GetVersion()
	assert.Equal(t, "v1.2.3", *version)
}

func TestStateGetVersionOverrideByConfiguration(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.2.3", *version)
}

func TestStateGetVersionBuildMetadata(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	versionBuildMetadata, _ := state.GetVersionBuildMetadata()
	assert.Nil(t, versionBuildMetadata)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3-alpha.5+build.123"))
	versionBuildMetadata, _ = state.GetVersionBuildMetadata()
	assert.Equal(t, "build.123", *versionBuildMetadata)
}

func TestStateGetVersionMajorNumber(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	versionMajorNumber, _ := state.GetVersionMajorNumber()
	assert.Equal(t, "1", *versionMajorNumber)
}

func TestStateGetVersionMinorNumber(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	versionMinorNumber, _ := state.GetVersionMinorNumber()
	assert.Equal(t, "2", *versionMinorNumber)
}

func TestStateGetVersionPatchNumber(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	versionPatchNumber, _ := state.GetVersionPatchNumber()
	assert.Equal(t, "3", *versionPatchNumber)
}

func TestStateGetVersionPreReleaseIdentifier(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	versionPreReleaseIdentifier, _ := state.GetVersionPreReleaseIdentifier()
	assert.Nil(t, versionPreReleaseIdentifier)

	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3-alpha.5+build.123"))
	versionPreReleaseIdentifier, _ = state.GetVersionPreReleaseIdentifier()
	assert.Equal(t, "alpha.5", *versionPreReleaseIdentifier)
}

func TestStateSetVersion(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	state.SetVersion(utl.PointerToString("1.2.3"))
	version, _ := state.GetVersion()
	assert.Equal(t, "1.2.3", *version)
}

func TestStateSetVersionOverrideByConfiguration(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	state, _ := NewStateWith(configuration)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.2.3", *version)

	err := state.SetVersion(utl.PointerToString("1.2.3"))
	assert.Error(t, err)
}

func TestStateGetVersionRange(t *testing.T) {
	// make sure the version is nil in the beginning (it's set only after the Infer task has run)
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	versionRange, _ := state.GetVersionRange()
	assert.Nil(t, versionRange)

	state.SetVersionRange(utl.PointerToString(".*"))
	versionRange, _ = state.GetVersionRange()
	assert.Equal(t, ".*", *versionRange)

	state.SetVersionRange(utl.PointerToString("v(.*)"))
	versionRange, _ = state.GetVersionRange()
	assert.Equal(t, "v(.*)", *versionRange)
}

func TestStateSetVersionRange(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	state.SetVersionRange(utl.PointerToString(".*"))
	versionRange, _ := state.GetVersionRange()
	assert.Equal(t, ".*", *versionRange)
}

/*
Save and resume
*/
func TestStateSaveAndResumeJSON(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")}, &map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	configurationLayerMock.SetResume(utl.PointerToBoolean(true))
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	stateFile, _ := os.Create(filepath.Join(tempDir, "state"+fmt.Sprintf("%p", t)+".json"))
	stateFileName := stateFile.Name()
	configurationLayerMock.SetStateFile(&stateFileName)
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	oldState, _ := NewStateWith(configuration)

	initialCommit := gitent.NewCommitWith("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, nil, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewMessageWith("initial commit", "initial commit", nil), nil)
	finalCommit := gitent.NewCommitWith("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, []string{"b50926577d36f403f4b3ebf51dfe34660b52eaa2"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewMessageWith("final commit", "final commit", nil), nil)

	// set a few values to use later on for comparison
	release := ent.NewReleaseWith(utl.PointerToString("MyRelease"), utl.PointerToString("today"))
	release.SetSections([]*ent.Section{ent.NewSectionWith(utl.PointerToString("MySection"), nil)})
	changelog := ent.NewChangelog()
	changelog.SetReleases([]*ent.Release{release})
	oldState.SetChangelog(changelog)
	oldState.SetVersion(utl.PointerToString("3.5.7"))
	oldState.SetVersionRange(utl.PointerToString(".*"))
	internals, _ := oldState.GetInternals()
	(*internals)["attr1"] = "value1"
	releaseScope, _ := oldState.GetReleaseScope()
	commits := releaseScope.GetCommits()
	commits = append(commits, finalCommit)
	commits = append(commits, initialCommit)
	releaseScope.SetCommits(commits)

	releaseScope.SetPreviousVersion(utl.PointerToString("4.5.6"))
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, []string{"c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewMessageWith("fix: a commit that fixes something", "fix: a commit that fixes something", nil), []gitent.Tag{*gitent.NewTagWith("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false)}))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, []string{"2e348e90e5e1b89c678555459aecbfc34e17ef44"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewMessageWith("feat: a commit that adds a feature", "feat: a commit that adds a feature", nil), []gitent.Tag{*gitent.NewTagWith("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false)}))

	releaseType := ent.NewReleaseType()
	releaseType.SetCollapseVersions(utl.PointerToBoolean(true))
	releaseType.SetCollapsedVersionQualifier(utl.PointerToString("rel"))
	releaseType.SetDescription(utl.PointerToString("Some description"))
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitCommitMessage(utl.PointerToString("Commit message"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetGitTagMessage(utl.PointerToString("Tag message"))
	releaseType.SetGitTagNames(&[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")})
	releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("b"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))})
	releaseType.SetMatchBranches(utl.PointerToString(".*"))
	releaseType.SetMatchEnvironmentVariables(&map[string]string{"USER": ".*", "PATH": ".*"})
	releaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseType.SetVersionRange(utl.PointerToString("1.x"))
	releaseType.SetVersionRangeFromBranchName(utl.PointerToBoolean(false))
	oldState.SetReleaseType(releaseType)

	// save the file
	err := io.Save(stateFileName, oldState)
	_, err = os.Stat(stateFileName)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Println("------ JSON state ------")
	//fmt.Println("Loading from: " + stateFileName)
	//fmt.Println("-----------------------------------------")
	//fileContent, _ := ioutil.ReadFile(stateFileName)
	//fmt.Println(string(fileContent))
	//fmt.Println("-----------------------------------------")

	// now we are ready to resume the file
	resumedState, err := Resume(stateFileName, configuration)
	assert.NoError(t, err)

	bump1, _ := oldState.GetBump()
	bump2, _ := resumedState.GetBump()
	assert.Nil(t, bump1)
	assert.Equal(t, bump1, bump2)

	changelog1, _ := oldState.GetChangelog()
	changelog2, _ := resumedState.GetChangelog()
	assert.NotNil(t, changelog1)
	assert.NotNil(t, changelog2)
	assert.Equal(t, 1, len(changelog2.GetReleases()))
	assert.Equal(t, "MyRelease", *(*changelog2.GetReleases()[0]).GetName())
	assert.Equal(t, "today", *(*changelog2.GetReleases()[0]).GetDate())
	assert.Equal(t, 1, len((*changelog2.GetReleases()[0]).GetSections()))
	assert.Equal(t, "MySection", *(*changelog2.GetReleases()[0]).GetSections()[0].GetName())

	internals1, _ := oldState.GetInternals()
	internals2, _ := resumedState.GetInternals()
	assert.Equal(t, *internals1, *internals2)
	assert.Equal(t, "value1", (*internals2)["attr1"])

	releaseScope1, _ := oldState.GetReleaseScope()
	releaseScope2, _ := resumedState.GetReleaseScope()
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetSHA(), (*releaseScope2.GetFinalCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction(), (*releaseScope2.GetFinalCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetDate(), (*releaseScope2.GetFinalCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage(), (*releaseScope2.GetFinalCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetFooters(), (*releaseScope2.GetFinalCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetFinalCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetFinalCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetParents(), (*releaseScope2.GetFinalCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetTags(), (*releaseScope2.GetFinalCommit()).GetTags())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetSHA(), (*releaseScope2.GetInitialCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction(), (*releaseScope2.GetInitialCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetDate(), (*releaseScope2.GetInitialCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage(), (*releaseScope2.GetInitialCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetFooters(), (*releaseScope2.GetInitialCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetInitialCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetInitialCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetParents(), (*releaseScope2.GetInitialCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetTags(), (*releaseScope2.GetInitialCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPreviousVersion(), *releaseScope2.GetPreviousVersion())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetSHA(), (*releaseScope2.GetPreviousVersionCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetDate(), (*releaseScope2.GetPreviousVersionCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetFooters(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetParents(), (*releaseScope2.GetPreviousVersionCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetTags(), (*releaseScope2.GetPreviousVersionCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetSHA(), (*releaseScope2.GetPrimeVersionCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetDate(), (*releaseScope2.GetPrimeVersionCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetFooters(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetParents(), (*releaseScope2.GetPrimeVersionCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetTags(), (*releaseScope2.GetPrimeVersionCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())

	timestamp1, _ := oldState.GetTimestamp()
	timestamp2, _ := resumedState.GetTimestamp()
	assert.Equal(t, *timestamp1, *timestamp2)

	version1, _ := oldState.GetVersion()
	version2, _ := resumedState.GetVersion()
	assert.Equal(t, *version1, *version2)

	versionMajorNumber1, _ := oldState.GetVersionMajorNumber()
	versionMajorNumber2, _ := resumedState.GetVersionMajorNumber()
	assert.Equal(t, *versionMajorNumber1, *versionMajorNumber2)

	versionMinorNumber1, _ := oldState.GetVersionMinorNumber()
	versionMinorNumber2, _ := resumedState.GetVersionMinorNumber()
	assert.Equal(t, *versionMinorNumber1, *versionMinorNumber2)

	versionPatchNumber1, _ := oldState.GetVersionPatchNumber()
	versionPatchNumber2, _ := resumedState.GetVersionPatchNumber()
	assert.Equal(t, *versionPatchNumber1, *versionPatchNumber2)

	versionBuildMetadata1, _ := oldState.GetVersionBuildMetadata()
	versionBuildMetadata2, _ := resumedState.GetVersionBuildMetadata()
	assert.Nil(t, versionBuildMetadata1)
	assert.Nil(t, versionBuildMetadata2)

	versionPreReleaseIdentifier1, _ := oldState.GetVersionPreReleaseIdentifier()
	versionPreReleaseIdentifier2, _ := resumedState.GetVersionPreReleaseIdentifier()
	assert.Nil(t, versionPreReleaseIdentifier1)
	assert.Nil(t, versionPreReleaseIdentifier2)

	versionRange1, _ := oldState.GetVersionRange()
	versionRange2, _ := resumedState.GetVersionRange()
	assert.Equal(t, *versionRange1, *versionRange2)

	releaseType1, _ := oldState.GetReleaseType()
	releaseType2, _ := resumedState.GetReleaseType()
	assert.Equal(t, *releaseType1.GetCollapseVersions(), *releaseType2.GetCollapseVersions())
	assert.Equal(t, *releaseType1.GetCollapsedVersionQualifier(), *releaseType2.GetCollapsedVersionQualifier())
	assert.Equal(t, *releaseType1.GetDescription(), *releaseType2.GetDescription())
	assert.Equal(t, *releaseType1.GetGitCommit(), *releaseType2.GetGitCommit())
	assert.Equal(t, *releaseType1.GetGitCommitMessage(), *releaseType2.GetGitCommitMessage())
	assert.Equal(t, *releaseType1.GetGitPush(), *releaseType2.GetGitPush())
	assert.Equal(t, *releaseType1.GetGitTag(), *releaseType2.GetGitTag())
	assert.Equal(t, *releaseType1.GetGitTagMessage(), *releaseType2.GetGitTagMessage())
	if releaseType1.GetGitTagNames() == nil {
		assert.Equal(t, *releaseType1.GetGitTagNames(), *releaseType2.GetGitTagNames())
	} else {
		for i, name := range *releaseType1.GetGitTagNames() {
			assert.Equal(t, *name, *(*releaseType2.GetGitTagNames())[i])
		}
	}
	if releaseType1.GetIdentifiers() == nil {
		assert.Equal(t, *releaseType1.GetIdentifiers(), *releaseType2.GetIdentifiers())
	} else {
		for i, identifier := range *releaseType1.GetIdentifiers() {
			assert.Equal(t, identifier.GetQualifier(), (*releaseType2.GetIdentifiers())[i].GetQualifier())
			assert.Equal(t, identifier.GetValue(), (*releaseType2.GetIdentifiers())[i].GetValue())
			assert.Equal(t, identifier.GetPosition(), (*releaseType2.GetIdentifiers())[i].GetPosition())
		}
	}
	assert.Equal(t, *releaseType1.GetMatchBranches(), *releaseType2.GetMatchBranches())
	assert.Equal(t, *releaseType1.GetMatchEnvironmentVariables(), *releaseType2.GetMatchEnvironmentVariables())
	assert.Equal(t, *releaseType1.GetMatchWorkspaceStatus(), *releaseType2.GetMatchWorkspaceStatus())
	assert.Equal(t, *releaseType1.GetPublish(), *releaseType2.GetPublish())
	assert.Equal(t, *releaseType1.GetVersionRange(), *releaseType2.GetVersionRange())
	assert.Equal(t, *releaseType1.GetGitTagMessage(), *releaseType2.GetGitTagMessage())
	assert.Equal(t, *releaseType1.GetVersionRangeFromBranchName(), *releaseType2.GetVersionRangeFromBranchName())

	// finally also test transient attributes, which should render to the same value even if they are computed on the fly
	directory1, _ := oldState.GetDirectory()
	directory2, _ := resumedState.GetDirectory()
	assert.Equal(t, *directory1, *directory2)

	coreVersion1, _ := oldState.GetCoreVersion()
	coreVersion2, _ := resumedState.GetCoreVersion()
	assert.Equal(t, coreVersion1, coreVersion2)

	latestVersion1, _ := oldState.GetLatestVersion()
	latestVersion2, _ := resumedState.GetLatestVersion()
	assert.Equal(t, latestVersion1, latestVersion2)

	newVersion1, _ := oldState.GetNewVersion()
	newVersion2, _ := resumedState.GetNewVersion()
	assert.Equal(t, newVersion1, newVersion2)

	newRelease1, _ := oldState.GetNewRelease()
	newRelease2, _ := resumedState.GetNewRelease()
	assert.Equal(t, newRelease1, newRelease2)

	scheme1, _ := oldState.GetScheme()
	scheme2, _ := resumedState.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)
}

func TestStateSaveAndResumeYAML(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")}, &map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	configurationLayerMock.SetResume(utl.PointerToBoolean(true))
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	stateFile, _ := os.Create(filepath.Join(tempDir, "state"+fmt.Sprintf("%p", t)+".yaml"))
	stateFileName := stateFile.Name()
	configurationLayerMock.SetStateFile(&stateFileName)
	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)
	oldState, _ := NewStateWith(configuration)

	initialCommit := gitent.NewCommitWith("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, nil, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewMessageWith("initial commit", "initial commit", nil), nil)
	finalCommit := gitent.NewCommitWith("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, []string{"b50926577d36f403f4b3ebf51dfe34660b52eaa2"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewMessageWith("final commit", "final commit", nil), nil)

	// set a few values to use later on for comparison
	release := ent.NewReleaseWith(utl.PointerToString("MyRelease"), utl.PointerToString("today"))
	release.SetSections([]*ent.Section{ent.NewSectionWith(utl.PointerToString("MySection"), nil)})
	changelog := ent.NewChangelog()
	changelog.SetReleases([]*ent.Release{release})
	oldState.SetChangelog(changelog)
	oldState.SetVersion(utl.PointerToString("3.5.7"))
	oldState.SetVersionRange(utl.PointerToString(".*"))
	internals, _ := oldState.GetInternals()
	(*internals)["attr1"] = "value1"
	releaseScope, _ := oldState.GetReleaseScope()
	commits := releaseScope.GetCommits()
	commits = append(commits, finalCommit)
	commits = append(commits, initialCommit)
	releaseScope.SetCommits(commits)

	releaseScope.SetPreviousVersion(utl.PointerToString("4.5.6"))
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, []string{"c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(0))), *gitent.NewMessageWith("fix: a commit that fixes something", "fix: a commit that fixes something", nil), []gitent.Tag{*gitent.NewTagWith("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false)}))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, []string{"2e348e90e5e1b89c678555459aecbfc34e17ef44"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampWithIn(time.Now().UnixMilli(), utl.PointerToInt(-120))), *gitent.NewMessageWith("feat: a commit that adds a feature", "feat: a commit that adds a feature", nil), []gitent.Tag{*gitent.NewTagWith("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false)}))

	releaseType := ent.NewReleaseType()
	releaseType.SetCollapseVersions(utl.PointerToBoolean(true))
	releaseType.SetCollapsedVersionQualifier(utl.PointerToString("rel"))
	releaseType.SetDescription(utl.PointerToString("Some description"))
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitCommitMessage(utl.PointerToString("Commit message"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetGitTagMessage(utl.PointerToString("Tag message"))
	releaseType.SetGitTagNames(&[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")})
	releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("b"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))})
	releaseType.SetMatchBranches(utl.PointerToString(".*"))
	releaseType.SetMatchEnvironmentVariables(&map[string]string{"USER": ".*", "PATH": ".*"})
	releaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseType.SetVersionRange(utl.PointerToString("1.x"))
	releaseType.SetVersionRangeFromBranchName(utl.PointerToBoolean(false))
	oldState.SetReleaseType(releaseType)

	// save the file
	err := io.Save(stateFileName, oldState)
	_, err = os.Stat(stateFileName)
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Println("------ YAML state ------")
	//fmt.Println("Loading from: " + stateFileName)
	//fmt.Println("-----------------------------------------")
	//fileContent, _ := ioutil.ReadFile(stateFileName)
	//fmt.Println(string(fileContent))
	//fmt.Println("-----------------------------------------")

	// now we are ready to resume the file
	resumedState, err := Resume(stateFileName, configuration)
	assert.NoError(t, err)

	bump1, _ := oldState.GetBump()
	bump2, _ := resumedState.GetBump()
	assert.Nil(t, bump1)
	assert.Equal(t, bump1, bump2)

	changelog1, _ := oldState.GetChangelog()
	changelog2, _ := resumedState.GetChangelog()
	assert.NotNil(t, changelog1)
	assert.NotNil(t, changelog2)
	assert.Equal(t, 1, len(changelog2.GetReleases()))
	assert.Equal(t, "MyRelease", *(*changelog2.GetReleases()[0]).GetName())
	assert.Equal(t, "today", *(*changelog2.GetReleases()[0]).GetDate())
	assert.Equal(t, 1, len((*changelog2.GetReleases()[0]).GetSections()))
	assert.Equal(t, "MySection", *(*changelog2.GetReleases()[0]).GetSections()[0].GetName())

	internals1, _ := oldState.GetInternals()
	internals2, _ := resumedState.GetInternals()
	assert.Equal(t, *internals1, *internals2)
	assert.Equal(t, "value1", (*internals2)["attr1"])

	releaseScope1, _ := oldState.GetReleaseScope()
	releaseScope2, _ := resumedState.GetReleaseScope()
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetSHA(), (*releaseScope2.GetFinalCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetFinalCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction(), (*releaseScope2.GetFinalCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetFinalCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetDate(), (*releaseScope2.GetFinalCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage(), (*releaseScope2.GetFinalCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetFooters(), (*releaseScope2.GetFinalCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetFinalCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetFinalCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetParents(), (*releaseScope2.GetFinalCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetFinalCommit()).GetTags(), (*releaseScope2.GetFinalCommit()).GetTags())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetSHA(), (*releaseScope2.GetInitialCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetInitialCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction(), (*releaseScope2.GetInitialCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetInitialCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetDate(), (*releaseScope2.GetInitialCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage(), (*releaseScope2.GetInitialCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetFooters(), (*releaseScope2.GetInitialCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetInitialCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetInitialCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetParents(), (*releaseScope2.GetInitialCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetInitialCommit()).GetTags(), (*releaseScope2.GetInitialCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPreviousVersion(), *releaseScope2.GetPreviousVersion())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetSHA(), (*releaseScope2.GetPreviousVersionCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPreviousVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPreviousVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetDate(), (*releaseScope2.GetPreviousVersionCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetFooters(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetPreviousVersionCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetParents(), (*releaseScope2.GetPreviousVersionCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetTags(), (*releaseScope2.GetPreviousVersionCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetSHA(), (*releaseScope2.GetPrimeVersionCommit()).GetSHA())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetEmail(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetName(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPrimeVersionCommit()).GetAuthorAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetEmail(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetName(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetTimeStamp())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset(), (*releaseScope2.GetPrimeVersionCommit()).GetCommitAction().GetTimeStamp().GetOffset())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetDate(), (*releaseScope2.GetPrimeVersionCommit()).GetDate())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetFooters(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetFooters())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetFullMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetFullMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetMessage().GetShortMessage(), (*releaseScope2.GetPrimeVersionCommit()).GetMessage().GetShortMessage())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetParents(), (*releaseScope2.GetPrimeVersionCommit()).GetParents())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetTags(), (*releaseScope2.GetPrimeVersionCommit()).GetTags())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())

	timestamp1, _ := oldState.GetTimestamp()
	timestamp2, _ := resumedState.GetTimestamp()
	assert.Equal(t, *timestamp1, *timestamp2)

	version1, _ := oldState.GetVersion()
	version2, _ := resumedState.GetVersion()
	assert.Equal(t, *version1, *version2)

	versionMajorNumber1, _ := oldState.GetVersionMajorNumber()
	versionMajorNumber2, _ := resumedState.GetVersionMajorNumber()
	assert.Equal(t, *versionMajorNumber1, *versionMajorNumber2)

	versionMinorNumber1, _ := oldState.GetVersionMinorNumber()
	versionMinorNumber2, _ := resumedState.GetVersionMinorNumber()
	assert.Equal(t, *versionMinorNumber1, *versionMinorNumber2)

	versionPatchNumber1, _ := oldState.GetVersionPatchNumber()
	versionPatchNumber2, _ := resumedState.GetVersionPatchNumber()
	assert.Equal(t, *versionPatchNumber1, *versionPatchNumber2)

	versionBuildMetadata1, _ := oldState.GetVersionBuildMetadata()
	versionBuildMetadata2, _ := resumedState.GetVersionBuildMetadata()
	assert.Nil(t, versionBuildMetadata1)
	assert.Nil(t, versionBuildMetadata2)

	versionPreReleaseIdentifier1, _ := oldState.GetVersionPreReleaseIdentifier()
	versionPreReleaseIdentifier2, _ := resumedState.GetVersionPreReleaseIdentifier()
	assert.Nil(t, versionPreReleaseIdentifier1)
	assert.Nil(t, versionPreReleaseIdentifier2)

	versionRange1, _ := oldState.GetVersionRange()
	versionRange2, _ := resumedState.GetVersionRange()
	assert.Equal(t, *versionRange1, *versionRange2)

	releaseType1, _ := oldState.GetReleaseType()
	releaseType2, _ := resumedState.GetReleaseType()
	assert.Equal(t, *releaseType1.GetCollapseVersions(), *releaseType2.GetCollapseVersions())
	assert.Equal(t, *releaseType1.GetCollapsedVersionQualifier(), *releaseType2.GetCollapsedVersionQualifier())
	assert.Equal(t, *releaseType1.GetDescription(), *releaseType2.GetDescription())
	assert.Equal(t, *releaseType1.GetGitCommit(), *releaseType2.GetGitCommit())
	assert.Equal(t, *releaseType1.GetGitCommitMessage(), *releaseType2.GetGitCommitMessage())
	assert.Equal(t, *releaseType1.GetGitPush(), *releaseType2.GetGitPush())
	assert.Equal(t, *releaseType1.GetGitTag(), *releaseType2.GetGitTag())
	assert.Equal(t, *releaseType1.GetGitTagMessage(), *releaseType2.GetGitTagMessage())
	if releaseType1.GetGitTagNames() == nil {
		assert.Equal(t, *releaseType1.GetGitTagNames(), *releaseType2.GetGitTagNames())
	} else {
		for i, name := range *releaseType1.GetGitTagNames() {
			assert.Equal(t, *name, *(*releaseType2.GetGitTagNames())[i])
		}
	}
	if releaseType1.GetIdentifiers() == nil {
		assert.Equal(t, *releaseType1.GetIdentifiers(), *releaseType2.GetIdentifiers())
	} else {
		for i, identifier := range *releaseType1.GetIdentifiers() {
			assert.Equal(t, identifier.GetQualifier(), (*releaseType2.GetIdentifiers())[i].GetQualifier())
			assert.Equal(t, identifier.GetValue(), (*releaseType2.GetIdentifiers())[i].GetValue())
			assert.Equal(t, identifier.GetPosition(), (*releaseType2.GetIdentifiers())[i].GetPosition())
		}
	}
	assert.Equal(t, *releaseType1.GetMatchBranches(), *releaseType2.GetMatchBranches())
	assert.Equal(t, *releaseType1.GetMatchEnvironmentVariables(), *releaseType2.GetMatchEnvironmentVariables())
	assert.Equal(t, *releaseType1.GetMatchWorkspaceStatus(), *releaseType2.GetMatchWorkspaceStatus())
	assert.Equal(t, *releaseType1.GetPublish(), *releaseType2.GetPublish())
	assert.Equal(t, *releaseType1.GetVersionRange(), *releaseType2.GetVersionRange())
	assert.Equal(t, *releaseType1.GetGitTagMessage(), *releaseType2.GetGitTagMessage())
	assert.Equal(t, *releaseType1.GetVersionRangeFromBranchName(), *releaseType2.GetVersionRangeFromBranchName())

	// finally also test transient attributes, which should render to the same value even if they are computed on the fly
	directory1, _ := oldState.GetDirectory()
	directory2, _ := resumedState.GetDirectory()
	assert.Equal(t, *directory1, *directory2)

	coreVersion1, _ := oldState.GetCoreVersion()
	coreVersion2, _ := resumedState.GetCoreVersion()
	assert.Equal(t, coreVersion1, coreVersion2)

	latestVersion1, _ := oldState.GetLatestVersion()
	latestVersion2, _ := resumedState.GetLatestVersion()
	assert.Equal(t, latestVersion1, latestVersion2)

	newVersion1, _ := oldState.GetNewVersion()
	newVersion2, _ := resumedState.GetNewVersion()
	assert.Equal(t, newVersion1, newVersion2)

	newRelease1, _ := oldState.GetNewRelease()
	newRelease2, _ := resumedState.GetNewRelease()
	assert.Equal(t, newRelease1, newRelease2)

	scheme1, _ := oldState.GetScheme()
	scheme2, _ := resumedState.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)
}

func TestSummary(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	// set a few values to use later on for comparison
	state.SetBranch(utl.PointerToString("master"))
	state.SetBump(utl.PointerToString("minor"))
	//state.SetCoreVersion() // no setter for this attribute as it's dynamically computed
	state.SetLatestVersion(utl.PointerToBoolean(true))
	//state.SetNewRelease() // no setter for this attribute as it's dynamically computed
	//state.SetNewVersion() // no setter for this attribute as it's dynamically computed
	//state.SetScheme() // no setter for this attribute as it's dynamically computed
	//state.SetTimestamp() // no setter for this attribute as it's dynamically computed
	state.SetVersion(utl.PointerToString("3.5.7"))
	releaseScope, err := state.GetReleaseScope()
	assert.NoError(t, err)
	releaseScope.SetPreviousVersion(utl.PointerToString("4.5.6"))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))

	summary, err := state.Summary()
	assert.NoError(t, err)

	// print the file to standard output for inspection purpose
	//fmt.Println("-------- SUMMARY --------")
	//fmt.Println("-----------------------------------------")
	//fmt.Println(summary)
	//fmt.Println("-----------------------------------------")

	// now we are ready to check the summary contents
	branch, _ := state.GetBranch()
	assert.True(t, strings.Contains(summary, "branch           = "+*branch))
	bump, _ := state.GetBump()
	assert.True(t, strings.Contains(summary, "bump             = "+*bump))
	coreVersion, _ := state.GetCoreVersion()
	assert.True(t, strings.Contains(summary, "core version     = "+strconv.FormatBool(coreVersion)))
	latestVersion, _ := state.GetLatestVersion()
	assert.True(t, strings.Contains(summary, "latest version   = "+strconv.FormatBool(*latestVersion)))
	newRelease, _ := state.GetNewRelease()
	assert.True(t, strings.Contains(summary, "new release      = "+strconv.FormatBool(newRelease)))
	newVersion, _ := state.GetNewVersion()
	assert.True(t, strings.Contains(summary, "new version      = "+strconv.FormatBool(newVersion)))
	scheme, _ := state.GetScheme()
	assert.True(t, strings.Contains(summary, "scheme           = "+(*scheme).String()))
	timestamp, _ := state.GetTimestamp()
	assert.True(t, strings.Contains(summary, "timestamp        = "+strconv.FormatInt(*timestamp, 10)))
	version, _ := state.GetVersion()
	assert.True(t, strings.Contains(summary, "current version  = "+*version))
	releaseScope, _ = state.GetReleaseScope()
	assert.True(t, strings.Contains(summary, "previous version = "+*releaseScope.GetPreviousVersion()))
	assert.True(t, strings.Contains(summary, "prime version    = "+*releaseScope.GetPrimeVersion()))
}
