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

package configuration

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	io "github.com/mooltiverse/nyx/modules/go/nyx/io"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

/*
Performs checks against default values
*/
func TestConfigurationDefaultsGetBump(t *testing.T) {
	configuration, _ := NewConfiguration()
	bump, _ := configuration.GetBump()
	if bump == nil {
		assert.Nil(t, bump)
	} else {
		assert.Equal(t, *ent.BUMP, *bump)
	}
}

func TestConfigurationDefaultsGetChangelog(t *testing.T) {
	configuration, _ := NewConfiguration()
	changelog, _ := configuration.GetChangelog()
	if changelog == nil {
		assert.Nil(t, changelog)
	} else {
		assert.Equal(t, *ent.CHANGELOG, *changelog)
		assert.Equal(t, (*ent.CHANGELOG).GetPath(), (*changelog).GetPath())
		assert.Equal(t, len(*ent.CHANGELOG.GetSections()), len(*changelog.GetSections()))
		assert.Equal(t, 0, len(*changelog.GetSections()))
		assert.Equal(t, len(*ent.CHANGELOG.GetSections()), len(*changelog.GetSubstitutions()))
		assert.Equal(t, 0, len(*changelog.GetSubstitutions()))
		assert.Equal(t, (*ent.CHANGELOG).GetTemplate(), (*changelog).GetTemplate())
	}
}

func TestConfigurationDefaultsGetCommitMessageConventions(t *testing.T) {
	configuration, _ := NewConfiguration()
	commitMessageConventions, _ := configuration.GetCommitMessageConventions()
	if commitMessageConventions == nil {
		assert.Nil(t, commitMessageConventions)
	} else {
		assert.Equal(t, *ent.COMMIT_MESSAGE_CONVENTIONS, *commitMessageConventions)
		assert.Equal(t, (*ent.COMMIT_MESSAGE_CONVENTIONS).GetEnabled(), (*commitMessageConventions).GetEnabled())
		assert.Equal(t, 0, len(*commitMessageConventions.GetItems()))
	}
}

func TestConfigurationDefaultsGetConfigurationFile(t *testing.T) {
	configuration, _ := NewConfiguration()
	configurationFile, _ := configuration.GetConfigurationFile()
	if configurationFile == nil {
		assert.Nil(t, configurationFile)
	} else {
		assert.Equal(t, *ent.CONFIGURATION_FILE, *configurationFile)
	}
}

func TestConfigurationDefaultsGetDirectory(t *testing.T) {
	configuration, _ := NewConfiguration()
	SetDefaultDirectory(nil) // make sure the default value is reset, in case previous tests left it dirty
	directory, _ := configuration.GetDirectory()
	if directory == nil {
		assert.Nil(t, directory)
	} else {
		assert.Equal(t, *ent.DIRECTORY, *directory)
	}
}

func TestConfigurationDefaultsSetDirectory(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	configuration, _ := NewConfiguration()
	directory, _ := configuration.GetDirectory()
	assert.Equal(t, tempDir, *directory)

	SetDefaultDirectory(nil) // clean up
}

func TestConfigurationDefaultsGetDryRun(t *testing.T) {
	configuration, _ := NewConfiguration()
	dryRun, _ := configuration.GetDryRun()
	if dryRun == nil {
		assert.Nil(t, dryRun)
	} else {
		assert.Equal(t, *ent.DRY_RUN, *dryRun)
	}
}

func TestConfigurationDefaultsGetGit(t *testing.T) {
	configuration, _ := NewConfiguration()
	git, _ := configuration.GetGit()
	if git == nil {
		assert.Nil(t, git)
	} else {
		assert.Equal(t, *ent.GIT, *git)
		assert.Equal(t, len(*(*ent.GIT).GetRemotes()), len(*(*git).GetRemotes()))
	}
}

func TestConfigurationDefaultsGetInitialVersion(t *testing.T) {
	configuration, _ := NewConfiguration()
	initialVersion, _ := configuration.GetInitialVersion()
	if initialVersion == nil {
		assert.Nil(t, initialVersion)
	} else {
		assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion)
	}
}

func TestConfigurationDefaultsGetPreset(t *testing.T) {
	configuration, _ := NewConfiguration()
	preset, _ := configuration.GetPreset()
	if preset == nil {
		assert.Nil(t, preset)
	} else {
		assert.Equal(t, *ent.PRESET, *preset)
	}
}

func TestConfigurationDefaultsGetReleaseAssets(t *testing.T) {
	configuration, _ := NewConfiguration()
	releaseAssets, _ := configuration.GetReleaseAssets()
	if releaseAssets == nil {
		assert.Nil(t, releaseAssets)
	} else {
		assert.Equal(t, *ent.RELEASE_ASSETS, *releaseAssets)
	}
}

func TestConfigurationDefaultsGetReleaseLenient(t *testing.T) {
	configuration, _ := NewConfiguration()
	releaseLenient, _ := configuration.GetReleaseLenient()
	if releaseLenient == nil {
		assert.Nil(t, releaseLenient)
	} else {
		assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient)
	}
}

func TestConfigurationDefaultsGetReleasePrefix(t *testing.T) {
	configuration, _ := NewConfiguration()
	releasePrefix, _ := configuration.GetReleasePrefix()
	if releasePrefix == nil {
		assert.Nil(t, releasePrefix)
	} else {
		assert.Equal(t, *ent.RELEASE_PREFIX, *releasePrefix)
	}
}

func TestConfigurationDefaultsGetReleaseTypes(t *testing.T) {
	configuration, _ := NewConfiguration()
	releaseTypes, _ := configuration.GetReleaseTypes()
	if releaseTypes == nil {
		assert.Nil(t, releaseTypes)
	} else {
		assert.Equal(t, *ent.RELEASE_TYPES, *releaseTypes)
		assert.Equal(t, (*ent.RELEASE_TYPES).GetEnabled(), (*releaseTypes).GetEnabled())
		assert.Equal(t, 1, len(*releaseTypes.GetItems()))
	}
}

func TestConfigurationDefaultsGetResume(t *testing.T) {
	configuration, _ := NewConfiguration()
	resume, _ := configuration.GetResume()
	if resume == nil {
		assert.Nil(t, resume)
	} else {
		assert.Equal(t, *ent.RESUME, *resume)
	}
}

func TestConfigurationDefaultsGetScheme(t *testing.T) {
	configuration, _ := NewConfiguration()
	scheme, _ := configuration.GetScheme()
	if scheme == nil {
		assert.Nil(t, scheme)
	} else {
		assert.Equal(t, *ent.SCHEME, *scheme)
	}
}

func TestConfigurationDefaultsGetServices(t *testing.T) {
	configuration, _ := NewConfiguration()
	services, _ := configuration.GetServices()
	if services == nil {
		assert.Nil(t, services)
	} else {
		assert.Equal(t, *ent.SERVICES, *services)
	}
}

func TestConfigurationDefaultsGetSharedConfigurationFile(t *testing.T) {
	configuration, _ := NewConfiguration()
	sharedConfigurationFile, _ := configuration.GetSharedConfigurationFile()
	if sharedConfigurationFile == nil {
		assert.Nil(t, sharedConfigurationFile)
	} else {
		assert.Equal(t, *ent.SHARED_CONFIGURATION_FILE, *sharedConfigurationFile)
	}
}

func TestConfigurationDefaultsGetStateFile(t *testing.T) {
	configuration, _ := NewConfiguration()
	stateFile, _ := configuration.GetStateFile()
	if stateFile == nil {
		assert.Nil(t, stateFile)
	} else {
		assert.Equal(t, *ent.STATE_FILE, *stateFile)
	}
}

func TestConfigurationDefaultsGetSubstitutions(t *testing.T) {
	configuration, _ := NewConfiguration()
	substitutions, _ := configuration.GetSubstitutions()
	if substitutions == nil {
		assert.Nil(t, substitutions)
	} else {
		assert.Equal(t, *ent.SUBSTITUTIONS, *substitutions)
		assert.Equal(t, (*ent.SUBSTITUTIONS).GetEnabled(), (*substitutions).GetEnabled())
		assert.Equal(t, 0, len(*substitutions.GetItems()))
	}
}

func TestConfigurationDefaultsGetSummary(t *testing.T) {
	configuration, _ := NewConfiguration()
	summary, _ := configuration.GetSummary()
	if summary == nil {
		assert.Nil(t, summary)
	} else {
		assert.Equal(t, *ent.SUMMARY, *summary)
	}
}

func TestConfigurationDefaultsGetSummaryFile(t *testing.T) {
	configuration, _ := NewConfiguration()
	summaryFile, _ := configuration.GetSummaryFile()
	if summaryFile == nil {
		assert.Nil(t, summaryFile)
	} else {
		assert.Equal(t, *ent.SUMMARY_FILE, *summaryFile)
	}
}

func TestConfigurationDefaultsGetVerbosity(t *testing.T) {
	configuration, _ := NewConfiguration()
	verbosity, _ := configuration.GetVerbosity()
	if verbosity == nil {
		assert.Nil(t, verbosity)
	} else {
		assert.Equal(t, *ent.VERBOSITY, *verbosity)
	}
}

func TestConfigurationDefaultsGetVersion(t *testing.T) {
	configuration, _ := NewConfiguration()
	version, _ := configuration.GetVersion()
	if version == nil {
		assert.Nil(t, version)
	} else {
		assert.Equal(t, *ent.VERSION, *version)
	}
}

/*
Performs checks against the injection of a command line configuration
*/
func TestConfigurationWithCommandLineConfigurationGetBump(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--bump=alpha",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.BUMP)
	bump1, _ := configurationLayerMock.GetBump()
	assert.Equal(t, "alpha", *bump1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.BUMP)
	bump2, _ := configuration.GetBump()
	assert.Nil(t, bump2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	bump2, _ = configuration.GetBump()
	assert.NotNil(t, bump2)
	assert.Equal(t, *bump1, *bump2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	bump2, _ = configuration.GetBump()
	assert.Nil(t, bump2)
}

func TestConfigurationWithCommandLineConfigurationGetChangelog(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--changelog-path=CHANGELOG.md",
		"--changelog-sections-Section1=regex1",
		"--changelog-sections-Section2=regex2",
		"--changelog-substitutions-Expression1=string1",
		"--changelog-template=changelog.tpl",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.CHANGELOG)
	changelog1, _ := configurationLayerMock.GetChangelog()
	assert.NotNil(t, changelog1)
	changelog2, _ := configuration.GetChangelog()
	assert.NotNil(t, changelog2)
	assert.NotEqual(t, changelog1, changelog2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, (*ent.CHANGELOG).GetPath())
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSubstitutions()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*ent.CHANGELOG).GetTemplate())
	assert.Nil(t, (*changelog2).GetTemplate())

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	changelog2, _ = configuration.GetChangelog()
	assert.Equal(t, "CHANGELOG.md", *(*changelog2).GetPath())
	assert.NotNil(t, (*changelog2).GetSections())
	assert.Equal(t, 2, len(*(*changelog2).GetSections()))
	assert.Equal(t, "regex1", (*(*changelog2).GetSections())["Section1"])
	assert.Equal(t, "regex2", (*(*changelog2).GetSections())["Section2"])
	assert.NotNil(t, (*changelog2).GetSubstitutions())
	assert.Equal(t, 1, len(*(*changelog2).GetSubstitutions()))
	assert.Equal(t, "string1", (*(*changelog2).GetSubstitutions())["Expression1"])
	assert.Equal(t, "changelog.tpl", *(*changelog2).GetTemplate())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithCommandLineConfiguration(nil)
	changelog2, _ = configuration.GetChangelog()
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*changelog2).GetTemplate())
}

func TestConfigurationWithCommandLineConfigurationGetCommitMessageConventions(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--commit-message-conventions-enabled=convention1",
		"--commit-message-conventions-convention1-expression=expr1",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.COMMIT_MESSAGE_CONVENTIONS)
	commitMessageConventions1, _ := configurationLayerMock.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions1)
	commitMessageConventions2, _ := configuration.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions2)
	assert.NotEqual(t, commitMessageConventions1, commitMessageConventions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.NotNil(t, (*commitMessageConventions2).GetEnabled())
	assert.NotNil(t, (*commitMessageConventions2).GetItems())
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, "convention1", *(*(*commitMessageConventions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetItems()))
	assert.Equal(t, "expr1", *(*(*(*commitMessageConventions2).GetItems())["convention1"]).GetExpression())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithCommandLineConfiguration(nil)
	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))
}

func TestConfigurationWithCommandLineConfigurationGetConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--configuration-file=" + os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE),
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	configurationFile1, _ := configurationLayerMock.GetConfigurationFile()
	assert.NotNil(t, configurationFile1)
	assert.Nil(t, ent.CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	configurationFile2, _ := configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, configurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.NotNil(t, configurationFile2)
	assert.Equal(t, *configurationFile1, *configurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
}

func TestConfigurationWithCommandLineConfigurationGetDirectory(t *testing.T) {
	SetDefaultDirectory(nil) // clean the singleton from previous runs
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--directory=some/directory",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DIRECTORY)
	directory1, _ := configurationLayerMock.GetDirectory()
	assert.Equal(t, "some/directory", *directory1)
	assert.NotEqual(t, *ent.DIRECTORY, *directory1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DIRECTORY)
	directory2, _ := configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *directory1, *directory2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)
}

func TestConfigurationWithCommandLineConfigurationGetDryRun(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--dry-run",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DRY_RUN)
	dryRun1, _ := configurationLayerMock.GetDryRun()
	assert.NotEqual(t, *ent.DRY_RUN, *dryRun1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DRY_RUN)
	dryRun2, _ := configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *dryRun1, *dryRun2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)
}

func TestConfigurationWithCommandLineConfigurationGetGit(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--git-remotes-origin-authenticationMethod=PUBLIC_KEY",
		"--git-remotes-origin-user=jdoe",
		"--git-remotes-origin-password=pwd",
		"--git-remotes-origin-privateKey=key",
		"--git-remotes-origin-passphrase=passphrase",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.GIT)
	git1, _ := configurationLayerMock.GetGit()
	assert.NotNil(t, git1)
	assert.NotEqual(t, *ent.GIT, *git1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, *ent.GIT)
	git2, _ := configuration.GetGit()
	assert.NotNil(t, git2)
	assert.NotEqual(t, *git1, *git2)

	assert.Equal(t, 0, len((*(*ent.GIT).GetRemotes())))
	assert.Equal(t, 0, len((*(*git2).GetRemotes())))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	git2, _ = configuration.GetGit()
	assert.NotNil(t, (*git2).GetRemotes())
	assert.Equal(t, 1, len(*(*git2).GetRemotes()))
	assert.Equal(t, ent.PUBLIC_KEY, *(*(*git2).GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, "pwd", *(*(*git2).GetRemotes())["origin"].GetPassword())
	assert.Equal(t, "jdoe", *(*(*git2).GetRemotes())["origin"].GetUser())
	assert.Equal(t, "key", *(*(*git2).GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, "passphrase", *(*(*git2).GetRemotes())["origin"].GetPassphrase())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	configuration.WithCommandLineConfiguration(nil)
	git2, _ = configuration.GetGit()
	assert.Equal(t, 0, len(*(*git2).GetRemotes()))
}

func TestConfigurationWithCommandLineConfigurationGetInitialVersion(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--initial-version=9.9.9",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion1, _ := configurationLayerMock.GetInitialVersion()
	assert.NotEqual(t, *ent.INITIAL_VERSION, *initialVersion1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion2, _ := configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *initialVersion1, *initialVersion2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)
}

func TestConfigurationWithCommandLineConfigurationGetPreset(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--preset=simple",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.PRESET)
	preset1, _ := configurationLayerMock.GetPreset()
	assert.Equal(t, "simple", *preset1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.PRESET)
	preset2, _ := configuration.GetPreset()
	assert.Nil(t, preset2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	preset2, _ = configuration.GetPreset()
	assert.Equal(t, *preset1, *preset2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	preset2, _ = configuration.GetPreset()
	assert.Nil(t, preset2)
}

func TestConfigurationWithCommandLineConfigurationGetReleaseAssets(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--release-assets-asset1-fileName=asset.txt",
		"--release-assets-asset1-description=Text Asset",
		"--release-assets-asset1-type=text/plain",
		"--release-assets-asset1-path=asset.txt",
		"--release-assets-asset2-fileName=asset.bin",
		"--release-assets-asset2-description=Binary Asset",
		"--release-assets-asset2-type=application/octet-stream",
		"--release-assets-asset2-path=asset.bin",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets1, _ := configurationLayerMock.GetReleaseAssets()
	assert.NotEqual(t, *ent.RELEASE_ASSETS, *releaseAssets1)
	assert.Equal(t, 2, len((*releaseAssets1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets2, _ := configuration.GetReleaseAssets()
	assert.Equal(t, *ent.RELEASE_ASSETS, *releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.Equal(t, 2, len((*releaseAssets2)))
	assert.NotNil(t, (*releaseAssets2)["asset1"])
	assert.NotNil(t, (*releaseAssets2)["asset2"])
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetFileName())
	assert.Equal(t, "Text Asset", *(*releaseAssets2)["asset1"].GetDescription())
	assert.Equal(t, "text/plain", *(*releaseAssets2)["asset1"].GetType())
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetPath())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetFileName())
	assert.Equal(t, "Binary Asset", *(*releaseAssets2)["asset2"].GetDescription())
	assert.Equal(t, "application/octet-stream", *(*releaseAssets2)["asset2"].GetType())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetPath())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	configuration.WithCommandLineConfiguration(nil)
	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.NotNil(t, releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))
}

func TestConfigurationWithCommandLineConfigurationGetReleaseLenient(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--release-lenient=false",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient1, _ := configurationLayerMock.GetReleaseLenient()
	assert.NotEqual(t, *ent.RELEASE_LENIENT, *releaseLenient1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient2, _ := configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *releaseLenient1, *releaseLenient2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)
}

func TestConfigurationWithCommandLineConfigurationGetReleasePrefix(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--release-prefix=testprefix",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix1, _ := configurationLayerMock.GetReleasePrefix()
	assert.Equal(t, "testprefix", *releasePrefix1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix2, _ := configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Equal(t, *releasePrefix1, *releasePrefix2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)
}

func TestConfigurationWithCommandLineConfigurationGetReleaseTypes(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--release-types-enabled=type1",
		"--release-types-publication-services=service1",
		"--release-types-remote-repositories=remote1",
		"--release-types-type1-assets=asset1,asset2",
		"--release-types-type1-collapse-versions=true",
		"--release-types-type1-collapsed-version-qualifier={{#sanitizeLower}}{{branch}}{{/sanitizeLower}}",
		"--release-types-type1-description=Release description",
		"--release-types-type1-filter-tags=^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
		"--release-types-type1-git-commit=true",
		"--release-types-type1-git-commit-message=Committing {{version}}",
		"--release-types-type1-git-push=true",
		"--release-types-type1-git-tag=true",
		"--release-types-type1-git-tag-message=Tagging {{version}}",
		"--release-types-type1-identifiers-0-position=" + ent.BUILD.String(),
		"--release-types-type1-identifiers-0-qualifier=build",
		"--release-types-type1-identifiers-0-value=12",
		"--release-types-type1-match-branches=",
		"--release-types-type1-match-environment-variables-PATH=.*",
		"--release-types-type1-publish=true",
		"--release-types-type1-version-range=",
		"--release-types-type1-version-range-from-branch-name=false",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes1, _ := configurationLayerMock.GetReleaseTypes()
	assert.NotEqual(t, *ent.RELEASE_TYPES, *releaseTypes1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes2, _ := configuration.GetReleaseTypes()
	assert.Equal(t, *ent.RELEASE_TYPES, *releaseTypes2)

	assert.NotNil(t, *ent.RELEASE_TYPES.GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.Equal(t, 1, len(*(*ent.RELEASE_TYPES).GetItems()))
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetItems())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetEnabled()))
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetPublicationServices()))
	assert.Equal(t, "service1", *(*(*releaseTypes2).GetPublicationServices())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetRemoteRepositories()))
	assert.Equal(t, "remote1", *(*(*releaseTypes2).GetRemoteRepositories())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
	assert.Equal(t, 2, len(*(*(*releaseTypes2).GetItems())["type1"].GetAssets()))
	assert.Equal(t, "asset1", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[0])
	assert.Equal(t, "asset2", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[1])
	assert.True(t, *(*(*releaseTypes2).GetItems())["type1"].GetCollapseVersions())
	assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *(*(*releaseTypes2).GetItems())["type1"].GetCollapsedVersionQualifier())
	assert.Equal(t, "Release description", *(*(*releaseTypes2).GetItems())["type1"].GetDescription())
	assert.Equal(t, "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", *(*(*releaseTypes2).GetItems())["type1"].GetFilterTags())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommit())
	assert.Equal(t, "Committing {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommitMessage())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitPush())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitTag())
	assert.Equal(t, "Tagging {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitTagMessage())
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers()) == 0)
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetMatchBranches())
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables()) == 0)
	assert.Nil(t, (*(*releaseTypes2).GetItems())["type1"].GetMatchWorkspaceStatus())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetPublish())
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetVersionRange())
	assert.Equal(t, false, *(*(*releaseTypes2).GetItems())["type1"].GetVersionRangeFromBranchName())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	configuration.WithCommandLineConfiguration(nil)
	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
}

func TestConfigurationWithCommandLineConfigurationGetResume(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--resume",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RESUME)
	resume1, _ := configurationLayerMock.GetResume()
	assert.NotEqual(t, *ent.RESUME, *resume1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RESUME)
	resume2, _ := configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	resume2, _ = configuration.GetResume()
	assert.Equal(t, *resume1, *resume2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	resume2, _ = configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)
}

func TestConfigurationWithCommandLineConfigurationGetScheme(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--scheme=" + ver.SEMVER.String(),
	})

	scheme1, _ := configurationLayerMock.GetScheme()
	// since there is only one scheme available, this assumption can't be assumed
	//assert.NotEqual(t, *ent.SCHEME, configurationLayerMock.scheme)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SCHEME)
	scheme2, _ := configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)
}

func TestConfigurationWithCommandLineConfigurationGetServices(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--services-github-type=GITHUB",
		"--services-github-options-AUTHENTICATION_TOKEN={{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
		"--services-github-options-REPOSITORY_NAME=repo1",
		"--services-github-options-REPOSITORY_OWNER=owner1",
		"--services-gitlab-type=GITLAB",
		"--services-gitlab-options-AUTHENTICATION_TOKEN={{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
		"--services-gitlab-options-REPOSITORY_NAME=repo2",
		"--services-gitlab-options-REPOSITORY_OWNER=owner2",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SERVICES)
	services1, _ := configurationLayerMock.GetServices()
	assert.NotEqual(t, *ent.SERVICES, *services1)
	assert.Equal(t, 2, len((*services1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SERVICES)
	services2, _ := configuration.GetServices()
	assert.Equal(t, *ent.SERVICES, *services2)
	assert.Equal(t, 0, len((*services2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	services2, _ = configuration.GetServices()
	assert.Equal(t, 2, len((*services2)))
	assert.NotNil(t, (*services2)["github"])
	assert.NotNil(t, (*services2)["gitlab"])
	assert.Equal(t, ent.GITHUB, *(*services2)["github"].GetType())
	assert.Equal(t, 3, len(*(*services2)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services2)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo1", (*(*services2)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner1", (*(*services2)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, ent.GITLAB, *(*services2)["gitlab"].GetType())
	assert.Equal(t, 3, len(*(*services2)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services2)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_OWNER"])

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	configuration.WithCommandLineConfiguration(nil)
	services2, _ = configuration.GetServices()
	assert.NotNil(t, services2)
	assert.Equal(t, 0, len((*services2)))
}

func TestConfigurationWithCommandLineConfigurationGetSharedConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--shared-configuration-file=" + os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE),
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	sharedConfigurationFile1, _ := configurationLayerMock.GetSharedConfigurationFile()
	assert.NotNil(t, sharedConfigurationFile1)
	assert.Nil(t, ent.SHARED_CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	sharedConfigurationFile2, _ := configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, sharedConfigurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Equal(t, *sharedConfigurationFile1, *sharedConfigurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
}

func TestConfigurationWithCommandLineConfigurationGetStateFile(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--state-file=state-file.yml",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.STATE_FILE)
	stateFile1, _ := configurationLayerMock.GetStateFile()
	assert.Equal(t, "state-file.yml", *stateFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.STATE_FILE)
	stateFile2, _ := configuration.GetStateFile()
	assert.Nil(t, stateFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	stateFile2, _ = configuration.GetStateFile()
	assert.Equal(t, *stateFile1, *stateFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	stateFile2, _ = configuration.GetStateFile()
	assert.Nil(t, stateFile2)
}

func TestConfigurationWithCommandLineConfigurationGetSubstitutions(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--substitutions-enabled=substitution1",
		"--substitutions-substitution1-files=glob1",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.SUBSTITUTIONS)
	substitutions1, _ := configurationLayerMock.GetSubstitutions()
	assert.NotNil(t, substitutions1)
	substitutions2, _ := configuration.GetSubstitutions()
	assert.NotNil(t, substitutions2)
	assert.NotEqual(t, substitutions1, substitutions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	substitutions2, _ = configuration.GetSubstitutions()
	assert.NotNil(t, (*substitutions2).GetEnabled())
	assert.NotNil(t, (*substitutions2).GetItems())
	assert.Equal(t, 1, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, "substitution1", *(*(*substitutions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*substitutions2).GetItems()))
	assert.Equal(t, "glob1", *(*(*(*substitutions2).GetItems())["substitution1"]).GetFiles())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithCommandLineConfiguration(nil)
	substitutions2, _ = configuration.GetSubstitutions()
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))
}

func TestConfigurationWithCommandLineConfigurationGetSummary(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--summary",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SUMMARY)
	summary1, _ := configurationLayerMock.GetSummary()
	assert.NotEqual(t, *ent.SUMMARY, *summary1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SUMMARY)
	summary2, _ := configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *summary1, *summary2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)
}

func TestConfigurationWithCommandLineConfigurationGetSummaryFile(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--summary-file=summary.txt",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile1, _ := configurationLayerMock.GetSummaryFile()
	assert.Equal(t, "summary.txt", *summaryFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile2, _ := configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Equal(t, *summaryFile1, *summaryFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)
}

func TestConfigurationWithCommandLineConfigurationGetVerbosity(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--trace",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.VERBOSITY)
	verbosity1, _ := configurationLayerMock.GetVerbosity()
	assert.NotEqual(t, *ent.VERBOSITY, *verbosity1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.VERBOSITY)
	verbosity2, _ := configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *verbosity1, *verbosity2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)
}

func TestConfigurationWithCommandLineConfigurationGetVersion(t *testing.T) {
	configurationLayerMock := NewCommandLineConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.withArguments([]string{
		"--version=11.12.13",
	})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.VERSION)
	version1, _ := configurationLayerMock.GetVersion()
	assert.Equal(t, "11.12.13", *version1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.VERSION)
	version2, _ := configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithCommandLineConfiguration(&cl)

	version2, _ = configuration.GetVersion()
	assert.Equal(t, *version1, *version2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithCommandLineConfiguration(nil)
	version2, _ = configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)
}

/*
Performs checks against the injection of a plugin configuration
*/
func TestConfigurationWithPluginConfigurationGetBump(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetBump(utl.PointerToString("alpha"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.BUMP)
	bump1, _ := configurationLayerMock.GetBump()
	assert.Equal(t, "alpha", *bump1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.BUMP)
	bump2, _ := configuration.GetBump()
	assert.Nil(t, bump2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	bump2, _ = configuration.GetBump()
	assert.NotNil(t, bump2)
	assert.Equal(t, *bump1, *bump2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	bump2, _ = configuration.GetBump()
	assert.Nil(t, bump2)
}

func TestConfigurationWithPluginConfigurationGetChangelog(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	changelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Section1": "regex1", "Section2": "regex2"}, utl.PointerToString("changelog.tpl"), &map[string]string{"Expression1": "string1"})
	configurationLayerMock.SetChangelog(changelogConfiguration)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.CHANGELOG)
	changelog1, _ := configurationLayerMock.GetChangelog()
	assert.NotNil(t, changelog1)
	changelog2, _ := configuration.GetChangelog()
	assert.NotNil(t, changelog2)
	assert.NotEqual(t, changelog1, changelog2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, (*ent.CHANGELOG).GetPath())
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSubstitutions()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*ent.CHANGELOG).GetTemplate())
	assert.Nil(t, (*changelog2).GetTemplate())

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	changelog2, _ = configuration.GetChangelog()
	assert.Equal(t, "CHANGELOG.md", *(*changelog2).GetPath())
	assert.NotNil(t, (*changelog2).GetSections())
	assert.Equal(t, 2, len(*(*changelog2).GetSections()))
	assert.Equal(t, "regex1", (*(*changelog2).GetSections())["Section1"])
	assert.Equal(t, "regex2", (*(*changelog2).GetSections())["Section2"])
	assert.NotNil(t, (*changelog2).GetSubstitutions())
	assert.Equal(t, 1, len(*(*changelog2).GetSubstitutions()))
	assert.Equal(t, "string1", (*(*changelog2).GetSubstitutions())["Expression1"])
	assert.Equal(t, "changelog.tpl", *(*changelog2).GetTemplate())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithPluginConfiguration(nil)
	changelog2, _ = configuration.GetChangelog()
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*changelog2).GetTemplate())
}

func TestConfigurationWithPluginConfigurationGetCommitMessageConventions(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention1")}, &map[string]*ent.CommitMessageConvention{"convention1": ent.NewCommitMessageConventionWith(utl.PointerToString("expr1"), &map[string]string{})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.COMMIT_MESSAGE_CONVENTIONS)
	commitMessageConventions1, _ := configurationLayerMock.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions1)
	commitMessageConventions2, _ := configuration.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions2)
	assert.NotEqual(t, commitMessageConventions1, commitMessageConventions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.NotNil(t, (*commitMessageConventions2).GetEnabled())
	assert.NotNil(t, (*commitMessageConventions2).GetItems())
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, "convention1", *(*(*commitMessageConventions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetItems()))
	assert.Equal(t, "expr1", *(*(*(*commitMessageConventions2).GetItems())["convention1"]).GetExpression())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithPluginConfiguration(nil)
	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))
}

func TestConfigurationWithPluginConfigurationGetConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// in order to make the test meaningful, make sure the default and mock values are different
	configurationFile1, _ := configurationLayerMock.GetConfigurationFile()
	assert.NotNil(t, configurationFile1)
	assert.Nil(t, ent.CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	configurationFile2, _ := configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, configurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.NotNil(t, configurationFile2)
	assert.Equal(t, *configurationFile1, *configurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
}

func TestConfigurationWithPluginConfigurationGetDirectory(t *testing.T) {
	SetDefaultDirectory(nil) // clean the singleton from previous runs
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetDirectory(utl.PointerToString("some/directory"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DIRECTORY)
	directory1, _ := configurationLayerMock.GetDirectory()
	assert.Equal(t, "some/directory", *directory1)
	assert.NotEqual(t, *ent.DIRECTORY, *directory1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DIRECTORY)
	directory2, _ := configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *directory1, *directory2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)
}

func TestConfigurationWithPluginConfigurationGetDryRun(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetDryRun(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DRY_RUN)
	dryRun1, _ := configurationLayerMock.GetDryRun()
	assert.NotEqual(t, *ent.DRY_RUN, *dryRun1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DRY_RUN)
	dryRun2, _ := configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *dryRun1, *dryRun2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)
}

func TestConfigurationWithPluginConfigurationGetGit(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	gitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), utl.PointerToString("jdoe"), utl.PointerToString("pwd"), utl.PointerToString("key"), utl.PointerToString("passphrase"))})
	configurationLayerMock.SetGit(gitConfiguration)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.GIT)
	git1, _ := configurationLayerMock.GetGit()
	assert.NotNil(t, git1)
	assert.NotEqual(t, *ent.GIT, *git1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, *ent.GIT)
	git2, _ := configuration.GetGit()
	assert.NotNil(t, git2)
	assert.NotEqual(t, *git1, *git2)

	assert.Equal(t, 0, len((*(*ent.GIT).GetRemotes())))
	assert.Equal(t, 0, len((*(*git2).GetRemotes())))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	git2, _ = configuration.GetGit()
	assert.NotNil(t, (*git2).GetRemotes())
	assert.Equal(t, 1, len(*(*git2).GetRemotes()))
	assert.Equal(t, ent.PUBLIC_KEY, *(*(*git2).GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, "pwd", *(*(*git2).GetRemotes())["origin"].GetPassword())
	assert.Equal(t, "jdoe", *(*(*git2).GetRemotes())["origin"].GetUser())
	assert.Equal(t, "key", *(*(*git2).GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, "passphrase", *(*(*git2).GetRemotes())["origin"].GetPassphrase())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	configuration.WithPluginConfiguration(nil)
	git2, _ = configuration.GetGit()
	assert.Equal(t, 0, len(*(*git2).GetRemotes()))
}

func TestConfigurationWithPluginConfigurationGetInitialVersion(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetInitialVersion(utl.PointerToString("9.9.9"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion1, _ := configurationLayerMock.GetInitialVersion()
	assert.NotEqual(t, *ent.INITIAL_VERSION, *initialVersion1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion2, _ := configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *initialVersion1, *initialVersion2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)
}

func TestConfigurationWithPluginConfigurationGetPreset(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetPreset(utl.PointerToString("simple"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.PRESET)
	preset1, _ := configurationLayerMock.GetPreset()
	assert.Equal(t, "simple", *preset1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.PRESET)
	preset2, _ := configuration.GetPreset()
	assert.Nil(t, preset2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	preset2, _ = configuration.GetPreset()
	assert.Equal(t, *preset1, *preset2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	preset2, _ = configuration.GetPreset()
	assert.Nil(t, preset2)
}

func TestConfigurationWithPluginConfigurationGetReleaseAssets(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"asset1": ent.NewAttachmentWith(utl.PointerToString("asset.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("asset.txt"), utl.PointerToString("text/plain")), "asset2": ent.NewAttachmentWith(utl.PointerToString("asset.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("asset.bin"), utl.PointerToString("application/octet-stream"))})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets1, _ := configurationLayerMock.GetReleaseAssets()
	assert.NotEqual(t, *ent.RELEASE_ASSETS, *releaseAssets1)
	assert.Equal(t, 2, len((*releaseAssets1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets2, _ := configuration.GetReleaseAssets()
	assert.Equal(t, *ent.RELEASE_ASSETS, *releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.Equal(t, 2, len((*releaseAssets2)))
	assert.NotNil(t, (*releaseAssets2)["asset1"])
	assert.NotNil(t, (*releaseAssets2)["asset2"])
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetFileName())
	assert.Equal(t, "Text Asset", *(*releaseAssets2)["asset1"].GetDescription())
	assert.Equal(t, "text/plain", *(*releaseAssets2)["asset1"].GetType())
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetPath())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetFileName())
	assert.Equal(t, "Binary Asset", *(*releaseAssets2)["asset2"].GetDescription())
	assert.Equal(t, "application/octet-stream", *(*releaseAssets2)["asset2"].GetType())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetPath())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	configuration.WithPluginConfiguration(nil)
	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.NotNil(t, releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))
}

func TestConfigurationWithPluginConfigurationGetReleaseLenient(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient1, _ := configurationLayerMock.GetReleaseLenient()
	assert.NotEqual(t, *ent.RELEASE_LENIENT, *releaseLenient1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient2, _ := configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *releaseLenient1, *releaseLenient2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)
}

func TestConfigurationWithPluginConfigurationGetReleasePrefix(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleasePrefix(utl.PointerToString("testprefix"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix1, _ := configurationLayerMock.GetReleasePrefix()
	assert.Equal(t, "testprefix", *releasePrefix1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix2, _ := configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Equal(t, *releasePrefix1, *releasePrefix2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)
}

func TestConfigurationWithPluginConfigurationGetReleaseTypes(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type1")}, &[]*string{utl.PointerToString("service1")}, &[]*string{utl.PointerToString("remote1")}, &map[string]*ent.ReleaseType{"type1": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), utl.PointerToString("Release description"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes1, _ := configurationLayerMock.GetReleaseTypes()
	assert.NotEqual(t, *ent.RELEASE_TYPES, *releaseTypes1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes2, _ := configuration.GetReleaseTypes()
	assert.Equal(t, *ent.RELEASE_TYPES, *releaseTypes2)

	assert.NotNil(t, *ent.RELEASE_TYPES.GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.Equal(t, 1, len(*(*ent.RELEASE_TYPES).GetItems()))
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetItems())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetEnabled()))
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetPublicationServices()))
	assert.Equal(t, "service1", *(*(*releaseTypes2).GetPublicationServices())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetRemoteRepositories()))
	assert.Equal(t, "remote1", *(*(*releaseTypes2).GetRemoteRepositories())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
	assert.Equal(t, 2, len(*(*(*releaseTypes2).GetItems())["type1"].GetAssets()))
	assert.Equal(t, "asset1", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[0])
	assert.Equal(t, "asset2", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[1])
	assert.True(t, *(*(*releaseTypes2).GetItems())["type1"].GetCollapseVersions())
	assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *(*(*releaseTypes2).GetItems())["type1"].GetCollapsedVersionQualifier())
	assert.Equal(t, "Release description", *(*(*releaseTypes2).GetItems())["type1"].GetDescription())
	assert.Equal(t, "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", *(*(*releaseTypes2).GetItems())["type1"].GetFilterTags())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommit())
	assert.Equal(t, "Committing {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommitMessage())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitPush())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitTag())
	assert.Equal(t, "Tagging {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitTagMessage())
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers()) == 0)
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetMatchBranches())
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables()) == 0)
	assert.Nil(t, (*(*releaseTypes2).GetItems())["type1"].GetMatchWorkspaceStatus())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetPublish())
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetVersionRange())
	assert.Equal(t, false, *(*(*releaseTypes2).GetItems())["type1"].GetVersionRangeFromBranchName())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	configuration.WithPluginConfiguration(nil)
	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
}

func TestConfigurationWithPluginConfigurationGetResume(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetResume(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RESUME)
	resume1, _ := configurationLayerMock.GetResume()
	assert.NotEqual(t, *ent.RESUME, *resume1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RESUME)
	resume2, _ := configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	resume2, _ = configuration.GetResume()
	assert.Equal(t, *resume1, *resume2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	resume2, _ = configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)
}

func TestConfigurationWithPluginConfigurationGetScheme(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))

	scheme1, _ := configurationLayerMock.GetScheme()
	// since there is only one scheme available, this assumption can't be assumed
	//assert.NotEqual(t, *ent.SCHEME, configurationLayerMock.scheme)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SCHEME)
	scheme2, _ := configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)
}

func TestConfigurationWithPluginConfigurationGetServices(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo1", "REPOSITORY_OWNER": "owner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo2", "REPOSITORY_OWNER": "owner2"})})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SERVICES)
	services1, _ := configurationLayerMock.GetServices()
	assert.NotEqual(t, *ent.SERVICES, *services1)
	assert.Equal(t, 2, len((*services1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SERVICES)
	services2, _ := configuration.GetServices()
	assert.Equal(t, *ent.SERVICES, *services2)
	assert.Equal(t, 0, len((*services2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	services2, _ = configuration.GetServices()
	assert.Equal(t, 2, len((*services2)))
	assert.NotNil(t, (*services2)["github"])
	assert.NotNil(t, (*services2)["gitlab"])
	assert.Equal(t, ent.GITHUB, *(*services2)["github"].GetType())
	assert.Equal(t, 3, len(*(*services2)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services2)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo1", (*(*services2)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner1", (*(*services2)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, ent.GITLAB, *(*services2)["gitlab"].GetType())
	assert.Equal(t, 3, len(*(*services2)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services2)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_OWNER"])

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	configuration.WithPluginConfiguration(nil)
	services2, _ = configuration.GetServices()
	assert.NotNil(t, services2)
	assert.Equal(t, 0, len((*services2)))
}

func TestConfigurationWithPluginConfigurationGetSharedConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// in order to make the test meaningful, make sure the default and mock values are different
	sharedConfigurationFile1, _ := configurationLayerMock.GetSharedConfigurationFile()
	assert.NotNil(t, sharedConfigurationFile1)
	assert.Nil(t, ent.SHARED_CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	sharedConfigurationFile2, _ := configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, sharedConfigurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Equal(t, *sharedConfigurationFile1, *sharedConfigurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
}

func TestConfigurationWithPluginConfigurationGetStateFile(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetStateFile(utl.PointerToString("state-file.yml"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.STATE_FILE)
	stateFile1, _ := configurationLayerMock.GetStateFile()
	assert.Equal(t, "state-file.yml", *stateFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.STATE_FILE)
	stateFile2, _ := configuration.GetStateFile()
	assert.Nil(t, stateFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	stateFile2, _ = configuration.GetStateFile()
	assert.Equal(t, *stateFile1, *stateFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	stateFile2, _ = configuration.GetStateFile()
	assert.Nil(t, stateFile2)
}

func TestConfigurationWithPluginConfigurationGetSubstitutions(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	substitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution1")}, &map[string]*ent.Substitution{"substitution1": ent.NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))})
	configurationLayerMock.SetSubstitutions(substitutions)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.SUBSTITUTIONS)
	substitutions1, _ := configurationLayerMock.GetSubstitutions()
	assert.NotNil(t, substitutions1)
	substitutions2, _ := configuration.GetSubstitutions()
	assert.NotNil(t, substitutions2)
	assert.NotEqual(t, substitutions1, substitutions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	substitutions2, _ = configuration.GetSubstitutions()
	assert.NotNil(t, (*substitutions2).GetEnabled())
	assert.NotNil(t, (*substitutions2).GetItems())
	assert.Equal(t, 1, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, "substitution1", *(*(*substitutions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*substitutions2).GetItems()))
	assert.Equal(t, "glob1", *(*(*(*substitutions2).GetItems())["substitution1"]).GetFiles())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithPluginConfiguration(nil)
	substitutions2, _ = configuration.GetSubstitutions()
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))
}

func TestConfigurationWithPluginConfigurationGetSummary(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSummary(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SUMMARY)
	summary1, _ := configurationLayerMock.GetSummary()
	assert.NotEqual(t, *ent.SUMMARY, *summary1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SUMMARY)
	summary2, _ := configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *summary1, *summary2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)
}

func TestConfigurationWithPluginConfigurationGetSummaryFile(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSummaryFile(utl.PointerToString("summary.txt"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile1, _ := configurationLayerMock.GetSummaryFile()
	assert.Equal(t, "summary.txt", *summaryFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile2, _ := configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Equal(t, *summaryFile1, *summaryFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)
}

func TestConfigurationWithPluginConfigurationGetVerbosity(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.TRACE))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.VERBOSITY)
	verbosity1, _ := configurationLayerMock.GetVerbosity()
	assert.NotEqual(t, *ent.VERBOSITY, *verbosity1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.VERBOSITY)
	verbosity2, _ := configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *verbosity1, *verbosity2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)
}

func TestConfigurationWithPluginConfigurationGetVersion(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetVersion(utl.PointerToString("11.12.13"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.VERSION)
	version1, _ := configurationLayerMock.GetVersion()
	assert.Equal(t, "11.12.13", *version1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.VERSION)
	version2, _ := configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithPluginConfiguration(&cl)

	version2, _ = configuration.GetVersion()
	assert.Equal(t, *version1, *version2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithPluginConfiguration(nil)
	version2, _ = configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)
}

/*
Performs checks against the injection of a runtime configuration
*/
func TestConfigurationWithRuntimeConfigurationGetBump(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetBump(utl.PointerToString("alpha"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.BUMP)
	bump1, _ := configurationLayerMock.GetBump()
	assert.Equal(t, "alpha", *bump1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.BUMP)
	bump2, _ := configuration.GetBump()
	assert.Nil(t, bump2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	bump2, _ = configuration.GetBump()
	assert.NotNil(t, bump2)
	assert.Equal(t, *bump1, *bump2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	bump2, _ = configuration.GetBump()
	assert.Nil(t, bump2)
}

func TestConfigurationWithRuntimeConfigurationGetChangelog(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	changelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Section1": "regex1", "Section2": "regex2"}, utl.PointerToString("changelog.tpl"), &map[string]string{"Expression1": "string1"})
	configurationLayerMock.SetChangelog(changelogConfiguration)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.CHANGELOG)
	changelog1, _ := configurationLayerMock.GetChangelog()
	assert.NotNil(t, changelog1)
	changelog2, _ := configuration.GetChangelog()
	assert.NotNil(t, changelog2)
	assert.NotEqual(t, changelog1, changelog2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, (*ent.CHANGELOG).GetPath())
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*ent.CHANGELOG).GetSubstitutions()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*ent.CHANGELOG).GetTemplate())
	assert.Nil(t, (*changelog2).GetTemplate())

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	changelog2, _ = configuration.GetChangelog()
	assert.Equal(t, "CHANGELOG.md", *(*changelog2).GetPath())
	assert.NotNil(t, (*changelog2).GetSections())
	assert.Equal(t, 2, len(*(*changelog2).GetSections()))
	assert.Equal(t, "regex1", (*(*changelog2).GetSections())["Section1"])
	assert.Equal(t, "regex2", (*(*changelog2).GetSections())["Section2"])
	assert.NotNil(t, (*changelog2).GetSubstitutions())
	assert.Equal(t, 1, len(*(*changelog2).GetSubstitutions()))
	assert.Equal(t, "string1", (*(*changelog2).GetSubstitutions())["Expression1"])
	assert.Equal(t, "changelog.tpl", *(*changelog2).GetTemplate())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithRuntimeConfiguration(nil)
	changelog2, _ = configuration.GetChangelog()
	assert.Nil(t, (*changelog2).GetPath())
	assert.Equal(t, 0, len(*(*changelog2).GetSections()))
	assert.Equal(t, 0, len(*(*changelog2).GetSubstitutions()))
	assert.Nil(t, (*changelog2).GetTemplate())
}

func TestConfigurationWithRuntimeConfigurationGetCommitMessageConventions(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention1")}, &map[string]*ent.CommitMessageConvention{"convention1": ent.NewCommitMessageConventionWith(utl.PointerToString("expr1"), &map[string]string{})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.COMMIT_MESSAGE_CONVENTIONS)
	commitMessageConventions1, _ := configurationLayerMock.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions1)
	commitMessageConventions2, _ := configuration.GetCommitMessageConventions()
	assert.NotNil(t, commitMessageConventions2)
	assert.NotEqual(t, commitMessageConventions1, commitMessageConventions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.COMMIT_MESSAGE_CONVENTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.NotNil(t, (*commitMessageConventions2).GetEnabled())
	assert.NotNil(t, (*commitMessageConventions2).GetItems())
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, "convention1", *(*(*commitMessageConventions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*commitMessageConventions2).GetItems()))
	assert.Equal(t, "expr1", *(*(*(*commitMessageConventions2).GetItems())["convention1"]).GetExpression())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithRuntimeConfiguration(nil)
	commitMessageConventions2, _ = configuration.GetCommitMessageConventions()
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*commitMessageConventions2).GetItems()))
}

func TestConfigurationWithRuntimeConfigurationGetConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// in order to make the test meaningful, make sure the default and mock values are different
	configurationFile1, _ := configurationLayerMock.GetConfigurationFile()
	assert.NotNil(t, configurationFile1)
	assert.Nil(t, ent.CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	configurationFile2, _ := configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, configurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.NotNil(t, configurationFile2)
	assert.Equal(t, *configurationFile1, *configurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	configurationFile2, _ = configuration.GetConfigurationFile()
	assert.Nil(t, configurationFile2)
}

func TestConfigurationWithRuntimeConfigurationGetDirectory(t *testing.T) {
	SetDefaultDirectory(nil) // clean the singleton from previous runs
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetDirectory(utl.PointerToString("some/directory"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DIRECTORY)
	directory1, _ := configurationLayerMock.GetDirectory()
	assert.Equal(t, "some/directory", *directory1)
	assert.NotEqual(t, *ent.DIRECTORY, *directory1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DIRECTORY)
	directory2, _ := configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *directory1, *directory2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	directory2, _ = configuration.GetDirectory()
	assert.Equal(t, *ent.DIRECTORY, *directory2)
}

func TestConfigurationWithRuntimeConfigurationGetDryRun(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetDryRun(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.DRY_RUN)
	dryRun1, _ := configurationLayerMock.GetDryRun()
	assert.NotEqual(t, *ent.DRY_RUN, *dryRun1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.DRY_RUN)
	dryRun2, _ := configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *dryRun1, *dryRun2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	dryRun2, _ = configuration.GetDryRun()
	assert.Equal(t, *ent.DRY_RUN, *dryRun2)
}

func TestConfigurationWithRuntimeConfigurationGetGit(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	gitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), utl.PointerToString("jdoe"), utl.PointerToString("pwd"), utl.PointerToString("key"), utl.PointerToString("passphrase"))})
	configurationLayerMock.SetGit(gitConfiguration)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.GIT)
	git1, _ := configurationLayerMock.GetGit()
	assert.NotNil(t, git1)
	assert.NotEqual(t, *ent.GIT, *git1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, *ent.GIT)
	git2, _ := configuration.GetGit()
	assert.NotNil(t, git2)
	assert.NotEqual(t, *git1, *git2)

	assert.Equal(t, 0, len((*(*ent.GIT).GetRemotes())))
	assert.Equal(t, 0, len((*(*git2).GetRemotes())))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	git2, _ = configuration.GetGit()
	assert.NotNil(t, (*git2).GetRemotes())
	assert.Equal(t, 1, len(*(*git2).GetRemotes()))
	assert.Equal(t, ent.PUBLIC_KEY, *(*(*git2).GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, "pwd", *(*(*git2).GetRemotes())["origin"].GetPassword())
	assert.Equal(t, "jdoe", *(*(*git2).GetRemotes())["origin"].GetUser())
	assert.Equal(t, "key", *(*(*git2).GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, "passphrase", *(*(*git2).GetRemotes())["origin"].GetPassphrase())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	configuration.WithRuntimeConfiguration(nil)
	git2, _ = configuration.GetGit()
	assert.Equal(t, 0, len(*(*git2).GetRemotes()))
}

func TestConfigurationWithRuntimeConfigurationGetInitialVersion(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetInitialVersion(utl.PointerToString("9.9.9"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion1, _ := configurationLayerMock.GetInitialVersion()
	assert.NotEqual(t, *ent.INITIAL_VERSION, *initialVersion1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.INITIAL_VERSION)
	initialVersion2, _ := configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *initialVersion1, *initialVersion2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	initialVersion2, _ = configuration.GetInitialVersion()
	assert.Equal(t, *ent.INITIAL_VERSION, *initialVersion2)
}

func TestConfigurationWithRuntimeConfigurationGetPreset(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetPreset(utl.PointerToString("simple"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.PRESET)
	preset1, _ := configurationLayerMock.GetPreset()
	assert.Equal(t, "simple", *preset1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.PRESET)
	preset2, _ := configuration.GetPreset()
	assert.Nil(t, preset2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	preset2, _ = configuration.GetPreset()
	assert.Equal(t, *preset1, *preset2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	preset2, _ = configuration.GetPreset()
	assert.Nil(t, preset2)
}

func TestConfigurationWithRuntimeConfigurationGetReleaseAssets(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"asset1": ent.NewAttachmentWith(utl.PointerToString("asset.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("asset.txt"), utl.PointerToString("text/plain")), "asset2": ent.NewAttachmentWith(utl.PointerToString("asset.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("asset.bin"), utl.PointerToString("application/octet-stream"))})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets1, _ := configurationLayerMock.GetReleaseAssets()
	assert.NotEqual(t, *ent.RELEASE_ASSETS, *releaseAssets1)
	assert.Equal(t, 2, len((*releaseAssets1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_ASSETS)
	releaseAssets2, _ := configuration.GetReleaseAssets()
	assert.Equal(t, *ent.RELEASE_ASSETS, *releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.Equal(t, 2, len((*releaseAssets2)))
	assert.NotNil(t, (*releaseAssets2)["asset1"])
	assert.NotNil(t, (*releaseAssets2)["asset2"])
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetFileName())
	assert.Equal(t, "Text Asset", *(*releaseAssets2)["asset1"].GetDescription())
	assert.Equal(t, "text/plain", *(*releaseAssets2)["asset1"].GetType())
	assert.Equal(t, "asset.txt", *(*releaseAssets2)["asset1"].GetPath())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetFileName())
	assert.Equal(t, "Binary Asset", *(*releaseAssets2)["asset2"].GetDescription())
	assert.Equal(t, "application/octet-stream", *(*releaseAssets2)["asset2"].GetType())
	assert.Equal(t, "asset.bin", *(*releaseAssets2)["asset2"].GetPath())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	configuration.WithRuntimeConfiguration(nil)
	releaseAssets2, _ = configuration.GetReleaseAssets()
	assert.NotNil(t, releaseAssets2)
	assert.Equal(t, 0, len((*releaseAssets2)))
}

func TestConfigurationWithRuntimeConfigurationGetReleaseLenient(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient1, _ := configurationLayerMock.GetReleaseLenient()
	assert.NotEqual(t, *ent.RELEASE_LENIENT, *releaseLenient1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_LENIENT)
	releaseLenient2, _ := configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *releaseLenient1, *releaseLenient2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	releaseLenient2, _ = configuration.GetReleaseLenient()
	assert.Equal(t, *ent.RELEASE_LENIENT, *releaseLenient2)
}

func TestConfigurationWithRuntimeConfigurationGetReleasePrefix(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetReleasePrefix(utl.PointerToString("testprefix"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix1, _ := configurationLayerMock.GetReleasePrefix()
	assert.Equal(t, "testprefix", *releasePrefix1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.RELEASE_PREFIX)
	releasePrefix2, _ := configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Equal(t, *releasePrefix1, *releasePrefix2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	releasePrefix2, _ = configuration.GetReleasePrefix()
	assert.Nil(t, releasePrefix2)
}

func TestConfigurationWithRuntimeConfigurationGetReleaseTypes(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type1")}, &[]*string{utl.PointerToString("service1")}, &[]*string{utl.PointerToString("remote1")}, &map[string]*ent.ReleaseType{"type1": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), utl.PointerToString("Release description"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes1, _ := configurationLayerMock.GetReleaseTypes()
	assert.NotEqual(t, *ent.RELEASE_TYPES, *releaseTypes1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RELEASE_TYPES)
	releaseTypes2, _ := configuration.GetReleaseTypes()
	assert.Equal(t, *ent.RELEASE_TYPES, *releaseTypes2)

	assert.NotNil(t, *ent.RELEASE_TYPES.GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, *ent.RELEASE_TYPES.GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.Equal(t, 1, len(*(*ent.RELEASE_TYPES).GetItems()))
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.NotNil(t, (*releaseTypes2).GetPublicationServices())
	assert.NotNil(t, (*releaseTypes2).GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes2).GetItems())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetEnabled()))
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetPublicationServices()))
	assert.Equal(t, "service1", *(*(*releaseTypes2).GetPublicationServices())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetRemoteRepositories()))
	assert.Equal(t, "remote1", *(*(*releaseTypes2).GetRemoteRepositories())[0])
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
	assert.Equal(t, 2, len(*(*(*releaseTypes2).GetItems())["type1"].GetAssets()))
	assert.Equal(t, "asset1", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[0])
	assert.Equal(t, "asset2", *(*(*(*releaseTypes2).GetItems())["type1"].GetAssets())[1])
	assert.True(t, *(*(*releaseTypes2).GetItems())["type1"].GetCollapseVersions())
	assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *(*(*releaseTypes2).GetItems())["type1"].GetCollapsedVersionQualifier())
	assert.Equal(t, "Release description", *(*(*releaseTypes2).GetItems())["type1"].GetDescription())
	assert.Equal(t, "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", *(*(*releaseTypes2).GetItems())["type1"].GetFilterTags())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommit())
	assert.Equal(t, "Committing {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitCommitMessage())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitPush())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetGitTag())
	assert.Equal(t, "Tagging {{version}}", *(*(*releaseTypes2).GetItems())["type1"].GetGitTagMessage())
	assert.Equal(t, "type1", *(*(*releaseTypes2).GetEnabled())[0])
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetIdentifiers()) == 0)
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetMatchBranches())
	assert.NotNil(t, *(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables())
	assert.False(t, len(*(*(*releaseTypes2).GetItems())["type1"].GetMatchEnvironmentVariables()) == 0)
	assert.Nil(t, (*(*releaseTypes2).GetItems())["type1"].GetMatchWorkspaceStatus())
	assert.Equal(t, "true", *(*(*releaseTypes2).GetItems())["type1"].GetPublish())
	assert.Equal(t, "", *(*(*releaseTypes2).GetItems())["type1"].GetVersionRange())
	assert.Equal(t, false, *(*(*releaseTypes2).GetItems())["type1"].GetVersionRangeFromBranchName())

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	configuration.WithRuntimeConfiguration(nil)
	releaseTypes2, _ = configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes2).GetEnabled())
	assert.Equal(t, 1, len(*(*releaseTypes2).GetItems()))
}

func TestConfigurationWithRuntimeConfigurationGetResume(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetResume(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.RESUME)
	resume1, _ := configurationLayerMock.GetResume()
	assert.NotEqual(t, *ent.RESUME, *resume1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.RESUME)
	resume2, _ := configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	resume2, _ = configuration.GetResume()
	assert.Equal(t, *resume1, *resume2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	resume2, _ = configuration.GetResume()
	assert.Equal(t, *ent.RESUME, *resume2)
}

func TestConfigurationWithRuntimeConfigurationGetScheme(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))

	scheme1, _ := configurationLayerMock.GetScheme()
	// since there is only one scheme available, this assumption can't be assumed
	//assert.NotEqual(t, *ent.SCHEME, configurationLayerMock.scheme)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SCHEME)
	scheme2, _ := configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *scheme1, *scheme2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	scheme2, _ = configuration.GetScheme()
	assert.Equal(t, *ent.SCHEME, *scheme2)
}

func TestConfigurationWithRuntimeConfigurationGetServices(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo1", "REPOSITORY_OWNER": "owner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo2", "REPOSITORY_OWNER": "owner2"})})

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SERVICES)
	services1, _ := configurationLayerMock.GetServices()
	assert.NotEqual(t, *ent.SERVICES, *services1)
	assert.Equal(t, 2, len((*services1)))

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SERVICES)
	services2, _ := configuration.GetServices()
	assert.Equal(t, *ent.SERVICES, *services2)
	assert.Equal(t, 0, len((*services2)))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	services2, _ = configuration.GetServices()
	assert.Equal(t, 2, len((*services2)))
	assert.NotNil(t, (*services2)["github"])
	assert.NotNil(t, (*services2)["gitlab"])
	assert.Equal(t, ent.GITHUB, *(*services2)["github"].GetType())
	assert.Equal(t, 3, len(*(*services2)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services2)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo1", (*(*services2)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner1", (*(*services2)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, ent.GITLAB, *(*services2)["gitlab"].GetType())
	assert.Equal(t, 3, len(*(*services2)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services2)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner2", (*(*services2)["gitlab"].GetOptions())["REPOSITORY_OWNER"])

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	configuration.WithRuntimeConfiguration(nil)
	services2, _ = configuration.GetServices()
	assert.NotNil(t, services2)
	assert.Equal(t, 0, len((*services2)))
}

func TestConfigurationWithRuntimeConfigurationGetSharedConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// in order to make the test meaningful, make sure the default and mock values are different
	sharedConfigurationFile1, _ := configurationLayerMock.GetSharedConfigurationFile()
	assert.NotNil(t, sharedConfigurationFile1)
	assert.Nil(t, ent.SHARED_CONFIGURATION_FILE)

	// make sure the initial values come from defaults, until we inject the command line configuration
	sharedConfigurationFile2, _ := configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
	assert.Equal(t, ent.CONFIGURATION_FILE, sharedConfigurationFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Equal(t, *sharedConfigurationFile1, *sharedConfigurationFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	sharedConfigurationFile2, _ = configuration.GetSharedConfigurationFile()
	assert.Nil(t, sharedConfigurationFile2)
}

func TestConfigurationWithRuntimeConfigurationGetStateFile(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetStateFile(utl.PointerToString("state-file.yml"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.STATE_FILE)
	stateFile1, _ := configurationLayerMock.GetStateFile()
	assert.Equal(t, "state-file.yml", *stateFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.STATE_FILE)
	stateFile2, _ := configuration.GetStateFile()
	assert.Nil(t, stateFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	stateFile2, _ = configuration.GetStateFile()
	assert.Equal(t, *stateFile1, *stateFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	stateFile2, _ = configuration.GetStateFile()
	assert.Nil(t, stateFile2)
}

func TestConfigurationWithRuntimeConfigurationGetSubstitutions(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	substitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution1")}, &map[string]*ent.Substitution{"substitution1": ent.NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))})
	configurationLayerMock.SetSubstitutions(substitutions)

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, *ent.SUBSTITUTIONS)
	substitutions1, _ := configurationLayerMock.GetSubstitutions()
	assert.NotNil(t, substitutions1)
	substitutions2, _ := configuration.GetSubstitutions()
	assert.NotNil(t, substitutions2)
	assert.NotEqual(t, substitutions1, substitutions2)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*ent.SUBSTITUTIONS).GetItems()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	substitutions2, _ = configuration.GetSubstitutions()
	assert.NotNil(t, (*substitutions2).GetEnabled())
	assert.NotNil(t, (*substitutions2).GetItems())
	assert.Equal(t, 1, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, "substitution1", *(*(*substitutions2).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*substitutions2).GetItems()))
	assert.Equal(t, "glob1", *(*(*(*substitutions2).GetItems())["substitution1"]).GetFiles())

	// now remove the command line configuration and test that now default values are returned again
	configuration.WithRuntimeConfiguration(nil)
	substitutions2, _ = configuration.GetSubstitutions()
	assert.Equal(t, 0, len(*(*substitutions2).GetEnabled()))
	assert.Equal(t, 0, len(*(*substitutions2).GetItems()))
}

func TestConfigurationWithRuntimeConfigurationGetSummary(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSummary(utl.PointerToBoolean(true))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.SUMMARY)
	summary1, _ := configurationLayerMock.GetSummary()
	assert.NotEqual(t, *ent.SUMMARY, *summary1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.SUMMARY)
	summary2, _ := configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *summary1, *summary2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	summary2, _ = configuration.GetSummary()
	assert.Equal(t, *ent.SUMMARY, *summary2)
}

func TestConfigurationWithRuntimeConfigurationGetSummaryFile(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetSummaryFile(utl.PointerToString("summary.txt"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile1, _ := configurationLayerMock.GetSummaryFile()
	assert.Equal(t, "summary.txt", *summaryFile1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.SUMMARY_FILE)
	summaryFile2, _ := configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Equal(t, *summaryFile1, *summaryFile2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	summaryFile2, _ = configuration.GetSummaryFile()
	assert.Nil(t, summaryFile2)
}

func TestConfigurationWithRuntimeConfigurationGetVerbosity(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.TRACE))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.NotNil(t, ent.VERBOSITY)
	verbosity1, _ := configurationLayerMock.GetVerbosity()
	assert.NotEqual(t, *ent.VERBOSITY, *verbosity1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.NotNil(t, ent.VERBOSITY)
	verbosity2, _ := configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *verbosity1, *verbosity2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)

	verbosity2, _ = configuration.GetVerbosity()
	assert.Equal(t, *ent.VERBOSITY, *verbosity2)
}

func TestConfigurationWithRuntimeConfigurationGetVersion(t *testing.T) {
	configurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	configurationLayerMock.SetVersion(utl.PointerToString("11.12.13"))

	// in order to make the test meaningful, make sure the default and mock values are different
	assert.Nil(t, ent.VERSION)
	version1, _ := configurationLayerMock.GetVersion()
	assert.Equal(t, "11.12.13", *version1)

	// make sure the initial values come from defaults, until we inject the command line configuration
	assert.Nil(t, ent.VERSION)
	version2, _ := configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)

	// inject the command line configuration and test the new value is returned from that
	var cl ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	version2, _ = configuration.GetVersion()
	assert.Equal(t, *version1, *version2)

	// now remove the command line configuration and test that now default values are returned again
	configuration, _ = configuration.WithRuntimeConfiguration(nil)
	version2, _ = configuration.GetVersion()
	assert.Equal(t, ent.VERSION, version2)
}

/*
Performs checks against the injection of multiple configuration layers
*/
func TestConfigurationWithMultipleConfigurationLayersGetBump(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()

	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetBump(utl.PointerToString("alpha"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--bump=beta",
	})
	highPriorityConfigurationLayerMock.SetBump(utl.PointerToString("gamma"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpBump, _ := highPriorityConfigurationLayerMock.GetBump()
	bump, _ := configuration.GetBump()
	assert.Equal(t, *hpBump, *bump)
}

func TestConfigurationWithMultipleConfigurationLayersGetChangelog(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG1.md"), &map[string]string{"SectionA1": "regexA1", "SectionA2": "regexA2"}, utl.PointerToString("changelog1.tpl"), &map[string]string{"Expression1": "string1"})
	lowPriorityConfigurationLayerMock.SetChangelog(lpChangelogConfiguration)
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--changelog-path=CHANGELOG2.md",
		"--changelog-sections-SectionB1=regexB1",
		"--changelog-sections-SectionB2=regexB2",
		"--changelog-substitutions-Expression2=string2",
		"--changelog-template=changelog2.tpl",
	})
	hpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG3.md"), &map[string]string{"SectionC1": "regexC1", "SectionC2": "regexC2"}, utl.PointerToString("changelog3.tpl"), &map[string]string{"Expression3": "string3"})
	highPriorityConfigurationLayerMock.SetChangelog(hpChangelogConfiguration)

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	changelog, _ := configuration.GetChangelog()
	assert.Equal(t, "CHANGELOG3.md", *changelog.GetPath())
	assert.NotNil(t, *changelog.GetSections())
	assert.Equal(t, 2, len(*changelog.GetSections()))
	assert.Equal(t, "regexC1", (*changelog.GetSections())["SectionC1"])
	assert.Equal(t, "regexC2", (*changelog.GetSections())["SectionC2"])
	assert.NotNil(t, *changelog.GetSubstitutions())
	assert.Equal(t, 1, len(*changelog.GetSubstitutions()))
	assert.Equal(t, "string3", (*changelog.GetSubstitutions())["Expression3"])
	assert.Equal(t, "changelog3.tpl", *changelog.GetTemplate())
}

func TestConfigurationWithMultipleConfigurationLayersGetCommitMessageConventions(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()

	lpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention1")}, &map[string]*ent.CommitMessageConvention{"convention1": ent.NewCommitMessageConventionWith(utl.PointerToString("expr1"), &map[string]string{})})
	lowPriorityConfigurationLayerMock.SetCommitMessageConventions(lpCommitMessageConventions)
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--commit-message-conventions-enabled=convention2",
		"--commit-message-conventions-convention2-expression=expr2",
	})
	hpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention3")}, &map[string]*ent.CommitMessageConvention{"convention3": ent.NewCommitMessageConventionWith(utl.PointerToString("expr3"), &map[string]string{})})
	highPriorityConfigurationLayerMock.SetCommitMessageConventions(hpCommitMessageConventions)

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	commitMessageConventions, _ := configuration.GetCommitMessageConventions()
	assert.NotNil(t, *commitMessageConventions.GetEnabled())
	assert.NotNil(t, *commitMessageConventions.GetItems())
	assert.Equal(t, 1, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, "convention3", *(*commitMessageConventions.GetEnabled())[0])
	assert.Equal(t, 1, len(*commitMessageConventions.GetItems()))
	assert.Equal(t, "expr3", *(*commitMessageConventions.GetItems())["convention3"].GetExpression())
}

func TestConfigurationWithMultipleConfigurationLayersGetConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	assert.NotNil(t, os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--configuration-file=" + os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE),
	})
	highPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpConfigurationFile, _ := highPriorityConfigurationLayerMock.GetConfigurationFile()
	configurationFile, _ := configuration.GetConfigurationFile()
	assert.Equal(t, *hpConfigurationFile, *configurationFile)
}

func TestConfigurationWithMultipleConfigurationLayersGetDirectory(t *testing.T) {
	SetDefaultDirectory(nil) // clean the singleton from previous runs
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("some/directory"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--directory=some/other/directory",
	})
	highPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("the/right/directory"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpDirectory, _ := highPriorityConfigurationLayerMock.GetDirectory()
	directory, _ := configuration.GetDirectory()
	assert.Equal(t, *hpDirectory, *directory)
}

func TestConfigurationWithMultipleConfigurationLayersGetDryRun(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--dry-run",
	})
	highPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(false))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpDryRun, _ := highPriorityConfigurationLayerMock.GetDryRun()
	dryRun, _ := configuration.GetDryRun()
	assert.Equal(t, *hpDryRun, *dryRun)
}

func TestConfigurationWithMultipleConfigurationLayersGetGit(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("jdoe1"), utl.PointerToString("pwd1"), nil, nil), "replica": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("stiger1"), utl.PointerToString("sec1"), nil, nil)})
	lowPriorityConfigurationLayerMock.SetGit(lpGitConfiguration)
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--git-remotes-origin-user=jdoe2",
		"--git-remotes-origin-password=pwd2",
		"--git-remotes-clone-user=stiger2",
		"--git-remotes-clone-password=sec2",
	})
	hpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), utl.PointerToString("jdoe3"), utl.PointerToString("pwd3"), utl.PointerToString("key3"), utl.PointerToString("passphrase3"))})
	highPriorityConfigurationLayerMock.SetGit(hpGitConfiguration)

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	git, _ := configuration.GetGit()
	assert.NotNil(t, *git.GetRemotes())
	assert.Equal(t, 3, len(*git.GetRemotes()))
	assert.Equal(t, ent.PUBLIC_KEY, *(*(*git).GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, "pwd3", *(*git.GetRemotes())["origin"].GetPassword())
	assert.Equal(t, "jdoe3", *(*git.GetRemotes())["origin"].GetUser())
	assert.Equal(t, "key3", *(*(*git).GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, "passphrase3", *(*(*git).GetRemotes())["origin"].GetPassphrase())
	assert.Nil(t, (*(*git).GetRemotes())["replica"].GetAuthenticationMethod())
	assert.Equal(t, "sec1", *(*git.GetRemotes())["replica"].GetPassword())
	assert.Equal(t, "stiger1", *(*git.GetRemotes())["replica"].GetUser())
	assert.Nil(t, (*(*git).GetRemotes())["replica"].GetPrivateKey())
	assert.Nil(t, (*(*git).GetRemotes())["replica"].GetPassphrase())
	assert.Nil(t, (*(*git).GetRemotes())["clone"].GetAuthenticationMethod())
	assert.Equal(t, "sec2", *(*git.GetRemotes())["clone"].GetPassword())
	assert.Equal(t, "stiger2", *(*git.GetRemotes())["clone"].GetUser())
	assert.Nil(t, (*(*git).GetRemotes())["clone"].GetPrivateKey())
	assert.Nil(t, (*(*git).GetRemotes())["clone"].GetPassphrase())
}

func TestConfigurationWithMultipleConfigurationLayersGetInitialVersion(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("9.9.9"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--initial-version=8.8.8",
	})
	highPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("7.7.7"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpInitialVersion, _ := highPriorityConfigurationLayerMock.GetInitialVersion()
	initialVersion, _ := configuration.GetInitialVersion()
	assert.Equal(t, *hpInitialVersion, *initialVersion)
}

func TestConfigurationWithMultipleConfigurationLayersGetPreset(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(SIMPLE_NAME))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--preset=" + SIMPLE_NAME,
	})
	highPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(EXTENDED_NAME))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpPreset, _ := highPriorityConfigurationLayerMock.GetPreset()
	preset, _ := configuration.GetPreset()
	assert.Equal(t, *hpPreset, *preset)
}

func TestConfigurationWithMultipleConfigurationLayersGetReleaseAssets(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"lpasset1": ent.NewAttachmentWith(utl.PointerToString("lpasset.txt"), utl.PointerToString("LP Text Asset"), utl.PointerToString("lpasset.txt"), utl.PointerToString("text/plain")), "lpasset2": ent.NewAttachmentWith(utl.PointerToString("lpasset.bin"), utl.PointerToString("LP Binary Asset"), utl.PointerToString("lpasset.bin"), utl.PointerToString("application/octet-stream"))})
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--release-assets-mpasset1-fileName=mpasset.txt",
		"--release-assets-mpasset1-description='MP Text Asset'",
		"--release-assets-mpasset1-type=text/plain",
		"--release-assets-mpasset1-path=mpasset.txt",
		"--release-assets-mpasset2-fileName=mpasset.bin",
		"--release-assets-mpasset2-description='MP Binary Asset'",
		"--release-assets-mpasset2-type=application/octet-stream",
		"--release-assets-mpasset2-path=mpasset.bin",
	})
	highPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"hpasset1": ent.NewAttachmentWith(utl.PointerToString("hpasset.txt"), utl.PointerToString("HP Text Asset"), utl.PointerToString("hpasset.txt"), utl.PointerToString("text/plain")), "hpasset2": ent.NewAttachmentWith(utl.PointerToString("hpasset.bin"), utl.PointerToString("HP Binary Asset"), utl.PointerToString("hpasset.bin"), utl.PointerToString("application/octet-stream"))})

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	releaseAssets, _ := configuration.GetReleaseAssets()
	assert.Equal(t, 6, len((*releaseAssets)))
	assert.NotNil(t, (*releaseAssets)["hpasset1"])
	assert.NotNil(t, (*releaseAssets)["hpasset2"])
	assert.Equal(t, "hpasset.txt", *(*releaseAssets)["hpasset1"].GetFileName())
	assert.Equal(t, "HP Text Asset", *(*releaseAssets)["hpasset1"].GetDescription())
	assert.Equal(t, "text/plain", *(*releaseAssets)["hpasset1"].GetType())
	assert.Equal(t, "hpasset.txt", *(*releaseAssets)["hpasset1"].GetPath())
	assert.Equal(t, "hpasset.bin", *(*releaseAssets)["hpasset2"].GetFileName())
	assert.Equal(t, "HP Binary Asset", *(*releaseAssets)["hpasset2"].GetDescription())
	assert.Equal(t, "application/octet-stream", *(*releaseAssets)["hpasset2"].GetType())
	assert.Equal(t, "hpasset.bin", *(*releaseAssets)["hpasset2"].GetPath())
}

func TestConfigurationWithMultipleConfigurationLayersGetReleaseLenient(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--release-lenient=false",
	})
	highPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpReleaseLenient, _ := highPriorityConfigurationLayerMock.GetReleaseLenient()
	releaseLenient, _ := configuration.GetReleaseLenient()
	assert.Equal(t, *hpReleaseLenient, *releaseLenient)
}

func TestConfigurationWithMultipleConfigurationLayersGetReleasePrefix(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("lpprefix"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--release-prefix=mpprefix",
	})
	highPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("hpprefix"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpReleasePrefix, _ := highPriorityConfigurationLayerMock.GetReleasePrefix()
	releasePrefix, _ := configuration.GetReleasePrefix()
	assert.Equal(t, *hpReleasePrefix, *releasePrefix)
}

func TestConfigurationWithMultipleConfigurationLayersGetReleaseTypes(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type1")}, &[]*string{utl.PointerToString("service1")}, &[]*string{utl.PointerToString("remote1")}, &map[string]*ent.ReleaseType{"type1": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("assetA1"), utl.PointerToString("assetA2")}, utl.PointerToBoolean(false), utl.PointerToString("{{branch1}}"), utl.PointerToString("Release description 1"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	lowPriorityConfigurationLayerMock.SetReleaseTypes(lpReleaseTypes)
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--release-types-enabled=type2",
		"--release-types-publication-services=service2",
		"--release-types-remote-repositories=remote2",
		"--release-types-type2-assets=assetB1,assetB2",
		"--release-types-type2-collapse-versions=true",
		"--release-types-type2-collapsed-version-qualifier={{branch2}}",
		"--release-types-type2-description=Release description 2",
		"--release-types-type2-filter-tags=^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
		"--release-types-type2-git-commit=true",
		"--release-types-type2-git-commit-message=Committing {{version}}",
		"--release-types-type2-git-push=true",
		"--release-types-type2-git-tag=true",
		"--release-types-type2-git-tag-message=Tagging {{version}}",
		"--release-types-type2-identifiers-0-position=" + ent.BUILD.String(),
		"--release-types-type2-identifiers-0-qualifier=build",
		"--release-types-type2-identifiers-0-value=12",
		"--release-types-type2-match-branches=",
		"--release-types-type2-match-environment-variables-PATH=.*",
		"--release-types-type2-publish=true",
		"--release-types-type2-version-range=",
		"--release-types-type2-version-range-from-branch-name=false",
	})
	hpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type3")}, &[]*string{utl.PointerToString("service3")}, &[]*string{utl.PointerToString("remote3")}, &map[string]*ent.ReleaseType{"type3": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("assetC1"), utl.PointerToString("assetC2")}, utl.PointerToBoolean(true), utl.PointerToString("{{branch3}}"), utl.PointerToString("Release description 3"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	highPriorityConfigurationLayerMock.SetReleaseTypes(hpReleaseTypes)

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	releaseTypes, _ := configuration.GetReleaseTypes()
	assert.NotNil(t, (*releaseTypes).GetEnabled())
	assert.NotNil(t, (*releaseTypes).GetPublicationServices())
	assert.NotNil(t, (*releaseTypes).GetRemoteRepositories())
	assert.NotNil(t, (*releaseTypes).GetItems())
	assert.Equal(t, 1, len(*(*releaseTypes).GetEnabled()))
	assert.Equal(t, "type3", *(*(*releaseTypes).GetEnabled())[0])
	assert.Equal(t, 1, len(*(*releaseTypes).GetPublicationServices()))
	assert.Equal(t, "service3", *(*(*releaseTypes).GetPublicationServices())[0])
	assert.Equal(t, 1, len(*(*releaseTypes).GetRemoteRepositories()))
	assert.Equal(t, "remote3", *(*(*releaseTypes).GetRemoteRepositories())[0])
	assert.Equal(t, 1, len(*(*releaseTypes).GetItems()))
	assert.Equal(t, 2, len(*(*(*releaseTypes).GetItems())["type3"].GetAssets()))
	assert.Equal(t, "assetC1", *(*(*(*releaseTypes).GetItems())["type3"].GetAssets())[0])
	assert.Equal(t, "assetC2", *(*(*(*releaseTypes).GetItems())["type3"].GetAssets())[1])
	assert.True(t, *(*(*releaseTypes).GetItems())["type3"].GetCollapseVersions())
	assert.Equal(t, "{{branch3}}", *(*(*releaseTypes).GetItems())["type3"].GetCollapsedVersionQualifier())
	assert.Equal(t, "Release description 3", *(*(*releaseTypes).GetItems())["type3"].GetDescription())
	assert.Equal(t, "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", *(*(*releaseTypes).GetItems())["type3"].GetFilterTags())
	assert.Equal(t, "true", *(*(*releaseTypes).GetItems())["type3"].GetGitCommit())
	assert.Equal(t, "Committing {{version}}", *(*(*releaseTypes).GetItems())["type3"].GetGitCommitMessage())
	assert.Equal(t, "true", *(*(*releaseTypes).GetItems())["type3"].GetGitPush())
	assert.Equal(t, "true", *(*(*releaseTypes).GetItems())["type3"].GetGitTag())
	assert.Equal(t, "Tagging {{version}}", *(*(*releaseTypes).GetItems())["type3"].GetGitTagMessage())
	assert.Nil(t, (*(*releaseTypes).GetItems())["type1"])
	assert.Nil(t, (*(*releaseTypes).GetItems())["type2"])
	assert.NotNil(t, (*(*releaseTypes).GetItems())["type3"])
	assert.NotNil(t, *(*(*releaseTypes).GetItems())["type3"].GetIdentifiers())
	assert.Equal(t, 1, len(*(*(*releaseTypes).GetItems())["type3"].GetIdentifiers()))
	assert.Equal(t, "", *(*(*releaseTypes).GetItems())["type3"].GetMatchBranches())
	assert.Equal(t, "true", *(*(*releaseTypes).GetItems())["type3"].GetPublish())
	assert.Equal(t, "", *(*(*releaseTypes).GetItems())["type3"].GetVersionRange())
	assert.Equal(t, false, *(*(*releaseTypes).GetItems())["type3"].GetVersionRangeFromBranchName())
}

func TestConfigurationWithMultipleConfigurationLayersGetResume(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--resume",
	})
	highPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(false))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpResume, _ := highPriorityConfigurationLayerMock.GetResume()
	resume, _ := configuration.GetResume()
	assert.Equal(t, *hpResume, *resume)
}

func TestConfigurationWithMultipleConfigurationLayersGetScheme(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--scheme=" + ver.SEMVER.String(),
	})
	highPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpScheme, _ := highPriorityConfigurationLayerMock.GetScheme()
	scheme, _ := configuration.GetScheme()
	assert.Equal(t, *hpScheme, *scheme)
}

func TestConfigurationWithMultipleConfigurationLayersGetServices(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken1", "REPOSITORY_NAME": "ignoredrepo1", "REPOSITORY_OWNER": "ignoredowner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken2", "REPOSITORY_NAME": "ignoredrepo2", "REPOSITORY_OWNER": "ignoredowner2"})})
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--services-github-type=GITHUB",
		"--services-github-options-AUTHENTICATION_TOKEN={{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
		"--services-github-options-REPOSITORY_NAME=repo1",
		"--services-github-options-REPOSITORY_OWNER=owner1",
		"--services-gitlab-type=GITLAB",
		"--services-gitlab-options-AUTHENTICATION_TOKEN={{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
		"--services-gitlab-options-REPOSITORY_NAME=repo2",
		"--services-gitlab-options-REPOSITORY_OWNER=owner2",
	})
	highPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo3", "REPOSITORY_OWNER": "owner3"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo4", "REPOSITORY_OWNER": "owner4"})})

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	services, _ := configuration.GetServices()
	assert.Equal(t, 2, len(*services))
	assert.NotNil(t, (*services)["github"])
	assert.NotNil(t, (*services)["gitlab"])
	assert.Equal(t, ent.GITHUB, *(*services)["github"].GetType())
	assert.Equal(t, 3, len(*(*services)["github"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", (*(*services)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo3", (*(*services)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner3", (*(*services)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, ent.GITLAB, *(*services)["gitlab"].GetType())
	assert.Equal(t, 3, len(*(*services)["gitlab"].GetOptions()))
	assert.Equal(t, "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", (*(*services)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo4", (*(*services)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, "owner4", (*(*services)["gitlab"].GetOptions())["REPOSITORY_OWNER"])
}

func TestConfigurationWithMultipleConfigurationLayersGetSharedConfigurationFile(t *testing.T) {
	assert.NotNil(t, os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	assert.NotNil(t, os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as a system property but it was not set")
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--shared-configuration-file=" + os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE),
	})
	highPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpSharedConfigurationFile, _ := highPriorityConfigurationLayerMock.GetSharedConfigurationFile()
	sharedConfigurationFile, _ := configuration.GetSharedConfigurationFile()
	assert.Equal(t, *hpSharedConfigurationFile, *sharedConfigurationFile)
}

func TestConfigurationWithMultipleConfigurationLayersGetStateFile(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.yaml"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--state-file=file.yml",
	})
	highPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.json"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpStateFile, _ := highPriorityConfigurationLayerMock.GetStateFile()
	stateFile, _ := configuration.GetStateFile()
	assert.Equal(t, *hpStateFile, *stateFile)
}

func TestConfigurationWithMultipleConfigurationLayersGetSubstitutions(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()

	lpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution1")}, &map[string]*ent.Substitution{"substitution1": ent.NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))})
	lowPriorityConfigurationLayerMock.SetSubstitutions(lpSubstitutions)
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--substitutions-enabled=substitution2",
		"--substitutions-substitution2-files=glob2",
	})
	hpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution3")}, &map[string]*ent.Substitution{"substitution3": ent.NewSubstitutionWith(utl.PointerToString("glob3"), utl.PointerToString("match3"), utl.PointerToString("replace3"))})
	highPriorityConfigurationLayerMock.SetSubstitutions(hpSubstitutions)

	// inject the command line configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	substitutions, _ := configuration.GetSubstitutions()
	assert.NotNil(t, *substitutions.GetEnabled())
	assert.NotNil(t, *substitutions.GetItems())
	assert.Equal(t, 1, len(*substitutions.GetEnabled()))
	assert.Equal(t, "substitution3", *(*substitutions.GetEnabled())[0])
	assert.Equal(t, 1, len(*substitutions.GetItems()))
	assert.Equal(t, "glob3", *(*substitutions.GetItems())["substitution3"].GetFiles())
}

func TestConfigurationWithMultipleConfigurationLayersGetSummary(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--summary",
	})
	highPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(false))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpSummary, _ := highPriorityConfigurationLayerMock.GetSummary()
	summary, _ := configuration.GetSummary()
	assert.Equal(t, *hpSummary, *summary)
}

func TestConfigurationWithMultipleConfigurationLayersGetSummaryFile(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.low"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--summary-file=summary.medium",
	})
	highPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.high"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpSummaryFile, _ := highPriorityConfigurationLayerMock.GetSummaryFile()
	summaryFile, _ := configuration.GetSummaryFile()
	assert.Equal(t, *hpSummaryFile, *summaryFile)
}

func TestConfigurationWithMultipleConfigurationLayersGetVerbosity(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.TRACE))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--info",
	})
	highPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.DEBUG))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpVerbosity, _ := highPriorityConfigurationLayerMock.GetVerbosity()
	verbosity, _ := configuration.GetVerbosity()
	assert.Equal(t, *hpVerbosity, *verbosity)
}

func TestConfigurationWithMultipleConfigurationLayersGetVersion(t *testing.T) {
	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewCommandLineConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("11.12.13"))
	mediumPriorityConfigurationLayerMock.withArguments([]string{
		"--version=21.22.23",
	})
	highPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("31.32.33"))

	// inject the plugin configuration and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	hpVersion, _ := highPriorityConfigurationLayerMock.GetVersion()
	version, _ := configuration.GetVersion()
	assert.Equal(t, *hpVersion, *version)
}

/*
Performs checks against the injection of a combination of configuration layers
*/
func TestConfigurationWithCombinedConfigurationWithStandardFiles(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	// create 4 different config files with the bump field different for each
	standardSharedConfiguration := NewSimpleConfigurationLayer()
	standardSharedConfiguration.SetBump(utl.PointerToString("standard-shared"))
	standardSharedConfiguration.SetReleasePrefix(utl.PointerToString("sharedPrefix"))
	standardLocalConfiguration := NewSimpleConfigurationLayer()
	standardLocalConfiguration.SetBump(utl.PointerToString("standard-local"))

	// save the standard files to standard locations
	standardSharedConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx-shared.json"))
	defer os.Remove(standardSharedConfigurationFile.Name())
	io.Save(standardSharedConfigurationFile.Name(), standardSharedConfiguration)

	standardLocalConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx.json"))
	defer os.Remove(standardLocalConfigurationFile.Name())
	io.Save(standardLocalConfigurationFile.Name(), standardLocalConfiguration)

	configuration, _ := NewConfiguration()

	bump, _ := configuration.GetBump()
	releasePrefix, _ := configuration.GetReleasePrefix()
	assert.Equal(t, "sharedPrefix", *releasePrefix) // this is configured only on the standard shared file
	assert.Equal(t, "standard-local", *bump)        // this is the value configured in the standard local file and has higher priority over the standard shared
}

func TestConfigurationWithCombinedConfigurationWithStandardAndCustomFiles(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	// create 4 different config files with the bump field different for each
	// here the standard local file adds the custom configuration file, which in turn adds the shared custom file
	// so we can test all the chain
	standardSharedConfiguration := NewSimpleConfigurationLayer()
	standardSharedConfiguration.SetBump(utl.PointerToString("standard-shared"))
	standardSharedConfiguration.SetReleasePrefix(utl.PointerToString("sharedPrefix"))
	standardLocalConfiguration := NewSimpleConfigurationLayer()
	standardLocalConfiguration.SetBump(utl.PointerToString("standard-local"))
	standardLocalConfiguration.SetConfigurationFile(utl.PointerToString("custom-local.json"))
	customSharedConfiguration := NewSimpleConfigurationLayer()
	customSharedConfiguration.SetBump(utl.PointerToString("custom-shared"))
	customLocalConfiguration := NewSimpleConfigurationLayer()
	customLocalConfiguration.SetBump(utl.PointerToString("custom-local"))
	customLocalConfiguration.SetSharedConfigurationFile(utl.PointerToString("custom-shared.json"))

	// save the standard files to standard locations
	standardSharedConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx-shared.json"))
	defer os.Remove(standardSharedConfigurationFile.Name())
	io.Save(standardSharedConfigurationFile.Name(), standardSharedConfiguration)
	standardLocalConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx.json"))
	defer os.Remove(standardLocalConfigurationFile.Name())
	io.Save(standardLocalConfigurationFile.Name(), standardLocalConfiguration)

	// save custom files to custom file names in the same directory
	customSharedConfigurationFile, _ := os.Create(filepath.Join(tempDir, "custom-shared.json"))
	defer os.Remove(customSharedConfigurationFile.Name())
	io.Save(customSharedConfigurationFile.Name(), customSharedConfiguration)
	customLocalConfigurationFile, _ := os.Create(filepath.Join(tempDir, "custom-local.json"))
	defer os.Remove(customLocalConfigurationFile.Name())
	io.Save(customLocalConfigurationFile.Name(), customLocalConfiguration)

	configuration, _ := NewConfiguration()

	bump, _ := configuration.GetBump()
	releasePrefix, _ := configuration.GetReleasePrefix()
	assert.Equal(t, "sharedPrefix", *releasePrefix) // this is configured only on the standard shared file
	assert.Equal(t, "custom-local", *bump)          // this is the value configured in the custom local file and has higher priority over the standard shared
}

func TestConfigurationWithCombinedConfigurationWithStandardCustomFilesAndCommandLine(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	// create 4 different config files with the bump field different for each
	// here the standard local file adds the custom configuration file, which in turn adds the shared custom file
	// so we can test all the chain
	standardSharedConfiguration := NewSimpleConfigurationLayer()
	standardSharedConfiguration.SetBump(utl.PointerToString("standard-shared"))
	standardSharedConfiguration.SetReleasePrefix(utl.PointerToString("sharedPrefix"))
	standardLocalConfiguration := NewSimpleConfigurationLayer()
	standardLocalConfiguration.SetBump(utl.PointerToString("standard-local"))
	standardLocalConfiguration.SetConfigurationFile(utl.PointerToString("custom-local.json"))
	customSharedConfiguration := NewSimpleConfigurationLayer()
	customSharedConfiguration.SetBump(utl.PointerToString("custom-shared"))
	customLocalConfiguration := NewSimpleConfigurationLayer()
	customLocalConfiguration.SetBump(utl.PointerToString("custom-local"))
	customLocalConfiguration.SetSharedConfigurationFile(utl.PointerToString("custom-shared.json"))
	customCmdlineConfiguration := NewSimpleConfigurationLayer()
	customCmdlineConfiguration.SetReleasePrefix(utl.PointerToString("cmdlinePrefix"))

	// also create the command line layer, which defines a different custom local file
	commandLineConfiguration := NewSimpleConfigurationLayer()
	commandLineConfiguration.SetBump(utl.PointerToString("cmdline"))
	commandLineConfiguration.SetConfigurationFile(utl.PointerToString("custom-cmdline.json"))

	// save the standard files to standard locations
	standardSharedConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx-shared.json"))
	defer os.Remove(standardSharedConfigurationFile.Name())
	io.Save(standardSharedConfigurationFile.Name(), standardSharedConfiguration)
	standardLocalConfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx.json"))
	defer os.Remove(standardLocalConfigurationFile.Name())
	io.Save(standardLocalConfigurationFile.Name(), standardLocalConfiguration)

	// save custom files to custom file names in the same directory
	customSharedConfigurationFile, _ := os.Create(filepath.Join(tempDir, "custom-shared.json"))
	defer os.Remove(customSharedConfigurationFile.Name())
	io.Save(customSharedConfigurationFile.Name(), customSharedConfiguration)
	customLocalConfigurationFile, _ := os.Create(filepath.Join(tempDir, "custom-local.json"))
	defer os.Remove(customLocalConfigurationFile.Name())
	io.Save(customLocalConfigurationFile.Name(), customLocalConfiguration)
	customCmdlineConfigurationFile, _ := os.Create(filepath.Join(tempDir, "custom-cmdline.json"))
	defer os.Remove(customCmdlineConfigurationFile.Name())
	io.Save(customCmdlineConfigurationFile.Name(), customCmdlineConfiguration)

	configuration, _ := NewConfiguration()

	bump, _ := configuration.GetBump()
	releasePrefix, _ := configuration.GetReleasePrefix()
	// first test against the standard files, which must have been loaded by default
	assert.Equal(t, "sharedPrefix", *releasePrefix) // this is configured only on the standard shared file
	assert.Equal(t, "custom-local", *bump)          // this is the value configured in the custom local file

	// inject the plugin configuration and test the new value is returned from that
	// what happens now is that the bump is taked directly from the command line layer,
	// but the release prefix must be read from the configuration that the command line defines
	// overwriting the previous configuration file
	var cl ConfigurationLayer = commandLineConfiguration
	configuration.WithCommandLineConfiguration(&cl)

	bump, _ = configuration.GetBump()
	releasePrefix, _ = configuration.GetReleasePrefix()
	assert.Equal(t, "cmdlinePrefix", *releasePrefix) // this must come from the custom configuration file defined by the command line config
	assert.Equal(t, "cmdline", *bump)                // this is the value configured in the command line and has higher priority over all others
}
