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

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

func TestSimpleConfigurationLayerGetBump(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	bump, error := simpleConfigurationLayer.GetBump()
	assert.NoError(t, error)
	assert.Nil(t, bump)

	simpleConfigurationLayer.SetBump(utl.PointerToString("b"))
	bump, error = simpleConfigurationLayer.GetBump()
	assert.NoError(t, error)
	assert.Equal(t, "b", *bump)
}

func TestSimpleConfigurationLayerGetChangelogConfiguration(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	cc, error := simpleConfigurationLayer.GetChangelog()
	assert.NoError(t, error)
	assert.NotNil(t, cc)

	ccParam, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Section1": "regex1", "Section2": "regex2"}, utl.PointerToString("changelog.tpl"), &map[string]string{"Expression1": "string1"})

	simpleConfigurationLayer.SetChangelog(ccParam)
	cc, error = simpleConfigurationLayer.GetChangelog()
	assert.NoError(t, error)
	assert.Equal(t, *ccParam, *cc)

	assert.Equal(t, 2, len(*cc.GetSections()))
	assert.Equal(t, 1, len(*cc.GetSubstitutions()))
}

func TestSimpleConfigurationLayerGetCommitMessageConventions(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	cmc, error := simpleConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, error)
	assert.NotNil(t, cmc)

	items := make(map[string]*ent.CommitMessageConvention)
	items["one"] = ent.NewCommitMessageConvention()
	items["two"] = ent.NewCommitMessageConvention()

	enabled := []*string{utl.PointerToString("one"), utl.PointerToString("two")}

	cmcParam, _ := ent.NewCommitMessageConventionsWith(&enabled, &items)

	simpleConfigurationLayer.SetCommitMessageConventions(cmcParam)
	cmc, error = simpleConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, error)
	assert.Equal(t, *cmcParam, *cmc)

	assert.Equal(t, 2, len(*cmc.GetEnabled()))
	assert.Equal(t, 2, len(*cmc.GetItems()))
}

func TestSimpleConfigurationLayerGetConfigurationFile(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	configurationFile, error := simpleConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, error)
	assert.Nil(t, configurationFile)

	simpleConfigurationLayer.SetConfigurationFile(utl.PointerToString("config.yml"))
	configurationFile, error = simpleConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, error)
	assert.Equal(t, "config.yml", *configurationFile)
}

func TestSimpleConfigurationLayerGetDirectory(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	directory, error := simpleConfigurationLayer.GetDirectory()
	assert.NoError(t, error)
	assert.Nil(t, directory)

	simpleConfigurationLayer.SetDirectory(utl.PointerToString("mydir"))
	directory, error = simpleConfigurationLayer.GetDirectory()
	assert.NoError(t, error)
	assert.Equal(t, "mydir", *directory)
}

func TestSimpleConfigurationLayerGetDryRun(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	dryRun, error := simpleConfigurationLayer.GetDryRun()
	assert.NoError(t, error)
	assert.Nil(t, dryRun)

	simpleConfigurationLayer.SetDryRun(utl.PointerToBoolean(true))
	dryRun, error = simpleConfigurationLayer.GetDryRun()
	assert.NoError(t, error)
	assert.Equal(t, true, *dryRun)
}

func TestSimpleConfigurationLayerGetGit(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	git, error := simpleConfigurationLayer.GetGit()
	assert.NoError(t, error)
	assert.NotNil(t, git)

	remotes := make(map[string]*ent.GitRemoteConfiguration)
	remotes["origin1"] = ent.NewGitRemoteConfigurationWith(utl.PointerToString("jdoe1"), utl.PointerToString("pwd1"))
	remotes["origin2"] = ent.NewGitRemoteConfigurationWith(utl.PointerToString("jdoe2"), utl.PointerToString("pwd2"))

	gitParam, _ := ent.NewGitConfigurationWith(&remotes)

	simpleConfigurationLayer.SetGit(gitParam)
	git, error = simpleConfigurationLayer.GetGit()
	assert.NoError(t, error)
	assert.Equal(t, *gitParam, *git)

	assert.Equal(t, 2, len(*git.GetRemotes()))
}

func TestSimpleConfigurationLayerGetInitialVersion(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	initialVersion, error := simpleConfigurationLayer.GetInitialVersion()
	assert.NoError(t, error)
	assert.Nil(t, initialVersion)

	simpleConfigurationLayer.SetInitialVersion(utl.PointerToString("0.3.5"))
	initialVersion, error = simpleConfigurationLayer.GetInitialVersion()
	assert.NoError(t, error)
	assert.Equal(t, "0.3.5", *initialVersion)
}

func TestSimpleConfigurationLayerGetPreset(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	preset, error := simpleConfigurationLayer.GetPreset()
	assert.NoError(t, error)
	assert.Nil(t, preset)

	simpleConfigurationLayer.SetPreset(utl.PointerToString("simple"))
	preset, error = simpleConfigurationLayer.GetPreset()
	assert.NoError(t, error)
	assert.Equal(t, "simple", *preset)
}

func TestSimpleConfigurationLayerGetReleaseAssets(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	ra := make(map[string]*ent.Attachment)

	ra["asset1"] = ent.NewAttachmentWith(utl.PointerToString("asset.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("asset.txt"), utl.PointerToString("text/plain"))
	ra["asset2"] = ent.NewAttachmentWith(utl.PointerToString("asset.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("asset.bin"), utl.PointerToString("application/octet-stream"))

	releaseAssets, error := simpleConfigurationLayer.GetReleaseAssets()
	assert.NoError(t, error)
	assert.NotNil(t, releaseAssets)
	assert.Equal(t, 0, len(*releaseAssets))

	simpleConfigurationLayer.SetReleaseAssets(&ra)
	releaseAssets, error = simpleConfigurationLayer.GetReleaseAssets()
	assert.NoError(t, error)
	assert.Equal(t, ra, *releaseAssets)
	assert.Equal(t, 2, len(*releaseAssets))
	assert.NotNil(t, (*releaseAssets)["asset1"])
	assert.NotNil(t, (*releaseAssets)["asset2"])
	assert.Equal(t, "asset.txt", *(*releaseAssets)["asset1"].GetFileName())
	assert.Equal(t, "Text Asset", *(*releaseAssets)["asset1"].GetDescription())
	assert.Equal(t, "text/plain", *(*releaseAssets)["asset1"].GetType())
	assert.Equal(t, "asset.txt", *(*releaseAssets)["asset1"].GetPath())
	assert.Equal(t, "asset.bin", *(*releaseAssets)["asset2"].GetFileName())
	assert.Equal(t, "Binary Asset", *(*releaseAssets)["asset2"].GetDescription())
	assert.Equal(t, "application/octet-stream", *(*releaseAssets)["asset2"].GetType())
	assert.Equal(t, "asset.bin", *(*releaseAssets)["asset2"].GetPath())
}

func TestSimpleConfigurationLayerGetReleaseLenient(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	releaseLenient, error := simpleConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, error)
	assert.Nil(t, releaseLenient)

	simpleConfigurationLayer.SetReleaseLenient(utl.PointerToBoolean(true))
	releaseLenient, error = simpleConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, error)
	assert.Equal(t, true, *releaseLenient)
}

func TestSimpleConfigurationLayerGetReleasePrefix(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	releasePrefix, error := simpleConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, error)
	assert.Nil(t, releasePrefix)

	simpleConfigurationLayer.SetReleasePrefix(utl.PointerToString("prefix"))
	releasePrefix, error = simpleConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, error)
	assert.Equal(t, "prefix", *releasePrefix)
}

func TestSimpleConfigurationLayerGetReleaseTypes(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	releaseTypes, error := simpleConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, error)
	assert.NotNil(t, releaseTypes)

	items := make(map[string]*ent.ReleaseType)
	items["one"] = ent.NewReleaseType()
	items["two"] = ent.NewReleaseType()

	enabled := []*string{utl.PointerToString("one"), utl.PointerToString("two")}
	publicationServices := []*string{utl.PointerToString("first"), utl.PointerToString("second")}
	remoteRepositories := []*string{utl.PointerToString("origin"), utl.PointerToString("replica")}

	releaseTypesParam, err := ent.NewReleaseTypesWith(&enabled, &publicationServices, &remoteRepositories, &items)
	assert.NoError(t, err)

	simpleConfigurationLayer.SetReleaseTypes(releaseTypesParam)
	releaseTypes, error = simpleConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, error)
	assert.Equal(t, *releaseTypesParam, *releaseTypes)

	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 2, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, 2, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, 2, len(*releaseTypes.GetItems()))
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetAssets())
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetAssets())
}

func TestSimpleConfigurationLayerGetResume(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	resume, error := simpleConfigurationLayer.GetResume()
	assert.NoError(t, error)
	assert.Nil(t, resume)

	simpleConfigurationLayer.SetResume(utl.PointerToBoolean(true))
	resume, error = simpleConfigurationLayer.GetResume()
	assert.NoError(t, error)
	assert.Equal(t, true, *resume)
}

func TestSimpleConfigurationLayerGetScheme(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	scheme, error := simpleConfigurationLayer.GetScheme()
	assert.NoError(t, error)
	assert.Nil(t, scheme)

	simpleConfigurationLayer.SetScheme(ver.PointerToScheme(ver.SEMVER))
	scheme, error = simpleConfigurationLayer.GetScheme()
	assert.NoError(t, error)
	assert.Equal(t, ver.SEMVER, *scheme)
}

func TestSimpleConfigurationLayerGetServices(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	s := make(map[string]*ent.ServiceConfiguration)
	s["github"] = ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo1", "REPOSITORY_OWNER": "owner1"})
	s["gitlab"] = ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo2", "REPOSITORY_OWNER": "owner2"})

	services, error := simpleConfigurationLayer.GetServices()
	assert.NoError(t, error)
	assert.NotNil(t, services)

	simpleConfigurationLayer.SetServices(&s)
	services, error = simpleConfigurationLayer.GetServices()
	assert.NoError(t, error)
	assert.Equal(t, s, *services)
}

func TestSimpleConfigurationLayerGetSharedConfigurationFile(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	sharedConfigurationFile, error := simpleConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, error)
	assert.Nil(t, sharedConfigurationFile)

	simpleConfigurationLayer.SetSharedConfigurationFile(utl.PointerToString("config.yml"))
	sharedConfigurationFile, error = simpleConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, error)
	assert.Equal(t, "config.yml", *sharedConfigurationFile)
}

func TestSimpleConfigurationLayerGetStateFile(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	stateFile, error := simpleConfigurationLayer.GetStateFile()
	assert.NoError(t, error)
	assert.Nil(t, stateFile)

	simpleConfigurationLayer.SetStateFile(utl.PointerToString("state.yml"))
	stateFile, error = simpleConfigurationLayer.GetStateFile()
	assert.NoError(t, error)
	assert.Equal(t, "state.yml", *stateFile)
}

func TestSimpleConfigurationLayerGetVerbosity(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	verbosity, error := simpleConfigurationLayer.GetVerbosity()
	assert.NoError(t, error)
	assert.Nil(t, verbosity)

	simpleConfigurationLayer.SetVerbosity(ent.PointerToVerbosity(ent.INFO))
	verbosity, error = simpleConfigurationLayer.GetVerbosity()
	assert.NoError(t, error)
	assert.Equal(t, ent.INFO, *verbosity)
}

func TestSimpleConfigurationLayerGetVersion(t *testing.T) {
	simpleConfigurationLayer := NewSimpleConfigurationLayer()

	version, error := simpleConfigurationLayer.GetVersion()
	assert.NoError(t, error)
	assert.Nil(t, version)

	simpleConfigurationLayer.SetVersion(utl.PointerToString("3.5.7"))
	version, error = simpleConfigurationLayer.GetVersion()
	assert.NoError(t, error)
	assert.Equal(t, "3.5.7", *version)
}
