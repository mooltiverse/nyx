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
	"os"      // https://pkg.go.dev/os
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

func TestEnvironmentConfigurationLayerGetBump(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	bump, err := environmentConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Nil(t, bump)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_BUMP=b",
	})

	bump, err = environmentConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "b", *bump)
}

func TestEnvironmentConfigurationLayerGetChangelog(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	changelog, err := environmentConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)
	assert.Nil(t, changelog.GetAppend())
	assert.Nil(t, changelog.GetPath())
	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))
	assert.Nil(t, changelog.GetTemplate())

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_CHANGELOG_PATH=CHANGELOG.md",
	})

	changelog, err = environmentConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Nil(t, changelog.GetAppend())
	assert.Equal(t, "CHANGELOG.md", *changelog.GetPath())
	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))
	assert.Nil(t, changelog.GetTemplate())

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_CHANGELOG_APPEND=head",
		"NYX_CHANGELOG_PATH=CHANGELOG.md",
		"NYX_CHANGELOG_SECTIONS_Section1=regex1",
		"NYX_CHANGELOG_SECTIONS_Section2=regex2",
		"NYX_CHANGELOG_SUBSTITUTIONS_Expr1=string1",
		"NYX_CHANGELOG_TEMPLATE=changelog.tpl",
	})

	changelog, err = environmentConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)
	assert.Equal(t, "head", *changelog.GetAppend())
	assert.Equal(t, "CHANGELOG.md", *changelog.GetPath())

	assert.Equal(t, 2, len(*changelog.GetSections()))
	sections := *changelog.GetSections()
	assert.Equal(t, "regex1", sections["Section1"])
	assert.Equal(t, "regex2", sections["Section2"])

	assert.Equal(t, 1, len(*changelog.GetSubstitutions()))
	substitutions := *changelog.GetSubstitutions()
	assert.Equal(t, "string1", substitutions["Expr1"])
	assert.Equal(t, "changelog.tpl", *changelog.GetTemplate())
}

func TestEnvironmentConfigurationLayerGetCommitMessageConventions(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	commitMessageConventions, err := environmentConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)
	assert.Equal(t, 0, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 0, len(*commitMessageConventions.GetItems()))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=one,two",
	})

	commitMessageConventions, err = environmentConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	enabled := *commitMessageConventions.GetEnabled()
	items := *commitMessageConventions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 0, len(items))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=one,two",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_one_EXPRESSION=",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_two_EXPRESSION=",
	})

	commitMessageConventions, err = environmentConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	enabled = *commitMessageConventions.GetEnabled()
	items = *commitMessageConventions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 2, len(*commitMessageConventions.GetItems()))
	assert.NotNil(t, *items["one"])
	assert.NotNil(t, *items["two"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=one,two",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_one_EXPRESSION=expr1",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_one_BUMP_EXPRESSIONS_alpha=alpha1",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_two_EXPRESSION=expr2",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_two_BUMP_EXPRESSIONS_beta=beta1",
		"NYX_COMMIT_MESSAGE_CONVENTIONS_two_BUMP_EXPRESSIONS_gamma=gamma1",
	})

	commitMessageConventions, err = environmentConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	enabled = *commitMessageConventions.GetEnabled()
	items = *commitMessageConventions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 2, len(items))
	assert.Equal(t, "expr1", *items["one"].GetExpression())
	assert.Equal(t, 1, len(*items["one"].GetBumpExpressions()))
	bumpExpressions := *items["one"].GetBumpExpressions()
	assert.Equal(t, "alpha1", bumpExpressions["alpha"])
	assert.Equal(t, "expr2", *items["two"].GetExpression())
	assert.Equal(t, 2, len(*items["two"].GetBumpExpressions()))
	bumpExpressions = *items["two"].GetBumpExpressions()
	assert.Equal(t, "beta1", bumpExpressions["beta"])
	assert.Equal(t, "gamma1", bumpExpressions["gamma"])
}

func TestEnvironmentConfigurationLayerGetConfigurationFile(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	configurationFile, err := environmentConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Nil(t, configurationFile)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_CONFIGURATION_FILE=config.yml",
	})

	configurationFile, err = environmentConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.yml", *configurationFile)
}

func TestEnvironmentConfigurationLayerGetDirectory(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	directory, err := environmentConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Nil(t, directory)

	dir, err := os.MkdirTemp("", "")
	assert.NoError(t, err)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_DIRECTORY=" + dir,
	})

	directory, err = environmentConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Equal(t, dir, *directory)

	defer os.RemoveAll(dir) // clean up
}

func TestEnvironmentConfigurationLayerGetDryRun(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	dryRun, err := environmentConfigurationLayer.GetDryRun()
	assert.NoError(t, err)
	assert.Nil(t, dryRun)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_DRY_RUN=true",
	})

	dryRun, err = environmentConfigurationLayer.GetDryRun()
	assert.NoError(t, err)
	assert.Equal(t, true, *dryRun)
}

func TestEnvironmentConfigurationLayerGetGit(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	git, err := environmentConfigurationLayer.GetGit()
	assert.NoError(t, err)
	assert.NotNil(t, git)
	assert.Equal(t, 0, len(*git.GetRemotes()))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_GIT_REMOTES_one_USER=jdoe",
		"NYX_GIT_REMOTES_two_USER=stiger",
	})

	git, err = environmentConfigurationLayer.GetGit()
	remotes := *git.GetRemotes()
	assert.NoError(t, err)
	assert.NotNil(t, git)

	assert.Equal(t, 2, len(*git.GetRemotes()))
	assert.NotNil(t, remotes["one"])
	assert.NotNil(t, remotes["two"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_GIT_REMOTES_one_AUTHENTICATION_METHOD=USER_PASSWORD",
		"NYX_GIT_REMOTES_one_USER=jdoe",
		"NYX_GIT_REMOTES_one_PASSWORD=pwd",
		"NYX_GIT_REMOTES_one_PRIVATE_KEY=pk1",
		"NYX_GIT_REMOTES_one_PASSPHRASE=pp1",
		"NYX_GIT_REMOTES_two_AUTHENTICATION_METHOD=PUBLIC_KEY",
		"NYX_GIT_REMOTES_two_USER=stiger",
		"NYX_GIT_REMOTES_two_PASSWORD=sct",
		"NYX_GIT_REMOTES_two_PRIVATE_KEY=pk2",
		"NYX_GIT_REMOTES_two_PASSPHRASE=pp2",
	})

	git, err = environmentConfigurationLayer.GetGit()
	remotes = *git.GetRemotes()

	assert.Equal(t, 2, len(*git.GetRemotes()))
	assert.Equal(t, ent.USER_PASSWORD, *remotes["one"].GetAuthenticationMethod())
	assert.Equal(t, "pwd", *remotes["one"].GetPassword())
	assert.Equal(t, "jdoe", *remotes["one"].GetUser())
	assert.Equal(t, "pk1", *remotes["one"].GetPrivateKey())
	assert.Equal(t, "pp1", *remotes["one"].GetPassphrase())
	assert.Equal(t, ent.PUBLIC_KEY, *remotes["two"].GetAuthenticationMethod())
	assert.Equal(t, "sct", *remotes["two"].GetPassword())
	assert.Equal(t, "stiger", *remotes["two"].GetUser())
	assert.Equal(t, "pk2", *remotes["two"].GetPrivateKey())
	assert.Equal(t, "pp2", *remotes["two"].GetPassphrase())
}

func TestEnvironmentConfigurationLayerGetInitialVersion(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	initialVersion, err := environmentConfigurationLayer.GetInitialVersion()
	assert.NoError(t, err)
	assert.Nil(t, initialVersion)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_INITIAL_VERSION=0.3.5",
	})

	initialVersion, err = environmentConfigurationLayer.GetInitialVersion()
	assert.NoError(t, err)
	assert.Equal(t, "0.3.5", *initialVersion)
}

func TestEnvironmentConfigurationLayerGetPreset(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	preset, err := environmentConfigurationLayer.GetPreset()
	assert.NoError(t, err)
	assert.Nil(t, preset)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_PRESET=simple",
	})

	preset, err = environmentConfigurationLayer.GetPreset()
	assert.NoError(t, err)
	assert.Equal(t, "simple", *preset)
}

func TestEnvironmentConfigurationLayerGetReleaseAssets(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	releaseAssets, err := environmentConfigurationLayer.GetReleaseAssets()
	assert.NoError(t, err)
	assert.NotNil(t, releaseAssets)
	assert.Equal(t, 0, len(*releaseAssets))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_ASSETS_asset1_TYPE=text/plain",
	})

	releaseAssets, err = environmentConfigurationLayer.GetReleaseAssets()

	assert.Equal(t, 1, len(*releaseAssets))
	assert.NotNil(t, (*releaseAssets)["asset1"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_ASSETS_asset1_FILE_NAME=asset.txt",
		"NYX_RELEASE_ASSETS_asset1_DESCRIPTION=Text Asset",
		"NYX_RELEASE_ASSETS_asset1_TYPE=text/plain",
		"NYX_RELEASE_ASSETS_asset1_PATH=asset.txt",
		"NYX_RELEASE_ASSETS_asset2_FILE_NAME=asset.bin",
		"NYX_RELEASE_ASSETS_asset2_DESCRIPTION=Binary Asset",
		"NYX_RELEASE_ASSETS_asset2_TYPE=application/octet-stream",
		"NYX_RELEASE_ASSETS_asset2_PATH=asset.bin",
	})

	releaseAssets, err = environmentConfigurationLayer.GetReleaseAssets()
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

func TestEnvironmentConfigurationLayerGetReleaseLenient(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	releaseLenient, err := environmentConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, err)
	assert.Nil(t, releaseLenient)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_LENIENT=true",
	})

	releaseLenient, err = environmentConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, err)
	assert.Equal(t, true, *releaseLenient)
}

func TestEnvironmentConfigurationLayerGetReleasePrefix(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	releasePrefix, err := environmentConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, err)
	assert.Nil(t, releasePrefix)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_PREFIX=prefix",
	})

	releasePrefix, err = environmentConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, err)
	assert.Equal(t, "prefix", *releasePrefix)
}

func TestEnvironmentConfigurationLayerGetReleaseTypes(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	releaseTypes, err := environmentConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 0, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 0, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, 0, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, 0, len(*releaseTypes.GetItems()))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_TYPES_ENABLED=one,two",
		"NYX_RELEASE_TYPES_PUBLICATION_SERVICES=first,second",
		"NYX_RELEASE_TYPES_REMOTE_REPOSITORIES=origin,replica",
	})

	releaseTypes, err = environmentConfigurationLayer.GetReleaseTypes()
	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, "one", *(*releaseTypes.GetEnabled())[0])
	assert.Equal(t, "two", *(*releaseTypes.GetEnabled())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, "first", *(*releaseTypes.GetPublicationServices())[0])
	assert.Equal(t, "second", *(*releaseTypes.GetPublicationServices())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, "origin", *(*releaseTypes.GetRemoteRepositories())[0])
	assert.Equal(t, "replica", *(*releaseTypes.GetRemoteRepositories())[1])
	assert.Equal(t, 0, len(*releaseTypes.GetItems()))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_TYPES_ENABLED=one,two",
		"NYX_RELEASE_TYPES_PUBLICATION_SERVICES=first,second",
		"NYX_RELEASE_TYPES_REMOTE_REPOSITORIES=origin,replica",
		"NYX_RELEASE_TYPES_one_COLLAPSE_VERSIONS=",
		"NYX_RELEASE_TYPES_two_COLLAPSE_VERSIONS=",
	})

	releaseTypes, err = environmentConfigurationLayer.GetReleaseTypes()
	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, "one", *(*releaseTypes.GetEnabled())[0])
	assert.Equal(t, "two", *(*releaseTypes.GetEnabled())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, "first", *(*releaseTypes.GetPublicationServices())[0])
	assert.Equal(t, "second", *(*releaseTypes.GetPublicationServices())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, "origin", *(*releaseTypes.GetRemoteRepositories())[0])
	assert.Equal(t, "replica", *(*releaseTypes.GetRemoteRepositories())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetItems()))
	assert.NotNil(t, *(*releaseTypes.GetItems())["one"])
	assert.NotNil(t, *(*releaseTypes.GetItems())["two"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RELEASE_TYPES_ENABLED=one,two",
		"NYX_RELEASE_TYPES_PUBLICATION_SERVICES=first,second",
		"NYX_RELEASE_TYPES_REMOTE_REPOSITORIES=origin,replica",
		"NYX_RELEASE_TYPES_one_ASSETS=asset1,asset2",
		"NYX_RELEASE_TYPES_one_COLLAPSE_VERSIONS=true",
		"NYX_RELEASE_TYPES_one_COLLAPSED_VERSION_QUALIFIER=qualifier1",
		"NYX_RELEASE_TYPES_one_DESCRIPTION=description1",
		"NYX_RELEASE_TYPES_one_GIT_COMMIT=true",
		"NYX_RELEASE_TYPES_one_GIT_PUSH=true",
		"NYX_RELEASE_TYPES_one_GIT_TAG=true",
		"NYX_RELEASE_TYPES_one_MATCH_BRANCHES=alpha,beta",
		"NYX_RELEASE_TYPES_one_MATCH_WORKSPACE_STATUS=" + ent.DIRTY.String(),
		"NYX_RELEASE_TYPES_one_PUBLISH=false",
		"NYX_RELEASE_TYPES_one_VERSION_RANGE=true",
		"NYX_RELEASE_TYPES_two_COLLAPSE_VERSIONS=false",
		"NYX_RELEASE_TYPES_two_DESCRIPTION=description2",
		"NYX_RELEASE_TYPES_two_FILTER_TAGS=filter2",
		"NYX_RELEASE_TYPES_two_GIT_COMMIT=false",
		"NYX_RELEASE_TYPES_two_GIT_COMMIT_MESSAGE=Commit message",
		"NYX_RELEASE_TYPES_two_GIT_PUSH=false",
		"NYX_RELEASE_TYPES_two_GIT_TAG=false",
		"NYX_RELEASE_TYPES_two_GIT_TAG_MESSAGE=Tag message",
		"NYX_RELEASE_TYPES_two_GIT_TAG_NAMES=one,two,three",
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_0_POSITION=" + ent.PRE_RELEASE.String(),
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_0_QUALIFIER=q1",
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_0_VALUE=v1",
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_1_QUALIFIER=q2",
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_1_VALUE=v2",
		// note we use ordinal 9 here, but the list will return it at the 2nd position anyway because it sorts them regardles of gaps between ordinals
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_9_POSITION=" + ent.BUILD.String(),
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_9_QUALIFIER=q3",
		"NYX_RELEASE_TYPES_two_IDENTIFIERS_9_VALUE=v3",
		"NYX_RELEASE_TYPES_two_MATCH_ENVIRONMENT_VARIABLES_PATH=any path",
		"NYX_RELEASE_TYPES_two_MATCH_ENVIRONMENT_VARIABLES_USER=any user",
		"NYX_RELEASE_TYPES_two_MATCH_WORKSPACE_STATUS=" + ent.CLEAN.String(),
		"NYX_RELEASE_TYPES_two_PUBLISH=true",
		"NYX_RELEASE_TYPES_two_PUBLISH_DRAFT=false",
		"NYX_RELEASE_TYPES_two_PUBLISH_PRE_RELEASE=true",
		"NYX_RELEASE_TYPES_two_RELEASE_NAME=myrelease",
		"NYX_RELEASE_TYPES_two_VERSION_RANGE_FROM_BRANCH_NAME=true",
	})

	releaseTypes, err = environmentConfigurationLayer.GetReleaseTypes()
	assert.Equal(t, 2, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, "one", *(*releaseTypes.GetEnabled())[0])
	assert.Equal(t, "two", *(*releaseTypes.GetEnabled())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, "first", *(*releaseTypes.GetPublicationServices())[0])
	assert.Equal(t, "second", *(*releaseTypes.GetPublicationServices())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, "origin", *(*releaseTypes.GetRemoteRepositories())[0])
	assert.Equal(t, "replica", *(*releaseTypes.GetRemoteRepositories())[1])
	assert.Equal(t, 2, len(*releaseTypes.GetItems()))
	assert.NotNil(t, *(*releaseTypes.GetItems())["one"])
	assert.NotNil(t, *(*releaseTypes.GetItems())["two"])
	assert.Equal(t, 2, len(*(*(*releaseTypes).GetItems())["one"].GetAssets()))
	assert.Equal(t, "asset1", *(*(*(*releaseTypes).GetItems())["one"].GetAssets())[0])
	assert.Equal(t, "asset2", *(*(*(*releaseTypes).GetItems())["one"].GetAssets())[1])
	assert.True(t, *(*(*releaseTypes.GetItems())["one"]).GetCollapseVersions())
	assert.Equal(t, "qualifier1", *(*(*releaseTypes.GetItems())["one"]).GetCollapsedVersionQualifier())
	assert.Equal(t, "description1", *(*(*releaseTypes.GetItems())["one"]).GetDescription())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetFilterTags())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["one"]).GetGitCommit())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetGitCommitMessage())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["one"]).GetGitTag())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetGitTagMessage())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetGitTagNames())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["one"]).GetGitPush())
	assert.Equal(t, 0, len(*(*(*releaseTypes.GetItems())["one"]).GetIdentifiers()))
	assert.Equal(t, "alpha,beta", *(*(*releaseTypes.GetItems())["one"]).GetMatchBranches())
	assert.Equal(t, 0, len(*(*(*releaseTypes.GetItems())["one"]).GetMatchEnvironmentVariables()))
	assert.Equal(t, ent.DIRTY, *(*(*releaseTypes.GetItems())["one"]).GetMatchWorkspaceStatus())
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["one"]).GetPublish())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetPublishDraft())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetPublishPreRelease())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetReleaseName())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["one"]).GetVersionRange())
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetVersionRangeFromBranchName())
	assert.Nil(t, (*(*releaseTypes).GetItems())["two"].GetAssets())
	assert.False(t, *(*(*releaseTypes.GetItems())["two"]).GetCollapseVersions())
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetCollapsedVersionQualifier())
	assert.Equal(t, "description2", *(*(*releaseTypes.GetItems())["two"]).GetDescription())
	assert.Equal(t, "filter2", *(*(*releaseTypes.GetItems())["two"]).GetFilterTags())
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["two"]).GetGitCommit())
	assert.Equal(t, "Commit message", *(*(*releaseTypes.GetItems())["two"]).GetGitCommitMessage())
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["two"]).GetGitTag())
	assert.Equal(t, "Tag message", *(*(*releaseTypes.GetItems())["two"]).GetGitTagMessage())
	assert.Equal(t, 3, len(*(*(*releaseTypes.GetItems())["two"]).GetGitTagNames()))
	assert.Equal(t, "one", *(*(*(*releaseTypes.GetItems())["two"]).GetGitTagNames())[0])
	assert.Equal(t, "two", *(*(*(*releaseTypes.GetItems())["two"]).GetGitTagNames())[1])
	assert.Equal(t, "three", *(*(*(*releaseTypes.GetItems())["two"]).GetGitTagNames())[2])
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["two"]).GetGitPush())
	assert.Equal(t, 3, len(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers()))
	assert.Equal(t, ent.PRE_RELEASE, *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[0].GetPosition())
	assert.Equal(t, "q1", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[0].GetQualifier())
	assert.Equal(t, "v1", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[0].GetValue())
	assert.Nil(t, (*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[1].GetPosition()) // this is the zero value for position, quite ugly when managed this way
	assert.Equal(t, "q2", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[1].GetQualifier())
	assert.Equal(t, "v2", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[1].GetValue())
	assert.Equal(t, ent.BUILD, *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[2].GetPosition())
	assert.Equal(t, "q3", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[2].GetQualifier())
	assert.Equal(t, "v3", *(*(*(*releaseTypes.GetItems())["two"]).GetIdentifiers())[2].GetValue())
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetMatchBranches())
	assert.Equal(t, 2, len(*(*(*releaseTypes.GetItems())["two"]).GetMatchEnvironmentVariables()))
	assert.Equal(t, "any path", (*(*(*releaseTypes.GetItems())["two"]).GetMatchEnvironmentVariables())["PATH"])
	assert.Equal(t, "any user", (*(*(*releaseTypes.GetItems())["two"]).GetMatchEnvironmentVariables())["USER"])
	assert.Equal(t, ent.CLEAN, *(*(*releaseTypes.GetItems())["two"]).GetMatchWorkspaceStatus())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["two"]).GetPublish())
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["two"]).GetPublishDraft())
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["two"]).GetPublishPreRelease())
	assert.Equal(t, "myrelease", *(*(*releaseTypes.GetItems())["two"]).GetReleaseName())
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetVersionRange())
	assert.True(t, *(*(*releaseTypes.GetItems())["two"]).GetVersionRangeFromBranchName())
}

func TestEnvironmentConfigurationLayerGetResume(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	resume, err := environmentConfigurationLayer.GetResume()
	assert.NoError(t, err)
	assert.Nil(t, resume)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_RESUME=true",
	})

	resume, err = environmentConfigurationLayer.GetResume()
	assert.NoError(t, err)
	assert.Equal(t, true, *resume)
}

func TestEnvironmentConfigurationLayerGetScheme(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	scheme, err := environmentConfigurationLayer.GetScheme()
	assert.NoError(t, err)
	assert.Nil(t, scheme)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SCHEME=" + ver.SEMVER.String(),
	})

	scheme, err = environmentConfigurationLayer.GetScheme()
	assert.NoError(t, err)
	assert.Equal(t, ver.SEMVER, *scheme)
}

func TestEnvironmentConfigurationLayerGetServices(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	services, err := environmentConfigurationLayer.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)
	assert.Equal(t, 0, len(*services))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SERVICES_github_TYPE=GITHUB",
	})

	services, err = environmentConfigurationLayer.GetServices()

	assert.Equal(t, 1, len(*services))
	assert.NotNil(t, (*services)["github"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SERVICES_github_TYPE=GITHUB",
		"NYX_SERVICES_github_OPTIONS_BASE_URI=https://someuri1.com/",
		"NYX_SERVICES_github_OPTIONS_AUTHENTICATION_TOKEN=1234567890",
		"NYX_SERVICES_github_OPTIONS_REPOSITORY_NAME=repo1",
		"NYX_SERVICES_github_OPTIONS_REPOSITORY_OWNER=owner1",
		"NYX_SERVICES_gitlab_TYPE=GITLAB",
		"NYX_SERVICES_gitlab_OPTIONS_BASE_URI=https://someuri2.com/",
		"NYX_SERVICES_gitlab_OPTIONS_AUTHENTICATION_TOKEN=abcdefghij",
		"NYX_SERVICES_gitlab_OPTIONS_REPOSITORY_NAME=repo2",
		"NYX_SERVICES_gitlab_OPTIONS_REPOSITORY_OWNER=owner2",
	})

	services, err = environmentConfigurationLayer.GetServices()
	assert.Equal(t, 2, len(*services))
	assert.NotNil(t, (*services)["github"])
	assert.NotNil(t, (*services)["gitlab"])
	assert.Equal(t, ent.GITHUB, *(*services)["github"].GetType())
	assert.Equal(t, ent.GITLAB, *(*services)["gitlab"].GetType())

	options := (*services)["github"].GetOptions()
	assert.Equal(t, 4, len(*options))
	assert.Equal(t, "https://someuri1.com/", (*options)["BASE_URI"])
	assert.Equal(t, "1234567890", (*options)["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo1", (*options)["REPOSITORY_NAME"])
	assert.Equal(t, "owner1", (*options)["REPOSITORY_OWNER"])

	options = (*services)["gitlab"].GetOptions()
	assert.Equal(t, 4, len(*options))
	assert.Equal(t, "https://someuri2.com/", (*options)["BASE_URI"])
	assert.Equal(t, "abcdefghij", (*options)["AUTHENTICATION_TOKEN"])
	assert.Equal(t, "repo2", (*options)["REPOSITORY_NAME"])
	assert.Equal(t, "owner2", (*options)["REPOSITORY_OWNER"])
}

func TestEnvironmentConfigurationLayerGetSharedConfigurationFile(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	sharedConfigurationFile, err := environmentConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, err)
	assert.Nil(t, sharedConfigurationFile)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SHARED_CONFIGURATION_FILE=config.yml",
	})

	sharedConfigurationFile, err = environmentConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.yml", *sharedConfigurationFile)
}

func TestEnvironmentConfigurationLayerGetStateFile(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	stateFile, err := environmentConfigurationLayer.GetStateFile()
	assert.NoError(t, err)
	assert.Nil(t, stateFile)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_STATE_FILE=state.yml",
	})

	stateFile, err = environmentConfigurationLayer.GetStateFile()
	assert.NoError(t, err)
	assert.Equal(t, "state.yml", *stateFile)
}

func TestEnvironmentConfigurationLayerGetSubstitutions(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	substitutions, err := environmentConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)
	assert.Equal(t, 0, len(*substitutions.GetEnabled()))
	assert.Equal(t, 0, len(*substitutions.GetItems()))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SUBSTITUTIONS_ENABLED=one,two",
	})

	substitutions, err = environmentConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	enabled := *substitutions.GetEnabled()
	items := *substitutions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 0, len(items))

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SUBSTITUTIONS_ENABLED=one,two",
		"NYX_SUBSTITUTIONS_one_FILES=",
		"NYX_SUBSTITUTIONS_two_FILES=",
	})

	substitutions, err = environmentConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	enabled = *substitutions.GetEnabled()
	items = *substitutions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 2, len(*substitutions.GetItems()))
	assert.NotNil(t, *items["one"])
	assert.NotNil(t, *items["two"])

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SUBSTITUTIONS_ENABLED=one,two",
		"NYX_SUBSTITUTIONS_one_FILES=*.json",
		"NYX_SUBSTITUTIONS_one_MATCH=version: 1.2.3",
		"NYX_SUBSTITUTIONS_two_FILES=*.toml",
		"NYX_SUBSTITUTIONS_two_MATCH=version: 4.5.6",
		"NYX_SUBSTITUTIONS_two_REPLACE=version: 7.8.9",
	})

	substitutions, err = environmentConfigurationLayer.GetSubstitutions()
	assert.NoError(t, err)
	assert.NotNil(t, substitutions)

	enabled = *substitutions.GetEnabled()
	items = *substitutions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 2, len(items))
	assert.Equal(t, "*.json", *items["one"].GetFiles())
	assert.Equal(t, "version: 1.2.3", *items["one"].GetMatch())
	assert.Nil(t, items["one"].GetReplace())
	assert.Equal(t, "*.toml", *items["two"].GetFiles())
	assert.Equal(t, "version: 4.5.6", *items["two"].GetMatch())
	assert.Equal(t, "version: 7.8.9", *items["two"].GetReplace())
}

func TestEnvironmentConfigurationLayerGetSummary(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	summary, err := environmentConfigurationLayer.GetSummary()
	assert.NoError(t, err)
	assert.Nil(t, summary)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SUMMARY=true",
	})

	summary, err = environmentConfigurationLayer.GetSummary()
	assert.NoError(t, err)
	assert.Equal(t, true, *summary)
}

func TestEnvironmentConfigurationLayerGetSummaryFile(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	summaryFile, err := environmentConfigurationLayer.GetSummaryFile()
	assert.NoError(t, err)
	assert.Nil(t, summaryFile)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_SUMMARY_FILE=summary.txt",
	})

	summaryFile, err = environmentConfigurationLayer.GetSummaryFile()
	assert.NoError(t, err)
	assert.Equal(t, "summary.txt", *summaryFile)
}

func TestEnvironmentConfigurationLayerGetVerbosity(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	verbosity, err := environmentConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Nil(t, verbosity)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_VERBOSITY=" + ent.INFO.String(),
	})

	verbosity, err = environmentConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.INFO, *verbosity)
}

func TestEnvironmentConfigurationLayerGetVersion(t *testing.T) {
	environmentConfigurationLayer := EnvironmentConfigurationLayer{}

	version, err := environmentConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Nil(t, version)

	// get a new instance or a stale set of environment variables is still in the configuration layer
	environmentConfigurationLayer = EnvironmentConfigurationLayer{}
	environmentConfigurationLayer.withEnvironmentVariables([]string{
		"NYX_VERSION=3.5.7",
	})

	version, err = environmentConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Equal(t, "3.5.7", *version)
}
