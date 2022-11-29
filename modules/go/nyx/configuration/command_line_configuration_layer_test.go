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
	"io/ioutil" // https://pkg.go.dev/io/ioutil
	"os"        // https://pkg.go.dev/os
	"testing"   // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

func TestCommandLineConfigurationLayerGetBump(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	bump, err := commandLineConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Nil(t, bump)

	// Test the extended version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--bump=b",
	})
	bump, err = commandLineConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "b", *bump)

	// Test the short version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-b=b",
	})
	bump, err = commandLineConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "b", *bump)

	// Test both the extended and the short version together
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-b=a",
		"--bump=b",
	})
	bump, err = commandLineConfigurationLayer.GetBump()
	assert.NoError(t, err)
	assert.Equal(t, "a", *bump)
}

func TestCommandLineConfigurationLayerGetChangelog(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	changelog, err := commandLineConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)
	assert.Nil(t, changelog.GetPath())
	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))
	assert.Nil(t, changelog.GetTemplate())

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--changelog-path=CHANGELOG.md",
	})

	changelog, err = commandLineConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)

	assert.Equal(t, "CHANGELOG.md", *changelog.GetPath())
	assert.Equal(t, 0, len(*changelog.GetSections()))
	assert.Equal(t, 0, len(*changelog.GetSubstitutions()))
	assert.Nil(t, changelog.GetTemplate())

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--changelog-path=CHANGELOG.md",
		"--changelog-sections-Section1=regex1",
		"--changelog-sections-Section2=regex2",
		"--changelog-substitutions-Expr1=string1",
		"--changelog-template=changelog.tpl",
	})

	changelog, err = commandLineConfigurationLayer.GetChangelog()
	assert.NoError(t, err)
	assert.NotNil(t, changelog)
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

func TestCommandLineConfigurationLayerGetCommitMessageConventions(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	commitMessageConventions, err := commandLineConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)
	assert.Equal(t, 0, len(*commitMessageConventions.GetEnabled()))
	assert.Equal(t, 0, len(*commitMessageConventions.GetItems()))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--commit-message-conventions-enabled=one,two",
	})

	commitMessageConventions, err = commandLineConfigurationLayer.GetCommitMessageConventions()
	assert.NoError(t, err)
	assert.NotNil(t, commitMessageConventions)

	enabled := *commitMessageConventions.GetEnabled()
	items := *commitMessageConventions.GetItems()
	assert.Equal(t, 2, len(enabled))
	assert.Equal(t, *enabled[0], "one")
	assert.Equal(t, *enabled[1], "two")
	assert.Equal(t, 0, len(items))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--commit-message-conventions-enabled=one,two",
		"--commit-message-conventions-one-expression=",
		"--commit-message-conventions-two-expression=",
	})

	commitMessageConventions, err = commandLineConfigurationLayer.GetCommitMessageConventions()
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

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--commit-message-conventions-enabled=one,two",
		"--commit-message-conventions-one-expression=expr1",
		"--commit-message-conventions-one-bumpExpressions-alpha=alpha1",
		"--commit-message-conventions-two-expression=expr2",
		"--commit-message-conventions-two-bumpExpressions-beta=beta1",
		"--commit-message-conventions-two-bumpExpressions-gamma=gamma1",
	})

	commitMessageConventions, err = commandLineConfigurationLayer.GetCommitMessageConventions()
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

func TestCommandLineConfigurationLayerGetConfigurationFile(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	configurationFile, err := commandLineConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Nil(t, configurationFile)

	// Test the extended version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--configuration-file=config.yml",
	})

	configurationFile, err = commandLineConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.yml", *configurationFile)

	// Test the short version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-c=config.yml",
	})

	configurationFile, err = commandLineConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.yml", *configurationFile)

	// Test both the extended and the short version together
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-c=config.json",
		"--configuration-file=config.yml",
	})

	configurationFile, err = commandLineConfigurationLayer.GetConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.json", *configurationFile)
}

func TestCommandLineConfigurationLayerGetDirectory(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	directory, err := commandLineConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Nil(t, directory)

	dir1, err := ioutil.TempDir("", "1")
	assert.NoError(t, err)
	dir2, err := ioutil.TempDir("", "2")
	assert.NoError(t, err)

	// Test the extended version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--directory=" + dir1,
	})

	directory, err = commandLineConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Equal(t, dir1, *directory)

	// Test the short version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-d=" + dir2,
	})

	directory, err = commandLineConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Equal(t, dir2, *directory)

	// Test both the extended and the short version together
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-d=" + dir1,
		"--directory=" + dir2,
	})

	directory, err = commandLineConfigurationLayer.GetDirectory()
	assert.NoError(t, err)
	assert.Equal(t, dir1, *directory)

	defer os.RemoveAll(dir1) // clean up
	defer os.RemoveAll(dir2) // clean up
}

func TestCommandLineConfigurationLayerGetDryRun(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	dryRun, err := commandLineConfigurationLayer.GetDryRun()
	assert.NoError(t, err)
	assert.Nil(t, dryRun)

	// Test the name and value version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--dry-run=false",
	})

	dryRun, err = commandLineConfigurationLayer.GetDryRun()
	assert.NoError(t, err)
	assert.Equal(t, false, *dryRun)

	// Test the flag version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--dry-run",
	})

	dryRun, err = commandLineConfigurationLayer.GetDryRun()
	assert.NoError(t, err)
	assert.Equal(t, true, *dryRun)
}

func TestCommandLineConfigurationLayerGetGit(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	git, err := commandLineConfigurationLayer.GetGit()
	assert.NoError(t, err)
	assert.NotNil(t, git)
	assert.Equal(t, 0, len(*git.GetRemotes()))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--git-remotes-one-user=jdoe",
		"--git-remotes-two-user=stiger",
	})

	git, err = commandLineConfigurationLayer.GetGit()
	remotes := *git.GetRemotes()
	assert.NoError(t, err)
	assert.NotNil(t, git)

	assert.Equal(t, 2, len(*git.GetRemotes()))
	assert.NotNil(t, remotes["one"])
	assert.NotNil(t, remotes["two"])

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--git-remotes-one-user=jdoe",
		"--git-remotes-one-password=pwd",
		"--git-remotes-two-user=stiger",
		"--git-remotes-two-password=sct",
	})

	git, err = commandLineConfigurationLayer.GetGit()
	remotes = *git.GetRemotes()

	assert.Equal(t, 2, len(*git.GetRemotes()))
	assert.Equal(t, "pwd", *remotes["one"].GetPassword())
	assert.Equal(t, "jdoe", *remotes["one"].GetUser())
	assert.Equal(t, "sct", *remotes["two"].GetPassword())
	assert.Equal(t, "stiger", *remotes["two"].GetUser())
}

func TestCommandLineConfigurationLayerGetInitialVersion(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	initialVersion, err := commandLineConfigurationLayer.GetInitialVersion()
	assert.NoError(t, err)
	assert.Nil(t, initialVersion)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--initial-version=0.3.5",
	})

	initialVersion, err = commandLineConfigurationLayer.GetInitialVersion()
	assert.NoError(t, err)
	assert.Equal(t, "0.3.5", *initialVersion)
}

func TestCommandLineConfigurationLayerGetPreset(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	preset, err := commandLineConfigurationLayer.GetPreset()
	assert.NoError(t, err)
	assert.Nil(t, preset)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--preset=simple",
	})

	preset, err = commandLineConfigurationLayer.GetPreset()
	assert.NoError(t, err)
	assert.Equal(t, "simple", *preset)
}

func TestCommandLineConfigurationLayerGetReleaseAssets(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	releaseAssets, err := commandLineConfigurationLayer.GetReleaseAssets()
	assert.NoError(t, err)
	assert.NotNil(t, releaseAssets)
	assert.Equal(t, 0, len(*releaseAssets))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-assets-asset1-type=text/plain",
	})

	releaseAssets, err = commandLineConfigurationLayer.GetReleaseAssets()

	assert.Equal(t, 1, len(*releaseAssets))
	assert.NotNil(t, (*releaseAssets)["asset1"])

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-assets-asset1-fileName=asset.txt",
		"--release-assets-asset1-description=Text Asset",
		"--release-assets-asset1-type=text/plain",
		"--release-assets-asset1-path=asset.txt",
		"--release-assets-asset2-fileName=asset.bin",
		"--release-assets-asset2-description=Binary Asset",
		"--release-assets-asset2-type=application/octet-stream",
		"--release-assets-asset2-path=asset.bin",
	})

	releaseAssets, err = commandLineConfigurationLayer.GetReleaseAssets()
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

func TestCommandLineConfigurationLayerGetReleaseLenient(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	releaseLenient, err := commandLineConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, err)
	assert.Nil(t, releaseLenient)

	// Test the name and value version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-lenient=false",
	})

	releaseLenient, err = commandLineConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, err)
	assert.Equal(t, false, *releaseLenient)

	// Test the name and value version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-lenient",
	})

	releaseLenient, err = commandLineConfigurationLayer.GetReleaseLenient()
	assert.NoError(t, err)
	assert.Equal(t, true, *releaseLenient)
}

func TestCommandLineConfigurationLayerGetReleasePrefix(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	releasePrefix, err := commandLineConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, err)
	assert.Nil(t, releasePrefix)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-prefix=prefix",
	})

	releasePrefix, err = commandLineConfigurationLayer.GetReleasePrefix()
	assert.NoError(t, err)
	assert.Equal(t, "prefix", *releasePrefix)
}

func TestCommandLineConfigurationLayerGetReleaseTypes(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	releaseTypes, err := commandLineConfigurationLayer.GetReleaseTypes()
	assert.NoError(t, err)
	assert.NotNil(t, releaseTypes)

	assert.Equal(t, 0, len(*releaseTypes.GetEnabled()))
	assert.Equal(t, 0, len(*releaseTypes.GetPublicationServices()))
	assert.Equal(t, 0, len(*releaseTypes.GetRemoteRepositories()))
	assert.Equal(t, 0, len(*releaseTypes.GetItems()))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-types-enabled=one,two",
		"--release-types-publication-services=first,second",
		"--release-types-remote-repositories=origin,replica",
	})

	releaseTypes, err = commandLineConfigurationLayer.GetReleaseTypes()
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

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-types-enabled=one,two",
		"--release-types-publication-services=first,second",
		"--release-types-remote-repositories=origin,replica",
		"--release-types-one-collapse-versions=",
		"--release-types-two-collapse-versions=",
	})

	releaseTypes, err = commandLineConfigurationLayer.GetReleaseTypes()
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
	assert.Nil(t, (*(*releaseTypes.GetItems())["one"]).GetAssets())
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetAssets())

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--release-types-enabled=one,two",
		"--release-types-publication-services=first,second",
		"--release-types-remote-repositories=origin,replica",
		"--release-types-one-assets=asset1,asset2",
		"--release-types-one-collapse-versions=true",
		"--release-types-one-collapsed-version-qualifier=qualifier1",
		"--release-types-one-description=description1",
		"--release-types-one-git-commit=true",
		"--release-types-one-git-push=true",
		"--release-types-one-git-tag=true",
		"--release-types-one-match-branches=alpha,beta",
		"--release-types-one-match-workspace-status=" + ent.DIRTY.String(),
		"--release-types-one-publish=false",
		"--release-types-one-version-range=true",
		"--release-types-two-collapse-versions=false",
		"--release-types-two-description=description2",
		"--release-types-two-filter-tags=filter2",
		"--release-types-two-git-commit=false",
		"--release-types-two-git-commit-message=Commit message",
		"--release-types-two-git-push=false",
		"--release-types-two-git-tag=false",
		"--release-types-two-git-tag-message=Tag message",
		"--release-types-two-identifiers-0-position=" + ent.PRE_RELEASE.String(),
		"--release-types-two-identifiers-0-qualifier=q1",
		"--release-types-two-identifiers-0-value=v1",
		"--release-types-two-identifiers-1-qualifier=q2",
		"--release-types-two-identifiers-1-value=v2",
		// note we use ordinal 9 here, but the list will return it at the 2nd position anyway because it sorts them regardles of gaps between ordinals
		"--release-types-two-identifiers-9-position=" + ent.BUILD.String(),
		"--release-types-two-identifiers-9-qualifier=q3",
		"--release-types-two-identifiers-9-value=v3",
		"--release-types-two-match-environment-variables-PATH=any path",
		"--release-types-two-match-environment-variables-USER=any user",
		"--release-types-two-match-workspace-status=" + ent.CLEAN.String(),
		"--release-types-two-publish=true",
		"--release-types-two-version-range-from-branch-name=true",
	})

	releaseTypes, err = commandLineConfigurationLayer.GetReleaseTypes()
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
	assert.Equal(t, "true", *(*(*releaseTypes.GetItems())["one"]).GetGitPush())
	assert.Equal(t, 0, len(*(*(*releaseTypes.GetItems())["one"]).GetIdentifiers()))
	assert.Equal(t, "alpha,beta", *(*(*releaseTypes.GetItems())["one"]).GetMatchBranches())
	assert.Equal(t, 0, len(*(*(*releaseTypes.GetItems())["one"]).GetMatchEnvironmentVariables()))
	assert.Equal(t, ent.DIRTY, *(*(*releaseTypes.GetItems())["one"]).GetMatchWorkspaceStatus())
	assert.Equal(t, "false", *(*(*releaseTypes.GetItems())["one"]).GetPublish())
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
	assert.Nil(t, (*(*releaseTypes.GetItems())["two"]).GetVersionRange())
	assert.True(t, *(*(*releaseTypes.GetItems())["two"]).GetVersionRangeFromBranchName())
}

func TestCommandLineConfigurationLayerGetResume(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	resume, err := commandLineConfigurationLayer.GetResume()
	assert.NoError(t, err)
	assert.Nil(t, resume)

	// Test the name and value version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--resume=false",
	})

	resume, err = commandLineConfigurationLayer.GetResume()
	assert.NoError(t, err)
	assert.Equal(t, false, *resume)

	// Test the flag version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--resume",
	})

	resume, err = commandLineConfigurationLayer.GetResume()
	assert.NoError(t, err)
	assert.Equal(t, true, *resume)
}

func TestCommandLineConfigurationLayerGetScheme(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	scheme, err := commandLineConfigurationLayer.GetScheme()
	assert.NoError(t, err)
	assert.Nil(t, scheme)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--scheme=" + ver.SEMVER.String(),
	})

	scheme, err = commandLineConfigurationLayer.GetScheme()
	assert.NoError(t, err)
	assert.Equal(t, ver.SEMVER, *scheme)
}

func TestCommandLineConfigurationLayerGetServices(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	services, err := commandLineConfigurationLayer.GetServices()
	assert.NoError(t, err)
	assert.NotNil(t, services)
	assert.Equal(t, 0, len(*services))

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--services-github-type=GITHUB",
	})

	services, err = commandLineConfigurationLayer.GetServices()

	assert.Equal(t, 1, len(*services))
	assert.NotNil(t, (*services)["github"])

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--services-github-type=GITHUB",
		"--services-github-options-BASE_URI=https://someuri1.com/",
		"--services-github-options-AUTHENTICATION_TOKEN=1234567890",
		"--services-github-options-REPOSITORY_NAME=repo1",
		"--services-github-options-REPOSITORY_OWNER=owner1",
		"--services-gitlab-type=GITLAB",
		"--services-gitlab-options-BASE_URI=https://someuri2.com/",
		"--services-gitlab-options-AUTHENTICATION_TOKEN=abcdefghij",
		"--services-gitlab-options-REPOSITORY_NAME=repo2",
		"--services-gitlab-options-REPOSITORY_OWNER=owner2",
	})

	services, err = commandLineConfigurationLayer.GetServices()
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

func TestCommandLineConfigurationLayerGetSharedConfigurationFile(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	sharedConfigurationFile, err := commandLineConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, err)
	assert.Nil(t, sharedConfigurationFile)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--shared-configuration-file=config.yml",
	})

	sharedConfigurationFile, err = commandLineConfigurationLayer.GetSharedConfigurationFile()
	assert.NoError(t, err)
	assert.Equal(t, "config.yml", *sharedConfigurationFile)
}

func TestCommandLineConfigurationLayerGetStateFile(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	stateFile, err := commandLineConfigurationLayer.GetStateFile()
	assert.NoError(t, err)
	assert.Nil(t, stateFile)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--state-file=state.yml",
	})

	stateFile, err = commandLineConfigurationLayer.GetStateFile()
	assert.NoError(t, err)
	assert.Equal(t, "state.yml", *stateFile)
}

func TestCommandLineConfigurationLayerGetVerbosity(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	verbosity, err := commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Nil(t, verbosity)

	// Test the extended version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--verbosity=" + ent.INFO.String(),
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.INFO, *verbosity)

	// Test the flag version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--fatal",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.FATAL, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--error",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.ERROR, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--warning",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.WARNING, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--info",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.INFO, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--debug",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.DEBUG, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--trace",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.TRACE, *verbosity)

	// Test the flag version, passing rubbish values (they must be ignored)
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--fatal=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.FATAL, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--error=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.ERROR, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--warning=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.WARNING, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--info=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.INFO, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--debug=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.DEBUG, *verbosity)

	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--trace=whatever",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.TRACE, *verbosity)

	// Test the extended and the flag version together
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--verbosity=" + ent.INFO.String(),
		"--fatal=",
		"--error=",
		"--warning=",
		"--info=",
		"--debug=",
		"--trace=",
	})

	verbosity, err = commandLineConfigurationLayer.GetVerbosity()
	assert.NoError(t, err)
	assert.Equal(t, ent.TRACE, *verbosity)
}

func TestCommandLineConfigurationLayerGetVersion(t *testing.T) {
	commandLineConfigurationLayer := CommandLineConfigurationLayer{}

	version, err := commandLineConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Nil(t, version)

	// Test the extended version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"--version=3.5.7",
	})

	version, err = commandLineConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Equal(t, "3.5.7", *version)

	// Test the short version
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}
	commandLineConfigurationLayer.withArguments([]string{
		"-v=4.5.7",
	})
	version, err = commandLineConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Equal(t, "4.5.7", *version)

	// Test both the extended and the short version together
	// get a new instance or a stale set of arguments is still in the configuration layer
	commandLineConfigurationLayer = CommandLineConfigurationLayer{}

	commandLineConfigurationLayer.withArguments([]string{
		"-v=4.5.7",
		"--version=3.5.7",
	})
	version, err = commandLineConfigurationLayer.GetVersion()
	assert.NoError(t, err)
	assert.Equal(t, "4.5.7", *version)
}

/*func TestCommandLineConfigurationLayerPrintHelp(t *testing.T) {
	PrintHelp()
}*/
