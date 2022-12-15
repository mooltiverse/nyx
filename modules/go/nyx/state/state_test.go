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

func TestStateGetNewRelease(t *testing.T) {
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)
	// inject a releaseType with the 'publish' flag to TRUE
	state.SetReleaseType(ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), nil, nil, nil, utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, nil, nil, nil, nil /*this is the 'publish' flag -> */, utl.PointerToString("true"), nil, utl.PointerToBoolean(false)))
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
	state.SetReleaseType(ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), nil, nil, nil, utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, nil, nil, nil, nil /*this is the 'publish' flag -> */, utl.PointerToString("false"), nil, utl.PointerToBoolean(false)))

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
	assert.True(t, time.Now().Unix() >= *timestamp)
}

func TestStateTouchTimestamp(t *testing.T) {
	// make sure that when touching the timestamp the new value is updated
	configuration, _ := cnf.NewConfiguration()
	state, _ := NewStateWith(configuration)

	oldTimestamp, _ := state.GetTimestamp()
	for ok := true; ok; ok = *oldTimestamp == time.Now().Unix() {
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

	initialCommit := gitent.NewCommitWith("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("initial commit", "initial commit", map[string]string{}), []gitent.Tag{})
	finalCommit := gitent.NewCommitWith("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, []string{"b50926577d36f403f4b3ebf51dfe34660b52eaa2"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("final commit", "final commit", map[string]string{}), []gitent.Tag{})

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
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, []string{"c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("fix: a commit that fixes something", "fix: a commit that fixes something", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false)}))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, []string{"2e348e90e5e1b89c678555459aecbfc34e17ef44"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("feat: a commit that adds a feature", "feat: a commit that adds a feature", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false)}))

	releaseType := ent.NewReleaseType()
	releaseType.SetCollapseVersions(utl.PointerToBoolean(true))
	releaseType.SetCollapsedVersionQualifier(utl.PointerToString("rel"))
	releaseType.SetDescription(utl.PointerToString("Some description"))
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitCommitMessage(utl.PointerToString("Commit message"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetGitTagMessage(utl.PointerToString("Tag message"))
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
	assert.Equal(t, *releaseScope1.GetPreviousVersion(), *releaseScope2.GetPreviousVersion())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetSHA(), (*releaseScope2.GetPreviousVersionCommit()).GetSHA())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetSHA(), (*releaseScope2.GetPrimeVersionCommit()).GetSHA())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())

	timestamp1, _ := oldState.GetTimestamp()
	timestamp2, _ := resumedState.GetTimestamp()
	assert.Equal(t, *timestamp1, *timestamp2)

	version1, _ := oldState.GetVersion()
	version2, _ := resumedState.GetVersion()
	assert.Equal(t, *version1, *version2)

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

	initialCommit := gitent.NewCommitWith("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("initial commit", "initial commit", map[string]string{}), []gitent.Tag{})
	finalCommit := gitent.NewCommitWith("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, []string{"b50926577d36f403f4b3ebf51dfe34660b52eaa2"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("final commit", "final commit", map[string]string{}), []gitent.Tag{})

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
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, []string{"c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("fix: a commit that fixes something", "fix: a commit that fixes something", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false)}))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, []string{"2e348e90e5e1b89c678555459aecbfc34e17ef44"}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", "jim@example.com"), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("feat: a commit that adds a feature", "feat: a commit that adds a feature", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false)}))

	releaseType := ent.NewReleaseType()
	releaseType.SetCollapseVersions(utl.PointerToBoolean(true))
	releaseType.SetCollapsedVersionQualifier(utl.PointerToString("rel"))
	releaseType.SetDescription(utl.PointerToString("Some description"))
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitCommitMessage(utl.PointerToString("Commit message"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetGitTagMessage(utl.PointerToString("Tag message"))
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
	assert.Equal(t, *releaseScope1.GetPreviousVersion(), *releaseScope2.GetPreviousVersion())
	assert.Equal(t, (*releaseScope1.GetPreviousVersionCommit()).GetSHA(), (*releaseScope2.GetPreviousVersionCommit()).GetSHA())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())
	assert.Equal(t, (*releaseScope1.GetPrimeVersionCommit()).GetSHA(), (*releaseScope2.GetPrimeVersionCommit()).GetSHA())
	assert.Equal(t, *releaseScope1.GetPrimeVersion(), *releaseScope2.GetPrimeVersion())

	timestamp1, _ := oldState.GetTimestamp()
	timestamp2, _ := resumedState.GetTimestamp()
	assert.Equal(t, *timestamp1, *timestamp2)

	version1, _ := oldState.GetVersion()
	version2, _ := resumedState.GetVersion()
	assert.Equal(t, *version1, *version2)

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
