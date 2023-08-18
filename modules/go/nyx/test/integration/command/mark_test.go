//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package command_test

import (
	"os"      // https://pkg.go.dev/os
	"testing" // https://pkg.go.dev/testing
	"time"    // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	nyx "github.com/mooltiverse/nyx/modules/go/nyx"
	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gitlab "github.com/mooltiverse/nyx/modules/go/nyx/services/gitlab"
	cmdtpl "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/command/template"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestMarkConstructor(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, command)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that the State method never returns a nil object
*/
func TestMarkState(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, (*command).State())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that the IsUpToDate() returns false when the command instance is just created and true after one execution in a repository
with at least one commit and in a clean state.
*/
func TestMarkIsUpToDate(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())

			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// running in an empty repository, with no commits, throws an error
			_, err = (*command).Run()
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			} else {
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
			}

			// and running again with no changes must still be up to date
			_, err = (*command).Run()
			assert.NoError(t, err)
			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			} else {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that the IsUpToDate() always returns true even when the repository is dirty.
*/
func TestMarkIsUpToDateInDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())

			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			} else {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
			}

			// if we add uncommitted files it must return still return true
			(*command).Script().AddRandomTextWorkbenchFiles(1)
			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			} else {
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// still true even after staging
				(*command).Script().Stage()
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// but returns false with a new commit as long as it doesn't run again
				(*command).Script().AndCommitWithTag("111.122.144")
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result with a commit message convention configured
*/
func TestMarkIdempotencyWithCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"minor": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// run a first time
				_, err := (*command).Run()
				assert.NoError(t, err)

				// collect its state values
				branch, _ := (*command).State().GetBranch()
				bump, _ := (*command).State().GetBump()
				coreVersion, _ := (*command).State().GetCoreVersion()
				latestVersion, _ := (*command).State().GetLatestVersion()
				newRelease, _ := (*command).State().GetNewRelease()
				newVersion, _ := (*command).State().GetNewVersion()
				releaseScope, _ := (*command).State().GetReleaseScope()
				commits := releaseScope.GetCommits()
				significantCommits := releaseScope.GetSignificantCommits()
				previousVersion := releaseScope.GetPreviousVersion()
				previousVersionCommit := releaseScope.GetPreviousVersionCommit()
				releaseType, _ := (*command).State().GetReleaseType()
				matchBranches := releaseType.GetMatchBranches()
				scheme, _ := (*command).State().GetScheme()
				timestamp, _ := (*command).State().GetTimestamp()
				version, _ := (*command).State().GetVersion()
				versionRange, _ := (*command).State().GetVersionRange()

				// collect repository values
				branches := (*command).Script().GetBranches()
				commitIDs := (*command).Script().GetCommitIDs()
				lastCommitID := (*command).Script().GetLastCommitID()
				tags := (*command).Script().GetTags()

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values for comparison
				branch2, _ := (*command).State().GetBranch()
				bump2, _ := (*command).State().GetBump()
				coreVersion2, _ := (*command).State().GetCoreVersion()
				latestVersion2, _ := (*command).State().GetLatestVersion()
				newRelease2, _ := (*command).State().GetNewRelease()
				newVersion2, _ := (*command).State().GetNewVersion()
				releaseScope2, _ := (*command).State().GetReleaseScope()
				commits2 := releaseScope2.GetCommits()
				significantCommits2 := releaseScope2.GetSignificantCommits()
				previousVersion2 := releaseScope2.GetPreviousVersion()
				previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
				releaseType2, _ := (*command).State().GetReleaseType()
				matchBranches2 := releaseType2.GetMatchBranches()
				scheme2, _ := (*command).State().GetScheme()
				timestamp2, _ := (*command).State().GetTimestamp()
				version2, _ := (*command).State().GetVersion()
				versionRange2, _ := (*command).State().GetVersionRange()

				// collect repository values for comparison
				branches2 := (*command).Script().GetBranches()
				commitIDs2 := (*command).Script().GetCommitIDs()
				lastCommitID2 := (*command).Script().GetLastCommitID()
				tags2 := (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, matchBranches, matchBranches2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// add some commits to the repository and after one run the task should be up to date
				(*command).Script().AndCommitWithTag("111.122.133")
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				releaseType2, _ = (*command).State().GetReleaseType()
				matchBranches2 = releaseType2.GetMatchBranches()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				// check that some values have changed
				assert.NotEqual(t, commits, commits2)
				assert.NotEqual(t, significantCommits, significantCommits2)
				assert.NotEqual(t, previousVersion, previousVersion2)
				assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, timestamp, timestamp2)
				assert.NotEqual(t, version, version2)
				assert.NotEqual(t, commitIDs, commitIDs2)
				assert.NotEqual(t, lastCommitID, lastCommitID2)
				assert.NotEqual(t, tags, tags2)

				// collect its state values again
				branch, _ = (*command).State().GetBranch()
				bump, _ = (*command).State().GetBump()
				coreVersion, _ = (*command).State().GetCoreVersion()
				latestVersion, _ = (*command).State().GetLatestVersion()
				newRelease, _ = (*command).State().GetNewRelease()
				newVersion, _ = (*command).State().GetNewVersion()
				releaseScope, _ = (*command).State().GetReleaseScope()
				commits = releaseScope.GetCommits()
				significantCommits = releaseScope.GetSignificantCommits()
				previousVersion = releaseScope.GetPreviousVersion()
				previousVersionCommit = releaseScope.GetPreviousVersionCommit()
				releaseType, _ = (*command).State().GetReleaseType()
				matchBranches = releaseType.GetMatchBranches()
				scheme, _ = (*command).State().GetScheme()
				timestamp, _ = (*command).State().GetTimestamp()
				version, _ = (*command).State().GetVersion()
				versionRange, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches = (*command).Script().GetBranches()
				commitIDs = (*command).Script().GetCommitIDs()
				lastCommitID = (*command).Script().GetLastCommitID()
				tags = (*command).Script().GetTags()

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				releaseType2, _ = (*command).State().GetReleaseType()
				matchBranches2 = releaseType2.GetMatchBranches()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, matchBranches, matchBranches2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				releaseType2, _ = (*command).State().GetReleaseType()
				matchBranches2 = releaseType2.GetMatchBranches()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, matchBranches, matchBranches2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result without a commit message convention configured
*/
func TestMarkIdempotencyWithoutCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// run a first time
			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// collect its state values
				branch, _ := (*command).State().GetBranch()
				bump, _ := (*command).State().GetBump()
				coreVersion, _ := (*command).State().GetCoreVersion()
				latestVersion, _ := (*command).State().GetLatestVersion()
				newRelease, _ := (*command).State().GetNewRelease()
				newVersion, _ := (*command).State().GetNewVersion()
				releaseScope, _ := (*command).State().GetReleaseScope()
				commits := releaseScope.GetCommits()
				significantCommits := releaseScope.GetSignificantCommits()
				previousVersion := releaseScope.GetPreviousVersion()
				previousVersionCommit := releaseScope.GetPreviousVersionCommit()
				scheme, _ := (*command).State().GetScheme()
				timestamp, _ := (*command).State().GetTimestamp()
				version, _ := (*command).State().GetVersion()
				versionRange, _ := (*command).State().GetVersionRange()

				// collect repository values
				branches := (*command).Script().GetBranches()
				commitIDs := (*command).Script().GetCommitIDs()
				lastCommitID := (*command).Script().GetLastCommitID()
				tags := (*command).Script().GetTags()

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values for comparison
				branch2, _ := (*command).State().GetBranch()
				bump2, _ := (*command).State().GetBump()
				coreVersion2, _ := (*command).State().GetCoreVersion()
				latestVersion2, _ := (*command).State().GetLatestVersion()
				newRelease2, _ := (*command).State().GetNewRelease()
				newVersion2, _ := (*command).State().GetNewVersion()
				releaseScope2, _ := (*command).State().GetReleaseScope()
				commits2 := releaseScope2.GetCommits()
				significantCommits2 := releaseScope2.GetSignificantCommits()
				previousVersion2 := releaseScope2.GetPreviousVersion()
				previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
				scheme2, _ := (*command).State().GetScheme()
				timestamp2, _ := (*command).State().GetTimestamp()
				version2, _ := (*command).State().GetVersion()
				versionRange2, _ := (*command).State().GetVersionRange()

				// collect repository values for comparison
				branches2 := (*command).Script().GetBranches()
				commitIDs2 := (*command).Script().GetCommitIDs()
				lastCommitID2 := (*command).Script().GetLastCommitID()
				tags2 := (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// add some commits to the repository and after one run the task should be up to date
				(*command).Script().AndCommitWithTag("111.122.133")
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				// check that some values have changed
				assert.NotEqual(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2) // with no convention no commit is significant
				assert.NotEqual(t, previousVersion, previousVersion2)
				assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, timestamp, timestamp2)
				assert.NotEqual(t, version, version2)
				assert.NotEqual(t, commitIDs, commitIDs2)
				assert.NotEqual(t, lastCommitID, lastCommitID2)
				assert.NotEqual(t, tags, tags2)

				// collect its state values again
				branch, _ = (*command).State().GetBranch()
				bump, _ = (*command).State().GetBump()
				coreVersion, _ = (*command).State().GetCoreVersion()
				latestVersion, _ = (*command).State().GetLatestVersion()
				newRelease, _ = (*command).State().GetNewRelease()
				newVersion, _ = (*command).State().GetNewVersion()
				releaseScope, _ = (*command).State().GetReleaseScope()
				commits = releaseScope.GetCommits()
				significantCommits = releaseScope.GetSignificantCommits()
				previousVersion = releaseScope.GetPreviousVersion()
				previousVersionCommit = releaseScope.GetPreviousVersionCommit()
				scheme, _ = (*command).State().GetScheme()
				timestamp, _ = (*command).State().GetTimestamp()
				version, _ = (*command).State().GetVersion()
				versionRange, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches = (*command).Script().GetBranches()
				commitIDs = (*command).Script().GetCommitIDs()
				lastCommitID = (*command).Script().GetLastCommitID()
				tags = (*command).Script().GetTags()

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result without a commit message convention configured
*/
func TestMarkIdempotencyInDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// run a first time
				_, err := (*command).Run()
				assert.NoError(t, err)

				// collect its state values
				branch, _ := (*command).State().GetBranch()
				bump, _ := (*command).State().GetBump()
				coreVersion, _ := (*command).State().GetCoreVersion()
				latestVersion, _ := (*command).State().GetLatestVersion()
				newRelease, _ := (*command).State().GetNewRelease()
				newVersion, _ := (*command).State().GetNewVersion()
				releaseScope, _ := (*command).State().GetReleaseScope()
				commits := releaseScope.GetCommits()
				significantCommits := releaseScope.GetSignificantCommits()
				previousVersion := releaseScope.GetPreviousVersion()
				previousVersionCommit := releaseScope.GetPreviousVersionCommit()
				scheme, _ := (*command).State().GetScheme()
				timestamp, _ := (*command).State().GetTimestamp()
				version, _ := (*command).State().GetVersion()
				versionRange, _ := (*command).State().GetVersionRange()

				// collect repository values
				branches := (*command).Script().GetBranches()
				commitIDs := (*command).Script().GetCommitIDs()
				lastCommitID := (*command).Script().GetLastCommitID()
				tags := (*command).Script().GetTags()

				// add some uncommitted changes
				(*command).Script().UpdateAllWorkbenchFiles()

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values for comparison
				branch2, _ := (*command).State().GetBranch()
				bump2, _ := (*command).State().GetBump()
				coreVersion2, _ := (*command).State().GetCoreVersion()
				latestVersion2, _ := (*command).State().GetLatestVersion()
				newRelease2, _ := (*command).State().GetNewRelease()
				newVersion2, _ := (*command).State().GetNewVersion()
				releaseScope2, _ := (*command).State().GetReleaseScope()
				commits2 := releaseScope2.GetCommits()
				significantCommits2 := releaseScope2.GetSignificantCommits()
				previousVersion2 := releaseScope2.GetPreviousVersion()
				previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
				scheme2, _ := (*command).State().GetScheme()
				timestamp2, _ := (*command).State().GetTimestamp()
				version2, _ := (*command).State().GetVersion()
				versionRange2, _ := (*command).State().GetVersionRange()

				// collect repository values for comparison
				branches2 := (*command).Script().GetBranches()
				commitIDs2 := (*command).Script().GetCommitIDs()
				lastCommitID2 := (*command).Script().GetLastCommitID()
				tags2 := (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// add some commits to the repository and after one run the task should be up to date
				(*command).Script().AndCommitWithTag("111.122.133")
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				// check that some values have changed
				assert.NotEqual(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2) // with no convention no commit is significant
				assert.NotEqual(t, previousVersion, previousVersion2)
				assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, timestamp, timestamp2)
				assert.NotEqual(t, version, version2)
				assert.NotEqual(t, commitIDs, commitIDs2)
				assert.NotEqual(t, lastCommitID, lastCommitID2)
				assert.NotEqual(t, tags, tags2)

				// collect state values again
				branch, _ = (*command).State().GetBranch()
				bump, _ = (*command).State().GetBump()
				coreVersion, _ = (*command).State().GetCoreVersion()
				latestVersion, _ = (*command).State().GetLatestVersion()
				newRelease, _ = (*command).State().GetNewRelease()
				newVersion, _ = (*command).State().GetNewVersion()
				releaseScope, _ = (*command).State().GetReleaseScope()
				commits = releaseScope.GetCommits()
				significantCommits = releaseScope.GetSignificantCommits()
				previousVersion = releaseScope.GetPreviousVersion()
				previousVersionCommit = releaseScope.GetPreviousVersionCommit()
				scheme, _ = (*command).State().GetScheme()
				timestamp, _ = (*command).State().GetTimestamp()
				version, _ = (*command).State().GetVersion()
				versionRange, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches = (*command).Script().GetBranches()
				commitIDs = (*command).Script().GetCommitIDs()
				lastCommitID = (*command).Script().GetLastCommitID()
				tags = (*command).Script().GetTags()

				// add some uncommitted changes
				(*command).Script().UpdateAllWorkbenchFiles()

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)

				// add some uncommitted changes
				(*command).Script().UpdateAllWorkbenchFiles()

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				// collect state values again
				branch2, _ = (*command).State().GetBranch()
				bump2, _ = (*command).State().GetBump()
				coreVersion2, _ = (*command).State().GetCoreVersion()
				latestVersion2, _ = (*command).State().GetLatestVersion()
				newRelease2, _ = (*command).State().GetNewRelease()
				newVersion2, _ = (*command).State().GetNewVersion()
				releaseScope2, _ = (*command).State().GetReleaseScope()
				commits2 = releaseScope2.GetCommits()
				significantCommits2 = releaseScope2.GetSignificantCommits()
				previousVersion2 = releaseScope2.GetPreviousVersion()
				previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
				scheme2, _ = (*command).State().GetScheme()
				timestamp2, _ = (*command).State().GetTimestamp()
				version2, _ = (*command).State().GetVersion()
				versionRange2, _ = (*command).State().GetVersionRange()

				// collect repository values again
				branches2 = (*command).Script().GetBranches()
				commitIDs2 = (*command).Script().GetCommitIDs()
				lastCommitID2 = (*command).Script().GetLastCommitID()
				tags2 = (*command).Script().GetTags()

				assert.Equal(t, branch, branch2)
				assert.Equal(t, bump, bump2)
				assert.Equal(t, coreVersion, coreVersion2)
				assert.Equal(t, latestVersion, latestVersion2)
				assert.Equal(t, newRelease, newRelease2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, commits, commits2)
				assert.Equal(t, significantCommits, significantCommits2)
				assert.Equal(t, previousVersion, previousVersion2)
				assert.Equal(t, previousVersionCommit, previousVersionCommit2)
				assert.Equal(t, scheme, scheme2)
				assert.Equal(t, timestamp, timestamp2)
				assert.Equal(t, version, version2)
				assert.Equal(t, versionRange, versionRange2)
				assert.Equal(t, branches, branches2)
				assert.Equal(t, commitIDs, commitIDs2)
				assert.Equal(t, lastCommitID, lastCommitID2)
				assert.Equal(t, tags, tags2)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkErrorOnRunWithValidButEmptyGitRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			_, err := (*command).Run()
			assert.Error(t, err)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// disable all commit message conventions so no commit yields to a bump identifier
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{},
				&map[string]*ent.CommitMessageConvention{})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("false"))
			releaseType.SetGitPush(utl.PointerToString("false"))
			releaseType.SetGitTag(utl.PointerToString("false"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.4", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// disable all commit message conventions so no commit yields to a bump identifier
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{},
				&map[string]*ent.CommitMessageConvention{})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("false"))
			releaseType.SetGitPush(utl.PointerToString("false"))
			releaseType.SetGitTag(utl.PointerToString("false"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// add some uncommitted changes
			(*command).Script().AndAddFiles()

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.4", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// disable all commit message conventions so no commit yields to a bump identifier
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{},
				&map[string]*ent.CommitMessageConvention{})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("true"))
			releaseType.SetGitPush(utl.PointerToString("true"))
			releaseType.SetGitTag(utl.PointerToString("true"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.4", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// disable all commit message conventions so no commit yields to a bump identifier
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{},
				&map[string]*ent.CommitMessageConvention{})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("true"))
			releaseType.SetGitPush(utl.PointerToString("true"))
			releaseType.SetGitTag(utl.PointerToString("true"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// add some uncommitted changes
			(*command).Script().AndAddFiles()

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.4", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("false"))
			releaseType.SetGitPush(utl.PointerToString("false"))
			releaseType.SetGitTag(utl.PointerToString("false"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.5", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("false"))
			releaseType.SetGitPush(utl.PointerToString("false"))
			releaseType.SetGitTag(utl.PointerToString("false"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// add some uncommitted changes
			(*command).Script().AndAddFiles()

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.5", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags), len((*command).Script().GetTags()))
				assert.Equal(t, 0, len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("true"))
			releaseType.SetGitPush(utl.PointerToString("true"))
			releaseType.SetGitTag(utl.PointerToString("true"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.5", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags)+1, len((*command).Script().GetTags()))
				assert.Equal(t, len((*command).Script().GetTags()), len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabled(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("true"))
			releaseType.SetGitPush(utl.PointerToString("true"))
			releaseType.SetGitTag(utl.PointerToString("true"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// add some uncommitted changes
			(*command).Script().AndAddFiles()

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				assert.Equal(t, "0.0.5", *version2)
				assert.NotEqual(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits)+1, len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags)+1, len((*command).Script().GetTags()))
				assert.Equal(t, len((*command).Script().GetTags()), len(remoteScript.GetTags()))
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingMultipleTagnames(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			remoteScript := gittools.BARE().RealizeBare(true)
			defer os.RemoveAll(remoteScript.GetWorkingDirectory())
			(*command).Script().AddRemote(remoteScript.GetWorkingDirectory(), "replica") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
			previousLastCommit := (*command).Script().GetLastCommitID()
			previousCommits := (*command).Script().GetCommitIDs()
			previousTags := (*command).Script().GetTags()
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommit(utl.PointerToString("true"))
			releaseType.SetGitPush(utl.PointerToString("true"))
			releaseType.SetGitTag(utl.PointerToString("true"))
			// here 0.0.1 is an existing tag so we test for rewriting
			releaseType.SetGitTagNames(&[]*string{utl.PointerToString("0.0.1"), utl.PointerToString("{{version}}"), utl.PointerToString("{{versionMajorNumber}}"), utl.PointerToString("{{versionMajorNumber}}.{{versionMinorNumber}}")})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{utl.PointerToString("replica")},
				&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				version2, _ := (*command).State().GetVersion()
				majorVersion2, _ := (*command).State().GetVersionMajorNumber()
				minorVersion2, _ := (*command).State().GetVersionMinorNumber()
				assert.Equal(t, "0.0.5", *version2)
				assert.Equal(t, previousLastCommit, (*command).Script().GetLastCommitID())
				assert.Equal(t, len(previousCommits), len((*command).Script().GetCommitIDs()))
				assert.Equal(t, len(previousTags)+3, len((*command).Script().GetTags()))
				_, ok := (*command).Script().GetTags()[*version2]
				assert.True(t, ok)
				_, ok = (*command).Script().GetTags()[*majorVersion2]
				assert.True(t, ok)
				_, ok = (*command).Script().GetTags()[*majorVersion2+"."+*minorVersion2]
				assert.True(t, ok)
				assert.Equal(t, len((*command).Script().GetTags()), len(remoteScript.GetTags()))
				_, ok = remoteScript.GetTags()[*version2]
				assert.True(t, ok)
				_, ok = remoteScript.GetTags()[*majorVersion2]
				assert.True(t, ok)
				_, ok = remoteScript.GetTags()[*majorVersion2+"."+*minorVersion2]
				assert.True(t, ok)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitHubClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingUsernameAndPasswordCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 101)
	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitHubRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""), nil, nil),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithUserNameAndPassword((*gitHubRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitHub.DeleteGitRepository(randomID)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitHubClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingUnprotectedPrivateKeyCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 103)
	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithPublicKey((*gitHubRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), nil)
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), nil, nil, utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), nil),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithPublicKey((*gitHubRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), nil)
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitHub.DeleteGitRepository((*gitHubRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitHubClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingProtectedPrivateKeyCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 103)
	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithPublicKey((*gitHubRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyPassphrase")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), nil, nil, utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyPassphrase"))),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithPublicKey((*gitHubRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitHubTestUserPrivateKeyPassphrase")))
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitHub.DeleteGitRepository((*gitHubRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitLabClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingUsernameAndPasswordCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 103)
	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitLabRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")), nil, nil),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithUserNameAndPassword((*gitLabRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitLab.DeleteGitRepository((*gitLabRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitLabClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingUnprotectedPrivateKeyCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 103)
	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithPublicKey((*gitLabRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), nil)
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), nil, nil, utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), nil),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithPublicKey((*gitLabRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), nil)
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitLab.DeleteGitRepository((*gitLabRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestMarkRunOnGitLabClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingProtectedPrivateKeyCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 103)
	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	replicaScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(replicaScript.GetWorkingDirectory())
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithPublicKey((*gitLabRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyPassphrase")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AddRemote(replicaScript.GetGitDirectory(), "replica")

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add a service configuration to pass the credentials required to push to the remote repository
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), nil, nil, utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyPassphrase"))),
	})
	// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"patch": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add a custom release type that always enables committing, tagging and pushing
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{}, &[]*string{utl.PointerToString("origin"), utl.PointerToString("replica")},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)
	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	state, err := nyx.Mark()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// clone the remote repo again into a a new directory and test
	remoteScript := gittools.CloneFromWithPublicKey((*gitLabRepository).GetSSHURL(), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyWithoutPassphrase")), utl.PointerToString(os.Getenv("gitLabTestUserPrivateKeyPassphrase")))
	defer os.RemoveAll(remoteScript.GetWorkingDirectory())

	version, _ := state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Mark()
	assert.NoError(t, err)
	version, _ = state.GetVersion()
	assert.Equal(t, "0.0.5", *version)
	assert.Equal(t, len(script.GetTags()), len(replicaScript.GetTags()))
	assert.Equal(t, len(script.GetTags()), len(remoteScript.GetTags()))

	// now delete it
	gitLab.DeleteGitRepository((*gitLabRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}
