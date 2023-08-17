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
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strings"       // https://pkg.go.dev/strings
	"testing"       // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	cmdtpl "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/command/template"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

/*
Reads the contents of the given file and returns its content as a string.
*/
func readFile(file string) string {
	b, err := os.ReadFile(file)
	if err != nil {
		panic(err)
	}
	return string(b)
}

/*
Writes the given contents to the given file
*/
func writeFile(file string, content string) {
	err := os.WriteFile(file, []byte(content), 0644)
	if err != nil {
		panic(err)
	}
}

func TestMakeConstructor(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, command)
		})
	}
}

/*
Check that the State method never returns a nil object
*/
func TestMakeState(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, (*command).State())
		})
	}
}

/*
Check that the IsUpToDate() returns false when the command instance is just created and true after one execution in a repository
with at least one commit and in a clean state
*/
func TestMakeIsUpToDate(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// running in an empty repository, with no commits, throws an error
			_, err = (*command).Run()
			assert.Error(t, err)

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
Check that the IsUpToDate() returns false when the changelog destination file is configured but
the file is missing
*/
func TestMakeIsUpToDateTestWithMissingChangelogFile(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			_, err = os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			} else {
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// now delete the file and make sure it's no longer up to date
				os.Remove(changelogFile)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.False(t, upToDate)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result with a commit message convention configured
*/
func TestMakeIdempotencyWithCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			// run a first time
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// collect its state values
				changelog, _ := (*command).State().GetChangelog()
				changelogReleases := changelog.GetReleases()
				newVersion, _ := (*command).State().GetNewVersion()
				version, _ := (*command).State().GetVersion()
				changelogFileContent := readFile(changelogFile)

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ := (*command).State().GetChangelog()
				changelogReleases2 := changelog2.GetReleases()
				newVersion2, _ := (*command).State().GetNewVersion()
				version2, _ := (*command).State().GetVersion()
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)
				assert.Equal(t, changelogReleases, changelogReleases2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				assert.Equal(t, changelogFileContent, readFile(changelogFile))

				// now delete the file and make sure it's no longer up to date
				os.Remove(changelogFile)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
				changelogFileContent = ""

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

				// check that some values have changed
				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version and the changelog hasn't been recreated
				assert.NotEqual(t, newVersion, newVersion2)
				assert.NotEqual(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// collect state values again
				changelogReleases = nil
				newVersion, _ = (*command).State().GetNewVersion()
				version, _ = (*command).State().GetVersion()

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result without a commit message convention configured
*/
func TestMakeIdempotencyWithoutCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			// run a first time
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists) // with no convention no version is generated and no changelog is written

				// collect its state values
				newVersion, _ := (*command).State().GetNewVersion()
				version, _ := (*command).State().GetVersion()

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ := (*command).State().GetChangelog()
				newVersion2, _ := (*command).State().GetNewVersion()
				version2, _ := (*command).State().GetVersion()
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
				assert.Nil(t, changelog2)
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)

				// add some commits to the repository and after one run the task should be up to date
				(*command).Script().AndCommitWithTag("111.122.133")
				upToDate, err = (*command).IsUpToDate()
				assert.False(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				// check that some values have changed
				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is still nil because there is no new version and the changelog hasn't been recreated
				assert.Equal(t, newVersion, newVersion2)
				assert.NotEqual(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// collect state values again
				version, _ = (*command).State().GetVersion()

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result with a commit message convention configured
*/
func TestMakeIdempotencyInDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			// run a first time
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// collect its state values
				changelog, _ := (*command).State().GetChangelog()
				changelogReleases := changelog.GetReleases()
				newVersion, _ := (*command).State().GetNewVersion()
				version, _ := (*command).State().GetVersion()
				changelogFileContent := readFile(changelogFile)

				// add some uncommitted changes
				(*command).Script().UpdateAllWorkbenchFiles()

				// run again and check that all values are still the same
				upToDate, err := (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ := (*command).State().GetChangelog()
				newVersion2, _ := (*command).State().GetNewVersion()
				version2, _ := (*command).State().GetVersion()
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)
				assert.Equal(t, changelogReleases, changelog.GetReleases())
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				assert.Equal(t, changelogFileContent, readFile(changelogFile))

				// now delete the file and make sure it's no longer up to date
				os.Remove(changelogFile)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
				changelogFileContent = ""

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

				// check that some values have changed
				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version and the changelog hasn't been recreated
				assert.NotEqual(t, newVersion, newVersion2)
				assert.NotEqual(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// collect state values again
				newVersion, _ = (*command).State().GetNewVersion()
				version, _ = (*command).State().GetVersion()
				changelogReleases = nil

				// run again and make sure values didn't change
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				// once more, also considering that its still up to date
				upToDate, err = (*command).IsUpToDate()
				assert.NoError(t, err)
				assert.True(t, upToDate)
				_, err = (*command).Run()
				assert.NoError(t, err)

				changelog2, _ = (*command).State().GetChangelog()
				newVersion2, _ = (*command).State().GetNewVersion()
				version2, _ = (*command).State().GetVersion()
				assert.Nil(t, changelog2) // the internal changelog is now nil because there is no new version
				assert.Equal(t, newVersion, newVersion2)
				assert.Equal(t, version, version2)
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithNoDestinationFile(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				changelog, _ := (*command).State().GetChangelog()
				assert.Nil(t, changelog)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithNoCommitConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.False(t, changelogFileExists)

				changelog, _ := (*command).State().GetChangelog()
				assert.Nil(t, changelog)
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithConventionalCommitsConventionAndWithoutSections(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// print the file to standard output for inspection purpose
				//fmt.Printf("------- CHANGELOG -------\n")
				//fmt.Printf("Loading from: %v\n", changelogFile)
				//fmt.Printf("-----------------------------------------\n")
				//fmt.Printf(readFile(changelogFile))
				//fmt.Println()
				//fmt.Printf("-----------------------------------------\n")

				// test the data model
				changelog, _ := (*command).State().GetChangelog()
				assert.Equal(t, 1, len(changelog.GetReleases()))
				assert.Equal(t, "0.1.0", *(*changelog.GetReleases()[0]).GetName())
				assert.Equal(t, 2, len((*changelog.GetReleases()[0]).GetSections()))
				assert.Equal(t, "feat", *(*(*changelog.GetReleases()[0]).GetSections()[0]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[0]).GetCommits()))
				assert.Equal(t, "fix", *(*(*changelog.GetReleases()[0]).GetSections()[1]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[1]).GetCommits()))

				// test the rendered file
				fileContent := readFile(changelogFile)
				assert.True(t, strings.HasPrefix(fileContent, "# Changelog"))                 // title header check
				assert.True(t, strings.Contains(fileContent, "## 0.1.0 "))                    // release header check
				assert.True(t, strings.Contains(fileContent, "### feat"))                     // section header check
				assert.True(t, strings.Contains(fileContent, "] feat: Untagged commit #2 (")) // partial line check
				assert.True(t, strings.Contains(fileContent, "### fix"))                      // section header check
				assert.True(t, strings.Contains(fileContent, "] fix: Untagged commit #1 ("))  // partial line check
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithConventionalCommitsConventionAndWithCustomSections(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the sections to be remapped
			changelogConfiguration.SetSections(&map[string]string{
				"Added": "^feat$",
				"Fixed": "^fix$",
			})
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// print the file to standard output for inspection purpose
				//fmt.Printf("------- CHANGELOG -------\n")
				//fmt.Printf("Loading from: %v\n", changelogFile)
				//fmt.Printf("-----------------------------------------\n")
				//fmt.Printf(readFile(changelogFile))
				//fmt.Println()
				//fmt.Printf("-----------------------------------------\n")

				// test the data model
				changelog, _ := (*command).State().GetChangelog()
				assert.Equal(t, 1, len(changelog.GetReleases()))
				assert.Equal(t, "0.1.0", *(*changelog.GetReleases()[0]).GetName())
				assert.Equal(t, 2, len((*changelog.GetReleases()[0]).GetSections()))
				assert.Equal(t, "Added", *(*(*changelog.GetReleases()[0]).GetSections()[0]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[0]).GetCommits()))
				assert.Equal(t, "Fixed", *(*(*changelog.GetReleases()[0]).GetSections()[1]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[1]).GetCommits()))

				// test the rendered file
				fileContent := readFile(changelogFile)
				assert.True(t, strings.HasPrefix(fileContent, "# Changelog"))                 // title header check
				assert.True(t, strings.Contains(fileContent, "## 0.1.0 "))                    // release header check
				assert.True(t, strings.Contains(fileContent, "### Added"))                    // section header check
				assert.True(t, strings.Contains(fileContent, "] feat: Untagged commit #2 (")) // partial line check
				assert.True(t, strings.Contains(fileContent, "### Fixed"))                    // section header check
				assert.True(t, strings.Contains(fileContent, "] fix: Untagged commit #1 ("))  // partial line check
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithConventionalCommitsConventionAndWithCustomSectionsAndSubstitutions(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the sections to be remapped
			changelogConfiguration.SetSections(&map[string]string{
				"Added": "^feat$",
				"Fixed": "^fix$",
			})
			// add the substitution rules to replace issue IDs with links
			changelogConfiguration.SetSubstitutions(&map[string]string{
				"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)",
			})
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// print the file to standard output for inspection purpose
				//fmt.Printf("------- CHANGELOG -------\n")
				//fmt.Printf("Loading from: %v\n", changelogFile)
				//fmt.Printf("-----------------------------------------\n")
				//fmt.Printf(readFile(changelogFile))
				//fmt.Println()
				//fmt.Printf("-----------------------------------------\n")

				// test the data model
				changelog, _ := (*command).State().GetChangelog()
				assert.Equal(t, 1, len(changelog.GetReleases()))
				assert.Equal(t, "0.1.0", *(*changelog.GetReleases()[0]).GetName())
				assert.Equal(t, 2, len((*changelog.GetReleases()[0]).GetSections()))
				assert.Equal(t, "Added", *(*(*changelog.GetReleases()[0]).GetSections()[0]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[0]).GetCommits()))
				assert.Equal(t, "Fixed", *(*(*changelog.GetReleases()[0]).GetSections()[1]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[1]).GetCommits()))

				// test the rendered file
				fileContent := readFile(changelogFile)
				assert.True(t, strings.HasPrefix(fileContent, "# Changelog"))                       // title header check
				assert.True(t, strings.Contains(fileContent, "## 0.1.0 "))                          // release header check
				assert.True(t, strings.Contains(fileContent, "### Added"))                          // section header check
				assert.False(t, strings.Contains(fileContent, " #2 "))                              // partial line check
				assert.True(t, strings.Contains(fileContent, "[#2](https://example.com/issues/2)")) // partial line check
				assert.True(t, strings.Contains(fileContent, "### Fixed"))                          // section header check
				assert.False(t, strings.Contains(fileContent, " #1 "))                              // partial line check
				assert.True(t, strings.Contains(fileContent, "[#1](https://example.com/issues/1)")) // partial line check
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithCustomTemplateFromLocalFile(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			// create the custom template, with simple strings used as markers
			templateFile := filepath.Join(destinationDir, "template.tpl")
			writeFile(templateFile, "# This is a custom changelog\n            {{#releases}}\n            ## {{name}} ({{date}})\n            {{/releases}}\n")
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			changelogConfiguration.SetTemplate(&templateFile)
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// print the file to standard output for inspection purpose
				//fmt.Printf("------- CHANGELOG -------\n")
				//fmt.Printf("Loading from: %v\n", changelogFile)
				//fmt.Printf("-----------------------------------------\n")
				//fmt.Printf(readFile(changelogFile))
				//fmt.Println()
				//fmt.Printf("-----------------------------------------\n")

				// test the data model
				changelog, _ := (*command).State().GetChangelog()
				assert.Equal(t, 1, len(changelog.GetReleases()))
				assert.Equal(t, "0.1.0", *(*changelog.GetReleases()[0]).GetName())
				assert.Equal(t, 2, len((*changelog.GetReleases()[0]).GetSections()))
				assert.Equal(t, "feat", *(*(*changelog.GetReleases()[0]).GetSections()[0]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[0]).GetCommits()))
				assert.Equal(t, "fix", *(*(*changelog.GetReleases()[0]).GetSections()[1]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[1]).GetCommits()))

				// test the rendered file
				fileContent := readFile(changelogFile)
				assert.True(t, strings.HasPrefix(fileContent, "# This is a custom changelog")) // title header check
				assert.True(t, strings.Contains(fileContent, "## 0.1.0 "))                     // release header check
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithCustomTemplateFromURL(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.ONE_BRANCH_SHORT_CONVENTIONAL_COMMITS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// first create the temporary directory and the abstract destination file
			destinationDir, _ := os.MkdirTemp("", "nyx-test-make-test-")
			defer os.RemoveAll(destinationDir)
			changelogFile := filepath.Join(destinationDir, "CHANGELOG.md")

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFile)
			// add the substitution rules to replace issue IDs with links
			changelogConfiguration.SetSubstitutions(&map[string]string{
				"(?m)#([0-9]+)(?s)": "[#%s](https://example.com/issues/%s)",
			})
			// load the preconfigured from the Nyx repository, getting the raw content, just to have it loaded from a remote URL
			changelogConfiguration.SetTemplate(utl.PointerToString("https://raw.githubusercontent.com/mooltiverse/nyx/main/modules/go/nyx/command/template/changelog.tpl"))
			// add the conventional commits convention
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")},
				&map[string]*ent.CommitMessageConvention{"conventionalCommits": cnf.COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := os.Stat(changelogFile)
			changelogFileExists := err == nil
			assert.False(t, changelogFileExists)

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so Run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				_, err = os.Stat(changelogFile)
				changelogFileExists = err == nil
				assert.True(t, changelogFileExists)

				// print the file to standard output for inspection purpose
				//fmt.Printf("------- CHANGELOG -------\n")
				//fmt.Printf("Loading from: %v\n", changelogFile)
				//fmt.Printf("-----------------------------------------\n")
				//fmt.Printf(readFile(changelogFile))
				//fmt.Println()
				//fmt.Printf("-----------------------------------------\n")

				// test the data model
				changelog, _ := (*command).State().GetChangelog()
				assert.Equal(t, 1, len(changelog.GetReleases()))
				assert.Equal(t, "0.1.0", *(*changelog.GetReleases()[0]).GetName())
				assert.Equal(t, 2, len((*changelog.GetReleases()[0]).GetSections()))
				assert.Equal(t, "feat", *(*(*changelog.GetReleases()[0]).GetSections()[0]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[0]).GetCommits()))
				assert.Equal(t, "fix", *(*(*changelog.GetReleases()[0]).GetSections()[1]).GetName())
				assert.Equal(t, 1, len((*(*changelog.GetReleases()[0]).GetSections()[1]).GetCommits()))

				// test the rendered file
				fileContent := readFile(changelogFile)
				assert.True(t, strings.HasPrefix(fileContent, "# Changelog")) // title header check
				assert.True(t, strings.Contains(fileContent, "## 0.1.0 "))    // release header check
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingCargoVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("cargo_version")},
				&map[string]*ent.Substitution{"cargo_version": cnf.CARGO_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			cargoFileInRootDirectory := filepath.Join(destinationDir, "Cargo.toml")
			cargoFileInSubDirectory := filepath.Join(destinationSubDir, "Cargo.toml")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{cargoFileInRootDirectory, cargoFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(cargoFileInRootDirectory, "[package]\nname = \"hello_world\" # the name of the package\nversion = \"91.92.93\" # the current version, obeying semver\nauthors = [\"Alice <a@example.com>\", \"Bob <b@example.com>\"]\n")
			writeFile(cargoFileInSubDirectory, "[package]\nname = \"hello_world\" # the name of the package\n   version   =     \"91.92.93\" # the current version, obeying semver\nauthors = [\"Alice <a@example.com>\", \"Bob <b@example.com>\"]\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(cargoFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version = \"0.1.0\""))

				fileContent = readFile(cargoFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version = \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingComposerVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("composer_version")},
				&map[string]*ent.Substitution{"composer_version": cnf.COMPOSER_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			composerFileInRootDirectory := filepath.Join(destinationDir, "composer.json")
			composerFileInSubDirectory := filepath.Join(destinationSubDir, "composer.json")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{composerFileInRootDirectory, composerFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(composerFileInRootDirectory, "{\n    \"version\": \"91.92.93\"\n}\n")
			writeFile(composerFileInSubDirectory, "{\n    \"version\"  :      \"91.92.93\"\n}\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(composerFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(composerFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingDartVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("dart_version")},
				&map[string]*ent.Substitution{"dart_version": cnf.DART_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			dartFileInRootDirectory := filepath.Join(destinationDir, "pubspec.yaml")
			dartFileInSubDirectory := filepath.Join(destinationSubDir, "pubspec.yaml")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{dartFileInRootDirectory, dartFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(dartFileInRootDirectory, "name: newtify\ndescription: >-\n    Blah blah\nversion: 91.92.93\nhomepage: https://example-pet-store.com/newtify\n")
			writeFile(dartFileInSubDirectory, "name: newtify\ndescription: >-\n    Blah blah\nversion: 91.92.93\nhomepage: https://example-pet-store.com/newtify\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(dartFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(dartFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingElixirVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("elixir_version")},
				&map[string]*ent.Substitution{"elixir_version": cnf.ELIXIR_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			elixirFileInRootDirectory := filepath.Join(destinationDir, "mix.exs")
			elixirFileInSubDirectory := filepath.Join(destinationSubDir, "mix.exs")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{elixirFileInRootDirectory, elixirFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(elixirFileInRootDirectory, "defmodule KV.MixProject do\n  use Mix.Project\n  def project do\n    [\n      app: :kv,\n      version: \"91.92.93\",\n      elixir: \"~> 1.11\",\n    ]\n  end\n")
			writeFile(elixirFileInSubDirectory, "defmodule KV.MixProject do\n  use Mix.Project\n  def project do\n    [\n      app: :kv,\n      version   :     \"91.92.93\"   ,\n      elixir: \"~> 1.11\",\n    ]\n  end\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(elixirFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(elixirFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingExpoVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("expo_version")},
				&map[string]*ent.Substitution{"expo_version": cnf.EXPO_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			expoFileInRootDirectory := filepath.Join(destinationDir, "app.json")
			expoFileInSubDirectory := filepath.Join(destinationSubDir, "app.config.json")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{expoFileInRootDirectory, expoFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(expoFileInRootDirectory, "{\n    \"expo\": {\n        \"name\": \"appname\",\n        \"slug\": \"appslug\",\n        \"version\": \"91.92.93\",\n        \"orientation\": \"portrait\",\n    }\n}\n")
			writeFile(expoFileInSubDirectory, "{\n    \"expo\": {\n        \"name\": \"appname\",\n        \"slug\": \"appslug\",\n        \"version\"   :     \"91.92.93\",\n        \"orientation\": \"portrait\",\n    }\n}\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(expoFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(expoFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingHelmVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("helm_version")},
				&map[string]*ent.Substitution{"helm_version": cnf.HELM_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			helmFileInRootDirectory := filepath.Join(destinationDir, "Chart.yaml")
			helmFileInSubDirectory := filepath.Join(destinationSubDir, "Chart.yaml")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{helmFileInRootDirectory, helmFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(helmFileInRootDirectory, "apiVersion: The chart API version\nname: The name of the chart (required)\nversion: \"91.92.93\"\nkubeVersion: A SemVer range of compatible Kubernetes versions (optional)\ndescription: A single-sentence description of this project (optional)\n")
			writeFile(helmFileInSubDirectory, "apiVersion: The chart API version\nname: The name of the chart (required)\nversion: \"91.92.93\"\nkubeVersion: A SemVer range of compatible Kubernetes versions (optional)\ndescription: A single-sentence description of this project (optional)\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(helmFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(helmFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "version: \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingNodeVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("node_version")},
				&map[string]*ent.Substitution{"node_version": cnf.NODE_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			nodeFileInRootDirectory := filepath.Join(destinationDir, "package.json")
			nodeFileInSubDirectory := filepath.Join(destinationSubDir, "package.json")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{nodeFileInRootDirectory, nodeFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(nodeFileInRootDirectory, "{\n    \"name\": \"foo\",\n    \"version\": \"91.92.93\",\n    \"description\": \"A packaged foo fooer for fooing foos\",\n}\n")
			writeFile(nodeFileInSubDirectory, "{\n    \"name\": \"foo\",\n    \"version\"   :     \"91.92.93\",\n    \"description\": \"A packaged foo fooer for fooing foos\",\n}\n")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(nodeFileInRootDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(nodeFileInSubDirectory)
				assert.False(t, strings.Contains(fileContent, "91.92.93"))
				assert.True(t, strings.Contains(fileContent, "\"version\": \"0.1.0\""))

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestMakeRunWithSubstitutionsUsingTextVersionPreset(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MAKE, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			// first create the temporary directory and a subdirectory
			destinationDir := (*command).Script().GetWorkingDirectory()
			destinationSubDir := filepath.Join(destinationDir, "sub")
			err := os.MkdirAll(destinationSubDir, os.ModePerm)
			assert.NoError(t, err)
			defer os.RemoveAll(destinationSubDir)
			defer os.RemoveAll(destinationDir)

			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			substitutions, err := ent.NewSubstitutionsWith(
				&[]*string{utl.PointerToString("text_version")},
				&map[string]*ent.Substitution{"text_version": cnf.TEXT_VERSION},
			)
			assert.NoError(t, err)
			configurationLayerMock.SetSubstitutions(substitutions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			textFileInRootDirectory := filepath.Join(destinationDir, "version.txt")
			textFileInSubDirectory := filepath.Join(destinationSubDir, "version.txt")
			otherFileInRootDirectory := filepath.Join(destinationDir, "other.txt")
			otherFileInSubDirectory := filepath.Join(destinationSubDir, "other.txt")
			//files := []string{textFileInRootDirectory, textFileInSubDirectory, otherFileInRootDirectory, otherFileInSubDirectory}

			writeFile(textFileInRootDirectory, "91.92.93")
			writeFile(textFileInSubDirectory, "91.92.93")

			writeFile(otherFileInRootDirectory, "Some text file not to be touched by the command")
			writeFile(otherFileInSubDirectory, "Some text file not to be touched by the command")

			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
			if (*command).GetContextName() != cmdtpl.STANDALONE_CONTEXT_NAME {
				// print the files to standard output for inspection purpose
				//for _, f := range files {
				//	fmt.Printf("--------------------- " + f + " ---------------------\n")
				//	fmt.Printf(readFile(f))
				//	fmt.Println()
				//	fmt.Printf("-----------------------------------------\n")
				//}

				// test the rendered files
				fileContent := readFile(textFileInRootDirectory)
				assert.Equal(t, "0.1.0", fileContent)

				fileContent = readFile(textFileInSubDirectory)
				assert.Equal(t, "0.1.0", fileContent)

				fileContent = readFile(otherFileInRootDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)

				fileContent = readFile(otherFileInSubDirectory)
				assert.Equal(t, "Some text file not to be touched by the command", fileContent)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}
