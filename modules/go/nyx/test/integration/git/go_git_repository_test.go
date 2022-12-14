//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package git_test

import (
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
	. "github.com/mooltiverse/nyx/modules/go/nyx/git"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func contains(elems []string, v string) bool {
	for _, s := range elems {
		if v == s {
			return true
		}
	}
	return false
}

func TestGoGitRepositoryCloneErrorWithNilDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	tr := REMOTE_TEST_REPOSITORY
	_, err := GitInstance().Clone(nil, &tr)
	assert.Error(t, err)
}

func TestGoGitRepositoryCloneErrorWithEmptyDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	tr := REMOTE_TEST_REPOSITORY
	es := ""
	_, err := GitInstance().Clone(&es, &tr)
	assert.Error(t, err)
	es = "  "
	_, err = GitInstance().Clone(&es, &tr)
	assert.Error(t, err)
}

func TestGoGitRepositoryCloneErrorWithNonEmptyDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	tr := REMOTE_TEST_REPOSITORY
	dir := script.GetWorkingDirectory()
	_, err := GitInstance().Clone(&dir, &tr)
	assert.Error(t, err)
}

func TestGoGitRepositoryCloneErrorWithNilURI(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	dir := "nyx-test-git-clone-test-"
	_, err := GitInstance().Clone(&dir, nil)
	assert.Error(t, err)
	defer os.RemoveAll(dir)
}

func TestGoGitRepositoryCloneErrorWithEmptyURI(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	dir := "nyx-test-git-clone-test-"
	uri := ""
	_, err := GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
	uri = "  "
	_, err = GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
	defer os.RemoveAll(dir)
}

func TestGoGitRepositoryCloneErrorWithNonExistingURI(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	dir := "nyx-test-git-clone-test-"
	uri := "https://adomainwiththisnamesuredoesnotexists.com/"
	_, err := GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
	defer os.RemoveAll(dir)
}

func TestGoGitRepositoryClone(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	tr := REMOTE_TEST_REPOSITORY
	dir := "nyx-test-git-clone-test-"
	directory := gitutil.NewTempDirectory("", &dir)
	defer os.RemoveAll(directory)

	_, err := os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)

	_, err = GitInstance().Clone(&directory, &tr)
	assert.NoError(t, err)

	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)
}

func TestGoGitRepositoryCloneWithNonRequiredCredentials(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	tr := REMOTE_TEST_REPOSITORY
	dir := "nyx-test-git-clone-test-"
	directory := gitutil.NewTempDirectory("", &dir)
	defer os.RemoveAll(directory)

	_, err := os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)

	// the 'gitHubTestUserToken' and 'gitHubTestUserToken' environment variables are set by the build script
	user := os.Getenv("gitHubTestUserToken")
	password := os.Getenv("gitHubTestUserToken")
	_, err = GitInstance().CloneWithCredentials(&directory, &tr, &user, &password)
	assert.NoError(t, err)

	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)
}

func TestGoGitRepositoryCloneWithoutRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 95)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), nil, nil)
	assert.Error(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryCloneWithRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 97)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryOpenErrorWithEmptyDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err := GitInstance().Open("")
	assert.Error(t, err)
	_, err = GitInstance().Open("  ")
	assert.Error(t, err)
}

func TestGoGitRepositoryOpenErrorWithNonExistingDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err := GitInstance().Open("adirectorywiththisnamesuredoesnotexists")
	assert.Error(t, err)
}

func TestGoGitRepositoryOpenErrorWithNewEmptyDirectory(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	directory := gitutil.NewTempDirectory("", nil)
	defer os.RemoveAll(directory)

	_, err := GitInstance().Open(directory)
	assert.Error(t, err)
}

func TestGoGitRepositoryOpen(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	_, err := GitInstance().Open(dir)
	assert.NoError(t, err)
}

func TestGoGitRepositoryAddErrorWithEmptyPaths(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	err = repository.Add([]string{})
	assert.Error(t, err)
}

func TestGoGitRepositoryAddErrorWithNilPaths(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	err = repository.Add(nil)
	assert.Error(t, err)
}

func TestGoGitRepositoryAdd(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remember the cache count may increas of more than 1 for each added file
	cacheCount := script.GetIndexEntryCount()
	script.AddRandomTextWorkbenchFiles(1)
	repository.Add([]string{"."})
	assert.True(t, cacheCount+1 <= script.GetIndexEntryCount())

	cacheCount = script.GetIndexEntryCount()
	script.AddRandomTextWorkbenchFiles(2)
	repository.Add([]string{"."})
	assert.True(t, cacheCount+2 <= script.GetIndexEntryCount())
}

func TestGoGitRepositoryCommitErrorWithNilMessageOn1Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	script.AddRandomTextWorkbenchFiles(1)
	_, err = repository.CommitWithMessage(nil)
	assert.Error(t, err)
}

func TestGoGitRepositoryCommit1Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	prevLastCommit := script.GetLastCommit()
	script.AddRandomTextWorkbenchFiles(1)
	script.Stage()

	msg := "A message"
	commit, err := repository.CommitWithMessage(&msg)
	assert.NoError(t, err)

	assert.NotEqual(t, prevLastCommit.Hash.String(), script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), commit.GetSHA())
	assert.Equal(t, script.GetLastCommit().Message, commit.GetMessage().GetFullMessage())
	assert.Equal(t, "A message", commit.GetMessage().GetFullMessage())
}

func TestGoGitRepositoryCommitErrorWithEmptyPathsOn2Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	msg := "A message"
	_, err = repository.CommitPathsWithMessage([]string{}, &msg)
	assert.Error(t, err)
}

func TestGoGitRepositoryCommitErrorWithNilPathsOn2Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	msg := "A message"
	_, err = repository.CommitPathsWithMessage(nil, &msg)
	assert.Error(t, err)
}

func TestGoGitRepositoryCommitErrorWithNilMessageOn2Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	script.AddRandomTextWorkbenchFiles(1)
	_, err = repository.CommitPathsWithMessage([]string{"."}, nil)
	assert.Error(t, err)
}

func TestGoGitRepositoryCommit2Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	prevLastCommit := script.GetLastCommit()
	script.AddRandomTextWorkbenchFiles(1)
	script.Stage()

	msg := "A message"
	commit, err := repository.CommitPathsWithMessage([]string{"."}, &msg)
	assert.NoError(t, err)

	assert.NotEqual(t, prevLastCommit.Hash.String(), script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), commit.GetSHA())
	assert.Equal(t, script.GetLastCommit().Message, commit.GetMessage().GetFullMessage())
	assert.Equal(t, "A message", commit.GetMessage().GetFullMessage())
}

func TestGoGitRepositoryCommitErrorWithNilMessageOn3Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	script.AddRandomTextWorkbenchFiles(1)
	_, err = repository.CommitWithMessageAndIdentities(nil, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.Error(t, err)
}

func TestGoGitRepositoryCommit3Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	prevLastCommit := script.GetLastCommit()
	script.AddRandomTextWorkbenchFiles(1)
	script.Stage()

	msg := "A message"
	commit, err := repository.CommitWithMessageAndIdentities(&msg, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("Sean Moe", "smoe@example.com"))
	assert.NoError(t, err)

	assert.NotEqual(t, prevLastCommit.Hash.String(), script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), commit.GetSHA())
	assert.Equal(t, script.GetLastCommit().Message, commit.GetMessage().GetFullMessage())
	assert.Equal(t, "A message", commit.GetMessage().GetFullMessage())

	assert.Equal(t, script.GetLastCommit().Author.Name, commit.GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, "John Doe", commit.GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, script.GetLastCommit().Author.Email, commit.GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, "jdoe@example.com", commit.GetAuthorAction().GetIdentity().GetEmail())

	assert.Equal(t, script.GetLastCommit().Committer.Name, commit.GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, "Sean Moe", commit.GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, script.GetLastCommit().Committer.Email, commit.GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, "smoe@example.com", commit.GetCommitAction().GetIdentity().GetEmail())
}

func TestGoGitRepositoryCommitErrorWithEmptyPathsOn4Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	msg := "A message"
	_, err = repository.CommitPathsWithMessageAndIdentities([]string{}, &msg, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.Error(t, err)
}

func TestGoGitRepositoryCommitErrorWithNilPathsOn4Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	msg := "A message"
	_, err = repository.CommitPathsWithMessageAndIdentities(nil, &msg, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.Error(t, err)
}

func TestGoGitRepositoryCommitErrorWithNilMessageOn4Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	script.AddRandomTextWorkbenchFiles(1)
	_, err = repository.CommitPathsWithMessageAndIdentities([]string{"."}, nil, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.Error(t, err)
}

func TestGoGitRepositoryCommit4Params(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	prevLastCommit := script.GetLastCommit()
	script.AddRandomTextWorkbenchFiles(1)
	script.Stage()

	msg := "A message"
	commit, err := repository.CommitPathsWithMessageAndIdentities([]string{"."}, &msg, gitent.NewIdentityWith("John Doe", "jdoe@example.com"), gitent.NewIdentityWith("Sean Moe", "smoe@example.com"))
	assert.NoError(t, err)

	assert.NotEqual(t, prevLastCommit.Hash.String(), script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), commit.GetSHA())
	assert.Equal(t, script.GetLastCommit().Message, commit.GetMessage().GetFullMessage())
	assert.Equal(t, "A message", commit.GetMessage().GetFullMessage())
}

func TestGoGitRepositoryPush(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	pushedRemote, err := repository.Push()
	assert.NoError(t, err)

	// now the default remote 'origin' has the first commit
	assert.Equal(t, "origin", pushedRemote)
	_ = remote1script.GetLastCommit()
	//assert.Nil(t, remote2script.GetLastCommit())

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	//assert.Nil(t, remote2script.GetLastCommit())

	// now push (to the default 'origin') and see the changes reflected
	pushedRemote, err = repository.Push()
	assert.NoError(t, err)

	// changes are reflected to 'origin' only
	assert.Equal(t, "origin", pushedRemote)
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	//assert.Nil(t, remote2script.GetLastCommit())
}

func TestGoGitRepositoryPushWithNonRequiredCredentials(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	// the 'gitHubTestUserToken' and 'gitHubTestUserToken' environment variables are set by the build script
	user := os.Getenv("gitHubTestUserToken")
	password := os.Getenv("gitHubTestUserToken")
	pushedRemote, err := repository.PushWithCredentials(&user, &password)
	assert.NoError(t, err)

	// now the default remote 'origin' has the first commit
	assert.Equal(t, "origin", pushedRemote)
	_ = remote1script.GetLastCommit()
	//assert.Nil(t, remote2script.GetLastCommit())

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	//assert.Nil(t, remote2script.GetLastCommit())

	// now push (to the default 'origin') and see the changes reflected
	pushedRemote, err = repository.PushWithCredentials(&user, &password)
	assert.NoError(t, err)

	// changes are reflected to 'origin' only
	assert.Equal(t, "origin", pushedRemote)
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	//assert.Nil(t, remote2script.GetLastCommit())
}

func TestGoGitRepositoryPushWithoutRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 95)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushWithCredentials(nil, nil)
	assert.Error(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryPushWithRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 97)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushWithCredentials(utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryPushToRemote(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	remoteName := "custom"
	pushedRemote, err := repository.PushToRemote(&remoteName)
	assert.NoError(t, err)

	// now the default remote 'origin' has the first commit
	assert.Equal(t, "custom", pushedRemote)
	//assert.Nil(t, remote1script.GetLastCommit())
	_ = remote2script.GetLastCommit()

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	//assert.Nil(t, remote1script.GetLastCommit())
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())

	// now push (to the default 'origin') and see the changes reflected
	pushedRemote, err = repository.PushToRemote(&remoteName)
	assert.NoError(t, err)

	// changes are reflected to 'origin' only
	assert.Equal(t, "custom", pushedRemote)
	//assert.Nil(t, remote1script.GetLastCommit())
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())
}

func TestGoGitRepositoryPushToRemoteWithNonRequiredCredentials(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	// the 'gitHubTestUserToken' and 'gitHubTestUserToken' environment variables are set by the build script
	user := os.Getenv("gitHubTestUserToken")
	password := os.Getenv("gitHubTestUserToken")
	remoteName := "custom"
	pushedRemote, err := repository.PushToRemoteWithCredentials(&remoteName, &user, &password)
	assert.NoError(t, err)

	// now the default remote 'origin' has the first commit
	assert.Equal(t, "custom", pushedRemote)
	//assert.Nil(t, remote1script.GetLastCommit())
	_ = remote2script.GetLastCommit()

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	//assert.Nil(t, remote1script.GetLastCommit())
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())

	// now push (to the default 'origin') and see the changes reflected
	pushedRemote, err = repository.PushToRemoteWithCredentials(&remoteName, &user, &password)
	assert.NoError(t, err)

	// changes are reflected to 'origin' only
	assert.Equal(t, "custom", pushedRemote)
	//assert.Nil(t, remote1script.GetLastCommit())
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())
}

func TestGoGitRepositoryPushToRemoteWithoutRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 95)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushToRemoteWithCredentials(utl.PointerToString("origin"), nil, nil)
	assert.Error(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryPushToRemoteWithRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 97)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushToRemoteWithCredentials(utl.PointerToString("origin"), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryPushToRemotes(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	remoteNames := []string{"origin", "custom"}
	pushedRemotes, err := repository.PushToRemotes(remoteNames)
	assert.NoError(t, err)

	// now the remotes have the first commit
	assert.True(t, contains(pushedRemotes, "origin"))
	assert.True(t, contains(pushedRemotes, "custom"))
	_ = remote1script.GetLastCommit()
	_ = remote2script.GetLastCommit()

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())

	// now push and see the changes reflected
	pushedRemotes, err = repository.PushToRemotes(remoteNames)
	assert.NoError(t, err)

	// changes are reflected to both remotes
	assert.True(t, contains(pushedRemotes, "origin"))
	assert.True(t, contains(pushedRemotes, "custom"))
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())
}

func TestGoGitRepositoryPushToRemotesWithNonRequiredCredentials(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	// also create two new empty repositories to use as remotes
	remote1script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote1script.GetWorkingDirectory())
	remote2script := gittools.BARE().RealizeBare(true)
	defer os.RemoveAll(remote2script.GetWorkingDirectory())
	script.AddRemote(remote1script.GetWorkingDirectory(), "origin") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
	script.AddRemote(remote2script.GetWorkingDirectory(), "custom") // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir

	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// remotes still have no commits
	//assert.Nil(t, remote1script.GetLastCommit())
	//assert.Nil(t, remote2script.GetLastCommit())

	// make a first sync, just to have a starting commit in remotes as well
	// the 'gitHubTestUserToken' and 'gitHubTestUserToken' environment variables are set by the build script
	user := os.Getenv("gitHubTestUserToken")
	password := os.Getenv("gitHubTestUserToken")
	remoteNames := []string{"origin", "custom"}
	pushedRemotes, err := repository.PushToRemotesWithCredentials(remoteNames, &user, &password)
	assert.NoError(t, err)

	// now the remotes have the first commit
	assert.True(t, contains(pushedRemotes, "origin"))
	assert.True(t, contains(pushedRemotes, "custom"))
	_ = remote1script.GetLastCommit()
	_ = remote2script.GetLastCommit()

	// add a commit into the local repo and make sure it's not into the others
	msg := "A commit message"
	script.AndCommitWith(&msg)
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	assert.NotEqual(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())

	// now push and see the changes reflected
	pushedRemotes, err = repository.PushToRemotesWithCredentials(remoteNames, &user, &password)
	assert.NoError(t, err)

	// changes are reflected to both remotes
	assert.True(t, contains(pushedRemotes, "origin"))
	assert.True(t, contains(pushedRemotes, "custom"))
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote1script.GetLastCommit().Hash.String())
	assert.Equal(t, script.GetLastCommit().Hash.String(), remote2script.GetLastCommit().Hash.String())
}

func TestGoGitRepositoryPushToRemotesWithoutRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 95)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushToRemotesWithCredentials([]string{"origin"}, nil, nil)
	assert.Error(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryPushToRemotesWithRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 97)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	defer os.RemoveAll(directory)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)
	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)

	_ = gittools.ONE_BRANCH_SHORT().ApplyIn(directory)
	repository, err := GitInstance().Open(directory)
	_, err = repository.PushToRemotesWithCredentials([]string{"origin"}, utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGoGitRepositoryTag(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// make sure an error is thrown when the tag name is nil
	_, err = repository.Tag(nil)
	assert.Error(t, err)

	assert.Equal(t, 0, len(script.GetTags()))

	tName := "ltag"
	lTag, err := repository.Tag(&tName)
	assert.NoError(t, err)

	latestCommit, err := repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "ltag", lTag.GetName())
	assert.Equal(t, 1, len(script.GetTags()))
	_, containsTag := script.GetTags()["ltag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["ltag"])
	assert.Equal(t, *script.GetCommitByTag("ltag"), latestCommit)

	// make sure an error is thrown when the tag name is duplicated
	lTag, err = repository.Tag(&tName)
	assert.Error(t, err)
}

func TestGoGitRepositoryTagWithMessage(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// make sure an error is thrown when the tag name is nil
	_, err = repository.TagWithMessage(nil, nil)
	assert.Error(t, err)
	tagMessage := "The tag message"
	_, err = repository.TagWithMessage(nil, &tagMessage)
	assert.Error(t, err)

	assert.Equal(t, 0, len(script.GetTags()))

	tName := "ltag"
	lTag, err := repository.TagWithMessage(&tName, nil)
	assert.NoError(t, err)

	latestCommit, err := repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "ltag", lTag.GetName())
	assert.Equal(t, 1, len(script.GetTags()))
	_, containsTag := script.GetTags()["ltag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["ltag"])
	assert.Equal(t, *script.GetCommitByTag("ltag"), latestCommit)

	tName = "atag"
	lTag, err = repository.TagWithMessage(&tName, &tagMessage)
	assert.NoError(t, err)

	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "atag", lTag.GetName())
	assert.Equal(t, 2, len(script.GetTags()))
	_, containsTag = script.GetTags()["atag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["atag"])
	assert.Equal(t, *script.GetCommitByTag("atag"), latestCommit)

	// make sure an error is thrown when the tag name is duplicated
	lTag, err = repository.TagWithMessage(&tName, nil)
	assert.Error(t, err)
	lTag, err = repository.TagWithMessage(&tName, &tagMessage)
	assert.Error(t, err)
}

func TestGoGitRepositoryTagWithMessageAndIdentity(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// make sure an error is thrown when the tag name is nil
	_, err = repository.TagWithMessageAndIdentity(nil, nil, nil)
	assert.Error(t, err)
	tagMessage := "The tag message"
	_, err = repository.TagWithMessageAndIdentity(nil, &tagMessage, nil)
	assert.Error(t, err)

	assert.Equal(t, 0, len(script.GetTags()))

	tName := "ltag"
	lTag, err := repository.TagWithMessageAndIdentity(&tName, nil, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, err := repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "ltag", lTag.GetName())
	assert.Equal(t, 1, len(script.GetTags()))
	_, containsTag := script.GetTags()["ltag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["ltag"])
	assert.Equal(t, *script.GetCommitByTag("ltag"), latestCommit)

	tName = "atag"
	lTag, err = repository.TagWithMessageAndIdentity(&tName, &tagMessage, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "atag", lTag.GetName())
	assert.Equal(t, 2, len(script.GetTags()))
	_, containsTag = script.GetTags()["atag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["atag"])
	assert.Equal(t, *script.GetCommitByTag("atag"), latestCommit)

	// make sure an error is thrown when the tag name is duplicated
	lTag, err = repository.TagWithMessageAndIdentity(&tName, nil, nil)
	assert.Error(t, err)
	lTag, err = repository.TagWithMessageAndIdentity(&tName, &tagMessage, nil)
	assert.Error(t, err)
}

func TestGoGitRepositoryTagCommitWithMessageAndIdentity(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// make sure an error is thrown when the tag name is nil
	_, err = repository.TagCommitWithMessageAndIdentity(nil, nil, nil, nil)
	assert.Error(t, err)
	tagMessage := "The tag message"
	_, err = repository.TagCommitWithMessageAndIdentity(nil, nil, &tagMessage, nil)
	assert.Error(t, err)

	assert.Equal(t, 0, len(script.GetTags()))

	tName := "ltag"
	latestCommit, err := repository.GetLatestCommit()
	lTag, err := repository.TagCommitWithMessageAndIdentity(&latestCommit, &tName, nil, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, err = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "ltag", lTag.GetName())
	assert.Equal(t, 1, len(script.GetTags()))
	_, containsTag := script.GetTags()["ltag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["ltag"])
	assert.Equal(t, *script.GetCommitByTag("ltag"), latestCommit)

	tName = "ltag2"
	lTag, err = repository.TagCommitWithMessageAndIdentity(nil, &tName, nil, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, err = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "ltag2", lTag.GetName())
	assert.Equal(t, 2, len(script.GetTags()))
	_, containsTag = script.GetTags()["ltag2"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["ltag2"])
	assert.Equal(t, *script.GetCommitByTag("ltag2"), latestCommit)

	tName = "atag"
	latestCommit, err = repository.GetLatestCommit()
	lTag, err = repository.TagCommitWithMessageAndIdentity(&latestCommit, &tName, &tagMessage, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "atag", lTag.GetName())
	assert.Equal(t, 3, len(script.GetTags()))
	_, containsTag = script.GetTags()["atag"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["atag"])
	assert.Equal(t, *script.GetCommitByTag("atag"), latestCommit)

	tName = "atag2"
	lTag, err = repository.TagCommitWithMessageAndIdentity(nil, &tName, &tagMessage, gitent.NewIdentityWith("John Doe", "jdoe@example.com"))
	assert.NoError(t, err)

	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, lTag.GetTarget())
	assert.Equal(t, "atag2", lTag.GetName())
	assert.Equal(t, 4, len(script.GetTags()))
	_, containsTag = script.GetTags()["atag2"]
	assert.True(t, containsTag)
	latestCommit, _ = repository.GetLatestCommit()
	assert.Equal(t, latestCommit, script.GetTags()["atag2"])
	assert.Equal(t, *script.GetCommitByTag("atag2"), latestCommit)

	// make sure an error is thrown when the tag name is duplicated
	lTag, err = repository.TagCommitWithMessageAndIdentity(&latestCommit, &tName, nil, nil)
	assert.Error(t, err)
	lTag, err = repository.TagCommitWithMessageAndIdentity(&latestCommit, &tName, &tagMessage, nil)
	assert.Error(t, err)
}

func TestGoGitRepositoryGetCurrentBranch(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.INITIAL_COMMIT().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	currentBranch, err := repository.GetCurrentBranch()
	assert.NoError(t, err)
	assert.Equal(t, "master", currentBranch)

	// add and stage some files
	script.AndAddFiles().AndStage().Commit("A commit")

	currentBranch, err = repository.GetCurrentBranch()
	assert.NoError(t, err)
	assert.Equal(t, "master", currentBranch)

	script.InBranch("testbranch")

	currentBranch, err = repository.GetCurrentBranch()
	assert.NoError(t, err)
	assert.Equal(t, "testbranch", currentBranch)
}

func TestGoGitRepositoryGetLatestCommitErrorWithRepositoryWithNoCommits(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	_, err = repository.GetLatestCommit()
	assert.Error(t, err)

	// add some new files and test
	script.AndAddFiles()

	_, err = repository.GetLatestCommit()
	assert.Error(t, err)

	// stage the files without committing
	script.AndStage()

	_, err = repository.GetLatestCommit()
	assert.Error(t, err)
}

func TestGoGitRepositoryGetLatestCommit(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// add and stage some files
	script.AndAddFiles().AndStage()

	// commit the files and get the commit SHA
	commitSHA1 := script.Commit("Test commit").Hash.String()
	latestCommit, err := repository.GetLatestCommit()
	assert.NoError(t, err)
	assert.Equal(t, commitSHA1, latestCommit)

	// make sure the root and latest commits are the same by now
	rootCommit, err := repository.GetRootCommit()
	assert.NoError(t, err)
	assert.Equal(t, rootCommit, latestCommit)

	// repeat the above with new changes
	script.AndAddNFiles(2)
	commitSHA2 := script.Commit("Test another commit").Hash.String()
	assert.NotEqual(t, commitSHA1, commitSHA2)
	latestCommit, err = repository.GetLatestCommit()
	assert.NoError(t, err)
	assert.Equal(t, commitSHA2, latestCommit)

	// make sure the root and latest commits are not the same anymore
	rootCommit, err = repository.GetRootCommit()
	assert.NoError(t, err)
	latestCommit, err = repository.GetLatestCommit()
	assert.NoError(t, err)
	assert.NotEqual(t, rootCommit, latestCommit)
}

func TestGoGitRepositoryGetRootCommitErrorWithRepositoryWithNoCommits(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	_, err = repository.GetRootCommit()
	assert.Error(t, err)

	// add some new files and test
	script.AndAddFiles()

	_, err = repository.GetRootCommit()
	assert.Error(t, err)

	// stage the files without committing
	script.AndStage()

	_, err = repository.GetRootCommit()
	assert.Error(t, err)
}

func TestGoGitRepositoryGetRootCommit(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// add and stage some files
	script.AndAddFiles().AndStage()

	// commit the files and get the commit SHA
	commitSHA1 := script.Commit("Test commit").Hash.String()
	rootCommit, err := repository.GetRootCommit()
	assert.NoError(t, err)
	assert.Equal(t, commitSHA1, rootCommit)

	// make sure the root and latest commits are the same by now
	latestCommit, err := repository.GetLatestCommit()
	assert.NoError(t, err)
	assert.Equal(t, rootCommit, latestCommit)

	// repeat the above with new changes
	script.AndAddNFiles(2)
	commitSHA2 := script.Commit("Test another commit").Hash.String()
	assert.NotEqual(t, commitSHA1, commitSHA2)
	rootCommit, err = repository.GetRootCommit()
	assert.NoError(t, err)
	assert.NotEqual(t, commitSHA2, latestCommit)

	// make sure the root and latest commits are not the same anymore
	rootCommit, err = repository.GetRootCommit()
	assert.NoError(t, err)
	latestCommit, err = repository.GetLatestCommit()
	assert.NoError(t, err)
	assert.NotEqual(t, rootCommit, latestCommit)
}

func TestGoGitRepositoryIsClean(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	clean, err := repository.IsClean()
	assert.NoError(t, err)
	assert.True(t, clean)

	// add some new files and test
	script.AndAddFiles()
	clean, err = repository.IsClean()
	assert.NoError(t, err)
	assert.False(t, clean)

	// stage the files without committing
	script.AndStage()
	clean, err = repository.IsClean()
	assert.NoError(t, err)
	assert.False(t, clean)

	// commit the files, now we're clean again
	script.AndCommit()
	clean, err = repository.IsClean()
	assert.NoError(t, err)
	assert.True(t, clean)
}

func TestGoGitRepositoryGetCommitTagsReturnsEmptyResultWithRepositoryWithNoCommits(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	tags, err := repository.GetCommitTags("")
	assert.NoError(t, err)
	assert.Equal(t, 0, len(tags))
}

func TestGoGitRepositoryGetCommitTags(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	// add a commit
	script.AndAddFiles().AndStage()
	commit := script.Commit("A message")

	// test with no tags
	tags, err := repository.GetCommitTags(commit.Hash.String())
	assert.NoError(t, err)
	assert.Equal(t, 0, len(tags))

	// test with one lightweight tag
	script.Tag("t1", nil)
	tags, err = repository.GetCommitTags(commit.Hash.String())
	assert.NoError(t, err)
	assert.Equal(t, 1, len(tags))
	assert.Equal(t, "t1", tags[0].GetName())
	assert.False(t, tags[0].IsAnnotated())

	// test with one more tag
	tagMessage := "Tag message"
	script.Tag("a1", &tagMessage)
	tags, err = repository.GetCommitTags(commit.Hash.String())
	assert.NoError(t, err)
	assert.Equal(t, 2, len(tags))
}

func TestGoGitRepositoryGetRemoteNamesWithNoRemotes(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	remoteNames, err := repository.GetRemoteNames()
	assert.Equal(t, 0, len(remoteNames))
}

func TestGoGitRepositoryGetRemoteNamesAfterClone(t *testing.T) {
	dir := "nyx-test-git-remote-names-test-"
	dir = gitutil.NewTempDirectory("", &dir)
	defer os.RemoveAll(dir)
	tr := REMOTE_TEST_REPOSITORY
	repository, err := GitInstance().Clone(&dir, &tr)
	assert.NoError(t, err)

	remoteNames, err := repository.GetRemoteNames()
	assert.Equal(t, 1, len(remoteNames))
	assert.Equal(t, "origin", remoteNames[0])
}

func TestGoGitRepositoryGetRemoteNamesAfterAddingLocalRepository(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	localRepositoryScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	localRepositoryScriptDir := localRepositoryScript.GetWorkingDirectory()

	script.AddRemote(localRepositoryScriptDir, "local")

	remoteNames, err := repository.GetRemoteNames()
	assert.Equal(t, 1, len(remoteNames))
	assert.Equal(t, "local", remoteNames[0])
}

func TestGoGitRepositoryGetRemoteNamesAfterCloneAndAddingLocalRepository(t *testing.T) {
	dir := "nyx-test-git-remote-names-test-"
	dir = gitutil.NewTempDirectory("", &dir)
	defer os.RemoveAll(dir)
	tr := REMOTE_TEST_REPOSITORY

	repository, err := GitInstance().Clone(&dir, &tr)
	assert.NoError(t, err)
	script := gittools.From(dir)

	localRepositoryScript := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(localRepositoryScript.GetWorkingDirectory())
	localRepositoryScriptDir := localRepositoryScript.GetWorkingDirectory()
	script.AddRemote(localRepositoryScriptDir, "local")

	remoteNames, err := repository.GetRemoteNames()
	assert.Equal(t, 2, len(remoteNames))
	assert.True(t, contains(remoteNames, "origin"))
	assert.True(t, contains(remoteNames, "local"))
}

func TestGoGitRepositoryWalkHistoryWithNoBoundaries(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, 10, len(visitedCommits))
	// make sure the last one is the root commit
	assert.Equal(t, 0, len(visitedCommits[len(visitedCommits)-1].GetParents()))
	rootCommit, err := repository.GetRootCommit()
	assert.NoError(t, err)
	assert.Equal(t, rootCommit, visitedCommits[len(visitedCommits)-1].GetSHA())
}

func TestGoGitRepositoryWalkHistoryErrorWithRepositoryWithNoCommits(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		return true
	})
	assert.Error(t, err)
}

func TestGoGitRepositoryWalkHistoryWithVisitorStoppingBrowsing(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return len(visitedCommits) < 2
	})
	assert.NoError(t, err)

	assert.Equal(t, 2, len(visitedCommits))
}

func TestGoGitRepositoryWalkHistoryWithStartBoundary(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, 10, len(visitedCommits))

	// now browse again with a start boundary (starting at the 3rd commit)
	var boundaryVisitedCommits []gitent.Commit
	start := visitedCommits[2].GetSHA()
	err = repository.WalkHistory(&start, nil, func(commit gitent.Commit) bool {
		boundaryVisitedCommits = append(boundaryVisitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, len(visitedCommits)-2, len(boundaryVisitedCommits))
	assert.Equal(t, visitedCommits[2].GetSHA(), boundaryVisitedCommits[0].GetSHA())                                                 // test the first visited commit
	assert.Equal(t, visitedCommits[len(visitedCommits)-1].GetSHA(), boundaryVisitedCommits[len(boundaryVisitedCommits)-1].GetSHA()) // test the last visited commit
}

func TestGoGitRepositoryWalkHistoryWithEndBoundary(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, 10, len(visitedCommits))

	// now browse again with a start boundary (starting at the 3rd commit)
	var boundaryVisitedCommits []gitent.Commit
	end := visitedCommits[len(visitedCommits)-3].GetSHA()
	err = repository.WalkHistory(nil, &end, func(commit gitent.Commit) bool {
		boundaryVisitedCommits = append(boundaryVisitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, len(visitedCommits)-2, len(boundaryVisitedCommits))
	assert.Equal(t, visitedCommits[0].GetSHA(), boundaryVisitedCommits[0].GetSHA())                                                 // test the first visited commit
	assert.Equal(t, visitedCommits[len(visitedCommits)-3].GetSHA(), boundaryVisitedCommits[len(boundaryVisitedCommits)-1].GetSHA()) // test the last visited commit
}

func TestGoGitRepositoryWalkHistoryWithBothBoundaries(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, 10, len(visitedCommits))

	// now browse again with a start boundary (starting at the 3rd commit)
	var boundaryVisitedCommits []gitent.Commit
	start := visitedCommits[2].GetSHA()
	end := visitedCommits[len(visitedCommits)-3].GetSHA()
	err = repository.WalkHistory(&start, &end, func(commit gitent.Commit) bool {
		boundaryVisitedCommits = append(boundaryVisitedCommits, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, len(visitedCommits)-4, len(boundaryVisitedCommits))
	assert.Equal(t, visitedCommits[2].GetSHA(), boundaryVisitedCommits[0].GetSHA())                                                 // test the first visited commit
	assert.Equal(t, visitedCommits[len(visitedCommits)-3].GetSHA(), boundaryVisitedCommits[len(boundaryVisitedCommits)-1].GetSHA()) // test the last visited commit
}

func TestGoGitRepositoryWalkHistoryWithStartBoundaryUnresolved(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	// this SHA is unknown to the repository, so it should throw an error
	start := "d0a19fc5776dc0c0b1a8d869c1117dac71065870"
	err = repository.WalkHistory(&start, nil, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.Error(t, err)
}

func TestGoGitRepositoryWalkHistoryWithEndBoundaryUnresolved(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	// this SHA is unknown to the repository, so it should throw an error
	end := "31cab6562ed66dfc71a4fcf65292a97fb81e0e75"
	err = repository.WalkHistory(nil, &end, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.Error(t, err)
}

func TestGoGitRepositoryWalkHistoryWithBothBoundariesUnresolved(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommits []gitent.Commit

	// these two SHAs are unknown to the repository, so they should throw an error
	start := "d0a19fc5776dc0c0b1a8d869c1117dac71065870"
	end := "31cab6562ed66dfc71a4fcf65292a97fb81e0e75"
	err = repository.WalkHistory(&start, &end, func(commit gitent.Commit) bool {
		visitedCommits = append(visitedCommits, commit)
		return true
	})
	assert.Error(t, err)
}

func TestGoGitRepositoryWalkHistoryWithEndBoundaryOutOfScope(t *testing.T) {
	// since the goGitRepository is not visible outside the package we need to retrieve it through the Git object
	script := gittools.TWO_BRANCH_SHORT_MERGED().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	dir := script.GetWorkingDirectory()
	repository, err := GitInstance().Open(dir)
	assert.NoError(t, err)
	// Keep track of the visited commits
	var visitedCommitsWithoutBoundaries []gitent.Commit
	var visitedCommitsWithBoundaries []gitent.Commit

	// find out the SHA-1 of the alpha branch HEAD, so it's out of scope
	script.Checkout("alpha")
	alphaHead := script.GetLastCommit().Hash.String()

	// and switch back to the master branch
	script.Checkout("master")

	// do a first walk with no boundaries
	err = repository.WalkHistory(nil, nil, func(commit gitent.Commit) bool {
		visitedCommitsWithoutBoundaries = append(visitedCommitsWithoutBoundaries, commit)
		return true
	})
	assert.NoError(t, err)

	// now do the same walk with boundaries
	// this boundary is out of the branch we're working in to the repository, so it should not affect the outcome
	err = repository.WalkHistory(nil, &alphaHead, func(commit gitent.Commit) bool {
		visitedCommitsWithBoundaries = append(visitedCommitsWithBoundaries, commit)
		return true
	})
	assert.NoError(t, err)

	assert.Equal(t, len(visitedCommitsWithoutBoundaries), len(visitedCommitsWithBoundaries))
}
