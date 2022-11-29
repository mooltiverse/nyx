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
	"strings"       // https://pkg.go.dev/strings
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	. "github.com/mooltiverse/nyx/modules/go/nyx/git"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

const (
	// Use this own project repo as the repository to clone for tests
	REMOTE_TEST_REPOSITORY = "https://github.com/mooltiverse/nyx.git"
)

func TestGitInstance(t *testing.T) {
	g := GitInstance()
	assert.NotNil(t, g)
}

func TestGitCloneErrorWithNilDirectory(t *testing.T) {
	tr := REMOTE_TEST_REPOSITORY
	_, err := GitInstance().Clone(nil, &tr)
	assert.Error(t, err)
}

func TestGitCloneErrorWithEmptyDirectory(t *testing.T) {
	tr := REMOTE_TEST_REPOSITORY
	es := ""
	_, err := GitInstance().Clone(&es, &tr)
	assert.Error(t, err)
	es = "  "
	_, err = GitInstance().Clone(&es, &tr)
	assert.Error(t, err)
}

func TestGitCloneErrorWithNonEmptyDirectory(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	tr := REMOTE_TEST_REPOSITORY
	dir := script.GetWorkingDirectory()
	_, err := GitInstance().Clone(&dir, &tr)
	assert.Error(t, err)
}

func TestGitCloneErrorWithNilURI(t *testing.T) {
	dir := "nyx-test-git-clone-test-"
	_, err := GitInstance().Clone(&dir, nil)
	assert.Error(t, err)
}

func TestGitCloneErrorWithEmptyURI(t *testing.T) {
	dir := "nyx-test-git-clone-test-"
	uri := ""
	_, err := GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
	uri = "  "
	_, err = GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
}

func TestGitCloneErrorWithNonExistingURI(t *testing.T) {
	dir := "nyx-test-git-clone-test-"
	uri := "https://adomainwiththisnamesuredoesnotexists.com/"
	_, err := GitInstance().Clone(&dir, &uri)
	assert.Error(t, err)
}

func TestGitClone(t *testing.T) {
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

func TestGitCloneWithNonRequiredCredentials(t *testing.T) {
	tr := REMOTE_TEST_REPOSITORY
	dir := "nyx-test-git-clone-test-"
	directory := gitutil.NewTempDirectory("", &dir)
	defer os.RemoveAll(directory)

	_, err := os.Stat(filepath.Join(directory, "README.md"))
	assert.Error(t, err)

	// the 'gitHubTestUserToken' and 'gitHubTestUserToken' environment variables are set by the build script
	user := os.Getenv("gitHubTestUserToken")
	password := os.Getenv("gitHubTestUserToken")
	user = strings.Replace(user, "\"", "", -1)
	password = strings.Replace(password, "\"", "", -1)
	_, err = GitInstance().CloneWithCredentials(&directory, &tr, &user, &password)
	assert.NoError(t, err)

	_, err = os.Stat(filepath.Join(directory, "README.md"))
	assert.NoError(t, err)
}

func TestGitCloneWithoutRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 91)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), nil, nil)
	assert.Error(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitCloneWithRequiredCredentials(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 93)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	// create a brand new test repository for this purpose
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, true)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	directory, err := os.MkdirTemp("", "nyx-test-git-clone-test-")
	_, err = GitInstance().CloneWithCredentials(&directory, utl.PointerToString((*gitHubRepository).GetHTTPURL()), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(os.Getenv("gitHubTestUserToken")))
	assert.NoError(t, err)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitOpenErrorWithEmptyDirectory(t *testing.T) {
	_, err := GitInstance().Open("")
	assert.Error(t, err)
	_, err = GitInstance().Open("  ")
	assert.Error(t, err)
}

func TestGitOpenErrorWithNonExistingDirectory(t *testing.T) {
	_, err := GitInstance().Open("adirectorywiththisnamesuredoesnotexists")
	assert.Error(t, err)
}

func TestGitOpenErrorWithNewEmptyDirectory(t *testing.T) {
	directory := gitutil.NewTempDirectory("", nil)
	defer os.RemoveAll(directory)

	_, err := GitInstance().Open(directory)
	assert.Error(t, err)
}

func TestGitOpen(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	dir := script.GetWorkingDirectory()
	_, err := GitInstance().Open(dir)
	assert.NoError(t, err)
}
