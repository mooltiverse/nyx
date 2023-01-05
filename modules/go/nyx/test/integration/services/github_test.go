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

package services_test

import (
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strings"       // https://pkg.go.dev/strings
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	svcapi "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestGitHubInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	_, err := github.Instance(map[string]string{})
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubInstanceErrorWithNilOptions(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	_, err := github.Instance(nil)
	assert.Error(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubSupports(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, f := range serviceFeatures {
		t.Run(f.String(), func(t *testing.T) {
			gitHub, err := github.Instance(map[string]string{})
			assert.NoError(t, err)
			if f == svcapi.GIT_HOSTING || f == svcapi.RELEASES || f == svcapi.RELEASE_ASSETS || f == svcapi.USERS {
				assert.True(t, gitHub.Supports(f))
			} else {
				assert.False(t, gitHub.Supports(f))
			}
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubUserServiceGetAuthenticatedUserErrorUsingGetAuthenticatedUserWithNoAuthenticationToken(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	gitHub, err := github.Instance(map[string]string{})
	assert.NoError(t, err)
	_, err = gitHub.GetAuthenticatedUser()
	assert.Error(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubUserServiceGetAuthenticatedUser(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	assert.NotEmpty(t, (*user).GetID())
	assert.NotEmpty(t, (*user).GetUserName())
	assert.NotEmpty(t, (*user).GetFullName())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubHostingServiceCreateGitRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 71)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, false)

	assert.Equal(t, "main", (*gitHubRepository).GetDefaultBranch())
	assert.Equal(t, "Test repository "+randomID, (*gitHubRepository).GetDescription())
	assert.Equal(t, randomID, (*gitHubRepository).GetName())
	assert.Equal(t, (*user).GetUserName()+"/"+randomID, (*gitHubRepository).GetFullName())
	assert.Equal(t, "https://github.com/"+(*user).GetUserName()+"/"+randomID+".git", (*gitHubRepository).GetHTTPURL())
	assert.Equal(t, "git@github.com:"+(*user).GetUserName()+"/"+randomID+".git", (*gitHubRepository).GetSSHURL())

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitHubReleaseServiceCreateRelease(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 77)

	// the 'gitHubTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)

	ownerName := (*user).GetUserName()
	repositoryName := (*gitHubRepository).GetName()
	release, err := gitHub.GetReleaseByTag(&ownerName, &repositoryName, "1.0.0-alpha.1")
	assert.NoError(t, err)
	assert.Nil(t, release)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	// when a token for user and password authentication for plain Git operations against a GitHub repository,
	// the user is the token and the password is the empty string
	script := gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED().ApplyOnCloneFromWithCredentials((*gitHubRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithCredentials(utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))

	// publish the release
	release, err = gitHub.PublishRelease(&ownerName, &repositoryName, utl.PointerToString("Release 1.0.0-alpha.1"), "1.0.0-alpha.1", utl.PointerToString("A test description for the release\non multiple lines\nlike these"))
	assert.NoError(t, err)
	assert.Equal(t, "Release 1.0.0-alpha.1", (*release).GetTitle())
	assert.Equal(t, "1.0.0-alpha.1", (*release).GetTag())

	// so far the release has no assets attached
	assert.True(t, (*release).GetAssets() == nil || len((*release).GetAssets()) == 0)

	// also upload some assets
	// if we uploade too quickly before the release is published next calls may fail
	time.Sleep(2000 * time.Millisecond)

	assetPath1, _ := os.CreateTemp("", "nyx-test-github-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-github-release-test-*.txt")
	assetPath2.Write([]byte("content2"))
	var assetsToUpload []ent.Attachment
	assetAbsolutePath1, _ := filepath.Abs(assetPath1.Name())
	assetsToUpload = append(assetsToUpload, *ent.NewAttachmentWith(utl.PointerToString("asset1"), utl.PointerToString("Text asset"), utl.PointerToString(assetAbsolutePath1), utl.PointerToString("text/plain")))
	assetAbsolutePath2, _ := filepath.Abs(assetPath2.Name())
	defer os.Remove(assetAbsolutePath1)
	defer os.Remove(assetAbsolutePath2)
	assetsToUpload = append(assetsToUpload, *ent.NewAttachmentWith(utl.PointerToString("asset2"), utl.PointerToString("Binary asset"), utl.PointerToString(assetAbsolutePath2), utl.PointerToString("application/octet-stream")))
	assetsToUpload = append(assetsToUpload, *ent.NewAttachmentWith(utl.PointerToString("nonexistentfile"), utl.PointerToString("Non existent asset"), utl.PointerToString("nonexistentfile"), utl.PointerToString("application/octet-stream")))       // this file does not exist and should only generate a warning
	assetsToUpload = append(assetsToUpload, *ent.NewAttachmentWith(utl.PointerToString("remote1"), utl.PointerToString("Remote link asset"), utl.PointerToString("http://www.example.com/remote1"), utl.PointerToString("application/octet-stream"))) // this is an URL and should be ignored
	releaseWithAssets, err := gitHub.PublishReleaseAssets(&ownerName, &repositoryName, release, assetsToUpload)
	assert.NoError(t, err)
	assert.Equal(t, 2, len((*releaseWithAssets).GetAssets()))
	for _, asset := range (*releaseWithAssets).GetAssets() {
		assert.True(t, *asset.GetFileName() == "asset1" || *asset.GetFileName() == "asset2")
		assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset")
		assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream")
		assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/"))
	}

	// now test again the getReleaseByTag and also make sure it has the assets
	releaseWithAssets, err = gitHub.GetReleaseByTag(&ownerName, &repositoryName, "1.0.0-alpha.1")
	assert.NoError(t, err)
	assert.Equal(t, "Release 1.0.0-alpha.1", (*releaseWithAssets).GetTitle())
	assert.Equal(t, "1.0.0-alpha.1", (*releaseWithAssets).GetTag())
	assert.Equal(t, 2, len((*releaseWithAssets).GetAssets()))
	for _, asset := range (*releaseWithAssets).GetAssets() {
		assert.True(t, *asset.GetFileName() == "asset1" || *asset.GetFileName() == "asset2")
		// The underlying library does not support uploading the description (as the Label field) so it's not returned here
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset")
		// It looks like the underlying library always returns "text/plain; charset=utf-8" here
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream")
		assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/"))
	}

	// now delete it
	err = gitHub.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}
