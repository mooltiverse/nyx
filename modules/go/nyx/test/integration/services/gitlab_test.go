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
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	svcapi "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
	gitlab "github.com/mooltiverse/nyx/modules/go/nyx/services/gitlab"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestGitLabInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	_, err := gitlab.Instance(map[string]string{})
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabInstanceErrorWithNilOptions(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	_, err := gitlab.Instance(nil)
	assert.Error(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabSupports(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, f := range serviceFeatures {
		t.Run(f.String(), func(t *testing.T) {
			gitLab, err := gitlab.Instance(map[string]string{})
			assert.NoError(t, err)
			if f == svcapi.GIT_HOSTING || f == svcapi.RELEASES || f == svcapi.RELEASE_ASSETS || f == svcapi.USERS {
				assert.True(t, gitLab.Supports(f))
			} else {
				assert.False(t, gitLab.Supports(f))
			}
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabUserServiceGetAuthenticatedUserErrorUsingGetAuthenticatedUserWithNoAuthenticationToken(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	gitLab, err := gitlab.Instance(map[string]string{})
	assert.NoError(t, err)
	_, err = gitLab.GetAuthenticatedUser()
	assert.Error(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabUserServiceGetAuthenticatedUser(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	assert.NotEmpty(t, (*user).GetID())
	assert.NotEmpty(t, (*user).GetUserName())
	assert.NotEmpty(t, (*user).GetFullName())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabHostingServiceCreateGitRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 71)

	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), true, false)

	assert.Equal(t, "main", (*gitLabRepository).GetDefaultBranch())
	assert.Equal(t, "Test repository "+randomID, (*gitLabRepository).GetDescription())
	assert.Equal(t, randomID, (*gitLabRepository).GetName())
	assert.Equal(t, randomID, (*gitLabRepository).GetFullName())
	assert.Equal(t, "https://gitlab.com/"+(*user).GetUserName()+"/"+randomID+".git", (*gitLabRepository).GetHTTPURL())
	assert.Equal(t, "git@gitlab.com:"+(*user).GetUserName()+"/"+randomID+".git", (*gitLabRepository).GetSSHURL())

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(4000 * time.Millisecond)

	// now delete it
	err = gitLab.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestGitLabReleaseServiceCreateRelease(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	randomID := gitutil.RandomAlphabeticString(5, 81)

	// the 'gitLabTestUserToken' environment variable is set by the build script
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)

	ownerName := (*user).GetUserName()
	repositoryName := (*gitLabRepository).GetName()
	release, err := gitLab.GetReleaseByTag(&ownerName, &repositoryName, "1.0.0-alpha.1")
	assert.NoError(t, err)
	assert.Nil(t, release)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	// when a token for user and password authentication for plain Git operations against a GitLab repository,
	// the user is the "PRIVATE-TOKEN" string and the password is the token
	script := gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED().ApplyOnCloneFromWithUserNameAndPassword((*gitLabRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithUserNameAndPassword(utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))

	// publish the release
	release, err = gitLab.PublishRelease(&ownerName, &repositoryName, utl.PointerToString("Release 1.0.0-alpha.1"), "1.0.0-alpha.1", utl.PointerToString("A test description for the release\non multiple lines\nlike these"), nil)
	assert.NoError(t, err)
	assert.Equal(t, "Release 1.0.0-alpha.1", (*release).GetTitle())
	assert.Equal(t, "1.0.0-alpha.1", (*release).GetTag())

	// so far the release has no assets attached
	assert.True(t, (*release).GetAssets() == nil || len((*release).GetAssets()) == 0)

	// also upload some assets
	// if we uploade too quickly before the release is published next calls may fail
	time.Sleep(2000 * time.Millisecond)

	assetPath1, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.txt")
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
	releaseWithAssets, err := gitLab.PublishReleaseAssets(&ownerName, &repositoryName, release, assetsToUpload)
	assert.NoError(t, err)
	assert.Equal(t, 3, len((*releaseWithAssets).GetAssets()))
	for _, asset := range (*releaseWithAssets).GetAssets() {
		assert.True(t, *asset.GetFileName() == "asset1" || *asset.GetFileName() == "asset2" || *asset.GetFileName() == "remote1")
		assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset" || *asset.GetDescription() == "Remote link asset")
		assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream")
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// now test again the getReleaseByTag and also make sure it has the assets
	releaseWithAssets, err = gitLab.GetReleaseByTag(&ownerName, &repositoryName, "1.0.0-alpha.1")
	assert.NoError(t, err)
	assert.Equal(t, "Release 1.0.0-alpha.1", (*releaseWithAssets).GetTitle())
	assert.Equal(t, "1.0.0-alpha.1", (*releaseWithAssets).GetTag())
	assert.Equal(t, 3, len((*releaseWithAssets).GetAssets()))
	for _, asset := range (*releaseWithAssets).GetAssets() {
		assert.True(t, *asset.GetFileName() == "Text asset" || *asset.GetFileName() == "Binary asset" || *asset.GetFileName() == "Remote link asset") // the description is not available via this API
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset" || *asset.GetDescription() == "Remote link asset") // the description is not available via this API
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream") // the content type is not available via this API
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// now delete it
	err = gitLab.DeleteGitRepository(randomID)
	assert.NoError(t, err)

	log.SetLevel(logLevel) // restore the original logging level
}
