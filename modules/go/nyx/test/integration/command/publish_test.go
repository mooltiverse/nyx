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
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

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

func TestPublishConstructor(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.PUBLISH, gittools.FROM_SCRATCH()) {
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
func TestPublishState(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.PUBLISH, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, (*command).State())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/**
 * Check that the IsUpToDate() returns false when the command instance is just created and true after one execution in a repository
 * with at least one commit and in a clean state
 */
func TestPublishIsUpToDate(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// simply test that running it twice returns false at the first run and true the second
			upToDate, err := (*command).IsUpToDate()
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				assert.False(t, upToDate)
			} else {
				assert.True(t, upToDate)
			}

			// and running again with no changes must still be up to date
			_, err = (*command).Run()
			assert.NoError(t, err)
			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				assert.False(t, upToDate)
			} else {
				assert.True(t, upToDate)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestPublishIdempotency(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.MARK, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// simply test that running it twice returns false at the first run and true the second
			upToDate, err := (*command).IsUpToDate()
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				assert.False(t, upToDate)
			} else {
				assert.True(t, upToDate)
			}

			// and running again with no changes must still be up to date
			_, err = (*command).Run()
			assert.NoError(t, err)
			// when the command is executed standalone, Infer is not executed so IsUpToDate() will always return false
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			if (*command).GetContextName() == cmdtpl.STANDALONE_CONTEXT_NAME {
				assert.False(t, upToDate)
			} else {
				assert.True(t, upToDate)
			}

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestPublishRunWithNewReleaseAndGlobalAssetsOnGitHubRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	randomID := gitutil.RandomAlphabeticString(5, 201)
	// the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	assetPath1, _ := os.CreateTemp("", "nyx-test-github-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-github-release-test-*.bin")
	assetPath2.Write([]byte("content2"))
	assetAbsolutePath1, _ := filepath.Abs(assetPath1.Name())
	assetAbsolutePath2, _ := filepath.Abs(assetPath2.Name())
	defer os.Remove(assetAbsolutePath1)
	defer os.Remove(assetAbsolutePath2)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitHubRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithUserNameAndPassword(utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add release assets to the configuration and also use templates for asset options to make sure they are rendered
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{
		"asset1":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET1.TXT{{/lower}}"), utl.PointerToString("{{#trim}} Text asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath1), utl.PointerToString("{{#trim}}  text/plain   {{/trim}}")),
		"asset2":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET2.BIN{{/lower}}"), utl.PointerToString("{{#trim}} Binary asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath2), utl.PointerToString("{{#trim}}  application/octet-stream   {{/trim}}")),
		"nonexistentfile": ent.NewAttachmentWith(utl.PointerToString("nonexistentfile"), utl.PointerToString("Non existent asset"), utl.PointerToString("nonexistentfile"), utl.PointerToString("application/octet-stream")),       // this file does not exist and should only generate a warning
		"remote1":         ent.NewAttachmentWith(utl.PointerToString("remote1"), utl.PointerToString("Remote link asset"), utl.PointerToString("http://www.example.com/remote1"), utl.PointerToString("application/octet-stream")), // this is an URL and should be skipped by GitHub
	})

	// add a mock convention that accepts all non nil messages and dumps the major identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"major": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add the test publishing service
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB),
			&map[string]string{
				github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken"),
				github.REPOSITORY_NAME_OPTION_NAME:      (*gitHubRepository).GetName(),
				github.REPOSITORY_OWNER_OPTION_NAME:     (*user).GetUserName(),
			}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""), nil, nil),
	})
	// add a custom release type that always enables committing, tagging and pushing
	// and all the publishing service enabled
	// this release type does not filter release assets so all global ones must be published
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{utl.PointerToString("github")}, &[]*string{},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	gitHubRelease, err := gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)
	assert.Nil(t, gitHubRelease)

	state, err := nyx.Publish()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// read the release from the hosting service
	gitHubRelease, err = gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitHubRelease)
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTitle())
	// the release assets must contain the two existing files but not the remote URL (not supported by GitHub), nor the non existing file
	assert.Equal(t, 2, len((*gitHubRelease).GetAssets()))
	for _, asset := range (*gitHubRelease).GetAssets() {
		assert.True(t, *asset.GetFileName() == "asset1.txt" || *asset.GetFileName() == "asset2.bin")
		// The underlying library does not support uploading the description (as the Label field) so it's not returned here
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset")
		// It looks like the underlying library always returns "text/plain; charset=utf-8" here
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream")
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // The path may start with https://api.github.com/repos/ or https://github.com/
	}

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Publish()
	assert.NoError(t, err)
	gitHubRelease, err = gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ = state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitHubRelease)
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTitle())
	// the release assets must contain the two existing files but not the remote URL (not supported by GitHub), nor the non existing file
	assert.Equal(t, 2, len((*gitHubRelease).GetAssets()))
	for _, asset := range (*gitHubRelease).GetAssets() {
		assert.True(t, *asset.GetFileName() == "asset1.txt" || *asset.GetFileName() == "asset2.bin")
		// The underlying library does not support uploading the description (as the Label field) so it's not returned here
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset")
		// It looks like the underlying library always returns "text/plain; charset=utf-8" here
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream")
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // The path may start with https://api.github.com/repos/ or https://github.com/
	}

	// now delete it
	gitHub.DeleteGitRepository(randomID)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestPublishRunWithNewReleaseAndFilteredAssetsOnGitHubRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	randomID := gitutil.RandomAlphabeticString(5, 202)
	// the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
	assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitHubRepository, err := gitHub.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	assetPath1, _ := os.CreateTemp("", "nyx-test-github-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-github-release-test-*.bin")
	assetPath2.Write([]byte("content2"))
	assetAbsolutePath1, _ := filepath.Abs(assetPath1.Name())
	assetAbsolutePath2, _ := filepath.Abs(assetPath2.Name())
	defer os.Remove(assetAbsolutePath1)
	defer os.Remove(assetAbsolutePath2)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitHubRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithUserNameAndPassword(utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add release assets to the configuration and also use templates for asset options to make sure they are rendered
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{
		"asset1":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET1.TXT{{/lower}}"), utl.PointerToString("{{#trim}} Text asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath1), utl.PointerToString("{{#trim}}  text/plain   {{/trim}}")),
		"asset2":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET2.BIN{{/lower}}"), utl.PointerToString("{{#trim}} Binary asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath2), utl.PointerToString("{{#trim}}  application/octet-stream   {{/trim}}")),
		"nonexistentfile": ent.NewAttachmentWith(utl.PointerToString("nonexistentfile"), utl.PointerToString("Non existent asset"), utl.PointerToString("nonexistentfile"), utl.PointerToString("application/octet-stream")),       // this file does not exist and should only generate a warning
		"remote1":         ent.NewAttachmentWith(utl.PointerToString("remote1"), utl.PointerToString("Remote link asset"), utl.PointerToString("http://www.example.com/remote1"), utl.PointerToString("application/octet-stream")), // this is an URL and should be skipped by GitHub
	})

	// add a mock convention that accepts all non nil messages and dumps the major identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"major": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add the test publishing service
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB),
			&map[string]string{
				github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken"),
				github.REPOSITORY_NAME_OPTION_NAME:      (*gitHubRepository).GetName(),
				github.REPOSITORY_OWNER_OPTION_NAME:     (*user).GetUserName(),
			}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""), nil, nil),
	})
	// add a custom release type that always enables committing, tagging and pushing
	// and all the publishing service enabled
	// this release type only allows the 'asset1' to be published as release asset
	releaseType := ent.NewReleaseType()
	releaseType.SetAssets(&[]*string{utl.PointerToString("asset1")})
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{utl.PointerToString("github")}, &[]*string{},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	gitHubRelease, err := gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)
	assert.Nil(t, gitHubRelease)

	state, err := nyx.Publish()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// read the release from the hosting service
	gitHubRelease, err = gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitHubRelease)
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTitle())
	// the release assets must contain only the 'asset1' as it was filtered by the release type
	assert.Equal(t, 1, len((*gitHubRelease).GetAssets()))
	for _, asset := range (*gitHubRelease).GetAssets() {
		assert.Equal(t, "asset1.txt", *asset.GetFileName())
		// The underlying library does not support uploading the description (as the Label field) so it's not returned here
		//assert.Equal(t, "Text asset", *asset.GetDescription())
		// It looks like the underlying library always returns "text/plain; charset=utf-8" here
		//assert.Equal(t, "text/plain", *asset.GetType())
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // The path may start with https://api.github.com/repos/ or https://github.com/
	}

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Publish()
	assert.NoError(t, err)
	gitHubRelease, err = gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHubRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ = state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitHubRelease)
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitHubRelease).GetTitle())
	// the release assets must contain only the 'asset1' as it was filtered by the release type
	assert.Equal(t, 1, len((*gitHubRelease).GetAssets()))
	for _, asset := range (*gitHubRelease).GetAssets() {
		assert.Equal(t, "asset1.txt", *asset.GetFileName())
		// The underlying library does not support uploading the description (as the Label field) so it's not returned here
		//assert.Equal(t, "Text asset", *asset.GetDescription())
		// It looks like the underlying library always returns "text/plain; charset=utf-8" here
		//assert.Equal(t, "text/plain", *asset.GetType())
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // The path may start with https://api.github.com/repos/ or https://github.com/
	}

	// now delete it
	gitHub.DeleteGitRepository(randomID)

	log.SetLevel(logLevel) // restore the original logging level
}

func TestPublishRunWithNewReleaseAndGlobalAssetsOnGitLabRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	randomID := gitutil.RandomAlphabeticString(5, 203)
	// the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	assetPath1, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.bin")
	assetPath2.Write([]byte("content2"))
	assetAbsolutePath1, _ := filepath.Abs(assetPath1.Name())
	assetAbsolutePath2, _ := filepath.Abs(assetPath2.Name())
	defer os.Remove(assetAbsolutePath1)
	defer os.Remove(assetAbsolutePath2)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	// when a token for user and password authentication for plain Git operations against a GitLab repository,
	// the user is the "PRIVATE-TOKEN" string and the password is the token
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitLabRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithUserNameAndPassword(utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add release assets to the configuration and also use templates for asset options to make sure they are rendered
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{
		"asset1":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET1.TXT{{/lower}}"), utl.PointerToString("{{#trim}} Text asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath1), utl.PointerToString("{{#trim}}  text/plain   {{/trim}}")),
		"asset2":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET2.BIN{{/lower}}"), utl.PointerToString("{{#trim}} Binary asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath2), utl.PointerToString("{{#trim}}  application/octet-stream   {{/trim}}")),
		"nonexistentfile": ent.NewAttachmentWith(utl.PointerToString("nonexistentfile"), utl.PointerToString("Non existent asset"), utl.PointerToString("nonexistentfile"), utl.PointerToString("application/octet-stream")),       // this file does not exist and should only generate a warning
		"remote1":         ent.NewAttachmentWith(utl.PointerToString("remote1"), utl.PointerToString("Remote link asset"), utl.PointerToString("http://www.example.com/remote1"), utl.PointerToString("application/octet-stream")), // this is an URL and should be skipped by GitHub
	})

	// add a mock convention that accepts all non nil messages and dumps the major identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"major": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add the test publishing service
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB),
			&map[string]string{
				gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken"),
				gitlab.REPOSITORY_NAME_OPTION_NAME:      (*gitLabRepository).GetName(),
				gitlab.REPOSITORY_OWNER_OPTION_NAME:     (*user).GetUserName(),
			}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")), nil, nil),
	})
	// add a custom release type that always enables committing, tagging and pushing
	// and all the publishing service enabled
	// this release type does not filter release assets so all global ones must be published
	releaseType := ent.NewReleaseType()
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{utl.PointerToString("gitlab")}, &[]*string{},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	gitLabRelease, err := gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)
	assert.Nil(t, gitLabRelease)

	state, err := nyx.Publish()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// read the release from the hosting service
	gitLabRelease, err = gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitLabRelease)
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTitle())
	// the release assets must contain the two existing files and the remote URL, but not the non existing file
	assert.Equal(t, 3, len((*gitLabRelease).GetAssets()))
	for _, asset := range (*gitLabRelease).GetAssets() {
		assert.True(t, *asset.GetFileName() == "Text asset" || *asset.GetFileName() == "Binary asset" || *asset.GetFileName() == "Remote link asset")
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset" || *asset.GetDescription() == "Remote link asset") // the description is not available via this API
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream") // the content type is not available via this API
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Publish()
	assert.NoError(t, err)
	gitLabRelease, err = gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ = state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitLabRelease)
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTitle())
	// the release assets must contain the two existing files and the remote URL, but not the non existing file
	assert.Equal(t, 3, len((*gitLabRelease).GetAssets()))
	for _, asset := range (*gitLabRelease).GetAssets() {
		assert.True(t, *asset.GetFileName() == "Text asset" || *asset.GetFileName() == "Binary asset" || *asset.GetFileName() == "Remote link asset")
		//assert.True(t, *asset.GetDescription() == "Text asset" || *asset.GetDescription() == "Binary asset" || *asset.GetDescription() == "Remote link asset") // the description is not available via this API
		//assert.True(t, *asset.GetType() == "text/plain" || *asset.GetType() == "application/octet-stream") // the content type is not available via this API
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// now delete it
	gitLab.DeleteGitRepository((*gitLabRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}

func TestPublishRunWithNewReleaseAndFilteredAssetsOnGitLabRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	randomID := gitutil.RandomAlphabeticString(5, 204)
	// the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
	assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	gitLabRepository, err := gitLab.CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)
	assert.NoError(t, err)

	assetPath1, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.txt")
	assetPath1.Write([]byte("content1"))
	assetPath2, _ := os.CreateTemp("", "nyx-test-gitlab-release-test-*.bin")
	assetPath2.Write([]byte("content2"))
	assetAbsolutePath1, _ := filepath.Abs(assetPath1.Name())
	assetAbsolutePath2, _ := filepath.Abs(assetPath2.Name())
	defer os.Remove(assetAbsolutePath1)
	defer os.Remove(assetAbsolutePath2)

	// if we clone too quickly next calls may fail
	time.Sleep(4000 * time.Millisecond)

	// when a token for user and password authentication for plain Git operations against a GitLab repository,
	// the user is the "PRIVATE-TOKEN" string and the password is the token
	script := gittools.ONE_BRANCH_SHORT().ApplyOnCloneFromWithUserNameAndPassword((*gitLabRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.PushWithUserNameAndPassword(utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))

	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	// add release assets to the configuration and also use templates for asset options to make sure they are rendered
	configurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{
		"asset1":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET1.TXT{{/lower}}"), utl.PointerToString("{{#trim}} Text asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath1), utl.PointerToString("{{#trim}}  text/plain   {{/trim}}")),
		"asset2":          ent.NewAttachmentWith(utl.PointerToString("{{#lower}}ASSET2.BIN{{/lower}}"), utl.PointerToString("{{#trim}} Binary asset   {{/trim}}"), utl.PointerToString(assetAbsolutePath2), utl.PointerToString("{{#trim}}  application/octet-stream   {{/trim}}")),
		"nonexistentfile": ent.NewAttachmentWith(utl.PointerToString("nonexistentfile"), utl.PointerToString("Non existent asset"), utl.PointerToString("nonexistentfile"), utl.PointerToString("application/octet-stream")),       // this file does not exist and should only generate a warning
		"remote1":         ent.NewAttachmentWith(utl.PointerToString("remote1"), utl.PointerToString("Remote link asset"), utl.PointerToString("http://www.example.com/remote1"), utl.PointerToString("application/octet-stream")), // this is an URL and should be skipped by GitHub
	})

	// add a mock convention that accepts all non nil messages and dumps the major identifier for each
	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
		&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
			&map[string]string{"major": ".*"})})
	configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
	// add the test publishing service
	configurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{
		"gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB),
			&map[string]string{
				gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken"),
				gitlab.REPOSITORY_NAME_OPTION_NAME:      (*gitLabRepository).GetName(),
				gitlab.REPOSITORY_OWNER_OPTION_NAME:     (*user).GetUserName(),
			}),
	})
	// set up the Git remote credentials
	gitConfiguration, _ := configurationLayerMock.GetGit()
	gitConfiguration.SetRemotes(&map[string]*ent.GitRemoteConfiguration{
		"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.USER_PASSWORD), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")), nil, nil),
	})
	// add a custom release type that always enables committing, tagging and pushing
	// and all the publishing service enabled
	// this release type only allows the 'asset1' to be published as release asset
	releaseType := ent.NewReleaseType()
	releaseType.SetAssets(&[]*string{utl.PointerToString("asset1")})
	releaseType.SetGitCommit(utl.PointerToString("true"))
	releaseType.SetGitPush(utl.PointerToString("true"))
	releaseType.SetGitTag(utl.PointerToString("true"))
	releaseType.SetPublish(utl.PointerToString("true"))
	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
		&[]*string{utl.PointerToString("gitlab")}, &[]*string{},
		&map[string]*ent.ReleaseType{"testReleaseType": releaseType})
	configurationLayerMock.SetReleaseTypes(releaseTypes)

	nyx := nyx.NewNyxIn(script.GetWorkingDirectory())
	nyxConfiguration, _ := nyx.Configuration()
	var configurationLayer cnf.ConfigurationLayer
	configurationLayer = configurationLayerMock
	nyxConfiguration.WithRuntimeConfiguration(&configurationLayer)

	gitLabRelease, err := gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)
	assert.Nil(t, gitLabRelease)

	state, err := nyx.Publish()
	assert.NoError(t, err)

	// if we read too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// read the release from the hosting service
	gitLabRelease, err = gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ := state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitLabRelease)
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTitle())
	// the release assets must contain only the 'asset1' as it was filtered by the release type
	assert.Equal(t, 1, len((*gitLabRelease).GetAssets()))
	for _, asset := range (*gitLabRelease).GetAssets() {
		assert.Equal(t, "Text asset", *asset.GetFileName())
		//assert.Equal(t, "Text asset", *asset.GetDescription()) // the description is not available via this API
		//assert.Equal(t, "text/plain", *asset.GetType()) // the content type is not available via this API
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// if we delete too quickly we often get a 404 from the server so let's wait a short while
	time.Sleep(2000 * time.Millisecond)

	// test for idempotency running the command again
	state, err = nyx.Publish()
	assert.NoError(t, err)
	gitLabRelease, err = gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitLabRepository).GetName()), "1.0.0")
	assert.NoError(t, err)

	version, _ = state.GetVersion()
	assert.Equal(t, "1.0.0", *version)
	assert.NotNil(t, gitLabRelease)
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTag())
	assert.Equal(t, "1.0.0", (*gitLabRelease).GetTitle())
	// the release assets must contain only the 'asset1' as it was filtered by the release type
	assert.Equal(t, 1, len((*gitLabRelease).GetAssets()))
	for _, asset := range (*gitLabRelease).GetAssets() {
		assert.Equal(t, "Text asset", *asset.GetFileName())
		//assert.Equal(t, "Text asset", *asset.GetDescription()) // the description is not available via this API
		//assert.Equal(t, "text/plain", *asset.GetType()) // the content type is not available via this API
		//assert.True(t, strings.HasPrefix(*asset.GetPath(), "https://api.github.com/repos/")) // as of now these URLS are like https://storage.googleapis.com...
	}

	// now delete it
	gitLab.DeleteGitRepository((*gitLabRepository).GetID())

	log.SetLevel(logLevel) // restore the original logging level
}
