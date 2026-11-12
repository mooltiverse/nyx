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

package tools

import (
	"os"      // https://pkg.go.dev/os
	"testing" // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"              // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert"   // https://pkg.go.dev/github.com/stretchr/testify/assert
	require "github.com/stretchr/testify/require" // https://pkg.go.dev/github.com/stretchr/testify/require

	gitlab "github.com/mooltiverse/nyx/src/go/nyx/services/gitlab"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

func TestGitLabRepositoryPoolRepositoryNameIsScopedByEnvironmentVisibilityAndSlot(t *testing.T) {
	pool, err := NewGitLabRepositoryPool("token", "env")
	assert.NoError(t, err)
	name, err := pool.RepositoryName(0)
	assert.NoError(t, err)
	assert.Equal(t, "nyx-test-pool-go-env-0", name)
	name, err = pool.PrivateRepositoryName(1)
	assert.NoError(t, err)
	assert.Equal(t, "nyx-test-pool-go-env-private-1", name)
}

func TestGitLabRepositoryPoolConstructorWithInvalidArguments(t *testing.T) {
	_, err := NewGitLabRepositoryPool("", "env")
	assert.Error(t, err)
	_, err = NewGitLabRepositoryPool(" ", "env")
	assert.Error(t, err)
	_, err = NewGitLabRepositoryPool("token", "__")
	assert.Error(t, err)
}

func TestGitLabRepositoryPoolAcquireReturnsCleanRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	defer log.SetLevel(logLevel) // restore the original logging level
	token := os.Getenv("gitLabTestUserToken")
	assert.NotEmpty(t, token, "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	pool, err := GitLabRepositoryPoolInstance(token)
	assert.NoError(t, err)

	repository, err := pool.Acquire(0)
	require.NoError(t, err)
	assert.NotNil(t, repository)
	name, _ := pool.RepositoryName(0)
	assert.Equal(t, name, (*repository).GetName())
	assert.Equal(t, HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH, (*repository).GetDefaultBranch())
	problems, err := pool.Inspect(0)
	assert.NoError(t, err)
	assert.Empty(t, problems)

	// a clone has the reference commit only
	script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(token))
	defer os.RemoveAll(script.GetWorkingDirectory())
	assert.Equal(t, []string{HostedRepositoryPoolReferenceCommitID().String()}, script.GetCommitIDs())
	assert.Equal(t, HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH, script.GetCurrentBranch())
	assert.Empty(t, script.GetTags())
}

func TestGitLabRepositoryPoolAcquireCleansDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	defer log.SetLevel(logLevel) // restore the original logging level
	token := os.Getenv("gitLabTestUserToken")
	assert.NotEmpty(t, token, "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
	pool, err := GitLabRepositoryPoolInstance(token)
	assert.NoError(t, err)
	repository, err := pool.Acquire(0)
	require.NoError(t, err)

	// leave the repository dirty in all the ways a test can: new commits on the default branch,
	// other branches, tags and releases
	script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(token))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AndAddFiles().AndCommitWith(utl.PointerToString("Dirty commit on the default branch")).AndTag("9.9.9", nil)
	script.PushWithUserNameAndPassword(utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(token))
	script.InBranch("dirty").AndAddFiles().AndCommitWith(utl.PointerToString("Dirty commit on another branch")).AndTag("9.9.10", utl.PointerToString("Annotated tag"))
	script.PushWithUserNameAndPassword(utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(token))
	gitLab, err := gitlab.Instance(map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: token})
	assert.NoError(t, err)
	user, err := gitLab.GetAuthenticatedUser()
	assert.NoError(t, err)
	_, err = gitLab.PublishRelease(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*repository).GetName()), utl.PointerToString("9.9.9"), "9.9.9", utl.PointerToString("A release left by a previous test"), nil)
	assert.NoError(t, err)
	problems, err := pool.Inspect(0)
	assert.NoError(t, err)
	assert.NotEmpty(t, problems)

	repository, err = pool.Acquire(0)
	require.NoError(t, err)

	problems, err = pool.Inspect(0)
	assert.NoError(t, err)
	assert.Empty(t, problems)
	release, err := gitLab.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*repository).GetName()), "9.9.9")
	assert.NoError(t, err)
	assert.Nil(t, release)
	clone := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(token))
	defer os.RemoveAll(clone.GetWorkingDirectory())
	assert.Equal(t, []string{HostedRepositoryPoolReferenceCommitID().String()}, clone.GetCommitIDs())
	assert.Empty(t, clone.GetTags())
}
