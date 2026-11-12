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
	"strings" // https://pkg.go.dev/strings
	"testing" // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"              // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert"   // https://pkg.go.dev/github.com/stretchr/testify/assert
	require "github.com/stretchr/testify/require" // https://pkg.go.dev/github.com/stretchr/testify/require

	github "github.com/mooltiverse/nyx/src/go/nyx/services/github"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

func TestGitHubRepositoryPoolRepositoryNameIsScopedByEnvironmentAndSlot(t *testing.T) {
	pool, err := NewGitHubRepositoryPool("token", "env")
	assert.NoError(t, err)
	name, err := pool.RepositoryName(0)
	assert.NoError(t, err)
	assert.Equal(t, "nyx-test-pool-go-env-0", name)
	name, err = pool.RepositoryName(3)
	assert.NoError(t, err)
	assert.Equal(t, "nyx-test-pool-go-env-3", name)

	other, err := NewGitHubRepositoryPool("token", "other")
	assert.NoError(t, err)
	otherName, err := other.RepositoryName(0)
	assert.NoError(t, err)
	name, _ = pool.RepositoryName(0)
	assert.NotEqual(t, name, otherName)
}

func TestGitHubRepositoryPoolRepositoryNameWithNegativeSlot(t *testing.T) {
	pool, err := NewGitHubRepositoryPool("token", "env")
	assert.NoError(t, err)
	_, err = pool.RepositoryName(-1)
	assert.Error(t, err)
}

func TestGitHubRepositoryPoolSanitize(t *testing.T) {
	assert.Equal(t, "local-john-doe", sanitizeHostedRepositoryPoolName("local-John Doe"))
	assert.Equal(t, "ci-windows", sanitizeHostedRepositoryPoolName("ci-Windows"))
	assert.Equal(t, "a-b", sanitizeHostedRepositoryPoolName("--a_.b--"))
	pool, err := NewGitHubRepositoryPool("token", "local-DOMAIN\\user")
	assert.NoError(t, err)
	assert.Equal(t, "local-domain-user", pool.GetEnvironment())
}

func TestHostedRepositoryPoolDefaultEnvironment(t *testing.T) {
	environment := HostedRepositoryPoolDefaultEnvironment()
	assert.NotEmpty(t, environment)
	assert.Equal(t, sanitizeHostedRepositoryPoolName(environment), environment)
	assert.True(t, strings.HasPrefix(environment, "ci-") || strings.HasPrefix(environment, "local-"))
}

func TestGitHubRepositoryPoolConstructorWithInvalidArguments(t *testing.T) {
	_, err := NewGitHubRepositoryPool("", "env")
	assert.Error(t, err)
	_, err = NewGitHubRepositoryPool(" ", "env")
	assert.Error(t, err)
	_, err = NewGitHubRepositoryPool("token", "")
	assert.Error(t, err)
	_, err = NewGitHubRepositoryPool("token", "__")
	assert.Error(t, err)
}

func TestHostedRepositoryPoolReferenceCommitIDIsDeterministic(t *testing.T) {
	assert.Equal(t, HostedRepositoryPoolReferenceCommitID(), HostedRepositoryPoolReferenceCommitID())
	assert.False(t, HostedRepositoryPoolReferenceCommitID().IsZero())
}

func TestGitHubRepositoryPoolAcquireReturnsCleanRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	defer log.SetLevel(logLevel) // restore the original logging level
	token := os.Getenv("gitHubTestUserToken")
	assert.NotEmpty(t, token, "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	pool, err := GitHubRepositoryPoolInstance(token)
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
	script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString(token), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	assert.Equal(t, []string{HostedRepositoryPoolReferenceCommitID().String()}, script.GetCommitIDs())
	assert.Equal(t, HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH, script.GetCurrentBranch())
	assert.Empty(t, script.GetTags())
}

func TestGitHubRepositoryPoolAcquireCleansDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	defer log.SetLevel(logLevel) // restore the original logging level
	token := os.Getenv("gitHubTestUserToken")
	assert.NotEmpty(t, token, "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	pool, err := GitHubRepositoryPoolInstance(token)
	assert.NoError(t, err)
	repository, err := pool.Acquire(0)
	require.NoError(t, err)

	// leave the repository dirty in all the ways a test can: new commits on the default branch,
	// other branches, tags and releases
	script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString(token), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	script.AndAddFiles().AndCommitWith(utl.PointerToString("Dirty commit on the default branch")).AndTag("9.9.9", nil)
	script.PushWithUserNameAndPassword(utl.PointerToString(token), utl.PointerToString(""))
	script.InBranch("dirty").AndAddFiles().AndCommitWith(utl.PointerToString("Dirty commit on another branch")).AndTag("9.9.10", utl.PointerToString("Annotated tag"))
	script.PushWithUserNameAndPassword(utl.PointerToString(token), utl.PointerToString(""))
	gitHub, err := github.Instance(map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: token})
	assert.NoError(t, err)
	user, err := gitHub.GetAuthenticatedUser()
	assert.NoError(t, err)
	_, err = gitHub.PublishRelease(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*repository).GetName()), utl.PointerToString("9.9.9"), "9.9.9", utl.PointerToString("A release left by a previous test"), nil)
	assert.NoError(t, err)
	problems, err := pool.Inspect(0)
	assert.NoError(t, err)
	assert.NotEmpty(t, problems)

	repository, err = pool.Acquire(0)
	require.NoError(t, err)

	problems, err = pool.Inspect(0)
	assert.NoError(t, err)
	assert.Empty(t, problems)
	release, err := gitHub.GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*repository).GetName()), "9.9.9")
	assert.NoError(t, err)
	assert.Nil(t, release)
	clone := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString(token), utl.PointerToString(""))
	defer os.RemoveAll(clone.GetWorkingDirectory())
	assert.Equal(t, []string{HostedRepositoryPoolReferenceCommitID().String()}, clone.GetCommitIDs())
	assert.Empty(t, clone.GetTags())
}

func TestGitHubRepositoryPoolPrivateRepositoryName(t *testing.T) {
	pool, err := NewGitHubRepositoryPool("token", "env")
	assert.NoError(t, err)
	name, err := pool.PrivateRepositoryName(0)
	assert.NoError(t, err)
	assert.Equal(t, "nyx-test-pool-go-env-private-0", name)
	_, err = pool.PrivateRepositoryName(-1)
	assert.Error(t, err)
}

func TestGitHubRepositoryPoolAcquireReturnsCleanPrivateRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	defer log.SetLevel(logLevel) // restore the original logging level
	token := os.Getenv("gitHubTestUserToken")
	assert.NotEmpty(t, token, "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
	pool, err := GitHubRepositoryPoolInstance(token)
	assert.NoError(t, err)

	repository, err := pool.AcquirePrivate(0)
	require.NoError(t, err)
	name, _ := pool.PrivateRepositoryName(0)
	assert.Equal(t, name, (*repository).GetName())
	problems, err := pool.InspectPrivate(0)
	assert.NoError(t, err)
	assert.Empty(t, problems)

	// the private repository can't be cloned without credentials
	assert.Panics(t, func() {
		script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), nil, nil)
		os.RemoveAll(script.GetWorkingDirectory())
	})
	script := CloneFromWithUserNameAndPassword((*repository).GetHTTPURL(), utl.PointerToString(token), utl.PointerToString(""))
	defer os.RemoveAll(script.GetWorkingDirectory())
	assert.Equal(t, []string{HostedRepositoryPoolReferenceCommitID().String()}, script.GetCommitIDs())
}

func TestHostedRepositoryPoolDefaultEnvironmentIsScopedByTestPackage(t *testing.T) {
	// tests in this package are run by the 'tools.test' binary
	assert.Equal(t, "tools", hostedRepositoryPoolTestPackage())
	assert.True(t, strings.HasSuffix(HostedRepositoryPoolDefaultEnvironment(), "-tools"))
}
