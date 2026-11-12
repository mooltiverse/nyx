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
	"errors"        // https://pkg.go.dev/errors
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"os/user"       // https://pkg.go.dev/os/user
	"path/filepath" // https://pkg.go.dev/path/filepath
	"reflect"       // https://pkg.go.dev/reflect
	"regexp"        // https://pkg.go.dev/regexp
	"strings"       // https://pkg.go.dev/strings
	"time"          // https://pkg.go.dev/time

	ggit "github.com/go-git/go-git/v5"                             // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitconfig "github.com/go-git/go-git/v5/config"                // https://pkg.go.dev/github.com/go-git/go-git/v5/config
	ggitplumbing "github.com/go-git/go-git/v5/plumbing"            // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing
	ggitfilemode "github.com/go-git/go-git/v5/plumbing/filemode"   // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/filemode
	ggitobject "github.com/go-git/go-git/v5/plumbing/object"       // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/object
	ggitstorer "github.com/go-git/go-git/v5/plumbing/storer"       // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/storer
	ggittransport "github.com/go-git/go-git/v5/plumbing/transport" // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/transport
	ggithttp "github.com/go-git/go-git/v5/plumbing/transport/http" // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/transport/http
	ggitmemory "github.com/go-git/go-git/v5/storage/memory"        // https://pkg.go.dev/github.com/go-git/go-git/v5/storage/memory
	log "github.com/sirupsen/logrus"                               // https://pkg.go.dev/github.com/sirupsen/logrus

	api "github.com/mooltiverse/nyx/src/go/nyx/services/api"
)

const (
	// The common prefix of all pooled repository names.
	HOSTED_REPOSITORY_POOL_NAME_PREFIX = "nyx-test-pool-go-"

	// The name of the only branch in a clean pooled repository, which is also the default branch.
	HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH = "main"

	// The description used for pooled repositories.
	hostedRepositoryPoolDescription = "Nyx integration tests pooled repository. Do not use it for anything else."

	// The message of the reference commit, the same used by hosting services when creating initialized repositories.
	hostedRepositoryPoolReferenceCommitMessage = "Initial commit\n"

	// The contents of the only file in the reference commit.
	hostedRepositoryPoolReferenceFileContent = "# Nyx test repository\n\nThis repository is reset at every use by Nyx integration tests. Do not use it for anything else.\n"

	// The name of the only file in the reference commit.
	hostedRepositoryPoolReferenceFileName = "README.md"

	// How many times an operation subject to eventual consistency is attempted before giving up.
	hostedRepositoryPoolAttempts = 10

	// The delay between attempts.
	hostedRepositoryPoolAttemptDelay = 1500 * time.Millisecond
)

var (
	// The author and committer of the reference commit. The timestamp is fixed so the commit ID is deterministic.
	hostedRepositoryPoolReferenceIdentity = ggitobject.Signature{Name: "Nyx Test", Email: "nyx-test@users.noreply.github.com", When: time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)}

	// The characters not allowed in environment names.
	hostedRepositoryPoolInvalidCharacters = regexp.MustCompile("[^a-z0-9-]+")

	// Leading and trailing dashes.
	hostedRepositoryPoolTrimDashes = regexp.MustCompile("^-+|-+$")
)

/*
The service specific operations used by a hostedRepositoryPool, where H is the service specific type representing a
remote repository.
*/
type hostedRepositoryProvider[H any] interface {
	// Returns the repository with the given name owned by the authenticated user. The boolean is false if the
	// repository doesn't exist.
	find(name string) (H, bool, error)

	// Creates an empty repository with the given name, owned by the authenticated user.
	create(name string, restricted bool) error

	// Deletes the given repository.
	delete(repository H) error

	// Returns the HTTP URL of the given repository, used for Git operations.
	httpURL(repository H) string

	// Returns the credentials for Git operations.
	gitAuth() *ggithttp.BasicAuth

	// Prepares the given repository for the reset, before the default branch is force pushed: deletes the service
	// specific contents that survive the deletion of tags and branches (i.e. releases and packages) and removes the
	// protections that would prevent the force push or the deletion of refs.
	beforePush(repository H) error

	// Completes the reset of the given repository after the default branch has been pushed: makes sure the
	// HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH branch is the default one and the repository visibility is the expected one.
	afterPush(repository H, restricted bool) error

	// Returns the list of differences between the service specific state of the given repository (i.e. default branch,
	// visibility, releases, packages) and a clean repository. Git refs are inspected separately.
	inspect(repository H, restricted bool) ([]string, error)

	// Returns the object returned to tests for the given repository.
	wrap(repository H) api.GitHostedRepository
}

/*
A pool of long-lived hosted repositories for integration tests, used instead of creating (and deleting) a new repository
for every test.

Hosting services apply rate limits on content creation, and repository creation in particular, which a test suite
creating one repository per test exceeds. Pooled repositories are created once and then reused across test runs.

Isolation between tests is preserved by resetting a repository every time it's acquired (not when it's released, so a
test that crashed or was interrupted can't affect the next one) and then verifying the result:

  - the service specific contents that survive the deletion of tags and branches (i.e. releases and packages) are
    deleted and the branch protections that would prevent the reset are removed
  - the HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH branch is force pushed to a deterministic reference commit (see
    HostedRepositoryPoolReferenceCommitID())
  - the HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH branch is set as the default branch and the repository visibility is set
    as requested
  - all other branches and all tags are deleted
  - the remote state is verified, retrying for a while to cope with eventual consistency: the only remote refs must be
    the HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH branch pointing to the reference commit and the service specific state
    must be clean

If the verification fails the repository is deleted and created again, which is the only case a repository is created
after the first time.

Repository names are scoped by environment (see HostedRepositoryPoolDefaultEnvironment()), visibility and slot, so that
concurrent runs on different environments (i.e. CI jobs and local runs) never share a repository. Tests within the same
environment run sequentially so no locking is needed. A test needing more than one repository with the same visibility
at the same time uses different slots.

Tests using pooled repositories must not change the repository settings (other than the contents and releases).
*/
type hostedRepositoryPool[H any] struct {
	// The environment this pool is scoped to.
	environment string

	// The service specific operations.
	provider hostedRepositoryProvider[H]
}

/*
Returns a pool scoped to the given environment, using the given provider.

Errors:

  - an error if the environment is empty after sanitization
*/
func newHostedRepositoryPool[H any](environment string, provider hostedRepositoryProvider[H]) (hostedRepositoryPool[H], error) {
	sanitized := sanitizeHostedRepositoryPoolName(environment)
	if sanitized == "" {
		return hostedRepositoryPool[H]{}, fmt.Errorf("the environment '%s' yields an empty name", environment)
	}
	return hostedRepositoryPool[H]{environment: sanitized, provider: provider}, nil
}

/*
Returns the environment pools are scoped to by default:

  - on GitHub Actions: ci-<runner OS>-<test package>, as the runner OS is the only dimension of the CI job matrix
  - elsewhere: local-<user name>-<test package>

The test package is part of the environment because 'go test' runs the tests of different packages in parallel
(as separate processes), while tests within the same package run sequentially. Scoping repositories by package
makes sure that tests running in parallel never share a repository. The package name is the name of the test
binary built by 'go test' (i.e. 'command' for 'command.test').
*/
func HostedRepositoryPoolDefaultEnvironment() string {
	pkg := hostedRepositoryPoolTestPackage()
	if strings.EqualFold(os.Getenv("GITHUB_ACTIONS"), "true") {
		runnerOS := os.Getenv("RUNNER_OS")
		if runnerOS == "" {
			runnerOS = "unknown"
		}
		return sanitizeHostedRepositoryPoolName("ci-" + runnerOS + "-" + pkg)
	}
	userName := "unknown"
	if u, err := user.Current(); err == nil && u.Username != "" {
		userName = u.Username
	}
	return sanitizeHostedRepositoryPoolName("local-" + userName + "-" + pkg)
}

/*
Returns the name of the package whose tests are running, inferred from the name of the test binary built by 'go test',
which is '<package>.test' (or '<package>.test.exe' on Windows).
*/
func hostedRepositoryPoolTestPackage() string {
	binary := strings.ToLower(filepath.Base(os.Args[0]))
	binary = strings.TrimSuffix(binary, ".exe")
	binary = strings.TrimSuffix(binary, ".test")
	if binary == "" {
		return "unknown"
	}
	return binary
}

/*
Returns the given value lowercased, with every character not allowed in repository names replaced by a dash and with no
leading or trailing dashes.
*/
func sanitizeHostedRepositoryPoolName(value string) string {
	return hostedRepositoryPoolTrimDashes.ReplaceAllString(hostedRepositoryPoolInvalidCharacters.ReplaceAllString(strings.ToLower(value), "-"), "")
}

/*
Returns the ID of the reference commit a clean repository has as the only commit. The commit is built from constant
contents and metadata, so its ID never changes.
*/
func HostedRepositoryPoolReferenceCommitID() ggitplumbing.Hash {
	hash, err := insertHostedRepositoryPoolReferenceCommit(ggitmemory.NewStorage())
	if err != nil {
		// this can only fail for programming errors as the storage is in memory
		panic(err)
	}
	return hash
}

/*
Stores the reference commit and the objects it refers to in the given storer and returns the commit ID.
*/
func insertHostedRepositoryPoolReferenceCommit(s ggitstorer.EncodedObjectStorer) (ggitplumbing.Hash, error) {
	blob := s.NewEncodedObject()
	blob.SetType(ggitplumbing.BlobObject)
	writer, err := blob.Writer()
	if err != nil {
		return ggitplumbing.ZeroHash, err
	}
	if _, err = writer.Write([]byte(hostedRepositoryPoolReferenceFileContent)); err != nil {
		return ggitplumbing.ZeroHash, err
	}
	if err = writer.Close(); err != nil {
		return ggitplumbing.ZeroHash, err
	}
	blobHash, err := s.SetEncodedObject(blob)
	if err != nil {
		return ggitplumbing.ZeroHash, err
	}

	tree := ggitobject.Tree{Entries: []ggitobject.TreeEntry{{Name: hostedRepositoryPoolReferenceFileName, Mode: ggitfilemode.Regular, Hash: blobHash}}}
	treeObject := s.NewEncodedObject()
	if err = tree.Encode(treeObject); err != nil {
		return ggitplumbing.ZeroHash, err
	}
	treeHash, err := s.SetEncodedObject(treeObject)
	if err != nil {
		return ggitplumbing.ZeroHash, err
	}

	commit := ggitobject.Commit{Author: hostedRepositoryPoolReferenceIdentity, Committer: hostedRepositoryPoolReferenceIdentity, Message: hostedRepositoryPoolReferenceCommitMessage, TreeHash: treeHash}
	commitObject := s.NewEncodedObject()
	if err = commit.Encode(commitObject); err != nil {
		return ggitplumbing.ZeroHash, err
	}
	return s.SetEncodedObject(commitObject)
}

/*
Returns the environment this pool is scoped to.
*/
func (p hostedRepositoryPool[H]) GetEnvironment() string {
	return p.environment
}

/*
Returns the name of the public repository for the given slot.

Errors:

  - an error if the slot is negative
*/
func (p hostedRepositoryPool[H]) RepositoryName(slot int) (string, error) {
	return p.repositoryName(slot, false)
}

/*
Returns the name of the private repository for the given slot.

Errors:

  - an error if the slot is negative
*/
func (p hostedRepositoryPool[H]) PrivateRepositoryName(slot int) (string, error) {
	return p.repositoryName(slot, true)
}

/*
Returns the name of the repository for the given slot and visibility.
*/
func (p hostedRepositoryPool[H]) repositoryName(slot int, restricted bool) (string, error) {
	if slot < 0 {
		return "", fmt.Errorf("the slot can't be negative: %d", slot)
	}
	visibility := ""
	if restricted {
		visibility = "private-"
	}
	return fmt.Sprintf("%s%s-%s%d", HOSTED_REPOSITORY_POOL_NAME_PREFIX, p.environment, visibility, slot), nil
}

/*
Returns the public repository for the given slot, after resetting it to a clean state and verifying the result.
The repository is created if it doesn't exist or if it can't be brought to a clean state.

Arguments are as follows:

  - slot the slot, starting from 0. Tests using more than one repository with the same visibility at the same time
    must use different slots.

Errors:

  - an error if the repository can't be retrieved, created or reset
*/
func (p hostedRepositoryPool[H]) Acquire(slot int) (*api.GitHostedRepository, error) {
	return p.acquire(slot, false)
}

/*
Returns the private repository for the given slot, after resetting it to a clean state and verifying the result.
The repository is created if it doesn't exist or if it can't be brought to a clean state.

Arguments are as follows:

  - slot the slot, starting from 0. Tests using more than one repository with the same visibility at the same time
    must use different slots.

Errors:

  - an error if the repository can't be retrieved, created or reset
*/
func (p hostedRepositoryPool[H]) AcquirePrivate(slot int) (*api.GitHostedRepository, error) {
	return p.acquire(slot, true)
}

/*
Returns the repository for the given slot and visibility, after resetting it to a clean state and verifying the result.
*/
func (p hostedRepositoryPool[H]) acquire(slot int, restricted bool) (*api.GitHostedRepository, error) {
	name, err := p.repositoryName(slot, restricted)
	if err != nil {
		return nil, err
	}
	repository, found, err := p.provider.find(name)
	if err != nil {
		return nil, err
	}
	if !found {
		log.Infof("the pooled repository '%s' does not exist yet and will be created", name)
		if repository, err = p.createAndWait(name, restricted); err != nil {
			return nil, err
		}
	}

	if err = p.reset(repository, restricted); err != nil {
		return nil, err
	}
	clean, found, err := p.awaitClean(name, restricted)
	if err != nil {
		return nil, err
	}
	if !found {
		// the reset didn't work, maybe due to some setting or state the reset doesn't handle, so start over
		problems, _ := p.inspectByName(name, restricted)
		log.Warnf("the pooled repository '%s' could not be reset and will be deleted and created again. Problems: %v", name, problems)
		if err = p.provider.delete(repository); err != nil {
			return nil, fmt.Errorf("unable to delete the pooled repository '%s': %w", name, err)
		}
		if repository, err = p.createAndWait(name, restricted); err != nil {
			return nil, err
		}
		if err = p.reset(repository, restricted); err != nil {
			return nil, err
		}
		if clean, found, err = p.awaitClean(name, restricted); err != nil {
			return nil, err
		}
		if !found {
			problems, _ = p.inspectByName(name, restricted)
			return nil, fmt.Errorf("the pooled repository '%s' is not clean even after being created again. Problems: %v", name, problems)
		}
	}

	res := p.provider.wrap(clean)
	return &res, nil
}

/*
Returns the list of differences between the current state of the public repository for the given slot and a clean
repository, without changing anything. The list is empty if the repository is clean.

Errors:

  - an error if the repository can't be inspected
*/
func (p hostedRepositoryPool[H]) Inspect(slot int) ([]string, error) {
	name, err := p.repositoryName(slot, false)
	if err != nil {
		return nil, err
	}
	return p.inspectByName(name, false)
}

/*
Returns the list of differences between the current state of the private repository for the given slot and a clean
repository, without changing anything. The list is empty if the repository is clean.

Errors:

  - an error if the repository can't be inspected
*/
func (p hostedRepositoryPool[H]) InspectPrivate(slot int) ([]string, error) {
	name, err := p.repositoryName(slot, true)
	if err != nil {
		return nil, err
	}
	return p.inspectByName(name, true)
}

/*
Returns the list of differences between the current state of the repository with the given name and a clean repository.
*/
func (p hostedRepositoryPool[H]) inspectByName(name string, restricted bool) ([]string, error) {
	repository, found, err := p.provider.find(name)
	if err != nil {
		return nil, err
	}
	if !found {
		return []string{fmt.Sprintf("the repository '%s' does not exist", name)}, nil
	}

	problems := []string{}
	refs, err := p.listRemoteRefs(p.provider.httpURL(repository))
	if err != nil {
		return nil, err
	}
	expectedRefs := map[string]string{"refs/heads/" + HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH: HostedRepositoryPoolReferenceCommitID().String()}
	if !reflect.DeepEqual(expectedRefs, refs) {
		problems = append(problems, fmt.Sprintf("unexpected refs: %v, expected: %v", refs, expectedRefs))
	}
	serviceProblems, err := p.provider.inspect(repository, restricted)
	if err != nil {
		return nil, err
	}
	return append(problems, serviceProblems...), nil
}

/*
Inspects the repository with the given name until it's clean or the attempts are exhausted, to cope with eventual
consistency. Returns the up to date repository and true if it's clean, false otherwise.
*/
func (p hostedRepositoryPool[H]) awaitClean(name string, restricted bool) (H, bool, error) {
	var zero H
	for attempt := 1; attempt <= hostedRepositoryPoolAttempts; attempt++ {
		problems, err := p.inspectByName(name, restricted)
		if err != nil {
			return zero, false, err
		}
		if len(problems) == 0 {
			// fetch the repository again as its attributes may have changed
			return p.provider.find(name)
		}
		time.Sleep(hostedRepositoryPoolAttemptDelay)
	}
	return zero, false, nil
}

/*
Creates the repository with the given name and waits until it's available. Creation is not retried, as failures are
often due to rate limits, which retrying would make worse.
*/
func (p hostedRepositoryPool[H]) createAndWait(name string, restricted bool) (H, error) {
	var repository H
	if err := p.provider.create(name, restricted); err != nil {
		return repository, fmt.Errorf("unable to create the pooled repository '%s': %w", name, err)
	}
	err := retryHostedRepositoryPoolOperation(func() error {
		var found bool
		var err error
		if repository, found, err = p.provider.find(name); err != nil {
			return err
		}
		if !found {
			return fmt.Errorf("the repository '%s' is not available yet", name)
		}
		return nil
	})
	return repository, err
}

/*
Resets the given repository to a clean state. See the hostedRepositoryPool description for details.
*/
func (p hostedRepositoryPool[H]) reset(repository H, restricted bool) error {
	if err := p.provider.beforePush(repository); err != nil {
		return err
	}

	url := p.provider.httpURL(repository)
	storage := ggitmemory.NewStorage()
	local, err := ggit.Init(storage, nil)
	if err != nil {
		return err
	}
	if _, err = local.CreateRemote(&ggitconfig.RemoteConfig{Name: "origin", URLs: []string{url}}); err != nil {
		return err
	}
	reference, err := insertHostedRepositoryPoolReferenceCommit(storage)
	if err != nil {
		return err
	}
	defaultRef := ggitplumbing.NewBranchReferenceName(HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH)
	if err = storage.SetReference(ggitplumbing.NewHashReference(defaultRef, reference)); err != nil {
		return err
	}

	// push the default branch first, as services refuse to delete the current default branch, which may be a different
	// one until it's changed by afterPush()
	if err = retryHostedRepositoryPoolOperation(func() error {
		return p.push(local, url, []ggitconfig.RefSpec{ggitconfig.RefSpec("+" + defaultRef.String() + ":" + defaultRef.String())})
	}); err != nil {
		return err
	}

	if err = p.provider.afterPush(repository, restricted); err != nil {
		return err
	}

	refs, err := p.listRemoteRefs(url)
	if err != nil {
		return err
	}
	deletions := []ggitconfig.RefSpec{}
	for ref := range refs {
		if ref != defaultRef.String() {
			deletions = append(deletions, ggitconfig.RefSpec(":"+ref))
		}
	}
	if len(deletions) > 0 {
		log.Debugf("deleting refs %v from the pooled repository '%s'", deletions, url)
		if err = retryHostedRepositoryPoolOperation(func() error { return p.push(local, url, deletions) }); err != nil {
			return err
		}
	}
	return nil
}

/*
Pushes the given ref specs from the given local repository to the given URL.
*/
func (p hostedRepositoryPool[H]) push(local *ggit.Repository, url string, refSpecs []ggitconfig.RefSpec) error {
	err := local.Push(&ggit.PushOptions{RemoteName: "origin", RefSpecs: refSpecs, Force: true, Auth: p.provider.gitAuth()})
	if err != nil && !errors.Is(err, ggit.NoErrAlreadyUpToDate) {
		return fmt.Errorf("unable to push %v to '%s': %w", refSpecs, url, err)
	}
	return nil
}

/*
Returns the branches and tags of the remote repository at the given URL as a map of ref names and the IDs they point to.
*/
func (p hostedRepositoryPool[H]) listRemoteRefs(url string) (map[string]string, error) {
	remote := ggit.NewRemote(ggitmemory.NewStorage(), &ggitconfig.RemoteConfig{Name: "origin", URLs: []string{url}})
	list, err := remote.List(&ggit.ListOptions{Auth: p.provider.gitAuth()})
	if err != nil {
		if errors.Is(err, ggittransport.ErrEmptyRemoteRepository) {
			return map[string]string{}, nil
		}
		return nil, fmt.Errorf("unable to list the refs of '%s': %w", url, err)
	}
	refs := map[string]string{}
	for _, ref := range list {
		refName := ref.Name().String()
		if (ref.Name().IsBranch() || ref.Name().IsTag()) && !strings.HasSuffix(refName, "^{}") {
			refs[refName] = ref.Hash().String()
		}
	}
	return refs, nil
}

/*
Runs the given operation until it succeeds or the attempts are exhausted, to cope with eventual consistency (i.e. on
newly created repositories). Returns the error from the last attempt.
*/
func retryHostedRepositoryPoolOperation(operation func() error) error {
	var err error
	for attempt := 1; attempt <= hostedRepositoryPoolAttempts; attempt++ {
		if err = operation(); err == nil {
			return nil
		}
		log.Debugf("attempt %d of %d failed: %v", attempt, hostedRepositoryPoolAttempts, err)
		time.Sleep(hostedRepositoryPoolAttemptDelay)
	}
	return err
}
