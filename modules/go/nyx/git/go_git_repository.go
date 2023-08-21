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

package git

import (
	"bufio"         // https://pkg.go.dev/bufio
	"bytes"         // https://pkg.go.dev/bytes
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"os/exec"       // https://pkg.go.dev/os/exec
	"path/filepath" // https://pkg.go.dev/filepath
	"strings"       // https://pkg.go.dev/strings

	ggit "github.com/go-git/go-git/v5"                                // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitconfig "github.com/go-git/go-git/v5/config"                   // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitplumbing "github.com/go-git/go-git/v5/plumbing"               // https://pkg.go.dev/github.com/go-git/go-git/v5
	gitignore "github.com/go-git/go-git/v5/plumbing/format/gitignore" // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitobject "github.com/go-git/go-git/v5/plumbing/object"          // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggittransport "github.com/go-git/go-git/v5/plumbing/transport"    // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggithttp "github.com/go-git/go-git/v5/plumbing/transport/http"    // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitssh "github.com/go-git/go-git/v5/plumbing/transport/ssh"      // https://pkg.go.dev/github.com/go-git/go-git/v5
	log "github.com/sirupsen/logrus"                                  // https://pkg.go.dev/github.com/sirupsen/logrus
	ssh "golang.org/x/crypto/ssh"                                     // https://pkg.go.dev/golang.org/x/crypto/ssh

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
)

var (
	// This flag tells if we already emitted the warning about the workaround documented at https://github.com/mooltiverse/nyx/issues/130
	// This warning is only needed for the workaround at https://github.com/mooltiverse/nyx/issues/130 so this variable can be
	// removed when the workaround is no longer needed.
	// TODO: remove this variable when https://github.com/mooltiverse/nyx/issues/130 is fixed
	workaround130WarningsEmitted = false

	// This flag tells if we already emitted the warning about the workaround documented at https://github.com/mooltiverse/nyx/pull/231
	// This warning is only needed for the workaround at https://github.com/mooltiverse/nyx/pull/231 so this variable can be
	// removed when the workaround is no longer needed.
	// TODO: remove this variable when https://github.com/mooltiverse/nyx/pull/231 is fixed
	workaround231WarningsEmitted = false
)

/*
A local repository implementation that encapsulates the backing go-git (https://pkg.go.dev/github.com/go-git/go-git/v5) library.
*/
type goGitRepository struct {
	// The directory of backing Git repository. We need to store this as the Git implementation doesn't expose methods to retrieve it.
	// This attribute is only needed for the workaround #130 (see the IsClean() method) and can be removed once the workaround is no longer needed.
	// TODO: remove the 'directory' attribute when https://github.com/mooltiverse/nyx/issues/130 is fixed
	directory string

	// The private instance of the underlying Git object.
	repository *ggit.Repository
}

/*
Builds the instance using the given backing object.
*/
// TODO: remove the 'directory' attribute when https://github.com/mooltiverse/nyx/issues/130 is fixed
func newGoGitRepository(directory string, repository *ggit.Repository) (goGitRepository, error) {
	if repository == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "repository")}
	}

	gitRepository := goGitRepository{}
	gitRepository.directory = directory
	gitRepository.repository = repository
	return gitRepository, nil
}

/*
Returns a new basic authentication method object using the given user name and password.

Returns nil if both the given credentials are nil.

  - user the user name to use when credentials are required. It may be nil.
    When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to use when credentials are required. It may be nil.
    When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
*/
func getBasicAuth(user *string, password *string) ggittransport.AuthMethod {
	if user == nil && password == nil {
		return nil
	} else if user != nil && password == nil {
		return &ggithttp.BasicAuth{Username: *user}
	} else if user == nil && password != nil {
		return &ggithttp.BasicAuth{Password: *password}
	} else {
		return &ggithttp.BasicAuth{Username: *user, Password: *password}
	}
}

/*
Returns a new public key authentication method object using the given private key and passphrase.

Returns nil if both the given credentials are nil.

  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func getPublicKeyAuth(privateKey *string, passphrase *string) ggittransport.AuthMethod {
	if privateKey != nil && "" != *privateKey {
		keyPassword := ""
		if passphrase != nil {
			keyPassword = *passphrase
		}
		// The key name is not relevant for us but it seems it must be 'git' for the underlying library,
		// as per https://github.com/src-d/go-git/issues/637
		publicKeys, err := ggitssh.NewPublicKeys("git", []byte(*privateKey), keyPassword)
		if err != nil {
			log.Errorf("cannot instantiate a PublicKeys object using the provided private key: %v", err)
			return nil
		}

		// disable host key checking
		publicKeys.HostKeyCallback = ssh.InsecureIgnoreHostKey()

		return publicKeys
	} else {
		// If no private key is passed just return a default auth method so that go-git will just load keys from their default
		// location (~/.ssh) and connect to ssh-agent (if available) by simply recognizing the remote repository
		// URL format (which must be SSH). See https://github.com/src-d/go-git/issues/550#issuecomment-323078245
		// Some other sources sugges to just return nil for using the default SSH client and settings
		log.Debugf("trying to instantiate a DefaultAuthBuilder for public key authentication, connecting to ssh-agent/Pageant")
		authBuilder, err := ggitssh.DefaultAuthBuilder("keymaster")
		if err != nil {
			log.Debugf("cannot instantiate a DefaultAuthBuilder for public key authentication, probably due to the service not being available: %v", err)
			return nil
		}
		return authBuilder
	}
}

/*
Returns a repository instance working in the given directory after cloning from the given URI.

Arguments are as follows:

- directory the directory where the repository has to be cloned. It is created if it doesn't exist.
- uri the URI of the remote repository to clone.

Errors can be:

- NilPointerError if any of the given objects is nil
- IllegalArgumentError if the given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func clone(directory *string, uri *string) (goGitRepository, error) {
	if directory == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null directory"}
	}
	if uri == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null URI"}
	}
	if "" == strings.TrimSpace(*directory) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank directory"}
	}
	if "" == strings.TrimSpace(*uri) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank URI"}
	}

	log.Debugf("cloning repository in directory '%s' from URI '%s'", *directory, *uri)

	options := &ggit.CloneOptions{URL: *uri}
	repository, err := ggit.PlainClone(*directory, false, options)
	if err != nil {
		return goGitRepository{}, &errs.GitError{Message: fmt.Sprintf("unable to clone the '%s' repository into '%s'", *uri, *directory), Cause: err}
	}

	return newGoGitRepository(*directory, repository)
}

/*
Returns a repository instance working in the given directory after cloning from the given URI.

Arguments are as follows:

  - directory the directory where the repository has to be cloned. It is created if it doesn't exist.
  - uri the URI of the remote repository to clone.
  - user the user name to use when credentials are required. If this and password are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to use when credentials are required. If this and user are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.

Errors can be:

- NilPointerError if any of the given objects is nil
- IllegalArgumentError if the given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func cloneWithUserNameAndPassword(directory *string, uri *string, user *string, password *string) (goGitRepository, error) {
	if directory == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null directory"}
	}
	if uri == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null URI"}
	}
	if "" == strings.TrimSpace(*directory) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank directory"}
	}
	if "" == strings.TrimSpace(*uri) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank URI"}
	}

	log.Debugf("cloning repository in directory '%s' from URI '%s' using username and password", *directory, *uri)

	options := &ggit.CloneOptions{URL: *uri}
	auth := getBasicAuth(user, password)
	if auth != nil {
		log.Debugf("username and password authentication will use custom authentication options")
		options.Auth = auth
	} else {
		log.Debugf("username and password authentication will not use any custom authentication options")
	}
	repository, err := ggit.PlainClone(*directory, false, options)
	if err != nil {
		return goGitRepository{}, &errs.GitError{Message: fmt.Sprintf("unable to clone the '%s' repository into '%s'", *uri, *directory), Cause: err}
	}

	return newGoGitRepository(*directory, repository)
}

/*
Returns a repository instance working in the given directory after cloning from the given URI.

Arguments are as follows:

  - directory the directory where the repository has to be cloned. It is created if it doesn't exist.
  - uri the URI of the remote repository to clone.
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.

Errors can be:

- NilPointerError if any of the given objects is nil
- IllegalArgumentError if the given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func cloneWithPublicKey(directory *string, uri *string, privateKey *string, passphrase *string) (goGitRepository, error) {
	if directory == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null directory"}
	}
	if uri == nil {
		return goGitRepository{}, &errs.NilPointerError{Message: "can't clone a repository instance with a null URI"}
	}
	if "" == strings.TrimSpace(*directory) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank directory"}
	}
	if "" == strings.TrimSpace(*uri) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank URI"}
	}

	log.Debugf("cloning repository in directory '%s' from URI '%s' using public key (SSH) authentication", *directory, *uri)

	options := &ggit.CloneOptions{URL: *uri}
	auth := getPublicKeyAuth(privateKey, passphrase)
	if auth != nil {
		log.Debugf("public key (SSH) authentication will use custom authentication options")
		options.Auth = auth
	} else {
		log.Debugf("public key (SSH) authentication will not use any custom authentication options")
	}
	repository, err := ggit.PlainClone(*directory, false, options)
	if err != nil {
		return goGitRepository{}, &errs.GitError{Message: fmt.Sprintf("unable to clone the '%s' repository into '%s'", *uri, *directory), Cause: err}
	}

	// TODO: remove the 'directory' attribute when https://github.com/mooltiverse/nyx/issues/130 is fixed
	return newGoGitRepository(*directory, repository)
}

/*
Returns a repository instance working in the given directory.

Arguments are as follows:

- directory the directory where the repository is.

Errors can be:

- IllegalArgumentError if the given object is illegal for some reason, like referring to an illegal repository
- IOError in case of any I/O issue accessing the repository
*/
func open(directory string) (goGitRepository, error) {
	if "" == strings.TrimSpace(directory) {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: "can't create a repository instance with a blank directory"}
	}
	repository, err := ggit.PlainOpen(directory)
	if err != nil {
		return goGitRepository{}, &errs.IllegalArgumentError{Message: fmt.Sprintf("unable to open Git repository in directory '%s'", directory), Cause: err}
	}
	// TODO: remove the 'directory' attribute when https://github.com/mooltiverse/nyx/issues/130 is fixed
	return newGoGitRepository(directory, repository)
}

/*
Resolves the commit with the given id using the repository object and returns it as a typed object.

This method is an utility wrapper around CommitObject which never returns
nil and throws GitError if the identifier cannot be resolved or any other error occurs.

Arguments are as follows:

- id the commit identifier to resolve. It must be a long or abbreviated SHA-1 but not nil.

Errors can be:

- GitError in case the given identifier cannot be resolved or any other issue is encountered
*/
func (r goGitRepository) parseCommit(id string) (ggitobject.Commit, error) {
	log.Tracef("parsing commit '%s'", id)
	commit, err := r.repository.CommitObject(ggitplumbing.NewHash(id))
	if err != nil {
		return ggitobject.Commit{}, &errs.GitError{Message: fmt.Sprintf("the '%s' commit identifier cannot be resolved as there is no such commit.", id), Cause: err}
	}
	return *commit, nil
}

/*
Resolves the object with the given id in the repository.

This method is an utility wrapper around ResolveRevision which never returns
nil and returns GitError if the identifier cannot be resolved or any other error occurs.

Arguments are as follows:

  - id the object identifier to resolve. It can't be nil. If it's a SHA-1 it can be long or abbreviated.
    For allowed values see ResolveRevision

Errors can be:

- GitError in case the given identifier cannot be resolved or any other issue is encountered
*/
func (r goGitRepository) resolve(id string) (ggitplumbing.Hash, error) {
	log.Tracef("resolving '%s'", id)

	rev, err := r.repository.ResolveRevision(ggitplumbing.Revision(id))
	if err != nil {
		return ggitplumbing.Hash{}, &errs.GitError{Message: fmt.Sprintf("the '%s' identifier cannot be resolved", id), Cause: err}
	}
	if rev == nil {
		if "HEAD" == id {
			log.Warnf("Repository identifier '%s' cannot be resolved. This means that the repository has just been initialized and has no commits yet or the repository is in a 'detached HEAD' state. See the documentation to fix this.", "HEAD")
		}
		return ggitplumbing.Hash{}, &errs.GitError{Message: fmt.Sprintf("Identifier '%s' cannot be resolved", id)}
	} else {
		return ggitplumbing.NewHash(rev.String()), nil
	}
}

/*
Arguments are as follows:

- paths the file patterns of the contents to add to stage. Cannot be nil or empty. The path "." represents
all files in the working area so with that you can add all locally changed files.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to add paths.
*/
func (r goGitRepository) Add(paths []string) error {
	log.Debugf("adding contents to repository staging area")
	if paths == nil || len(paths) == 0 {
		return &errs.GitError{Message: fmt.Sprintf("cannot stage a nil or empty set of paths")}
	}

	worktree, err := r.repository.Worktree()
	if err != nil {
		return &errs.GitError{Message: fmt.Sprintf("an error occurred when getting the current worktree for the repository"), Cause: err}
	}
	// TODO: remove this workaround (before the 'for' statement) when https://github.com/mooltiverse/nyx/issues/219 is fixed
	// The go-git library has a bug that sometimes does not obey with the .gitignore file so we use the
	// workaround suggested here: https://github.com/go-git/go-git/issues/597#issuecomment-1301637889
	// to read the .gitignore and programmatically add the paths to the Worktree Excludes.
	// This workaround is here to cope with:
	// - https://github.com/mooltiverse/nyx/issues/219
	// as long as the go-git library doesn't fix the bug. Bugs to keep an eye on for a fix are:
	// - https://github.com/go-git/go-git/issues/597
	if _, err := os.Stat(filepath.Join(r.directory, ".gitignore")); err == nil {
		if !workaround231WarningsEmitted {
			log.Warnf("workaround #231: due to the underlying go-git library not obeying to the .gitignore files the .gitignore content is read and each item passed to the Worktree Excludes. For more see https://github.com/mooltiverse/nyx/issues/219")
			// make sure we emit this warning only once
			workaround231WarningsEmitted = true
		}
		gitIgnoreFile, err := os.Open(filepath.Join(r.directory, ".gitignore"))
		defer gitIgnoreFile.Close()
		if err != nil {
			return &errs.GitError{Message: fmt.Sprintf("unable to read .gitignore (needed for workaround https://github.com/mooltiverse/nyx/issues/219)"), Cause: err}
		}
		gitIgnoreFileScanner := bufio.NewScanner(gitIgnoreFile)
		gitIgnoreFileScanner.Split(bufio.ScanLines)
		for gitIgnoreFileScanner.Scan() {
			ignorePattern := gitIgnoreFileScanner.Text()
			if !workaround231WarningsEmitted {
				log.Debugf("add %s from .gitignore to ignore list (needed for workaround https://github.com/mooltiverse/nyx/issues/219)", ignorePattern)
			}
			worktree.Excludes = append(worktree.Excludes, gitignore.ParsePattern(ignorePattern, nil))
		}
		// End of the workaround
	}
	for _, path := range paths {
		err := worktree.AddWithOptions(&ggit.AddOptions{All: true, Path: "", Glob: path})
		if err != nil {
			return &errs.GitError{Message: fmt.Sprintf("an error occurred when trying to add paths to the staging area"), Cause: err}
		}
	}

	return nil
}

/*
Commits changes to the repository. Files to commit must be staged separately using Add.

- message the commit message. Cannot be nil.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to commit.
*/
func (r goGitRepository) CommitWithMessage(message *string) (gitent.Commit, error) {
	return r.CommitWithMessageAndIdentities(message, nil, nil)
}

/*
Commits changes to the repository. Files to commit must be staged separately using Add.

Arguments are as follows:

- message the commit message. Cannot be nil.
- author the object modelling the commit author informations. It may be nil, in which case the default
for the repository will be used
- committer the object modelling the committer informations. It may be nil, in which case the default
for the repository will be used

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to commit.
*/
func (r goGitRepository) CommitWithMessageAndIdentities(message *string, author *gitent.Identity, committer *gitent.Identity) (gitent.Commit, error) {
	log.Debugf("committing changes to repository")

	if message == nil {
		return gitent.Commit{}, &errs.GitError{Message: fmt.Sprintf("cannot commit with a nil message")}
	}

	worktree, err := r.repository.Worktree()
	if err != nil {
		return gitent.Commit{}, &errs.GitError{Message: fmt.Sprintf("an error occurred when getting the current worktree for the repository"), Cause: err}
	}
	var gAuthor *ggitobject.Signature = nil
	var gCommitter *ggitobject.Signature = nil
	if author != nil {
		gAuthor = &ggitobject.Signature{Name: author.Name, Email: author.Email}
	}
	if committer != nil {
		gCommitter = &ggitobject.Signature{Name: committer.Name, Email: committer.Email}
	}
	commitHash, err := worktree.Commit(*message, &ggit.CommitOptions{All: false, Author: gAuthor, Committer: gCommitter})
	if err != nil {
		return gitent.Commit{}, &errs.GitError{Message: fmt.Sprintf("an error occurred when trying to commit"), Cause: err}
	}
	commit, err := r.repository.CommitObject(commitHash)
	if err != nil {
		return gitent.Commit{}, &errs.GitError{Message: fmt.Sprintf("an error occurred when retrieving the commit that has been created"), Cause: err}
	}
	return CommitFrom(*commit, []gitent.Tag{}), nil
}

/*
Adds the given files to the staging area and commits changes to the repository. This method is a shorthand
for Add and CommitWithMessage.

Arguments are as follows:

  - paths the file patterns of the contents to add to stage. Cannot be nil or empty. The path "." represents
    all files in the working area so with that you can add all locally changed files.
  - message the commit message. Cannot be nil.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to commit.
*/
func (r goGitRepository) CommitPathsWithMessage(paths []string, message *string) (gitent.Commit, error) {
	return r.CommitPathsWithMessageAndIdentities(paths, message, nil, nil)
}

/*
Adds the given files to the staging area and commits changes to the repository. This method is a shorthand
for Add and CommitWithMessageAndIdentities.

Arguments are as follows:

  - paths the file patterns of the contents to add to stage. Cannot be nil or empty. The path "." represents
    all files in the working area so with that you can add all locally changed files.
  - message the commit message. Cannot be nil.
  - author the object modelling the commit author informations. It may be nil, in which case the default
    for the repository will be used
  - committer the object modelling the committer informations. It may be nil, in which case the default
    for the repository will be used

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to commit.
*/
func (r goGitRepository) CommitPathsWithMessageAndIdentities(paths []string, message *string, author *gitent.Identity, committer *gitent.Identity) (gitent.Commit, error) {
	err := r.Add(paths)
	if err != nil {
		return gitent.Commit{}, &errs.GitError{Message: fmt.Sprintf("an error occurred while staging contents to the repository"), Cause: err}
	}
	return r.CommitWithMessageAndIdentities(message, author, committer)
}

/*
Returns a set of objects representing all the tags for the given commit.

Arguments are as follows:

- commit the SHA-1 identifier of the commit to get the tags for. It can be a full or abbreviated SHA-1.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository.
*/
func (r goGitRepository) GetCommitTags(commit string) ([]gitent.Tag, error) {
	log.Debugf("retrieving tags for commit '%s'", commit)
	var res []gitent.Tag
	tagsIterator, err := r.repository.Tags()
	if err != nil {
		return nil, &errs.GitError{Message: fmt.Sprintf("cannot list repository tags"), Cause: err}
	}
	if err := tagsIterator.ForEach(func(ref *ggitplumbing.Reference) error {
		// in order to check if the tag has this commit as target we first need to figure out if it's annotated or lightweight
		tagObject, err := r.repository.TagObject(ref.Hash())
		switch err {
		case nil:
			// it's an annotated tag
			if strings.HasPrefix(tagObject.Target.String(), commit) {
				res = append(res, TagFrom(r.repository, *ref))
			}
		case ggitplumbing.ErrObjectNotFound:
			// it's a lightweight tag
			if strings.HasPrefix(ref.Hash().String(), commit) {
				res = append(res, TagFrom(r.repository, *ref))
			}
		default:
			// Some other error occurred
			return &errs.GitError{Message: fmt.Sprintf("error while listing repository tags"), Cause: err}
		}
		return nil
	}); err != nil {
		return nil, &errs.GitError{Message: fmt.Sprintf("error while listing repository tags"), Cause: err}
	}
	return res, nil
}

/*
Returns the name of the current branch or a commit SHA-1 if the repository is in the detached head state.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or is in the 'detached HEAD' state.
*/
func (r goGitRepository) GetCurrentBranch() (string, error) {
	ref, err := r.repository.Head()
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("unable to resolve reference to HEAD"), Cause: err}
	}

	// also strip the leading "refs/heads/" from the reference name
	return strings.Replace(ref.Name().String(), "refs/heads/", "", 1), nil
}

/*
Returns the SHA-1 identifier of the last commit in the current branch.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or is in the 'detached HEAD' state.
*/
func (r goGitRepository) GetLatestCommit() (string, error) {
	ref, err := r.repository.Head()
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("unable to resolve reference to HEAD"), Cause: err}
	}
	commitSHA := ref.Hash().String()
	log.Debugf("repository latest commit in HEAD branch is '%s'", commitSHA)
	return commitSHA, nil
}

/*
Returns the SHA-1 identifier of the first commit in the repository (the only commit with no parents).

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or is in the 'detached HEAD' state.
*/
func (r goGitRepository) GetRootCommit() (string, error) {
	ref, err := r.repository.Head()
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("unable to resolve reference to HEAD"), Cause: err}
	}
	// the Log method doesn't let us follow the firt parent, so we need to go through all commits and stop at the end
	commit, err := r.parseCommit(ref.Hash().String())
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("an error occurred while walking the commit history at commit '%s'", ref.Hash().String()), Cause: err}
	}
	for len(commit.ParentHashes) > 0 {
		c, err := r.repository.CommitObject(commit.ParentHashes[0]) // always follow the first parent, ignore others, if any
		if err != nil {
			return "", &errs.GitError{Message: fmt.Sprintf("an error occurred while walking the commit history at commit '%s'", ref.Hash().String()), Cause: err}
		}
		commit = *c
	}
	commitSHA := commit.Hash.String()
	log.Debugf("repository latest commit in HEAD branch is '%s'", commitSHA)
	return commitSHA, nil
}

/*
Returns a set of objects representing all the tags for the repository.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository.
*/
func (r goGitRepository) GetTags() ([]gitent.Tag, error) {
	log.Debugf("retrieving all tags")
	var res []gitent.Tag
	tagsIterator, err := r.repository.Tags()
	if err != nil {
		return nil, &errs.GitError{Message: fmt.Sprintf("cannot list repository tags"), Cause: err}
	}
	if err := tagsIterator.ForEach(func(ref *ggitplumbing.Reference) error {
		switch err {
		case nil:
			// it's an annotated tag
			res = append(res, TagFrom(r.repository, *ref))
		case ggitplumbing.ErrObjectNotFound:
			// it's a lightweight tag
			res = append(res, TagFrom(r.repository, *ref))
		default:
			// Some other error occurred
			return &errs.GitError{Message: fmt.Sprintf("error while listing repository tags"), Cause: err}
		}
		return nil
	}); err != nil {
		return nil, &errs.GitError{Message: fmt.Sprintf("error while listing repository tags"), Cause: err}
	}
	return res, nil
}

/*
Returns the names of configured remote repositories.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or is in the 'detached HEAD' state.
*/
func (r goGitRepository) GetRemoteNames() ([]string, error) {
	log.Debugf("retrieving repository remote names")
	remotes, err := r.repository.Remotes()
	if err != nil {
		return nil, &errs.GitError{Message: fmt.Sprintf("unable to get the repository remotes"), Cause: err}
	}
	remoteNames := make([]string, len(remotes))
	for i, rmt := range remotes {
		remoteNames[i] = rmt.Config().Name
	}

	log.Debugf("repository remote names are '%v'", remoteNames)
	return remoteNames, nil
}

/*
Returns true if the repository is clean, which is when no differences exist between the working tree, the index,
and the current HEAD.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or is in the 'detached HEAD' state.
*/
func (r goGitRepository) IsClean() (bool, error) {
	log.Debugf("checking repository clean status")
	wt, err := r.repository.Worktree()
	if err != nil {
		return false, &errs.GitError{Message: fmt.Sprintf("unable to get the repository worktree"), Cause: err}
	}
	status, err := wt.Status()
	if err != nil {
		return false, &errs.GitError{Message: fmt.Sprintf("unable to get the repository worktree status"), Cause: err}
	}
	log.Debugf("repository clean status is: '%v' ('%v')", status.IsClean(), status.String())
	for fileName, fileStatus := range status {
		log.Tracef("repository status for '%v' is: untracked='%v', staging='%v', worktree='%v', extra='%v', ", fileName, status.IsUntracked(fileName), string((*fileStatus).Staging), string((*fileStatus).Worktree), (*fileStatus).Extra)
	}
	log.Tracef("repository status flags are: Unmodified = ' ', Untracked = '?', Modified = 'M', Added = 'A', Deleted = 'D', Renamed = 'R', Copied = 'C', UpdatedButUnmerged = 'U'")

	// TODO: remove this workaround (within the 'if' statement) when https://github.com/mooltiverse/nyx/issues/130 is fixed
	// The go-git library has a bug that sometimes makes it return 'false' from status.IsClean() (meaning the repository is
	// DIRTY, with uncommitted changes) even when it's clean (proven by using git on the command line).
	// As per my tests, the bug occurs when the repository has text files with CR or CRLF (line endings), but is probably
	// also connected to repositories with LFS and maybe others.
	// This workaround is here to cope with:
	// - https://github.com/mooltiverse/nyx/issues/130
	// - https://github.com/mooltiverse/nyx/issues/129
	// as long as the go-git library doesn't fix the bug. Bugs to keep an eye on for a fix are:
	// - https://github.com/go-git/go-git/issues/500
	// - https://github.com/go-git/go-git/issues/436
	// - https://github.com/go-git/go-git/issues/227
	// - https://github.com/go-git/go-git/issues/91
	clean := status.IsClean()
	if !clean {
		// When the repository return false (which may be wrong), double check by running the git executable.
		log.Debugf("workaround #130: go-git returned 'false' when the repository status was checked to see whether it was clean or not, this means it considers the repository in a DIRTY state. However, go-git has a bug which sometimes returns 'false' even when the Git command returns true so now the 'git' command, if available, will be executed to double check, and its output will be considered the only one reliable, overcoming the result provided by the go-git library")
		commandPath, err := exec.LookPath("git")
		if err != nil {
			log.Debugf("workaround #130: an error was returned when looking for the 'git' command in the local PATH, so the 'git' command will not be executed and the workaround cannot proceed. The error is: %v", err)
			if !workaround130WarningsEmitted {
				log.Warnf("workaround #130: the 'git' command wasn't found in the current PATH so the workaround documented at https://github.com/mooltiverse/nyx/issues/130 is disabled and the current Git repository status (CLEAN or DIRTY) may be wrong due to a bug in the underlying go-git library; disregard this message if you are not relying on the repository status in your release types configuration or you don't notice any suspect behavior that may be due to the repository status being wrongly detected")
				// make sure we emit this warning only once
				workaround130WarningsEmitted = true
			}
			return clean, nil
		}
		out := new(bytes.Buffer)
		cmd := &exec.Cmd{Path: commandPath, Dir: r.directory, Env: os.Environ(), Args: []string{"git", "status", "--porcelain"}, Stdout: out, Stderr: out}
		log.Debugf("workaround #130: running the 'git' executable '%s' in directory '%s': %s", commandPath, r.directory, cmd.String())
		err = cmd.Run()
		if err != nil {
			log.Debugf("workaround #130: an error was returned when running the 'git' command so the workaround cannot proceed. The error is: '%v' and the command output is '%s'", err, out.String())
			return clean, nil
		}
		log.Debugf("workaround #130: the 'git status' command returned (empty means the repository is clean): '%v'", out.String())
		// if the output is the empty string the repository is clean
		if "" == strings.TrimSpace(out.String()) {
			log.Debugf("workaround #130: the 'git status' command returned an empty output so the repository is clean")
			clean = true
		} else {
			log.Debugf("workaround #130: the 'git status' command returned a non-empty output so the repository is dirty")
			clean = false
		}
	}

	return clean, nil
}

/*
Pushes local changes in the current branch to the default remote origin.
This method allows using user name and password authentication (also used for tokens).

Returns the local name of the remotes that has been pushed.

Arguments are as follows:

  - user the user name to create when credentials are required. If this and password are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to create when credentials are required. If this and user are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushWithUserNameAndPassword(user *string, password *string) (string, error) {
	s := DEFAULT_REMOTE_NAME
	return r.PushToRemoteWithUserNameAndPassword(&s, user, password)
}

/*
Pushes local changes in the current branch to the default remote origin.
This method allows using SSH authentication.

Returns the local name of the remotes that has been pushed.

Arguments are as follows:

  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushWithPublicKey(privateKey *string, passphrase *string) (string, error) {
	s := DEFAULT_REMOTE_NAME
	return r.PushToRemoteWithPublicKey(&s, privateKey, passphrase)
}

/*
Pushes local changes in the current branch to the default remote origin.
This method allows using user name and password authentication (also used for tokens).

Returns the local name of the remotes that has been pushed.

Arguments are as follows:

  - remote the name of the remote to push to. If nil or empty the default remote name (origin) is used.
  - user the user name to create when credentials are required. If this and password are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to create when credentials are required. If this and user are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushToRemoteWithUserNameAndPassword(remote *string, user *string, password *string) (string, error) {
	remoteString := ""
	if remote != nil {
		remoteString = *remote
	}
	log.Debugf("pushing changes to remote repository '%s' using username and password", remoteString)

	// get the current branch name
	ref, err := r.repository.Head()
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("unable to resolve reference to HEAD"), Cause: err}
	}
	currentBranchRef := ref.Name()
	// the refspec is in the localBranch:remoteBranch form, and we assume they both have the same name here
	branchRefSpec := ggitconfig.RefSpec(currentBranchRef + ":" + currentBranchRef)
	tagsRefSpec := ggitconfig.RefSpec("refs/tags/*:refs/tags/*") // this is required to also push tags

	// the force flag may be required to update existing tags, especially when tag aliases are used
	options := &ggit.PushOptions{RemoteName: remoteString, Force: true, RefSpecs: []ggitconfig.RefSpec{branchRefSpec, tagsRefSpec}}
	auth := getBasicAuth(user, password)
	if auth != nil {
		log.Debugf("username and password authentication will use custom authentication options")
		options.Auth = auth
	} else {
		log.Debugf("username and password authentication will not use any custom authentication options")
	}

	err = r.repository.Push(options)
	if err != nil {
		if err == ggit.NoErrAlreadyUpToDate {
			log.Debugf("remote repository was already up-to-date")
		} else {
			return "", &errs.GitError{Message: fmt.Sprintf("an error occurred when trying to push"), Cause: err}
		}
	}
	return remoteString, nil
}

/*
Pushes local changes in the current branch to the default remote origin.
This method allows using SSH authentication.

Returns the local name of the remotes that has been pushed.

Arguments are as follows:

  - remote the name of the remote to push to. If nil or empty the default remote name (origin) is used.
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushToRemoteWithPublicKey(remote *string, privateKey *string, passphrase *string) (string, error) {
	remoteString := ""
	if remote != nil {
		remoteString = *remote
	}
	log.Debugf("pushing changes to remote repository '%s' using public key (SSH) authentication", remoteString)

	// get the current branch name
	ref, err := r.repository.Head()
	if err != nil {
		return "", &errs.GitError{Message: fmt.Sprintf("unable to resolve reference to HEAD"), Cause: err}
	}
	currentBranchRef := ref.Name()
	// the refspec is in the localBranch:remoteBranch form, and we assume they both have the same name here
	branchRefSpec := ggitconfig.RefSpec(currentBranchRef + ":" + currentBranchRef)
	tagsRefSpec := ggitconfig.RefSpec("refs/tags/*:refs/tags/*") // this is required to also push tags

	// the force flag may be required to update existing tags, especially when tag aliases are used
	options := &ggit.PushOptions{RemoteName: remoteString, Force: true, RefSpecs: []ggitconfig.RefSpec{branchRefSpec, tagsRefSpec}}
	auth := getPublicKeyAuth(privateKey, passphrase)
	if auth != nil {
		log.Debugf("public key (SSH) authentication will use custom authentication options")
		options.Auth = auth
	} else {
		log.Debugf("public key (SSH) authentication will not use any custom authentication options")
	}

	err = r.repository.Push(options)
	if err != nil {
		if err == ggit.NoErrAlreadyUpToDate {
			log.Debugf("remote repository was already up-to-date")
		} else {
			return "", &errs.GitError{Message: fmt.Sprintf("an error occurred when trying to push"), Cause: err}
		}
	}
	return remoteString, nil
}

/*
Pushes local changes in the current branch to the given remotes.
This method allows using user name and password authentication (also used for tokens).

Returns a collection with the local names of remotes that have been pushed.

Arguments are as follows:

  - remotes remotes the names of remotes to push to. If nil or empty the default remote name (origin) is used.
  - user the user name to create when credentials are required. If this and password are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to create when credentials are required. If this and user are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushToRemotesWithUserNameAndPassword(remotes []string, user *string, password *string) ([]string, error) {
	log.Debugf("pushing changes to '%d' remote repositories using username and password", len(remotes))
	var res []string
	for _, remote := range remotes {
		r, err := r.PushToRemoteWithUserNameAndPassword(&remote, user, password)
		if err != nil {
			return nil, err
		}
		res = append(res, r)
	}
	return res, nil
}

/*
Pushes local changes in the current branch to the given remotes.
This method allows using SSH authentication.

Returns a collection with the local names of remotes that have been pushed.

Arguments are as follows:

  - remotes remotes the names of remotes to push to. If nil or empty the default remote name (origin) is used.
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.

Errors can be:

- GitError in case some problem is encountered with the underlying Git repository, preventing to push.
*/
func (r goGitRepository) PushToRemotesWithPublicKey(remotes []string, privateKey *string, passphrase *string) ([]string, error) {
	log.Debugf("pushing changes to '%d' remote repositories using public key (SSH) authentication", len(remotes))
	var res []string
	for _, remote := range remotes {
		r, err := r.PushToRemoteWithPublicKey(&remote, privateKey, passphrase)
		if err != nil {
			return nil, err
		}
		res = append(res, r)
	}
	return res, nil
}

/*
Tags the latest commit in the current branch with a tag with the given name. The resulting tag is lightweight.
If the tag already exists it's updated.

Returns the object modelling the new tag that was created. Never nil.

Arguments are as follows:

- name the name of the tag. Cannot be nil

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
    (i.e. when the tag name is nil).
*/
func (r goGitRepository) Tag(name *string) (gitent.Tag, error) {
	return r.TagWithMessage(name, nil)
}

/*
Tags the latest commit in the current branch with a tag with the given name and optional message.
If the tag already exists it's updated.

Returns the object modelling the new tag that was created. Never nil.

Arguments are as follows:

  - name the name of the tag. Cannot be nil
  - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
    annotated tag

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
    (i.e. when the tag name is nil).
*/
func (r goGitRepository) TagWithMessage(name *string, message *string) (gitent.Tag, error) {
	return r.TagWithMessageAndIdentity(name, message, nil)
}

/*
Tags the latest commit in the current branch with a tag with the given name and optional message using the optional
tagger identity.
If the tag already exists it's updated.

Returns the object modelling the new tag that was created. Never nil.

Arguments are as follows:

  - name the name of the tag. Cannot be nil
  - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
    annotated tag
  - tagger the optional identity of the tagger. If nil Git defaults are used. If message is nil this is ignored.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
    (i.e. when the tag name is nil).
*/
func (r goGitRepository) TagWithMessageAndIdentity(name *string, message *string, tagger *gitent.Identity) (gitent.Tag, error) {
	return r.TagCommitWithMessageAndIdentity(nil, name, message, tagger)
}

/*
Tags the object represented by the given SHA-1 with a tag with the given name and optional message using the optional
tagger identity.
If the tag already exists it's updated.

Returns the object modelling the new tag that was created. Never nil.

Arguments are as follows:

  - target the SHA-1 identifier of the object to tag. If nil the latest commit in the current branch is tagged.
  - name the name of the tag. Cannot be nil
  - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
    annotated tag
  - tagger the optional identity of the tagger. If nil Git defaults are used. If message is nil this is ignored.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
    (i.e. when the tag name is nil).
*/
func (r goGitRepository) TagCommitWithMessageAndIdentity(target *string, name *string, message *string, tagger *gitent.Identity) (gitent.Tag, error) {
	if name == nil {
		return gitent.Tag{}, &errs.GitError{Message: fmt.Sprintf("tag name cannot be nil")}
	}

	// go-git does not support updating (forcing) existing tags so in order to update we first need to delete the previous tag
	_, err := r.repository.Tag(*name)
	if err == nil {
		// err is != nil if the tag was not found
		log.Debugf("the repository already had a tag '%s' so it will be deleted first", *name)
		err = r.repository.DeleteTag(*name)
		if err != nil {
			return gitent.Tag{}, &errs.GitError{Message: fmt.Sprintf("unable to delete Git tag '%s' for update", *name), Cause: err}
		}
	}

	log.Debugf("tagging as '%s'", *name)
	var createTagOptions *ggit.CreateTagOptions = nil
	if message != nil {
		var gTagger *ggitobject.Signature = nil
		if tagger != nil {
			gTagger = &ggitobject.Signature{Name: tagger.Name, Email: tagger.Email}
		}
		// create an annotated tag, pass a CreateTagOptions
		// when the message is nil we create a lightweight tag so CreateTagOptions needs to be nil
		createTagOptions = &ggit.CreateTagOptions{Tagger: gTagger, Message: *message}
	}
	var targetHash ggitplumbing.Hash
	if target == nil {
		commitSHA, err := r.GetLatestCommit()
		if err != nil {
			return gitent.Tag{}, &errs.GitError{Message: fmt.Sprintf("unable to get the latest commit (HEAD)"), Cause: err}
		}
		targetHash = ggitplumbing.NewHash(commitSHA)
	} else {
		targetHash = ggitplumbing.NewHash(*target)
	}
	ref, err := r.repository.CreateTag(*name, targetHash, createTagOptions)

	if err != nil {
		return gitent.Tag{}, &errs.GitError{Message: fmt.Sprintf("unable to create Git tag"), Cause: err}
	}
	return TagFrom(r.repository, *ref), nil
}

/*
Browse the repository commit history using the given visitor to inspect each commit. Commits are
evaluated in Git's natural order, from the most recent to oldest.

Arguments are as follows:

  - start the optional SHA-1 id of the commit to start from. If nil the latest commit in the
    current branch (HEAD) is used. This can be a long or abbreviated SHA-1. If this commit cannot be
    resolved within the repository a GitError is thrown.
  - end the optional SHA-1 id of the commit to end with, included. If nil the repository root
    commit is used (until the given visitor returns false). If this commit is not reachable
    from the start it will be ignored. This can be a long or abbreviated SHA-1. If this commit cannot be resolved
    within the repository a GitError is thrown.
  - visit the visitor function that will receive commit data to evaluate. If nil this method takes no action.
    The function isits a single commit and receives all of the commit simplified fields. Returns true
    to keep browsing next commits or false to stop.

Errors can be:

  - GitError in case some problem is encountered with the underlying Git repository, including when
    the repository has no commits yet or a given commit identifier cannot be resolved.
*/
func (r goGitRepository) WalkHistory(start *string, end *string, visit func(commit gitent.Commit) bool) error {
	if visit == nil {
		return nil
	}
	startString := "not defined"
	if start != nil {
		startString = *start
	}
	endString := "not defined"
	if end != nil {
		endString = *end
	}
	log.Debugf("walking commit history. Start commit boundary is '%s'. End commit boundary is '%s'", startString, endString)
	log.Debugf("upon merge commits only the first parent is considered.")

	var commit *ggitobject.Commit
	if start == nil {
		startHash, err := r.GetLatestCommit()
		if err != nil {
			return err
		}
		c, err := r.parseCommit(startHash)
		commit = &c
		if err != nil {
			return err
		}
	} else {
		c, err := r.parseCommit(*start)
		commit = &c
		if err != nil {
			return err
		}
	}
	log.Tracef("start boundary resolved to commit '%s'", commit.Hash.String())

	if end != nil {
		// make sure it can be resolved
		c, err := r.parseCommit(*end)
		endCommit := &c
		if err != nil {
			return err
		}
		log.Tracef("end boundary resolved to commit '%s'", endCommit.Hash.String())
	}

	for commit != nil {
		log.Tracef("visiting commit '%s'", commit.Hash.String())

		tags, err := r.GetCommitTags(commit.Hash.String())
		if err != nil {
			return err
		}
		visitorContinues := visit(CommitFrom(*commit, tags))

		if !visitorContinues {
			log.Debugf("commit history walk interrupted by visitor")
			break
		} else if end != nil && strings.HasPrefix(commit.Hash.String(), *end) {
			log.Debugf("commit history walk reached the end boundary '%s'", *end)
			break
		} else if len(commit.ParentHashes) == 0 {
			commit = nil
			log.Debugf("commit history walk reached the end")
			break
		} else {
			commit, err = r.repository.CommitObject(commit.ParentHashes[0]) // follow the first parent upon merge commits
			if err != nil {
				return &errs.GitError{Message: fmt.Sprintf("an error occurred while walking through commits"), Cause: err}
			}
		}
	}
	return nil
}
