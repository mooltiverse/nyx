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
	"bytes"         // https://pkg.go.dev/bytes
	"io"            // https://pkg.go.dev/io
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strings"       // https://pkg.go.dev/strings

	ggit "github.com/go-git/go-git/v5"                             // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitconfig "github.com/go-git/go-git/v5/config"                // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitplumbing "github.com/go-git/go-git/v5/plumbing"            // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitobject "github.com/go-git/go-git/v5/plumbing/object"       // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggittransport "github.com/go-git/go-git/v5/plumbing/transport" // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggithttp "github.com/go-git/go-git/v5/plumbing/transport/http" // https://pkg.go.dev/github.com/go-git/go-git/v5
	ggitssh "github.com/go-git/go-git/v5/plumbing/transport/ssh"   // https://pkg.go.dev/github.com/go-git/go-git/v5
	ssh "golang.org/x/crypto/ssh"                                  // https://pkg.go.dev/golang.org/x/crypto/ssh

	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
)

/*
A Git repository utility class used to test. This class is used to dynamically create a repository that can be used for tests
and exposes some high level methods and attributes.
*/
type Workbench struct {
	// The directory of backing Git repository. We need to store this as the Git implementation doesn't expose methods to retrieve it
	Directory string

	// The backing Git repository.
	Repository ggit.Repository
}

/*
Configures the given repository

Arguments are as follows:

  - repo the repository to configure
*/
func configureRepository(repo ggit.Repository) ggit.Repository {
	// set the repository configuration
	cfg, err := repo.Config()
	if err != nil {
		panic(err)
	}
	// the following user configuration has been added to avoid errors like "author field is required" during tests
	// in case this causes any issues it can be changed to anything or parametrized but for now this works
	cfg.User.Name = "John Doe"
	cfg.User.Email = "johndoe@example.com"
	err = repo.SetConfig(cfg)
	if err != nil {
		panic(err)
	}

	return repo
}

/*
Creates a Git repository instance.

Arguments are as follows:

  - directory the directory to create the repository in
  - bare if true the repository is initialized as a bare repository, otherwise it will have a work tree.
    If initialize is false this parameter is ignored.
  - initialize if true the repository has to be initialized, otherwise false.
*/
func newRepositoryWithInitializationAndSettings(directory string, bare bool, initialize bool) ggit.Repository {
	if initialize {
		repo, err := ggit.PlainInit(directory, bare)
		if err != nil {
			panic(err)
		}
		// set the repository configuration
		configureRepository(*repo)

		return *repo
	} else {
		repo, err := ggit.PlainOpen(directory)
		if err != nil {
			panic(err)
		}
		// set the repository configuration
		configureRepository(*repo)

		return *repo
	}
}

/*
Creates a new repository instance using the given backing git instance.

Arguments are as follows:

- dir the directory where the repository lives
- repo the backing git repository instance
*/
func newWorkbenchUsing(dir string, repo ggit.Repository) Workbench {
	return Workbench{Directory: dir, Repository: repo}
}

/*
Opens a new repository instance in the given directory.

Arguments are as follows:

  - directory directory the directory to open the repository in
*/
func NewWorkbenchIn(directory string) Workbench {
	return NewWorkbenchWithInitializationAndSettings(directory, false, false)
}

/*
Creates a Git instance on a new repository.

Arguments are as follows:

  - directory the directory to create the repository in
  - bare if true the repository is initialized as a bare repository, otherwise it will have a work tree.
    If initialize is false this parameter is ignored.
  - initialize if true the repository has to be initialized, otherwise false.
*/
func NewWorkbenchWithInitializationAndSettings(directory string, bare bool, initialize bool) Workbench {
	return newWorkbenchUsing(directory, newRepositoryWithInitializationAndSettings(directory, bare, initialize))
}

/*
Creates a new repository instance in a new temporary directory.

Arguments are as follows:

- initialize if true the new repository is also initialized
*/
func NewWorkbenchWithInitialization(initialize bool) Workbench {
	dir, err := os.MkdirTemp("", "nyx-test-workbench-")
	if err != nil {
		panic(err)
	}
	return NewWorkbenchWithInitializationAndSettings(dir, false, initialize)
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
		authBuilder, err := ggitssh.DefaultAuthBuilder("keymaster")
		if err != nil {
			return nil
		}
		return authBuilder
	}
}

/*
Clones the repository at the given URI into the given directory.
This method allows using user name and password authentication (also used for tokens).

Arguments are as follows:

  - uri the URI of the repository to clone from
  - directory the directory to clone the repository in
  - user the user name to create when credentials are required. If this and password are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
  - password the password to create when credentials are required. If this and user are both nil
    then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
    this value may be the token or something other than a token, depending on the remote provider.
*/
func CloneIntoWithUserNameAndPassword(uri string, directory string, user *string, password *string) Workbench {
	options := &ggit.CloneOptions{URL: uri}
	auth := getBasicAuth(user, password)
	if auth != nil {
		options.Auth = auth
	}

	repo, err := ggit.PlainClone(directory, false, options)
	if err != nil {
		panic(err)
	}
	return newWorkbenchUsing(directory, *repo)
}

/*
Clones the repository at the given URI into the given directory.
This method allows using SSH authentication.

Arguments are as follows:

  - uri the URI of the repository to clone from
  - directory the directory to clone the repository in
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func CloneIntoWithPublicKey(uri string, directory string, privateKey *string, passphrase *string) Workbench {
	options := &ggit.CloneOptions{URL: uri}
	auth := getPublicKeyAuth(privateKey, passphrase)
	if auth != nil {
		options.Auth = auth
	}

	repo, err := ggit.PlainClone(directory, false, options)
	if err != nil {
		panic(err)
	}
	return newWorkbenchUsing(directory, *repo)
}

/*
Adds the given number of text files to the repository root directory. Files have content but they are not staged or committed.
Returns the collection of the new files.

Arguments are as follows:

- count the number of files to add
*/
func (w Workbench) AddRandomTextWorkbenchFiles(count int) []string {
	res := make([]string, count)

	for i := 0; i <= count; i++ {
		/*wt, err := w.Repository.Worktree()
		if err != nil {
			panic(err)
		}*/
		err := os.WriteFile(w.GetWorkingDirectory()+"/"+gitutil.RandomAlphabeticString(5, 7*i)+".txt", []byte(gitutil.RandomAlphabeticString(5, 13*i)), 0644)
		if err != nil {
			panic(err)
		}
	}

	return res
}

/*
Adds the repository in the given metadata directory to the configured remotes of this repository, using the given name.
This is only suitable for local repositories to be added as remotes to other local repositories.

Arguments are as follows:

- gitDir the Git metadata directory of the repository to add as remote
- name the name to use for the new remote repository in the local one
*/
func (w Workbench) AddRemote(gitDir string, name string) {
	_, err := w.Repository.CreateRemote(&ggitconfig.RemoteConfig{Name: name, URLs: []string{gitDir}})
	if err != nil {
		panic(err)
	}
}

/*
Commits the staged files with the given commit message.

Arguments are as follows:

- message the commit message
*/
func (w Workbench) Commit(message string) ggitobject.Commit {
	worktree, err := w.Repository.Worktree()
	if err != nil {
		panic(err)
	}
	commitHash, err := worktree.Commit(message, &ggit.CommitOptions{All: false})
	if err != nil {
		panic(err)
	}
	commit, err := w.Repository.CommitObject(commitHash)
	if err != nil {
		panic(err)
	}
	return *commit
}

/*
Creates a new branch if none with the given name exists yet and checks it out.
Watch out as there must be one commit before this command runs without error because the HEAD
ref may not be initialized yet.

Arguments are as follows:

-name the branch name
*/
func (w Workbench) Checkout(name string) {
	worktree, err := w.Repository.Worktree()
	if err != nil {
		panic(err)
	}
	// try to check it out if it already exists
	err = worktree.Checkout(&ggit.CheckoutOptions{Branch: ggitplumbing.NewBranchReferenceName(name), Create: false})
	if err != nil {
		// the branch does not exist yet, create it
		err = worktree.Checkout(&ggit.CheckoutOptions{Branch: ggitplumbing.NewBranchReferenceName(name), Create: true})
		if err != nil {
			panic(err)
		}
	}
}

/*
Returns the names of all branches in the repository.
*/
func (w Workbench) GetBranches() []string {
	res := []string{}
	branchesIterator, err := w.Repository.Branches()
	if err != nil {
		panic(err)
	}

	err = branchesIterator.ForEach(func(b *ggitplumbing.Reference) error {
		// also strip the "refs/heads/" from the branch name
		res = append(res, strings.Replace(b.Name().String(), "refs/heads/", "", 1))
		return nil
	})
	if err != nil {
		panic(err)
	}

	return res
}

/*
Returns the SHA-1 of the commit the given tag points to, if any, or nil otherwise.

Arguments are as follows:

- tag the tag to search the commit for
*/
func (w Workbench) GetCommitByTag(tag string) *string {
	tagReference, err := w.Repository.Tag(tag)
	if err != nil {
		return nil
	} else {
		annotatedTag, err := w.Repository.TagObject(tagReference.Hash())
		if err == nil {
			// it's an annotated tag, annotatedTag is valid
			s := annotatedTag.Target.String()
			return &s
		} else {
			// it's a lightweight tag, annotatedTag is not valid
			s := tagReference.Hash().String()
			return &s
		}
	}
}

/*
Returns the list of all commit SHAs by querying the repository starting from HEAD.
*/
func (w Workbench) GetCommitIDs() []string {
	commits := w.GetCommits()
	res := make([]string, len(commits))
	for i, c := range commits {
		res[i] = c.Hash.String()
	}
	return res
}

/*
Returns the list of all commits by querying the repository starting from HEAD.
*/
func (w Workbench) GetCommits() []ggitobject.Commit {
	res := []ggitobject.Commit{}
	ref, err := w.Repository.Head()
	if err != nil {
		panic(err)
	}
	commit, err := w.Repository.CommitObject(ref.Hash())
	if err != nil {
		panic(err)
	}

	for commit != nil {
		res = append(res, *commit)
		if len(commit.ParentHashes) > 0 {
			commit, err = w.Repository.CommitObject(commit.ParentHashes[0]) // follow the first parent upon merge commits
			if err != nil {
				commit = nil
				panic(err)
			}
		} else {
			commit = nil
		}
	}
	return res
}

/*
Returns the name of the current branch (the long version, without stripping the refs/heads/ prefix).
*/
func (w Workbench) getCurrentBranchLong() string {
	ref, err := w.Repository.Head()
	if err != nil {
		panic(err)
	}
	return ref.Name().String()
}

/*
Returns the name of the current branch.
*/
func (w Workbench) GetCurrentBranch() string {
	return strings.Replace(w.getCurrentBranchLong(), "refs/heads/", "", 1)
}

/*
Returns the collection of regular files in the repository root directory.
*/
func (w Workbench) GetFiles() []string {
	res := []string{}
	files, err := os.ReadDir(w.Directory)
	if err != nil {
		panic(err)
	}
	for _, file := range files {
		// avoid adding the ".git" directory (and any other directory)
		if !file.IsDir() {
			res = append(res, filepath.Join(w.Directory, file.Name()))
		}
	}

	return res
}

/*
Returns the Git metadata directory for the repository
*/
func (w Workbench) GetGitDirectory() string {
	// the underlying implementation doesn't give us this value, so let's just assume it's the '.git' directory inside the working directory
	return w.GetWorkingDirectory() + "/.git"
}

/*
Returns the number of entries in the repository index.
*/
func (w Workbench) GetIndexEntryCount() int {
	count := 0
	// the underlying implementation doesn't expose this value, so let's just count the objects in the repo
	objectsIterator, err := w.Repository.Objects()
	if err != nil {
		panic(err)
	}

	err = objectsIterator.ForEach(func(o ggitobject.Object) error {
		count++
		return nil
	})
	if err != nil {
		panic(err)
	}

	return count
}

/*
Returns the last commit in the current branch.
*/
func (w Workbench) GetLastCommit() ggitobject.Commit {
	ref, err := w.Repository.Head()
	if err != nil {
		panic(err)
	}
	commit, err := w.Repository.CommitObject(ref.Hash())
	if err != nil {
		panic(err)
	}
	return *commit
}

/*
Returns the SHA-1 ID of the last commit in the current branch.
*/
func (w Workbench) GetLastCommitID() string {
	return w.GetLastCommit().Hash.String()
}

/*
Returns the root commit.
*/
func (w Workbench) GetRootCommit() ggitobject.Commit {
	ref, err := w.Repository.Head()
	if err != nil {
		panic(err)
	}
	// the Log method doesn't let us follow the firt parent, so we need to go through all commits and stop at the end
	commit, err := w.Repository.CommitObject(ref.Hash())
	if err != nil {
		panic(err)
	}
	for len(commit.ParentHashes) > 0 {
		commit, err = w.Repository.CommitObject(commit.ParentHashes[0]) // always follow the first parent, ignore others, if any
		if err != nil {
			panic(err)
		}
	}
	return *commit
}

/*
Returns the SHA-1 ID of the root commit.
*/
func (w Workbench) GetRootCommitID() string {
	return w.GetRootCommit().Hash.String()
}

/*
Return a list of tag objects, resulting from a query to the underlying repository.
Keys are tag names (without prefix) and values are the tagged objects.
*/
func (w Workbench) GetTags() map[string]string {
	res := map[string]string{}
	tagsIterator, err := w.Repository.Tags()
	if err != nil {
		panic(err)
	}
	if err := tagsIterator.ForEach(func(ref *ggitplumbing.Reference) error {
		// in order to check if the tag has this commit as target we first need to figure out if it's annotated or lightweight
		tagObject, err := w.Repository.TagObject(ref.Hash())
		switch err {
		case nil:
			// it's an annotated tag
			res[strings.Replace(ref.Name().String(), "refs/tags/", "", 1)] = tagObject.Target.String()
		case ggitplumbing.ErrObjectNotFound:
			// it's a lightweight tag
			res[strings.Replace(ref.Name().String(), "refs/tags/", "", 1)] = ref.Hash().String()
		default:
			// Some other error occurred
			panic(err)
		}
		return nil
	}); err != nil {
		panic(err)
	}
	return res
}

/*
Commits merge the contents of the given branch into the current one creating a commit with the given message.

Arguments are as follows:

- fromBranch the name of the branch to merge from
- message the commit message
*/
func (w Workbench) Merge(fromBranch string, message string) ggitobject.Commit {
	// Merge is not supported by go-git so we need to run Git as an external command for that.
	var buffer bytes.Buffer

	gitutil.RunCommand([]string{"git", "merge", "-m", message, fromBranch}, []string{}, &w.Directory, &buffer)

	// to make sure the backing repository is up to date after calling the external command let's create a new instance
	repo, err := ggit.PlainOpen(w.Directory)
	if err != nil {
		panic(err)
	}
	w.Repository = *repo

	// now return the latest commit, which is the one made by the merge
	return w.GetLastCommit()
}

/*
Adds all the local changed files to the staging area, without committing.
*/
func (w Workbench) Stage() {
	w.StageGlob(".")
}

/*
Adds files matched by the given pattern to the staging area, without committing.

Arguments are as follows:

- pattern the pattern of the files to stage (i.e. '.' to stage anything)
*/
func (w Workbench) StageGlob(pattern string) {
	worktree, err := w.Repository.Worktree()
	if err != nil {
		panic(err)
	}
	err = worktree.AddWithOptions(&ggit.AddOptions{All: false, Path: "", Glob: pattern})
	if err != nil {
		panic(err)
	}
}

/*
Tags the given object with the given name.

Arguments are as follows:

- name the tag name
- message the tag message (used only for annotated tags, otherwise can be nil)
- target the object to tag
*/
func (w Workbench) TagObject(name string, message *string, target *ggitplumbing.Hash) ggitplumbing.Reference {
	var createTagOptions *ggit.CreateTagOptions = nil
	if message != nil {
		// create an annotated tag, pass a CreateTagOptions
		// when the message is nil we create a lightweight tag so CreateTagOptions needs to be nil
		createTagOptions = &ggit.CreateTagOptions{Message: *message}
	}
	ref, err := w.Repository.CreateTag(name, *target, createTagOptions)
	if err != nil {
		panic(err)
	}
	return *ref
}

/*
*
Tags the last commit with the given name.

Arguments are as follows:

- name the tag name
- message the tag message (used only for annotated tags, otherwise can be nil)
*/
func (w Workbench) Tag(name string, message *string) ggitplumbing.Reference {
	target := w.GetLastCommit().Hash
	return w.TagObject(name, message, &target)
}

/*
Returns the working directory for the repository
*/
func (w Workbench) GetWorkingDirectory() string {
	return w.Directory
}

/*
Prints repository informations to the given output stream.

Use this method like in this example:

var buf bytes.Buffer
workbench.PrintInfo(&buf)
fmt.Println(buf.String())

Arguments are as follows:

- out the stream to print the info to
*/
func (w Workbench) PrintInfo(out io.Writer) {
	gitutil.PrintRepositoryInfo(w.Directory, out, "")
}

/*
Pushes all commits to the default remote repository using no credentials.
*/
func (w Workbench) Push() {
	w.PushWithUserNameAndPassword(nil, nil)
}

/*
Pushes all commits to the default remote repository using the given credentials.

Arguments are as follows:

- user the optional user name to use when credentials are required.
- password the optional password to use when credentials are required.
*/
func (w Workbench) PushWithUserNameAndPassword(user *string, password *string) {
	w.PushToWithCredentials("origin", user, password)
}

/*
Pushes all commits to the given remote repository using no credentials.

Arguments are as follows:

- remote the remote repository to push to.
*/
func (w Workbench) PushTo(remote string) {
	w.PushToWithCredentials(remote, nil, nil)
}

/*
Pushes all commits to the given remote repository using the given credentials.

Arguments are as follows:

- remote the remote repository to push to.
- user the optional user name to use when credentials are required.
- password the optional password to use when credentials are required.
*/
func (w Workbench) PushToWithCredentials(remote string, user *string, password *string) {
	branchRefSpec := ggitconfig.RefSpec(w.getCurrentBranchLong() + ":" + w.getCurrentBranchLong())
	tagsRefSpec := ggitconfig.RefSpec("refs/tags/*:refs/tags/*") // this is required to also push tags
	pushOptions := ggit.PushOptions{RemoteName: remote, RefSpecs: []ggitconfig.RefSpec{branchRefSpec, tagsRefSpec}, Auth: getBasicAuth(user, password)}
	err := w.Repository.Push(&pushOptions)
	if err != nil {
		panic(err)
	}
}

/*
Replaces the content of all the files in the repository created using this workbench.
*/
func (w Workbench) UpdateAllWorkbenchFiles() {
	w.UpdateWorkbenchFiles(w.GetFiles())
}

/*
Replaces the content of the given files created using this workbench with new random content.

Arguments are as follows:

- files the collection of files to update
*/
func (w Workbench) UpdateWorkbenchFiles(files []string) {
	for i, f := range files {
		err := os.WriteFile(f, []byte(gitutil.RandomAlphabeticString(5, i*23)), 0644)
		if err != nil {
			panic(err)
		}
	}
}
