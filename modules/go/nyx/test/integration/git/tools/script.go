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
	"path/filepath" // https://pkg.go.dev/path/filepath

	ggit "github.com/go-git/go-git/v5" // https://pkg.go.dev/github.com/go-git/go-git/v5

	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
)

/*
An utility class built on top of a test repository and providing some coarse grained commands to be used fluently.
*/
type Script struct {
	// Extend Workbench by composition
	Workbench
}

/*
Creates a new script instance using the given backing git instance.

Arguments are as follows:

- dir the directory where the repository lives
- repo the backing git repository instance
*/
func newScriptUsing(dir string, repo ggit.Repository) Script {
	script := Script{}

	script.Workbench.Directory = dir
	script.Workbench.Repository = repo

	return script
}

/*
Creates a new script instance in the given directory.

Arguments are as follows:

- directory the directory to create the repository in
*/
func NewScriptIn(directory string) Script {
	return NewScriptWithInitializationAndSettings(directory, false, false)
}

/*
Creates a Git script on a new repository.

Arguments are as follows:

  - directory the directory to create the repository in.
  - bare if true the repository is initialized as a bare repository, otherwise it will have a work tree.
    If initialize is false this parameter is ignored.
  - initialize if true the repository has to be initialized, otherwise false.
*/
func NewScriptWithInitializationAndSettings(directory string, bare bool, initialize bool) Script {
	return newScriptUsing(directory, newRepositoryWithInitializationAndSettings(directory, bare, initialize))
}

/*
Returns a new script instance for a repository cloned from the given URI in a new temporary directory.
This method allows using user name and password authentication (also used for tokens).

Arguments are as follows:

- uri the URI to clone from
- user the optional user name to use when credentials are required.
- password the optional password to use when credentials are required.
*/
func CloneFromWithUserNameAndPassword(uri string, user *string, password *string) Script {
	prefix := "nyx-test-script-"
	directory := gitutil.NewTempDirectory("", &prefix)
	return CloneFromToWithUserNameAndPassword(uri, directory, user, password)
}

/*
Returns a new script instance for a repository cloned from the given URI in a new temporary directory.
This method allows using SSH authentication.

Arguments are as follows:

  - uri the URI to clone from
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func CloneFromWithPublicKey(uri string, privateKey *string, passphrase *string) Script {
	prefix := "nyx-test-script-"
	directory := gitutil.NewTempDirectory("", &prefix)
	return CloneFromToWithPublicKey(uri, directory, privateKey, passphrase)
}

/*
Returns a new script instance for a repository cloned from the given URI in the given directory.
This method allows using user name and password authentication (also used for tokens).

Arguments are as follows:

- uri the URI to clone from
- directory the directory to clone to. It must exist and be empty
- user the optional user name to use when credentials are required.
- password the optional password to use when credentials are required.
*/
func CloneFromToWithUserNameAndPassword(uri string, directory string, user *string, password *string) Script {
	CloneIntoWithUserNameAndPassword(uri, directory, user, password)
	return NewScriptIn(directory)
}

/*
Returns a new script instance for a repository cloned from the given URI in the given directory.
This method allows using SSH authentication.

Arguments are as follows:

  - uri the URI to clone from
  - directory the directory to clone to. It must exist and be empty
  - privateKey the SSH private key. If nil the private key will be searched in its default location
    (i.e. in the users' $HOME/.ssh directory).
  - passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
    This is required when the private key is password protected as this implementation does not support prompting
    the user interactively for entering the password.
*/
func CloneFromToWithPublicKey(uri string, directory string, privateKey *string, passphrase *string) Script {
	CloneIntoWithPublicKey(uri, directory, privateKey, passphrase)
	return NewScriptIn(directory)
}

/*
Returns a new script instance for a new bare repository to be created and initialized in the given directory

Arguments are as follows:

- directory the working directory for the script. It must exist and be empty
*/
func BareIn(directory string) Script {
	return NewScriptWithInitializationAndSettings(directory, true, true)
}

/*
Returns a new script instance for a new bare repository to be created and initialized in a new temporary directory
*/
func Bare() Script {
	prefix := "nyx-test-script-"
	return NewScriptWithInitializationAndSettings(gitutil.NewTempDirectory("", &prefix), true, true)
}

/*
Returns a new script instance working in the given directory

Arguments are as follows:

- directory the working directory for the script. It must exist and already have a Git repository in it
*/
func From(directory string) Script {
	return NewScriptIn(directory)
}

/*
Returns a new script instance for a new repository to be created and initialized in the given directory

Arguments are as follows:

- directory the working directory for the script. It must exist and be empty
*/
func FromScratchIn(directory string) Script {
	return NewScriptWithInitializationAndSettings(directory, false, true)
}

/*
Returns a new script instance for a new repository to be created and initialized in a new temporary directory
*/
func FromScratch() Script {
	prefix := "nyx-test-script-"
	return NewScriptWithInitializationAndSettings(gitutil.NewTempDirectory("", &prefix), false, true)
}

/*
Adds some files (one) to the repository. Files have content but they are not staged or committed.
*/
func (s Script) AndAddFiles() Script {
	return s.AndAddNFiles(1)
}

/*
Adds the given number of files to the repository. Files have content but they are not staged or committed.

Arguments are as follows:

- count the number of files to add
*/
func (s Script) AndAddNFiles(count int) Script {
	s.AddRandomTextWorkbenchFiles(count)
	return s
}

/*
Changes the contents of all files in the repository. Changes are not staged or committed.
*/
func (s Script) AndUpdateFiles() Script {
	s.UpdateAllWorkbenchFiles()
	return s
}

/*
Adds the modified contents to the sraging area.
*/
func (s Script) AndStage() Script {
	s.Stage()
	return s
}

/*
Commits the staged changes with a random commit message
*/
func (s Script) AndCommit() Script {
	return s.AndCommitWith(nil)
}

/*
Commits the staged changes with the given commit message

Arguments are as follows:

- message the commit message. If nil a random commit message is generated
*/
func (s Script) AndCommitWith(message *string) Script {
	if message == nil {
		s.Commit("Commit " + gitutil.RandomAlphabeticString(3, 27))
	} else {
		s.Commit(*message)
	}
	return s
}

/*
Tags the latest commit with the given name. The tag is a lightweight tag unless the given message is not nil,
in which case the message is used for the annotation.

Arguments are as follows:

  - name the tag value
  - message the optional tag message, if nil no message is applied and the tag is lightweight,
    otherwise it's an annotated tag
*/
func (s Script) AndTag(name string, message *string) Script {
	hash := s.GetLastCommit().Hash
	s.TagObject(name, message, &hash)
	return s
}

/*
Moves to the given branch and creates it if it doesn't exist.

Arguments are as follows:

- name the name of the branch to checkout
*/
func (s Script) InBranch(name string) Script {
	s.Checkout(name)
	return s
}

/*
Adds a batch of operations to the git repository in the current branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a lightweight tag on the commit.

Arguments are as follows:

- tagName the tag name to create on the commit
*/
func (s Script) AndCommitWithTag(tagName string) Script {
	return s.AndCommitWithTagNameAndMessage(tagName, nil)
}

/*
Adds a batch of operations to the git repository in the current branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a tag on the commit. The tag is lightweight if tagMessage is nil, otherwise
it is annotated.

Arguments are as follows:

  - tagName the tag name to create on the commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndCommitWithTagNameAndMessage(tagName string, tagMessage *string) Script {
	return s.AndCommitWithMessageAndTagNameAndMessage(nil, tagName, tagMessage)
}

/*
Adds a batch of operations to the git repository in the current branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a tag on the commit. The tag is lightweight if tagMessage is nil, otherwise
it is annotated.

Arguments are as follows:

  - commitMessage the commit message. If nil a random message is generated
  - tagName the tag name to create on the commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndCommitWithMessageAndTagNameAndMessage(commitMessage *string, tagName string, tagMessage *string) Script {
	if len(s.GetFiles()) == 0 {
		s.AndAddFiles()
	}
	return s.AndStage().AndCommitWith(commitMessage).AndTag(tagName, tagMessage)
}

/*
Adds a batch of operations to the git repository in the given branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a lightweight tag on the commit.

Arguments are as follows:

- branchName the name of the branch to create the commit in
- tagName the tag name to create on the commit
*/
func (s Script) AndCommitWithTagInBranch(branchName string, tagName string) Script {
	s.Checkout(branchName)
	return s.AndCommitWithTag(tagName)
}

/*
Adds a batch of operations to the git repository in the given branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a tag on the commit. The tag is lightweight if tagMessage is nil, otherwise
it is annotated.

Arguments are as follows:

  - branchName the name of the branch to create the commit in
  - tagName the tag name to create on the commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndCommitWithTagMessageInBranch(branchName string, tagName string, tagMessage *string) Script {
	return s.AndCommitWithMessageAndTagMessageInBranch(branchName, nil, tagName, tagMessage)
}

/*
Adds a batch of operations to the git repository in the given branch.
A batch is made of change to all the files in the repository (new files are added if there is none), a commit
and a tag on the commit. The tag is lightweight if tagMessage is nil, otherwise
it is annotated.

Arguments are as follows:

  - branchName the name of the branch to create the commit in
  - commitMessage the commit message. If nil a random message is generated
  - tagName the tag name to create on the commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndCommitWithMessageAndTagMessageInBranch(branchName string, commitMessage *string, tagName string, tagMessage *string) Script {
	s.Checkout(branchName)
	return s.AndCommitWithMessageAndTagNameAndMessage(commitMessage, tagName, tagMessage)
}

/*
Commits merge the contents of the given branch into the current one creating a commit with a random message.

Arguments are as follows:

- fromBranch the name of the branch to merge from
*/
func (s Script) AndMergeFrom(fromBranch string) Script {
	return s.AndMergeFromWithMessage(fromBranch, nil)
}

/*
Commits merge the contents of the given branch into the current one creating a commit with the given message.

Arguments are as follows:

- fromBranch the name of the branch to merge from
- commitMessage the commit message. If nil a random message is generated
*/
func (s Script) AndMergeFromWithMessage(fromBranch string, commitMessage *string) Script {
	if commitMessage == nil {
		s.Merge(fromBranch, "Merge "+gitutil.RandomAlphabeticString(3, 31))
	} else {
		s.Merge(fromBranch, *commitMessage)
	}
	return s
}

/*
Commits merge the contents of the given branch into the current one creating a commit with the given message.

Arguments are as follows:

- fromBranch the name of the branch to merge from
- commitMessage the commit message. If nil a random message is generated
- tagName the name of the tag to apply to the merge commit
*/
func (s Script) AndMergeFromWithMessageAndTag(fromBranch string, commitMessage *string, tagName string) Script {
	return s.AndMergeFromWithMessageAndTagMessage(fromBranch, commitMessage, tagName, nil)
}

/*
Commits merge the contents of the given branch into the current one creating a commit with the given message.

Arguments are as follows:

  - fromBranch the name of the branch to merge from
  - commitMessage the commit message. If nil a random message is generated
  - tagName the name of the tag to apply to the merge commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndMergeFromWithMessageAndTagMessage(fromBranch string, commitMessage *string, tagName string, tagMessage *string) Script {
	if commitMessage == nil {
		s.Merge(fromBranch, "Merge "+gitutil.RandomAlphabeticString(3, 31))
	} else {
		s.Merge(fromBranch, *commitMessage)
	}
	s.Tag(tagName, tagMessage)
	return s
}

/*
Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
returns the target branch is the current one.

Arguments are as follows:

- toBranch the name of the branch to merge to, it will also be the current branch after the commit
*/
func (s Script) AndMergeInto(toBranch string) Script {
	message := "Merge " + gitutil.RandomAlphabeticString(3, 37)
	return s.AndMergeIntoWithMessage(toBranch, &message)
}

/*
Commits merge the contents of the current branch into the given one creating a commit with the given message. When this method
returns the target branch is the current one.

Arguments are as follows:

- toBranch the name of the branch to merge to, it will also be the current branch after the commit
- commitMessage the commit message. If nil a random message is generated
*/
func (s Script) AndMergeIntoWithMessage(toBranch string, commitMessage *string) Script {
	return s.AndMergeFromIntoWithMessage(toBranch, s.GetCurrentBranch(), commitMessage)
}

/*
Commits merge the contents of the given branch into the target one creating a commit with the given message. When this method
returns the target branch is the current one.

Arguments are as follows:

- toBranch the name of the branch to merge to, it will also be the current branch after the commit
- fromBranch the name of the branch to merge from
- commitMessage the commit message. If nil a random message is generated
*/
func (s Script) AndMergeFromIntoWithMessage(toBranch string, fromBranch string, commitMessage *string) Script {
	s.Checkout(toBranch)
	return s.AndMergeFromWithMessage(fromBranch, commitMessage)
}

/*
Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
returns the target branch is the current one.

Arguments are as follows:

- toBranch the name of the branch to merge to, it will also be the current branch after the commit
- tagName the name of the tag to apply to the merge commit
*/
func (s Script) AndMergeIntoWithTag(toBranch string, tagName string) Script {
	return s.AndMergeIntoWitTagMessage(toBranch, tagName, nil)
}

/*
Commits merge the contents of the current branch into the given one creating a commit with a random message. When this method
returns the target branch is the current one.

Arguments are as follows:

  - toBranch the name of the branch to merge to, it will also be the current branch after the commit
  - tagName the name of the tag to apply to the merge commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndMergeIntoWitTagMessage(toBranch string, tagName string, tagMessage *string) Script {
	message := "Merge " + gitutil.RandomAlphabeticString(3, 51)
	s.AndMergeIntoWithMessage(toBranch, &message)
	s.Tag(tagName, tagMessage)
	return s
}

/*
Commits merge the contents of the given branch into the target one creating a commit with the given message. When this method
returns the target branch is the current one.

Arguments are as follows:

  - toBranch the name of the branch to merge to, it will also be the current branch after the commit
  - fromBranch the name of the branch to merge from
  - commitMessage the commit message. If nil a random message is generated
  - tagName the name of the tag to apply to the merge commit
  - tagMessage the tag message to create on the commit, if not nil makes the tag annotated
    otherwise the tag will be lightweight
*/
func (s Script) AndMergeFromIntoWithMessageAndTagMessage(toBranch string, fromBranch string, commitMessage *string, tagName string, tagMessage *string) Script {
	s.Checkout(toBranch)
	s.AndMergeFromWithMessage(fromBranch, commitMessage)
	s.Tag(tagName, tagMessage)
	return s
}

/*
Adds the given lines to the .gitignore file. The file is created if doesn't exist yet.

Arguments are as follows:

- items the lines to write to the .gitignore file.
*/
func (s Script) AndIgnore(items ...string) Script {
	gitutil.AppendLines(filepath.Join(s.Workbench.Directory, ".gitignore"), items...)
	return s
}
