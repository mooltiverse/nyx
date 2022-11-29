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

/*
This is the Git package for Nyx, encapsulating the underlying Git implementation.
*/
package git

import (
	ggit "github.com/go-git/go-git/v5" // https://pkg.go.dev/github.com/go-git/go-git/v5

	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
)

const (
	// The default remote name.
	DEFAULT_REMOTE_NAME = ggit.DefaultRemoteName
)

/*
This interface models coarse grained, implementation independent methods used by Nyx to access a Git repository.
*/
type Repository interface {
	/*
	   Arguments are as follows:

	   - paths the file patterns of the contents to add to stage. Cannot be nil or empty. The path "." represents
	     all files in the working area so with that you can add all locally changed files.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to add paths.
	*/
	Add(paths []string) error

	/*
	   Commits changes to the repository. Files to commit must be staged separately using Add.

	   - message the commit message. Cannot be nil.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to commit.
	*/
	CommitWithMessage(message *string) (gitent.Commit, error)

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
	CommitWithMessageAndIdentities(message *string, author *gitent.Identity, committer *gitent.Identity) (gitent.Commit, error)

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
	CommitPathsWithMessage(paths []string, message *string) (gitent.Commit, error)

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
	CommitPathsWithMessageAndIdentities(paths []string, message *string, author *gitent.Identity, committer *gitent.Identity) (gitent.Commit, error)

	/*
	   Returns a set of abjects representing all the tags for the given commit.

	   Arguments are as follows:

	   - commit the SHA-1 identifier of the commit to get the tags for. It can be a full or abbreviated SHA-1.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository.
	*/
	GetCommitTags(commit string) ([]gitent.Tag, error)

	/*
	   Returns the name of the current branch or a commit SHA-1 if the repository is in the detached head state.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, including when
	     the repository has no commits yet or is in the 'detached HEAD' state.
	*/
	GetCurrentBranch() (string, error)

	/*
	   Returns the SHA-1 identifier of the last commit in the current branch.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, including when
	     the repository has no commits yet or is in the 'detached HEAD' state.
	*/
	GetLatestCommit() (string, error)

	/*
	   Returns the names of configured remote repositories.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, including when
	     the repository has no commits yet or is in the 'detached HEAD' state.
	*/
	GetRemoteNames() ([]string, error)

	/*
	   Returns the SHA-1 identifier of the first commit in the repository (the only commit with no parents).

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, including when
	     the repository has no commits yet or is in the 'detached HEAD' state.
	*/
	GetRootCommit() (string, error)

	/*
	   Returns true if the repository is clean, which is when no differences exist between the working tree, the index,
	   and the current HEAD.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, including when
	     the repository has no commits yet or is in the 'detached HEAD' state.
	*/
	IsClean() (bool, error)

	/*
	   Pushes local changes in the current branch to the default remote origin.

	   Returns the local name of the remotes that has been pushed

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to push.
	*/
	Push() (string, error)

	/*
	   Pushes local changes in the current branch to the default remote origin.

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
	PushWithCredentials(user *string, password *string) (string, error)

	/*
	   Pushes local changes in the current branch to the given remote.

	   Returns the local name of the remotes that has been pushed.

	   Arguments are as follows:

	   - remote the name of the remote to push to. If nil or empty the default remote name (origin) is used.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to push.
	*/
	PushToRemote(remote *string) (string, error)

	/*
	   Pushes local changes in the current branch to the default remote origin.

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
	PushToRemoteWithCredentials(remote *string, user *string, password *string) (string, error)

	/*
	   Pushes local changes in the current branch to the given remotes.

	   Returns a collection with the local names of remotes that have been pushed.

	   Arguments are as follows:

	   - remotes the names of remotes to push to. If nil or empty the default remote name (origin) is used.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to push.
	*/
	PushToRemotes(remotes []string) ([]string, error)

	/*
	   Pushes local changes in the current branch to the given remotes.

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
	PushToRemotesWithCredentials(remotes []string, user *string, password *string) ([]string, error)

	/*
	   Tags the latest commit in the current branch with a tag with the given name. The resulting tag is lightweight.

	   Returns the object modelling the new tag that was created. Never nil.

	   Arguments are as follows:

	   - name the name of the tag. Cannot be nil

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
	     (i.e. when the tag name is nil or there is already a tag with the given name in the repository).
	*/
	Tag(name *string) (gitent.Tag, error)

	/*
	   Tags the latest commit in the current branch with a tag with the given name and optional message.

	   Returns the object modelling the new tag that was created. Never nil.

	   Arguments are as follows:

	   - name the name of the tag. Cannot be nil
	   - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
	     annotated tag

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
	     (i.e. when the tag name is nil or there is already a tag with the given name in the repository).
	*/
	TagWithMessage(name *string, message *string) (gitent.Tag, error)

	/*
	   Tags the latest commit in the current branch with a tag with the given name and optional message using the optional
	   tagger identity.

	   Returns the object modelling the new tag that was created. Never nil.

	   Arguments are as follows:

	   - name the name of the tag. Cannot be nil
	   - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
	     annotated tag
	   - tagger the optional identity of the tagger. If nil Git defaults are used. If message is nil this is ignored.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
	     (i.e. when the tag name is nil or there is already a tag with the given name in the repository).
	*/
	TagWithMessageAndIdentity(name *string, message *string, tagger *gitent.Identity) (gitent.Tag, error)

	/*
	   Tags the object represented by the given SHA-1 with a tag with the given name and optional message using the optional
	   tagger identity.

	   Returns the object modelling the new tag that was created. Never nil.

	   Arguments are as follows:

	   - target the SHA-1 identifier of the object to tag. If nil the latest commit in the current branch is tagged.
	   - name the name of the tag. Cannot be nil
	   - message the optional tag message. If nil the new tag will be lightweight, otherwise it will be an
	     annotated tag
	   - tagger the optional identity of the tagger. If nil Git defaults are used. If message is nil this is ignored.

	   Errors can be:

	   - GitError in case some problem is encountered with the underlying Git repository, preventing to tag
	     (i.e. when the tag name is nil or there is already a tag with the given name in the repository).
	*/
	TagCommitWithMessageAndIdentity(target *string, name *string, message *string, tagger *gitent.Identity) (gitent.Tag, error)

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
	WalkHistory(start *string, end *string, visit func(commit gitent.Commit) bool) error
}
