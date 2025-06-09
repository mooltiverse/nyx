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

package command

import (
	"fmt"     // https://pkg.go.dev/fmt
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/src/go/errors"
	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	git "github.com/mooltiverse/nyx/src/go/nyx/git"
	stt "github.com/mooltiverse/nyx/src/go/nyx/state"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

const (
	// The common prefix used for all the internal state attributes managed by this class.
	MARK_INTERNAL_ATTRIBUTE_PREFIX = "mark"

	// The common prefix used for all the internal state attributes managed by this class, representing an input.
	MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX = MARK_INTERNAL_ATTRIBUTE_PREFIX + "." + "input"

	// The common prefix used for all the internal state attributes managed by this class, representing an output.
	MARK_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX = MARK_INTERNAL_ATTRIBUTE_PREFIX + "." + "output"

	// The name used for the internal state attribute where we store the commit flag.
	MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "git" + "." + "commit"

	// The name used for the internal state attribute where we store the push flag.
	MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "git" + "." + "push"

	// The name used for the internal state attribute where we store the tag flag.
	MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "configured" + "." + "git" + "." + "tag"

	// The name used for the internal state attribute where we store current branch name.
	MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "current" + "." + "branch"

	// The name used for the internal state attribute where we store the SHA-1 of the last commit in the current branch by the time this command was last executed.
	MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "last" + "." + "commit"

	// The name used for the internal state attribute where we store the initial commit.
	MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "initialCommit"

	// The flag telling if the current version is new.
	MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "newVersion"

	// The name used for the internal state attribute where we store the version.
	MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION = MARK_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "version"

	// The name used for the internal state attribute where we store the last commit created by this command.
	MARK_INTERNAL_OUPUT_ATTRIBUTE_COMMIT = MARK_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX + "." + "commit"
)

/*
The Mark command takes care of tagging and committing into the Git repository.

This class is not meant to be used in multi-threaded environments.
*/
type Mark struct {
	// Extend abstractCommand by composition
	abstractCommand
}

/*
Standard constructor.

Arguments are as follows:

- state the state reference
- repository the repository reference

Error is:

- NilPointerError: if a given argument is nil
*/
func NewMark(state *stt.State, repository *git.Repository) (*Mark, error) {
	if state == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the State object cannot be nil")}
	}
	if repository == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the Repository object cannot be nil")}
	}
	log.Debugf("new Mark command object")

	res := &Mark{}
	res.abstractCommand.repository = repository
	res.abstractCommand.state = state
	return res, nil
}

/*
Commits pending changes to the Git repository, applies a release tags and pushes changes to remotes.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Mark) commit() error {
	clean, err := (*c.Repository()).IsClean()
	if err != nil {
		return err
	}
	if clean {
		log.Debugf("repository is clean, no commits need to be made")
	} else {
		dryRun, err := c.State().GetConfiguration().GetDryRun()
		if err != nil {
			return err
		}
		if *dryRun {
			log.Infof("Git commit skipped due to dry run")
		} else {
			log.Debugf("committing local changes")

			releaseType, err := c.State().GetReleaseType()
			if err != nil {
				return err
			}
			commitMessage, err := c.renderTemplate(releaseType.GetGitCommitMessage())
			if err != nil {
				return err
			}
			if commitMessage == nil || "" == strings.TrimSpace(*commitMessage) {
				log.Debugf("the configured commit message template yields to an empty commit message. Using default template '%s'", *ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE)
				commitMessage, err = c.renderTemplate(ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE)
				if err != nil {
					return err
				}
			}

			// Here we commit all uncommitted files (of course if they're not ignored by .gitignore). Should we pick a specific subset instead? Maybe among the artifacts produced by Nyx?
			// Here we can also specify the Author and Committer Identity as per https://github.com/mooltiverse/nyx/issues/65
			finalCommit, err := (*c.Repository()).CommitPathsWithMessage([]string{"."}, commitMessage)
			if err != nil {
				return err
			}
			log.Debugf("local changes committed at '%s'", finalCommit.GetSHA())
			commitSHA := finalCommit.GetSHA()
			c.putInternalAttribute(MARK_INTERNAL_OUPUT_ATTRIBUTE_COMMIT, &commitSHA)

			log.Debugf("adding commit '%s' to the release scope", finalCommit.GetSHA())
			releaseScope, err := c.State().GetReleaseScope()
			if err != nil {
				return err
			}

			releaseCommits := releaseScope.GetCommits()
			releaseCommits = append(releaseCommits, &finalCommit)
			releaseScope.SetCommits(releaseCommits)
		}
	}
	return nil
}

/*
Applies release tags to the latest commit.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Mark) tag() error {
	dryRun, err := c.State().GetConfiguration().GetDryRun()
	if err != nil {
		return err
	}
	if *dryRun {
		log.Infof("Git tag skipped due to dry run")
	} else {
		releaseType, err := c.State().GetReleaseType()
		if err != nil {
			return err
		}
		tagMessage, err := c.renderTemplate(releaseType.GetGitTagMessage())
		if err != nil {
			return err
		}
		latestCommit, err := (*c.Repository()).GetLatestCommit()
		if err != nil {
			return err
		}
		if releaseType.GetGitTagNames() == nil || len(*releaseType.GetGitTagNames()) == 0 {
			log.Debugf("no tag name has been configured for this release type so no tag is applied")
		} else {
			for _, tagTemplate := range *releaseType.GetGitTagNames() {
				tag, err := c.renderTemplate(tagTemplate)
				if err != nil {
					return err
				}
				forceFlag, err := c.renderTemplateAsBoolean(releaseType.GetGitTagForce())
				if err != nil {
					return err
				}

				log.Tracef("tag template '%s' renders to '%s'", *tagTemplate, *tag)
				log.Debugf("tag force flag is '%t'", forceFlag)
				log.Debugf("tagging latest commit '%s' with tag '%s'", latestCommit, *tag)
				if tagMessage != nil && "" == strings.TrimSpace(*tagMessage) {
					tagMessage = nil
				}
				// Here we can also specify the Tagger Identity as per https://github.com/mooltiverse/nyx/issues/65
				gitTag, err := (*c.Repository()).TagWithMessageAndForce(tag, tagMessage, forceFlag)
				if err != nil {
					return err
				}

				log.Debugf("tag '%s' applied to commit '%s'", gitTag.Name, latestCommit)
			}
		}
	}
	return nil
}

/*
Pushes changes to remotes.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Mark) push() error {
	dryRun, err := c.State().GetConfiguration().GetDryRun()
	if err != nil {
		return err
	}
	if *dryRun {
		log.Infof("Git push skipped due to dry run")
	} else {
		log.Debugf("pushing local changes to remotes")
		releaseType, err := c.State().GetReleaseType()
		if err != nil {
			return err
		}
		releaseTypes, err := c.State().GetConfiguration().GetReleaseTypes()
		if err != nil {
			return err
		}
		remotes := releaseTypes.GetRemoteRepositories()
		if remotes == nil || len(*remotes) == 0 {
			log.Debugf("the list of remotes is not defined. Using the default remote '%s'", git.DEFAULT_REMOTE_NAME)
			remotes = &[]*string{utl.PointerToString(git.DEFAULT_REMOTE_NAME)}
		}
		for _, remote := range *remotes {
			log.Debugf("pushing local changes to remote '%s'", *remote)

			// Now we need to find the credentials by going through all the configured remotes and finding
			// the one that supports the target remote.
			log.Debugf("looking up credentials for remote '%s'", *remote)
			var authenticationMethod *ent.AuthenticationMethod
			var user *string
			var password *string
			var privateKey *string
			var passphrase *string
			gitConfiguration, err := c.State().GetConfiguration().GetGit()
			if err != nil {
				return err
			}
			if gitConfiguration == nil || gitConfiguration.GetRemotes() == nil {
				log.Debugf("no Git remote repository has been configured")
			} else {
				gitRemoteConfiguration, ok := (*gitConfiguration.GetRemotes())[*remote]
				if ok {
					log.Debugf("using configured credentials for remote '%s'", *remote)
					authenticationMethod = gitRemoteConfiguration.GetAuthenticationMethod()
					user, err = c.renderTemplate(gitRemoteConfiguration.GetUser())
					if err != nil {
						return err
					}
					password, err = c.renderTemplate(gitRemoteConfiguration.GetPassword())
					if err != nil {
						return err
					}
					privateKey, err = c.renderTemplate(gitRemoteConfiguration.GetPrivateKey())
					if err != nil {
						return err
					}
					passphrase, err = c.renderTemplate(gitRemoteConfiguration.GetPassphrase())
					if err != nil {
						return err
					}
				} else {
					log.Debugf("no configuration available for remote '%s'", *remote)
				}
			}

			// finally push
			forceFlag, err := c.renderTemplateAsBoolean(releaseType.GetGitPushForce())
			if err != nil {
				return err
			}
			log.Debugf("push force flag is '%t'", forceFlag)
			if authenticationMethod != nil && ent.PUBLIC_KEY == *authenticationMethod {
				log.Debugf("attempting push to '%s' using public key credentials.", *remote)

				_, err = (*c.Repository()).PushToRemoteWithPublicKeyAndForce(remote, privateKey, passphrase, forceFlag)
				if err != nil {
					return err
				}
			} else {
				if user == nil && password == nil {
					log.Debugf("no credentials were configured for remote '%s'. Attempting anonymous push.", *remote)
				} else {
					log.Debugf("attempting push to '%s' using user name and password credentials.", *remote)
				}

				_, err = (*c.Repository()).PushToRemoteWithUserNameAndPasswordAndForce(remote, user, password, forceFlag)
				if err != nil {
					return err
				}
			}

			log.Debugf("local changes pushed to remote '%s'", *remote)
		}
	}
	return nil
}

/*
This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
of the IsUpToDate() method can find them and determine if the command is already up to date.

This method is meant to be invoked at the end of a successful Run().

	Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Mark) storeStatusInternalAttributes() error {
	log.Debugf("storing the Mark command internal attributes to the State")
	dryRun, err := c.State().GetConfiguration().GetDryRun()
	if err != nil {
		return err
	}
	if !*dryRun {
		releaseType, err := c.State().GetReleaseType()
		if err != nil {
			return err
		}
		if releaseType == nil {
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT, nil)
			if err != nil {
				return err
			}
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH, nil)
			if err != nil {
				return err
			}
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG, nil)
			if err != nil {
				return err
			}
		} else {
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT, releaseType.GetGitCommit())
			if err != nil {
				return err
			}
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH, releaseType.GetGitPush())
			if err != nil {
				return err
			}
			err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG, releaseType.GetGitTag())
			if err != nil {
				return err
			}
		}
		currentBranch, err := c.getCurrentBranch()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
		if err != nil {
			return err
		}
		latestCommit, err := c.getLatestCommit()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
		var initialCommitSHA *string
		releaseScope, err := c.State().GetReleaseScope()
		if err != nil {
			return err
		}
		if releaseScope != nil {
			initialCommit := releaseScope.GetInitialCommit()
			if initialCommit != nil {
				initialCommitSHAString := initialCommit.GetSHA()
				initialCommitSHA = &initialCommitSHAString
			}
		}
		err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, initialCommitSHA)
		if err != nil {
			return err
		}
		newVersion, err := c.State().GetNewVersion()
		if err != nil {
			return err
		}
		newVersionString := strconv.FormatBool(newVersion)
		err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, &newVersionString)
		if err != nil {
			return err
		}
		version, err := c.State().GetVersion()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, version)
		if err != nil {
			return err
		}
	}
	return nil
}

/*
Returns true if this command is up to date, which means that the internal State would not
change by running the command again. It other words, when this method returns true any
invocation of the Run method is needless and idempotent about the state.

This method uses the quickest method to verify whether the state is up to date or not. This method must not rely on
dependencies and it must always evaluate its own status independently.

As a general rule this method checks if its inputs (i.e. from the configuration) have changed since the last run.

Error is:
- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Mark) IsUpToDate() (bool, error) {
	// Never up to date if this command hasn't stored a version yet into the state or the stored version is different than the state version
	version, err := c.State().GetVersion()
	if err != nil {
		return false, err
	}
	isStateVersionUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, version)
	if err != nil {
		return false, err
	}
	if version == nil || !isStateVersionUpTodate {
		log.Debugf("the Mark command is not up to date because the internal state has no version yet or the state version doesn't match the version previously generated by Mark")
		return false, nil
	}

	// The command is never considered up to date when the repository branch or last commit has changed
	currentBranch, err := c.getCurrentBranch()
	if err != nil {
		return false, err
	}
	isCurrentBranchUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
	if err != nil {
		return false, err
	}
	latestCommit, err := c.getLatestCommit()
	if err != nil {
		return false, err
	}
	isLatestCommitUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
	if err != nil {
		return false, err
	}
	if !isCurrentBranchUpTodate || !isLatestCommitUpTodate {
		log.Debugf("the Mark command is not up to date because the last commit or the current branch has changed")
		return false, nil
	}

	// The command is never considered up to date when the commit, tag or push configurantion flags have changed
	releaseType, err := c.State().GetReleaseType()
	if err != nil {
		return false, err
	}
	isGitCommitUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT, releaseType.GetGitCommit())
	if err != nil {
		return false, err
	}
	isGitPushUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH, releaseType.GetGitPush())
	if err != nil {
		return false, err
	}
	isGitTagUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG, releaseType.GetGitTag())
	if err != nil {
		return false, err
	}
	if !isGitCommitUpTodate || !isGitPushUpTodate || !isGitTagUpTodate {
		log.Debugf("the Mark command is not up to date because the configuration of Git flags has changed")
		return false, nil
	}

	// Check if configuration parameters have changed
	var initialCommitSHA *string
	releaseScope, err := c.State().GetReleaseScope()
	if err != nil {
		return false, err
	}
	if releaseScope != nil {
		initialCommit := releaseScope.GetInitialCommit()
		if initialCommit != nil {
			initialCommitSHAString := initialCommit.GetSHA()
			initialCommitSHA = &initialCommitSHAString
		}
	}
	isInitialCommitUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, initialCommitSHA)
	if err != nil {
		return false, err
	}
	newVersion, err := c.State().GetNewVersion()
	if err != nil {
		return false, err
	}
	newVersionString := strconv.FormatBool(newVersion)
	isNewVersionUpTodate, err := c.isInternalAttributeUpToDate(MARK_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, &newVersionString)
	if err != nil {
		return false, err
	}
	res := isInitialCommitUpTodate && isNewVersionUpTodate
	if res {
		log.Debugf("the Mark command is up to date")
	} else {
		log.Debugf("the Mark command is not up to date because the configuration or the internal state has changed")
	}
	return res, nil
}

/*
Runs the command and returns the updated reference to the state object. In order to improve performances you should only
invoke this method when IsUpToDate returns false.

Error is:
- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Mark) Run() (*stt.State, error) {
	log.Debugf("running the Mark command...")

	newVersion, err := c.State().GetNewVersion()
	if err != nil {
		return nil, err
	}
	if newVersion {
		if c.State().HasReleaseType() {
			releaseType, err := c.State().GetReleaseType()
			if err != nil {
				return nil, err
			}
			// COMMIT
			doCommit, err := c.renderTemplateAsBoolean(releaseType.GetGitCommit())
			if err != nil {
				return nil, err
			}
			if doCommit {
				log.Debugf("the release type has the git commit flag enabled")
				err = c.commit()
				if err != nil {
					return nil, err
				}
			} else {
				log.Debugf("the release type has the git commit flag disabled")
			}

			// TAG
			doTag, err := c.renderTemplateAsBoolean(releaseType.GetGitTag())
			if err != nil {
				return nil, err
			}
			if doTag {
				log.Debugf("the release type has the git tag flag enabled")
				err = c.tag()
				if err != nil {
					return nil, err
				}
			} else {
				log.Debugf("the release type has the git tag flag disabled")
			}

			// PUSH
			doPush, err := c.renderTemplateAsBoolean(releaseType.GetGitPush())
			if err != nil {
				return nil, err
			}
			if doPush {
				log.Debugf("the release type has the git push flag enabled")
				err = c.push()
				if err != nil {
					return nil, err
				}
			} else {
				log.Debugf("the release type has the git push flag disabled")
			}
		} else {
			log.Warnf("no release type available. Nothing to release.")
		}
	} else {
		log.Debugf("no version change detected. Nothing to release.")
	}

	err = c.storeStatusInternalAttributes()
	if err != nil {
		return nil, err
	}

	return c.State(), nil
}
