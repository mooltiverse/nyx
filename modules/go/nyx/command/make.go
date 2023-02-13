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
	_ "embed"       // https://pkg.go.dev/embed
	"fmt"           // https://pkg.go.dev/fmt
	"io/ioutil"     // https://pkg.go.dev/io/ioutil
	"net/http"      // https://pkg.go.dev/net/http
	"net/url"       // https://pkg.go.dev/net/url
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strconv"       // https://pkg.go.dev/strconv
	"strings"       // https://pkg.go.dev/strings
	"time"          // https://pkg.go.dev/time

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"     // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	tpl "github.com/mooltiverse/nyx/modules/go/nyx/template"
)

const (
	// The name of the resource to load for the default template.
	DEFAULT_TEMPLATE_RESOURCE_NAME = "changelog.tpl"

	// The common prefix used for all the internal state attributes managed by this class.
	MAKE_INTERNAL_ATTRIBUTE_PREFIX = "make"

	// The common prefix used for all the internal state attributes managed by this class, representing an input.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX = MAKE_INTERNAL_ATTRIBUTE_PREFIX + "." + "input"

	// The name used for the internal state attribute where we store the path to the changelog file.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "changelog" + "." + "file"

	// The name used for the internal state attribute where we store current branch name.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "current" + "." + "branch"

	// The name used for the internal state attribute where we store the SHA-1 of the last
	// commit in the current branch by the time this command was last executed.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "repository" + "." + "last" + "." + "commit"

	// The name used for the internal state attribute where we store the initial commit.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "initialCommit"

	// The flag telling if the current version is new.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "newVersion"

	// The name used for the internal state attribute where we store the version.
	MAKE_INTERNAL_INPUT_ATTRIBUTE_VERSION = MAKE_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "version"
)

var (
	// The default template is embedded at compile time and available in this variable.
	//go:embed template/changelog.tpl
	defaultTemplate string
)

/*
The Make command takes care of building the release artifacts.

This class is not meant to be used in multi-threaded environments.
*/
type Make struct {
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
func NewMake(state *stt.State, repository *git.Repository) (*Make, error) {
	if state == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the State object cannot be nil")}
	}
	if repository == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the Repository object cannot be nil")}
	}
	log.Debugf("new Make command object")

	res := &Make{}
	res.abstractCommand.repository = repository
	res.abstractCommand.state = state
	return res, nil
}

/*
Returns the reference to the configured changelog file, if configured, or nil
of no destination file has been set by the configuration.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
*/
func (c *Make) getChangelogFile() (*string, error) {
	changelogConfiguration, err := c.State().GetConfiguration().GetChangelog()
	if err != nil {
		return nil, err
	}
	if changelogConfiguration == nil || changelogConfiguration.GetPath() == nil || "" == strings.TrimSpace(*changelogConfiguration.GetPath()) {
		return nil, nil
	}

	changelogFile := *changelogConfiguration.GetPath()
	// if the file path is relative make it relative to the configured directory
	if !filepath.IsAbs(changelogFile) {
		configurationDirectory, err := c.State().GetConfiguration().GetDirectory()
		if err != nil {
			return nil, err
		}
		if configurationDirectory != nil {
			changelogFile = filepath.Join(*configurationDirectory, changelogFile)
		}
	}

	return &changelogFile, nil
}

/*
Returns string containing the template to be used for rendering.
If the configuration overrides the template then the reader will use to that
template otherwise the default template will be returned.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
*/
func (c *Make) getChangelogTemplate() (string, error) {
	changelogConfiguration, err := c.State().GetConfiguration().GetChangelog()
	if err != nil {
		return "", err
	}
	if changelogConfiguration == nil || changelogConfiguration.GetTemplate() == nil || "" == strings.TrimSpace(*changelogConfiguration.GetTemplate()) {
		log.Debugf("the changelog template has not been overridden by configuration. Loading the default template.")
		// pick the embedded standard template
		return defaultTemplate, nil
	} else {
		templatePath := *changelogConfiguration.GetTemplate()
		templateURL, err := url.Parse(templatePath)
		// The URL parses for local paths also, so to distinguish between a local path and an actual URL we also check for the Host part
		if err == nil && "" != strings.TrimSpace(templateURL.Host) {
			// try loading the file as an URL
			response, err := http.Get(templateURL.String())
			if err != nil {
				return "", &errs.DataAccessError{Message: fmt.Sprintf("unable to load the configured changelog template file from URL '%s'", templatePath), Cause: err}
			}
			defer response.Body.Close()
			templateBytes, err := ioutil.ReadAll(response.Body)
			if err != nil {
				return "", &errs.DataAccessError{Message: fmt.Sprintf("unable to load the configured changelog template file from URL '%s'", templatePath), Cause: err}
			}
			return string(templateBytes), nil
		} else {
			// it's a local file, not an URL, so load it as such
			templateBytes, err := os.ReadFile(templatePath)
			if err != nil {
				return "", &errs.DataAccessError{Message: fmt.Sprintf("unable to load the configured changelog template file from '%s'", templatePath), Cause: err}
			}
			return string(templateBytes), nil
		}
	}
}

/*
Builds the changelog assets.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Make) buildChangelog() error {
	// The destination path is also used as a flag to enable or disable the changelog generation, so if it's not configured the changelog is not generated
	changelogFile, err := c.getChangelogFile()
	if err != nil {
		return err
	}

	newVersion, err := c.State().GetNewVersion()
	if err != nil {
		return err
	}
	if changelogFile == nil {
		log.Debugf("changelog has not been configured or it has no destination path. Skipping the changelog generation.")
	} else if !newVersion {
		log.Debugf("no new version has been inferred so the changelog generation will be skipped.")
	} else {
		log.Debugf("building the changelog data model")

		changelog := ent.NewChangelog()
		err = c.State().SetChangelog(changelog)
		if err != nil {
			return err
		}

		// As of this version we can just pick the commits from the current release scope but in future versions,
		// when we need to have the 'Unreleased' fictional release plus, optionally, older releases,
		// (as per https://github.com/mooltiverse/nyx/issues/79) we'll need to walk the commit history just like the
		// Infer command does.

		// Create the timestamp string
		timestamp, err := c.State().GetTimestamp()
		if err != nil {
			return err
		}
		date := time.UnixMilli(*timestamp).UTC()
		dateString := date.Format("2006-01-02")

		// As of now we just have one release: the one being issued
		version, err := c.State().GetVersion()
		if err != nil {
			return err
		}
		release := ent.NewReleaseWith(version, &dateString)
		changelog.SetReleases([]*ent.Release{release})

		// As of now we can just pick the commits from the current release scope.
		// We just need to distribute the commits among sections, which means filtering and translating
		// the sections if the user has configured them, or just use the commit 'type's as section names
		// if the user didn't map the sections
		releaseScope, err := c.State().GetReleaseScope()
		if err != nil {
			return err
		}
		changelogConfiguration, err := c.State().GetConfiguration().GetChangelog()
		if err != nil {
			return err
		}
		for _, commit := range releaseScope.GetCommits() {
			// Now we need to infer the commit type by using the commit message conventions
			var commitType *string
			commitMessageConventions, err := c.State().GetConfiguration().GetCommitMessageConventions()
			if err != nil {
				return err
			}
			if commitMessageConventions.GetItems() != nil {
				log.Debugf("trying to infer the commit type based on the commit message of commit '%s'", commit.GetSHA())
				for cmcEntryKey, cmcEntryValue := range *commitMessageConventions.GetItems() {
					log.Debugf("evaluating commit '%s' against message convention '%s'", commit.GetSHA(), cmcEntryKey)
					re, err := regexp2.Compile(*cmcEntryValue.GetExpression(), 0)
					if err != nil {
						return &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot compile regular expression '%s'", *cmcEntryValue.GetExpression()), Cause: err}
					}
					match, err := re.FindStringMatch(commit.GetMessage().GetFullMessage())
					if err != nil {
						return &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot evaluate regular expression '%s' against '%s'", *cmcEntryValue.GetExpression(), commit.GetMessage().GetFullMessage()), Cause: err}
					}
					if match != nil {
						commitTypeGroup := match.GroupByName("type")
						if commitTypeGroup != nil && len(commitTypeGroup.Captures) > 0 {
							commitTypeString := commitTypeGroup.Captures[0].String()
							commitType = &commitTypeString
							log.Debugf("the type of commit '%s' is '%s'", commit.GetSHA(), *commitType)
						}
					}
					if commitType == nil {
						log.Debugf("the commit type cannot be inferred for commit '%s' using the regular expression '%s' from commit message convention '%s'", commit.GetSHA(), *cmcEntryValue.GetExpression(), cmcEntryKey)
					}
				}
			}
			if commitType == nil || "" == strings.TrimSpace(*commitType) {
				log.Debugf("no commit message convention has been configured or the configured commit message conventions do not allow to infer the 'type' for commit '%s'. The commit will not appear in the changelog.", commit.GetSHA())
			} else {
				// If the user has defined some sections mapping we need to map the commit type to those sections,
				// otherwise the section will be the commit type
				if changelogConfiguration.GetSections() == nil || len(*changelogConfiguration.GetSections()) == 0 {
					log.Debugf("changelog sections haven't been defined by user. Commit '%s' will appear in section '%s' (same as the commit type)", commit.GetSHA(), *commitType)
					releaseCommits := release.GetSection(*commitType, true).GetCommits()
					commitCopy := commit // avoid appending the same item by creating a copy of the item
					releaseCommits = append(releaseCommits, commitCopy)
					release.GetSection(*commitType, true).SetCommits(releaseCommits)
				} else {
					for sectionEntryKey, sectionEntryValue := range *changelogConfiguration.GetSections() {
						log.Debugf("evaluating commit type '%s' against changelog section '%s'", *commitType, sectionEntryKey)
						re, err := regexp2.Compile(sectionEntryValue, 0)
						if err != nil {
							return &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot compile regular expression '%s'", sectionEntryValue), Cause: err}
						}
						match, err := re.MatchString(*commitType)
						if err != nil {
							return &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot evaluate regular expression '%s' against '%s'", sectionEntryValue, *commitType), Cause: err}
						}
						if match {
							log.Debugf("expression '%s' for section '%s' successfully matches type '%s' so commit '%s' will appear under the '%s' section", sectionEntryValue, sectionEntryKey, *commitType, commit.GetSHA(), sectionEntryKey)
							releaseCommits := release.GetSection(sectionEntryKey, true).GetCommits()
							commitCopy := commit // avoid appending the same item by creating a copy of the item
							releaseCommits = append(releaseCommits, commitCopy)
							release.GetSection(sectionEntryKey, true).SetCommits(releaseCommits)

							break
						} else {
							log.Debugf("expression '%s' for section '%s' does not match type '%s'. Trying with next sections, if any.", sectionEntryValue, sectionEntryKey, *commitType)
							continue
						}
					}
				}
			}
		}

		dryRun, err := c.State().GetConfiguration().GetDryRun()
		if err != nil {
			return err
		}
		log.Debugf("rendering the changelog")
		if *dryRun {
			log.Infof("changelog rendering skipped due to dry run")
		} else {
			template, err := c.getChangelogTemplate()
			if err != nil {
				return err
			}

			changelogBuffer, err := tpl.Render(template, changelog)
			if err != nil {
				return &errs.DataAccessError{Message: fmt.Sprintf("unable to render the changelog to file '%s'. Make sure the path to the file exists and can be written.", *changelogFile), Cause: err}
			}

			// if substitutions have been defined, let's apply them
			if changelogConfiguration.GetSubstitutions() != nil {
				log.Debugf("applying configured substitutions to the changelog")
				for substitutionEntryKey, substitutionEntryValue := range *changelogConfiguration.GetSubstitutions() {
					re, err := regexp2.Compile(substitutionEntryKey, 0)
					if err != nil {
						return &errs.IllegalPropertyError{Message: fmt.Sprintf("cannot compile regular expression '%s'", substitutionEntryKey), Cause: err}
					}
					substitutionMatcher, err := re.FindStringMatch(changelogBuffer)
					if err != nil {
						return &errs.IllegalPropertyError{Message: fmt.Sprintf("regular expression '%s' can't be matched", substitutionEntryKey), Cause: err}
					}
					for substitutionMatcher != nil {
						substitutionMatcherGroupOldStringToBeReplaced := substitutionMatcher.GroupByNumber(0) //
						substitutionMatcherGroupValueToUseInReplacement := substitutionMatcher.GroupByNumber(1)
						if substitutionMatcherGroupOldStringToBeReplaced != nil && len(substitutionMatcherGroupOldStringToBeReplaced.Captures) > 0 && substitutionMatcherGroupValueToUseInReplacement != nil && len(substitutionMatcherGroupValueToUseInReplacement.Captures) > 0 {
							oldStringToBeReplaced := substitutionMatcherGroupOldStringToBeReplaced.Captures[0].String()
							valueToUseInReplacement := substitutionMatcherGroupValueToUseInReplacement.Captures[0].String()
							var newString string
							// we need to detect the exact number of occurrences of '%s' and pass the Sprintf parameter that exact number of times
							// otherwise the resulting string will contain an error like %!!(MISSING)(EXTRA string=a, string=a, string=a)
							occurrences := strings.Count(substitutionEntryValue, "%s")
							replacementOccurrences := make([]any, occurrences)
							for i := 0; i < occurrences; i++ {
								replacementOccurrences[i] = valueToUseInReplacement
							}
							newString = fmt.Sprintf(substitutionEntryValue, replacementOccurrences[0:occurrences]...)
							changelogBuffer = strings.ReplaceAll(changelogBuffer, oldStringToBeReplaced, newString)
							substitutionMatcher, err = re.FindNextMatch(substitutionMatcher)
							if err != nil {
								return &errs.IllegalPropertyError{Message: fmt.Sprintf("regular expression '%s' can't be matched", substitutionEntryKey), Cause: err}
							}
						} else {
							substitutionMatcher = nil
						}
					}
				}
				log.Debugf("configured substitutions have been applied to the changelog")
			}

			// now write to the actual destination file
			err = os.WriteFile(*changelogFile, []byte(changelogBuffer), 0644)
			if err != nil {
				return &errs.DataAccessError{Message: fmt.Sprintf("unable to render the changelog to file '%s'. Make sure the path to the file exists and can be written.", *changelogFile), Cause: err}
			}

			log.Debugf("the changelog has been saved to '%s'", *changelogFile)
		}
	}
	return nil
}

/*
Builds the configured assets.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Make) buildAssets() error {
	// The only asset to build is the changelog
	return c.buildChangelog()
}

/*
Reset the attributes store by this command into the internal state object.
This is required before running the command in order to make sure that the new execution is not affected by a stale status coming from previous runs.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
*/
func (c *Make) clearStateOutputAttributes() error {
	log.Debugf("clearing the state from Make outputs")
	err := c.State().SetChangelog(nil)
	return err
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
func (c *Make) storeStatusInternalAttributes() error {
	log.Debugf("Storing the Make command internal attributes to the State")
	dryRun, err := c.State().GetConfiguration().GetDryRun()
	if err != nil {
		return err
	}
	if !*dryRun {
		changelogFile, err := c.getChangelogFile()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE, changelogFile)
		if err != nil {
			return err
		}
		currentBranch, err := c.getCurrentBranch()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
		if err != nil {
			return err
		}
		latestCommit, err := c.getLatestCommit()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
		if err != nil {
			return err
		}
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
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, initialCommitSHA)
		if err != nil {
			return err
		}
		newVersion, err := c.State().GetNewVersion()
		if err != nil {
			return err
		}
		newVersionString := strconv.FormatBool(newVersion)
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, &newVersionString)
		if err != nil {
			return err
		}
		version, err := c.State().GetVersion()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(MAKE_INTERNAL_INPUT_ATTRIBUTE_VERSION, version)
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
func (c *Make) IsUpToDate() (bool, error) {
	// Never up to date if this command hasn't stored a version yet into the state
	version, err := c.State().GetVersion()
	if err != nil {
		return false, err
	}
	if version == nil {
		log.Debugf("the Make command is not up to date because the internal state has no version yet")
		return false, nil
	}

	newVersion, err := c.State().GetNewVersion()
	if err != nil {
		return false, err
	}
	if newVersion {
		changelogFile, err := c.getChangelogFile()
		if err != nil {
			return false, err
		}
		if changelogFile != nil {
			changelog, err := c.State().GetChangelog()
			if err != nil {
				return false, err
			}
			// The command is never considered up to date when the configuration requires a changelog file but the state has no such object reference
			if changelog == nil {
				log.Debugf("the Make command is not up to date because a changelog file has been configured ('%s') but the internal state has no changelog yet", *changelogFile)
				return false, nil
			}
			isChangelogFileUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_CHANGELOG_FILE, changelogFile)
			if err != nil {
				return false, err
			}
			// The command is never considered up to date when the changelog file hasn't been saved yet or it has changed
			if !isChangelogFileUpTodate {
				log.Debugf("the Make command is not up to date because a changelog file has been configured ('%s') but the configured path has changed", *changelogFile)
				return false, nil
			}
			_, err = os.Stat(*changelogFile)
			if err != nil {
				log.Debugf("the Make command is not up to date because a changelog file has been configured ('%s') but the file does not exist", *changelogFile)
				return false, nil
			}
		}
	} else {
		log.Debugf("no new version has been generated so up-to-date checks for the Make command in regards to the changelog file are skipped")
	}

	// The command is never considered up to date when the repository branch or last commit has changed
	currentBranch, err := c.getCurrentBranch()
	if err != nil {
		return false, err
	}
	isCurrentBranchUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, &currentBranch)
	if err != nil {
		return false, err
	}
	latestCommit, err := c.getLatestCommit()
	if err != nil {
		return false, err
	}
	isLatestCommitUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, &latestCommit)
	if err != nil {
		return false, err
	}
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
	isInitialCommitUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, initialCommitSHA)
	if err != nil {
		return false, err
	}
	if !isCurrentBranchUpTodate || !isLatestCommitUpTodate || !isInitialCommitUpTodate {
		log.Debugf("the Make command is not up to date because the range of commits or the current branch has changed")
		return false, nil
	}

	// Check if configuration parameters have changed
	isStateVersionUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_VERSION, version)
	if err != nil {
		return false, err
	}
	newVersionString := strconv.FormatBool(newVersion)
	isNewVersionUpTodate, err := c.isInternalAttributeUpToDate(MAKE_INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, &newVersionString)
	if err != nil {
		return false, err
	}
	res := isStateVersionUpTodate && isNewVersionUpTodate
	if res {
		log.Debugf("the Make command is up to date")
	} else {
		log.Debugf("the Make command is not up to date because the configuration or the internal state has changed")
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
func (c *Make) Run() (*stt.State, error) {
	log.Debugf("Running the Make command...")

	err := c.clearStateOutputAttributes()
	if err != nil {
		return nil, err
	}

	err = c.buildAssets()
	if err != nil {
		return nil, err
	}

	err = c.storeStatusInternalAttributes()
	if err != nil {
		return nil, err
	}

	return c.State(), nil
}
