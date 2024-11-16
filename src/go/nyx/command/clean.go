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

package command

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strings"       // https://pkg.go.dev/strings

	log "github.com/sirupsen/logrus" // https://github.com/Sirupsen/logrus, https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/src/go/errors"
	git "github.com/mooltiverse/nyx/src/go/nyx/git"
	stt "github.com/mooltiverse/nyx/src/go/nyx/state"
)

/*
The Clean command takes care of cleaning the release process and reverting the repository state to its initial state.

This class is not meant to be used in multi-threaded environments.
*/
type Clean struct {
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
func NewClean(state *stt.State, repository *git.Repository) (*Clean, error) {
	if state == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the State object cannot be nil")}
	}
	if repository == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the Repository object cannot be nil")}
	}
	log.Debugf("new Clean command object")

	res := &Clean{}
	res.abstractCommand.repository = repository
	res.abstractCommand.state = state
	return res, nil
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
func (c *Clean) IsUpToDate() (bool, error) {
	log.Debugf("checking whether the Clean command is up to date")

	// Check if there a State file
	stateFilePath, err := c.State().GetConfiguration().GetStateFile()
	if err != nil {
		return false, err
	}
	if stateFilePath != nil && "" != strings.TrimSpace(*stateFilePath) {
		// if the file path is relative make it relative to the configured directory
		if !filepath.IsAbs(*stateFilePath) {
			directory, err := c.State().GetConfiguration().GetDirectory()
			if err != nil {
				return false, err
			}
			stateFileAbsolutePath := filepath.Join(*directory, *stateFilePath)
			stateFilePath = &stateFileAbsolutePath
		}
		_, err := os.Stat(*stateFilePath)
		if err == nil {
			log.Debugf("the Clean command is not up to date because the state file has been configured ('%s') and is present on the file system so it can be deleted", *stateFilePath)
			return false, nil
		}
	}

	// Check if there a summary file
	summaryFilePath, err := c.State().GetConfiguration().GetSummaryFile()
	if err != nil {
		return false, err
	}
	if summaryFilePath != nil && "" != strings.TrimSpace(*summaryFilePath) {
		// if the file path is relative make it relative to the configured directory
		if !filepath.IsAbs(*summaryFilePath) {
			directory, err := c.State().GetConfiguration().GetDirectory()
			if err != nil {
				return false, err
			}
			summaryFileAbsolutePath := filepath.Join(*directory, *summaryFilePath)
			summaryFilePath = &summaryFileAbsolutePath
		}
		_, err := os.Stat(*summaryFilePath)
		if err == nil {
			log.Debugf("the Clean command is not up to date because the summary file has been configured ('%s') and is present on the file system so it can be deleted", *summaryFilePath)
			return false, nil
		}
	}

	// Check if there a Changelog file
	changelogConfiguration, err := c.State().GetConfiguration().GetChangelog()
	if err != nil {
		return false, err
	}
	if changelogConfiguration != nil {
		changelogFilePath := changelogConfiguration.GetPath()
		if changelogFilePath != nil && "" != strings.TrimSpace(*changelogFilePath) {
			// if the file path is relative make it relative to the configured directory
			if !filepath.IsAbs(*changelogFilePath) {
				directory, err := c.State().GetConfiguration().GetDirectory()
				if err != nil {
					return false, err
				}
				changeFileAbsolutePath := filepath.Join(*directory, *changelogFilePath)
				changelogFilePath = &changeFileAbsolutePath
			}
			_, err := os.Stat(*changelogFilePath)
			if err == nil {
				log.Debugf("the Clean command is not up to date because the changelog file has been configured ('%s') and is present on the file system so it can be deleted", *changelogFilePath)
				return false, nil
			}
		}
	}

	// Otherwise return true
	return true, nil
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
func (c *Clean) Run() (*stt.State, error) {
	log.Debugf("running the Clean command...")

	// Delete the state file, if any
	stateFilePath, err := c.State().GetConfiguration().GetStateFile()
	if err != nil {
		return nil, err
	}
	if stateFilePath != nil && "" != strings.TrimSpace(*stateFilePath) {
		// if the file path is relative make it relative to the configured directory
		if !filepath.IsAbs(*stateFilePath) {
			directory, err := c.State().GetConfiguration().GetDirectory()
			if err != nil {
				return nil, err
			}
			stateFileAbsolutePath := filepath.Join(*directory, *stateFilePath)
			stateFilePath = &stateFileAbsolutePath
		}
		log.Debugf("deleting state file '%s', if present", *stateFilePath)
		_, err := os.Stat(*stateFilePath)
		if err == nil {
			err = os.Remove(*stateFilePath)
			if err != nil {
				return nil, err
			}
		}
	}

	// Delete the summary file, if any
	summaryFilePath, err := c.State().GetConfiguration().GetSummaryFile()
	if err != nil {
		return nil, err
	}
	if summaryFilePath != nil && "" != strings.TrimSpace(*summaryFilePath) {
		// if the file path is relative make it relative to the configured directory
		if !filepath.IsAbs(*summaryFilePath) {
			directory, err := c.State().GetConfiguration().GetDirectory()
			if err != nil {
				return nil, err
			}
			summaryFileAbsolutePath := filepath.Join(*directory, *summaryFilePath)
			summaryFilePath = &summaryFileAbsolutePath
		}
		log.Debugf("deleting summary file '%s', if present", *summaryFilePath)
		_, err := os.Stat(*summaryFilePath)
		if err == nil {
			err = os.Remove(*summaryFilePath)
			if err != nil {
				return nil, err
			}
		}
	}

	// Delete the changelog file, if any
	changelogConfiguration, err := c.State().GetConfiguration().GetChangelog()
	if err != nil {
		return nil, err
	}
	if changelogConfiguration != nil {
		changelogFilePath := changelogConfiguration.GetPath()
		if changelogFilePath != nil && "" != strings.TrimSpace(*changelogFilePath) {
			// if the file path is relative make it relative to the configured directory
			if !filepath.IsAbs(*changelogFilePath) {
				directory, err := c.State().GetConfiguration().GetDirectory()
				if err != nil {
					return nil, err
				}
				changelogFileAbsolutePath := filepath.Join(*directory, *changelogFilePath)
				changelogFilePath = &changelogFileAbsolutePath
			}
			log.Debugf("deleting changelog file '%s', if present", *changelogFilePath)
			_, err := os.Stat(*changelogFilePath)
			if err == nil {
				err = os.Remove(*changelogFilePath)
				if err != nil {
					return nil, err
				}
			}
		}
	}

	return c.State(), nil
}
