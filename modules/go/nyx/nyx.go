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
This is the main package for Nyx, also providing the entry point.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package nyx

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	io "github.com/mooltiverse/nyx/modules/go/nyx/io"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
)

/*
The Nyx entry point and main class.

This class is not thread safe.
*/
type Nyx struct {
	// The internal configuration object.
	//
	// This object is lazily initialized so in order to make sure you get a valid reference you should always use
	// Configuration() instead of reading this member.
	config *cnf.Configuration

	// The internal Git repository object.
	//
	// This object is lazily initialized so in order to make sure you get a valid reference you should always use
	// repository() instead of reading this member.
	repository *git.Repository

	// The internal state object.
	//
	// This object is lazily initialized so in order to make sure you get a valid reference you should always use
	// State() instead of reading this member.
	state *stt.State

	// This map stores instances of commands so that they can be reused.
	//
	// Instances are lazily created and stored here.
	commands map[string]*cmd.Command
}

/*
Default constructor.
*/
func NewNyx() *Nyx {
	log.Trace("new Nyx instance")

	// this is not actually needed for production but some tests may leave the default directory dirty, so this is safer
	cnf.SetDefaultDirectory(nil)

	res := &Nyx{}
	res.commands = make(map[string]*cmd.Command)
	return res
}

/*
Creates a new Nyx instance using the given directory as the base directory.

Arguments are as follows:

- directory the default directory. If not nil this overrides the configuration directory
*/
func NewNyxIn(directory string) *Nyx {
	log.Tracef("new Nyx instance in directory '%s'", directory)

	// this is not actually needed for production but some tests may leave the default directory dirty, so this is safer
	cnf.SetDefaultDirectory(&directory)

	res := &Nyx{}
	res.commands = make(map[string]*cmd.Command)
	return res
}

/*
Returns the configuration.

Error is:
- DataAccessError: in case an option cannot be read or accessed.
- IllegalPropertyError: in case an option has been defined but has incorrect values or it can't be resolved.
*/
func (n *Nyx) Configuration() (*cnf.Configuration, error) {
	if n.config == nil {
		log.Debug("instantiating the initial configuration")
		configuration, err := cnf.NewConfiguration()
		if err != nil {
			return nil, err
		}
		n.config = configuration
	}
	return n.config, nil
}

/*
Returns the repository.

Error is:
- DataAccessError: in case an option cannot be read or accessed.
- IllegalPropertyError: in case an option has been defined but has incorrect values or it can't be resolved.
- GitError: in case of unexpected issues when accessing the Git repository.
*/
func (n *Nyx) Repository() (*git.Repository, error) {
	if n.repository == nil {
		configuration, err := n.Configuration()
		if err != nil {
			return nil, err
		}
		repoDir, err := configuration.GetDirectory()
		if err != nil {
			return nil, err
		}
		log.Debugf("instantiating the Git repository in '%s'", *repoDir)
		repository, err := git.GitInstance().Open(*repoDir)
		if err != nil {
			return nil, err
		}
		n.repository = &repository
	}
	return n.repository, nil
}

/*
Returns the state. The state may be created from scratch or loaded from a previously saved file, if the configuration says so.

Error is:
- DataAccessError: in case an option cannot be read or accessed.
- IllegalPropertyError: in case an option has been defined but has incorrect values or it can't be resolved.
*/
func (n *Nyx) State() (*stt.State, error) {
	if n.state == nil {
		configuration, err := n.Configuration()
		if err != nil {
			return nil, err
		}

		resume, err := configuration.GetResume()
		if err != nil {
			return nil, err
		}
		if resume != nil && *resume {
			stateFile, err := configuration.GetStateFile()
			if err != nil {
				return nil, err
			}
			if stateFile == nil {
				log.Warning("the resume flag has been set but no state file has been configured. The state file will not be resumed.")
			} else {
				// if the file path is relative make it relative to the configured directory
				if !filepath.IsAbs(*stateFile) {
					directory, err := configuration.GetDirectory()
					if err != nil {
						return nil, err
					}
					stateFileAbsolutePath := filepath.Join(*directory, *stateFile)
					stateFile = &stateFileAbsolutePath
				}
				_, err = os.Stat(*stateFile)
				if err == nil {
					log.Debugf("resuming the state from file '%s'", *stateFile)
					state, err := stt.Resume(*stateFile, configuration)
					if err != nil {
						return nil, err
					}
					n.state = state
				} else {
					log.Warningf("the resume flag has been set and the state file has been configured but no state file exists at the given location %s. The state file will not be resumed.", *stateFile)
				}
			}
		}

		// if the state was not resumed from a file, instantiate a new one
		if n.state == nil {
			log.Debug("instantiating the initial state")
			state, err := stt.NewStateWith(configuration)
			n.state = state
			return n.state, err
		}
	}
	return n.state, nil
}

/*
Creates a command instance from the given command identifier.

Arguments are as follows:

- command the identifier of the command to create the instance for

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
*/
func (n *Nyx) newCommandInstance(command cmd.Commands) (*cmd.Command, error) {
	log.Debugf("creating new command instance '%s'", command.String())
	repository, err := n.Repository()
	if err != nil {
		return nil, err
	}
	state, err := n.State()
	if err != nil {
		return nil, err
	}
	var res cmd.Command
	switch command {
	case cmd.CLEAN:
		res, err = cmd.NewClean(state, repository)
		if err != nil {
			return nil, err
		}
		return &res, nil
	case cmd.INFER:
		res, err = cmd.NewInfer(state, repository)
		if err != nil {
			return nil, err
		}
		return &res, nil
	case cmd.MAKE:
		res, err = cmd.NewMake(state, repository)
		if err != nil {
			return nil, err
		}
		return &res, nil
	case cmd.MARK:
		res, err = cmd.NewMark(state, repository)
		if err != nil {
			return nil, err
		}
		return &res, nil
	case cmd.PUBLISH:
		res, err = cmd.NewPublish(state, repository)
		if err != nil {
			return nil, err
		}
		return &res, nil
	default:
		// this is never reached, but in case...
		return nil, &errs.IllegalArgumentError{Message: fmt.Sprintf("unknown command '%s'", command.String())}
	}
}

/*
Gets a command instance. If the command is already in the internal cache that instance is returned,
otherwise a new instance is created and stored for later use, before it's returned.

Arguments are as follows:

- command the command

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
*/
func (n *Nyx) getCommandInstance(command cmd.Commands) (*cmd.Command, error) {
	log.Debugf("looking up command instance '%s' from cache", command.String())
	res, ok := n.commands[command.String()]
	if ok {
		log.Debugf("command instance '%s' found in cache", command.String())
		return res, nil
	} else {
		log.Debugf("no command instance '%s' found in cache", command.String())
	}

	res, err := n.newCommandInstance(command)
	if err != nil {
		return nil, err
	}
	n.commands[command.String()] = res
	return res, nil
}

/*
Runs the given command through its Run() method.

Arguments are as follows:

- command the command
- saveState a boolean that, when true saves the State to the configured state file if not nil

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) runCommand(command cmd.Commands, saveState bool) error {
	log.Debugf("running command '%s'", command.String())
	commandInstance, err := n.getCommandInstance(command)
	if err != nil {
		return err
	}
	isUpToDate, err := (*commandInstance).IsUpToDate()
	if err != nil {
		return err
	}
	if isUpToDate {
		log.Debugf("command '%s' is up to date, skipping.", command.String())
	} else {
		log.Debugf("command '%s' is not up to date, running...", command.String())
		_, err := (*commandInstance).Run()
		if err != nil {
			return err
		}
		log.Debugf("command '%s' finished.", command.String())

		// optionally save the state file
		configuration, err := n.Configuration()
		if err != nil {
			return err
		}
		stateFile, err := configuration.GetStateFile()
		if err != nil {
			return err
		}
		if saveState && stateFile != nil {
			log.Debugf("storing the state to '%s'", *stateFile)
			state, err := n.State()
			if err != nil {
				return err
			}
			err = io.Save(*stateFile, state)
			if err != nil {
				return err
			}
			log.Debugf("state stored to to '%s'", *stateFile)
		}
	}
	return nil
}

/*
Runs true if the given command has already run and is up to date, false otherwise.

Arguments are as follows:

- command the command to query the status for

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
*/
func (n *Nyx) IsUpToDate(command cmd.Commands) (bool, error) {
	commandInstance, ok := n.commands[command.String()]
	if ok && commandInstance != nil {
		res, err := (*commandInstance).IsUpToDate()
		if err != nil {
			return false, err
		}
		log.Debugf("checking if command '%s' is up to date: '%v'", command.String(), res)
		return res, nil
	} else {
		log.Debugf("checking if command '%s' is up to date: '%v'", command.String(), false)
		return false, nil
	}
}

/*
Runs the given command.

Arguments are as follows:

- command the identifier of the command to run.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Run(command cmd.Commands) error {
	log.Debugf("Nyx.run(%s)", command.String())
	switch command {
	case cmd.CLEAN:
		return n.Clean()
	case cmd.INFER:
		_, err := n.Infer()
		return err
	case cmd.MAKE:
		_, err := n.Make()
		return err
	case cmd.MARK:
		_, err := n.Mark()
		return err
	case cmd.PUBLISH:
		_, err := n.Publish()
		return err
	default:
		// this is never reached, but in case...
		return &errs.IllegalArgumentError{Message: fmt.Sprintf("unknown command '%s'", command.String())}
	}
}

/*
Runs the Clean command to restore the state of the workspace to ints initial state.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Clean() error {
	log.Debugf("Nyx.clean()")

	// this command has no dependencies

	// run the command
	return n.runCommand(cmd.CLEAN, false)
}

/*
Runs the Infer command and returns the updated state.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Infer() (*stt.State, error) {
	log.Debugf("Nyx.infer()")

	// this command has no dependencies

	// run the command
	err := n.runCommand(cmd.INFER, true)
	if err != nil {
		return nil, err
	}

	return n.State()
}

/*
Runs the Make command and returns the updated state.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Make() (*stt.State, error) {
	log.Debugf("Nyx.make()")

	// run dependent tasks first
	_, err := n.Infer()
	if err != nil {
		return nil, err
	}

	// run the command
	err = n.runCommand(cmd.MAKE, true)
	if err != nil {
		return nil, err
	}

	return n.State()
}

/*
Runs the Mark command and returns the updated state.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Mark() (*stt.State, error) {
	log.Debugf("Nyx.mark()")

	// run dependent tasks first
	_, err := n.Make()
	if err != nil {
		return nil, err
	}

	// run the command
	err = n.runCommand(cmd.MARK, true)
	if err != nil {
		return nil, err
	}

	return n.State()
}

/*
Runs the Publish command and returns the updated state. Dependencies of this command are also executed first.

Error is:
- DataAccessError: in case the configuration can't be loaded for some reason.
- IllegalPropertyError: in case the configuration has some illegal options.
- GitError: in case of unexpected issues when accessing the Git repository.
- ReleaseError: if the task is unable to complete for reasons due to the release process.
*/
func (n *Nyx) Publish() (*stt.State, error) {
	log.Debugf("Nyx.publish()")

	// run dependent tasks first
	_, err := n.Mark()
	if err != nil {
		return nil, err
	}

	// run the command
	err = n.runCommand(cmd.PUBLISH, true)
	if err != nil {
		return nil, err
	}

	return n.State()
}
