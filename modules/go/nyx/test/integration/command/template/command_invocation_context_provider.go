//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package command_template

import (
	"errors" // https://pkg.go.dev/errors
	"fmt"    // https://pkg.go.dev/fmt

	nyx "github.com/mooltiverse/nyx/modules/go/nyx"
	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
)

/*
Returns a new command instance for the given command selector, using a Git repository in the given directory.
*/
func newCommand(command cmd.Commands, directory string) *cmd.Command {
	configuration, err := cnf.NewConfiguration()
	if err != nil {
		panic(err)
	}
	state, err := stt.NewStateWith(configuration)
	if err != nil {
		panic(err)
	}
	repository, err := git.GitInstance().Open(directory)

	var res cmd.Command
	switch command {
	case cmd.CLEAN:
		res, err = cmd.NewClean(state, &repository)
		if err != nil {
			panic(err)
		}
		return &res
	case cmd.INFER:
		res, err = cmd.NewInfer(state, &repository)
		if err != nil {
			panic(err)
		}
		return &res
	case cmd.MAKE:
		res, err = cmd.NewMake(state, &repository)
		if err != nil {
			panic(err)
		}
		return &res
	case cmd.MARK:
		res, err = cmd.NewMark(state, &repository)
		if err != nil {
			panic(err)
		}
		return &res
	case cmd.PUBLISH:
		res, err = cmd.NewPublish(state, &repository)
		if err != nil {
			panic(err)
		}
		return &res
	default:
		// this is never reached, but in case...
		panic(errors.New(fmt.Sprintf("unknown command '%s'", command.String())))
	}
}

/*
Returns a new Nyx instance using a Git repository in the given directory.
*/
func newNyx(directory string) *nyx.Nyx {
	return nyx.NewNyxIn(directory)
}

/*
Returns the available command proxies for the given command selector after initializing the given baseline.
The returned command proxies can be used to run the required command in their specific context.

Arguments are as follows:

- command the selector identifying the command to build the proxies for
- baseline a scenarion that is newly realized for each proxy and generates a Git repository to test with
*/
func CommandInvocationProxies(command cmd.Commands, baseline gittools.Scenario) []*CommandProxy {
	standaloneScript := baseline.Realize()
	nyxScript := baseline.Realize()
	return []*CommandProxy{NewStandaloneCommandProxy(newCommand(command, standaloneScript.GetWorkingDirectory()), standaloneScript), NewNyxCommandProxy(newNyx(nyxScript.GetWorkingDirectory()), command, nyxScript)}
}
