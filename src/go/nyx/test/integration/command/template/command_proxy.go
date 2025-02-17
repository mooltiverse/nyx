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
	cmd "github.com/mooltiverse/nyx/src/go/nyx/command"
	gittools "github.com/mooltiverse/nyx/src/go/nyx/test/integration/git/tools"
)

/*
This interface models a proxy used to run a Command in some context.

This interface can be implemented by different contexts in order to run a certain command
in a context specific way.

For example this package provides two different contexts: one to run commands as standalone
objects and another to run the command through the Nyx class business methods.
*/
type CommandProxy interface {
	// Extend the command interface by embedding it
	cmd.Command

	/*
		Returns the name of the context this proxy run in.
	*/
	GetContextName() string

	/*
		Returns the script representing the Git scenario under test.
	*/
	Script() gittools.Script
}
