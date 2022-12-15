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
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
)

/*
The Command interface must be implemented by all Nyx commands.
*/
type Command interface {
	/*
		Returns the state object.
	*/
	State() *stt.State

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
	IsUpToDate() (bool, error)

	/*
		Runs the command and returns the updated reference to the state object. In order to improve performances you should only
		invoke this method when IsUpToDate returns false.

		Error is:
		- DataAccessError in case the configuration can't be loaded for some reason.
		- IllegalPropertyError in case the configuration has some illegal options.
		- GitError in case of unexpected issues when accessing the Git repository.
		- ReleaseError if the task is unable to complete for reasons due to the release process.
	*/
	Run() (*stt.State, error)
}
