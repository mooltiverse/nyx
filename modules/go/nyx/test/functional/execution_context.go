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

package functional_test

import (
	"os/exec" // https://pkg.go.dev/os/exec
)

/*
This interface models a an execution context that allows running the Nyx executable from within a specific environment.

This interface can be implemented by different contexts in order to run a certain command
in a context specific way.
*/
type ExecutionContext interface {
	/*
		Returns the command object used to run the executable.

		Arguments are as follows:

		- repoDir the directory containing the Git repository
		- env the map of environment variables to pass to Nyx
		- args the command line arguments to pass to the Nyx executable
	*/
	GetCommand(repoDir string, env map[string]string, args []string) *exec.Cmd
}
