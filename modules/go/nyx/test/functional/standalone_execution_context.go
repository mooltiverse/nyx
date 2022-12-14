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
This package provides functional tests.
*/
package functional_test

import (
	"os"            // https://pkg.go.dev/os
	"os/exec"       // https://pkg.go.dev/os/exec
	"path/filepath" // https://pkg.go.dev/path/filepath
	"runtime"       // https://pkg.go.dev/runtime

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus
)

const (
	/*
	 The name of the environment variable used to pass the path of the directory containing Nyx binary files.
	 If this variable is not passed the Nyx executable will be assumed to be in the current working directory.
	*/
	NYX_BIN_DIR_ENVIRONMENT_VARIABLE = "nyxBinDir"
)

/*
Returns the absolute path to the Nyx binary file to use when running tests.

Here we read the NYX_BIN_DIR_ENVIRONMENT_VARIABLE environment variable, then build the binary name by using the current
platform and architecture, and adding an optional extension.

Regardless of the directory that may be passed as the NYX_BIN_DIR_ENVIRONMENT_VARIABLE, the binary executable file is
assumed to be in the form nyx-<runtime.GOOS>-<runtime.GOARCH>[.exe]
*/
func getAbsoluteBinaryPath() string {
	binaryName := "nyx" + "-" + runtime.GOOS + "-" + runtime.GOARCH
	if runtime.GOOS == "windows" {
		binaryName = binaryName + ".exe"
	}
	if os.Getenv(NYX_BIN_DIR_ENVIRONMENT_VARIABLE) == "" {
		log.Warnf("no '%s' environment variable has been set, executable file for functional tests is assumed to be in the current working directory", NYX_BIN_DIR_ENVIRONMENT_VARIABLE)
	}

	return filepath.Join(os.Getenv(NYX_BIN_DIR_ENVIRONMENT_VARIABLE), binaryName)
}

/*
This execution context allows running the Nyx executable as a local executable.
*/
type StandaloneExecutionContext struct {
}

/*
Returns a new execution context to run Nyx as a local executable.
*/
func NewStandaloneExecutionContext() ExecutionContext {
	var executionContext ExecutionContext
	standaloneExecutionContext := &StandaloneExecutionContext{}
	executionContext = standaloneExecutionContext
	return executionContext
}

/*
Takes the given input map and translates it into a string slice, where each eantry from the input map
becomes a single string in the form "name=value".
*/
func translateEnvironmentVariables(env map[string]string) []string {
	res := []string{}
	for key, value := range env {
		res = append(res, key+"="+value)
	}
	return res
}

/*
Returns the command object used to run the executable.

Arguments are as follows:

- repoDir the directory containing the Git repository
- env the map of environment variables to pass to Nyx
- args the command line arguments to pass to the Nyx executable
*/
func (ctx *StandaloneExecutionContext) GetCommand(repoDir string, env map[string]string, args []string) *exec.Cmd {
	cmd := exec.Command(getAbsoluteBinaryPath(), args...)
	if env != nil && len(env) > 0 {
		cmd.Env = append(os.Environ(), translateEnvironmentVariables(env)...)
	}
	cmd.Dir = repoDir
	return cmd
}
