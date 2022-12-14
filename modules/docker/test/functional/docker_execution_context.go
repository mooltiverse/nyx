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
package docker_functional_test

import (
	"os"      // https://pkg.go.dev/os
	"os/exec" // https://pkg.go.dev/os/exec
	"os/user" // https://pkg.go.dev/os/user
	"runtime" // https://pkg.go.dev/runtime

	. "github.com/mooltiverse/nyx/modules/go/nyx/test/functional"
)

const (
	/*
		The name of the environment variable used to pass the Nyx Docker image to run.
	*/
	NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE = "nyxDockerImage"
)

var (
	// The full path to the Nyx executable file
	nyxExecutableAbsolutePath string
)

/*
This execution context allows running the Nyx from within a Docker container.
*/
type DockerExecutionContext struct {
}

/*
Returns a new execution context to run Nyx from within a Docker container.
*/
func NewDockerExecutionContext() ExecutionContext {
	var executionContext ExecutionContext
	dockerExecutionContext := &DockerExecutionContext{}
	executionContext = dockerExecutionContext
	return executionContext
}

/*
Takes the given input map and translates it into a string slice, where each eantry from the input map
becomes a single string in the form "-e 'name=value'".
*/
func translateEnvironmentVariables(env map[string]string) []string {
	res := []string{}
	for key, value := range env {
		res = append(res, "-e")
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
func (ctx *DockerExecutionContext) GetCommand(repoDir string, env map[string]string, args []string) *exec.Cmd {
	if os.Getenv(NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE) == "" {
		panic("no " + NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE + " environment variable has been set, unable to select the Docker image to run")
	}

	cmdArgs := []string{"run" /*, "-it"*/, "--rm", "-v", repoDir + ":/project"}
	if runtime.GOOS != "windows" {
		// map the container user to the local user ID, like -u $(id -u):$(id -g)
		// do this on all platforms but Windows
		usr, err := user.Current()
		if err != nil {
			panic(err)
		}
		cmdArgs = append(cmdArgs, "-u="+usr.Uid+":"+usr.Gid)
	}
	if env != nil && len(env) > 0 {
		cmdArgs = append(cmdArgs, translateEnvironmentVariables(env)...)
	}
	cmdArgs = append(cmdArgs, os.Getenv(NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE))
	cmdArgs = append(cmdArgs, args...)
	cmd := exec.Command("docker", cmdArgs...)
	return cmd
}
