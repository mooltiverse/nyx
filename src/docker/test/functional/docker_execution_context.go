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

In order to make these tests work both from regular hosts and from inside other containers
(i.e. Development Containers) we need to mount the test repository data with Docker volumes,
otherwise, if we mount a folder into the test container, the folder that Docker tries to
mount is always from the host, even when running inside a development container, and in that
case tests fail.
*/
package docker_functional_test

import (
	"os"      // https://pkg.go.dev/os
	"os/exec" // https://pkg.go.dev/os/exec

	// https://pkg.go.dev/os/user
	"path/filepath" // https://pkg.go.dev/path/filepath

	// https://pkg.go.dev/runtime
	. "github.com/mooltiverse/nyx/src/go/nyx/test/functional"
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
Returns a name that can be used for a Docker volume.

The name of the volume is just the base name of given repository directory name,
which already contains random characters.

By using the same name for the volume as the repository name it's also easier to
match the two, when needed.

See the documentation on top of this file about why we need these volumes and containers.
*/
func dockerVolumeNameFromRepoDir(repoDir string) string {
	return filepath.Base(repoDir)
}

/*
Returns a name that can be used for a disposeable Docker container.

The name of the container is built on the base name of given repository directory name,
which already contains random characters.

By using the same name for the container as the repository name it's also easier to
match the two, when needed.

See the documentation on top of this file about why we need these volumes and containers.
*/
func dockerDisposeableContainerNameFromRepoDir(repoDir string) string {
	return "disposeable-" + dockerVolumeNameFromRepoDir(repoDir)
}

/*
Returns the command objects used to run the test. Commands are required to be executed
in the same order they appear in the list.

The test repository is mounted from a Docker volume.

Arguments are as follows:

- repoDir the directory containing the Git repository
- env the map of environment variables to pass to Nyx
- args the command line arguments to pass to the Nyx executable
*/
func (ctx *DockerExecutionContext) GetTestCommands(repoDir string, env map[string]string, args []string) []*exec.Cmd {
	if os.Getenv(NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE) == "" {
		panic("no " + NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE + " environment variable has been set, unable to select the Docker image to run")
	}

	// the volume mounted here is created by commands from GetPreTestCommands
	cmdArgs := []string{"run" /*, "-it"*/, "--rm", "-v", dockerVolumeNameFromRepoDir(repoDir) + ":/project"}
	// As long as files are copied in the PreTest commands in a Docker volume as root (Alpine's default, and Alpine uses root)
	// let's just run as root (by default), otherwise, on GitHub Actions, where the host user is other than root (runner:docker, to be specific),
	// files inside the container aren't modifiable by the runtime user
	cmdArgs = append(cmdArgs, "-u=root:root")
	/*if runtime.GOOS != "windows" {
		// map the container user to the local user ID, like -u $(id -u):$(id -g)
		// do this on all platforms but Windows
		usr, err := user.Current()
		if err != nil {
			panic(err)
		}
		cmdArgs = append(cmdArgs, "-u="+usr.Uid+":"+usr.Gid)
	}*/
	if env != nil && len(env) > 0 {
		cmdArgs = append(cmdArgs, translateEnvironmentVariables(env)...)
	}
	cmdArgs = append(cmdArgs, os.Getenv(NYX_DOCKER_IMAGE_ENVIRONMENT_VARIABLE))
	cmdArgs = append(cmdArgs, args...)
	cmd := exec.Command("docker", cmdArgs...)
	return []*exec.Cmd{
		cmd,
	}
}

/*
Returns the command objects used to run before the test. The returned list may be empty.
Commands are required to be executed in the same order they appear in the list.

Arguments are as follows:

- repoDir the directory containing the Git repository
- env the map of environment variables to pass to Nyx
- args the command line arguments to pass to the Nyx executable
*/
func (ctx *DockerExecutionContext) GetPreTestCommands(repoDir string, env map[string]string, args []string) []*exec.Cmd {
	disposeableContainerName := dockerDisposeableContainerNameFromRepoDir(repoDir)

	return []*exec.Cmd{
		// create the volume
		exec.Command("docker", "volume", "create", dockerVolumeNameFromRepoDir(repoDir)),
		// create a temporary disposeable Alpine container just to copy the files in the volume
		exec.Command("docker", "container", "create", "--name", disposeableContainerName, "-v", dockerVolumeNameFromRepoDir(repoDir)+":/project", "alpine"),
		// copy the test repository file to the volume mounted in the disposeable container
		// note the "/." at the end of the source path (see https://docs.docker.com/engine/reference/commandline/cp/)
		exec.Command("docker", "cp", "-a", repoDir+"/.", disposeableContainerName+":"+"/project"),
	}
}

/*
Returns the command object used to run after the test. The returned list may be empty.
Commands are required to be executed in the same order they appear in the list.

Arguments are as follows:

- repoDir the directory containing the Git repository
- env the map of environment variables to pass to Nyx
- args the command line arguments to pass to the Nyx executable
*/
func (ctx *DockerExecutionContext) GetPostTestCommands(repoDir string, env map[string]string, args []string) []*exec.Cmd {
	// Copy the project files from the volume back to the test repo dir so they can be inspected
	return []*exec.Cmd{
		// copy the test repository file from the volume mounted in the disposeable container back to the local folder
		// note the "/." at the end of the source path (see https://docs.docker.com/engine/reference/commandline/cp/)
		exec.Command("docker", "cp", dockerDisposeableContainerNameFromRepoDir(repoDir)+":"+"/project/.", repoDir),
	}
}

/*
Returns the command object used to run after the test, before the test exits.
The returned list may be empty.
Commands are required to be executed in the same order they appear in the list.

Arguments are as follows:

- repoDir the directory containing the Git repository
- env the map of environment variables to pass to Nyx
- args the command line arguments to pass to the Nyx executable
*/
func (ctx *DockerExecutionContext) GetCleanUpCommands(repoDir string, env map[string]string, args []string) []*exec.Cmd {
	// remove disposeable containers and volumes
	return []*exec.Cmd{
		exec.Command("docker", "container", "rm", dockerDisposeableContainerNameFromRepoDir(repoDir)),
		exec.Command("docker", "volume", "rm", dockerVolumeNameFromRepoDir(repoDir)),
	}
}
