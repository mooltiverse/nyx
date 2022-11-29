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

package util

import (
	"io"      // https://pkg.go.dev/io
	"os/exec" // https://pkg.go.dev/os/exec
)

/*
Runs the command (and options) given in the input array in the given directory and redirecting the output to the given stream.

Arguments are as follows:

- commandAndArguments the command arguments as they must appear on the command line, in order. The first element is the command
- env the environment vaiables to set, or nil if no environment variable needs to be set
- directory the directory to run the command in, or nil to use the current working directory
- out the output stream to redirect the output to
*/
func RunCommand(commandAndArguments []string, env []string, directory *string, out io.Writer) {
	commandPath, err := exec.LookPath(commandAndArguments[0])
	if err != nil {
		panic(err)
	}

	cmd := &exec.Cmd{Path: commandPath, Args: commandAndArguments}
	if env != nil {
		cmd.Env = env
	}
	if directory != nil {
		cmd.Dir = *directory
	}

	cmd.Stderr = out
	cmd.Stdout = out
	err = cmd.Run()
	if err != nil {
		panic(err)
	}
}
