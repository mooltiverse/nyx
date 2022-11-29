//go:build functional
// +build functional

// Only run these tests as part of the functional test suite, when the 'functional' build flag is passed (i.e. running go test --tags=functional)

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
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"os/exec"       // https://pkg.go.dev/os/exec
	"path/filepath" // https://pkg.go.dev/path/filepath
	"runtime"       // https://pkg.go.dev/runtime
	"testing"       // https://pkg.go.dev/testing
	// assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

const (
	/*
	 * The name of the environment variable used to pass the path of the directory containing Nyx binary files
	 */
	GO_BIN_DIR_ENVIRONMENT_VARIABLE = "goBinDir"
)

var (
	nyxExecutable string
)

func init() {
	nyxExecutable = getAbsoluteBinaryPath()
}

/*
Returns the path to the Nyx binary file to use when running tests.

Here we read the GO_BIN_DIR_ENVIRONMENT_VARIABLE environment variable, then build the binary name by using the current
platform and architecture, and adding an optional extension.
*/
func getAbsoluteBinaryPath() string {
	binaryName := "nyx" + "-" + runtime.GOOS + "-" + runtime.GOARCH
	if runtime.GOOS == "windows" {
		binaryName = binaryName + ".exe"
	}
	if os.Getenv(GO_BIN_DIR_ENVIRONMENT_VARIABLE) == "" {
		fmt.Printf("No '%s' environment variable. Executable file for functional tests is assumed to be in the current working directory\n", GO_BIN_DIR_ENVIRONMENT_VARIABLE)
	}

	return filepath.Join(os.Getenv(GO_BIN_DIR_ENVIRONMENT_VARIABLE), binaryName)

}

// this method can be removed as soon as we have a concrete functional test in this package
func TestDummyFunctional(t *testing.T) {
	// just print out a string to verify these tests are run
	fmt.Printf("****** FUNCTIONAL TESTS ******\n")

	fmt.Printf("running %s\n", nyxExecutable)

	out, _ /*err*/ := exec.Command(nyxExecutable, "--dryRun", "--trace", "publish").Output() // these are just dummy arguments
	fmt.Printf("%v\n", string(out))
	//assert.NoError(t, err) // there will be an error as long as we don't run in a Git repository

	fmt.Printf("***END OF FUNCTIONAL TESTS ***\n")
}
