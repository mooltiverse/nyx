//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

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

package main

import (
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
)

func TestMainSelectCommand(t *testing.T) {
	// test that the default command is returned when none is on the command line
	selectedCommand, err := selectCommand([]string{})
	assert.NoError(t, err)
	assert.Equal(t, selectedCommand, cmd.INFER)

	// test that an error is returned when multiple commands are passed
	selectedCommand, err = selectCommand([]string{"clean", "infer"})
	assert.Error(t, err)

	// test that an error is returned when an unrecognized command is passed
	selectedCommand, err = selectCommand([]string{"dosomething"})
	assert.Error(t, err)

	// test that an error is returned when an unrecognized command is passed along with a correct one
	selectedCommand, err = selectCommand([]string{"dosomething", "infer"})
	assert.Error(t, err)

	// test that the correct command is returned among other options (here options are not checked except for their '-' prefix)
	selectedCommand, err = selectCommand([]string{"-1", "-add", "make", "-option1=value", "-option2="})
	assert.NoError(t, err)
	assert.Equal(t, selectedCommand, cmd.MAKE)
}
