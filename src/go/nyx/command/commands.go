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
This package provides the implementation for all the commands (or tasks) provided by Nyx.

This package actually holds all the Nyx core business logic.
*/
package command

import (
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/src/go/errors"
)

/*
The enumeration of available commands.
*/
type Commands string

const (
	// The Clean command.
	CLEAN Commands = "CLEAN"

	// The Infer command.
	INFER Commands = "INFER"

	// The Make command.
	MAKE Commands = "MAKE"

	// The Mark command.
	MARK Commands = "MARK"

	// The Publish command.
	PUBLISH Commands = "PUBLISH"
)

/*
Returns the string representation of the command
*/
func (c Commands) String() string {
	switch c {
	case CLEAN:
		return "CLEAN"
	case INFER:
		return "INFER"
	case MAKE:
		return "MAKE"
	case MARK:
		return "MARK"
	case PUBLISH:
		return "PUBLISH"
	default:
		// this is never reached, but in case...
		panic("unknown Commands. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the command corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown command is passed
*/
func ValueOfCommands(s string) (Commands, error) {
	switch s {
	case "CLEAN":
		return CLEAN, nil
	case "INFER":
		return INFER, nil
	case "MAKE":
		return MAKE, nil
	case "MARK":
		return MARK, nil
	case "PUBLISH":
		return PUBLISH, nil
	default:
		return CLEAN, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal command '%s'", s)}
	}
}
