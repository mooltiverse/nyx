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

package entities

import (
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

/*
This class maps allowed values for a Git workspace status.
*/
type WorkspaceStatus string

const (
	// The workspace has no uncommitted changes.
	CLEAN WorkspaceStatus = "CLEAN"

	// The workspace has uncommitted changes.
	DIRTY WorkspaceStatus = "DIRTY"
)

/*
Returns the string representation of the workspace status
*/
func (ws WorkspaceStatus) String() string {
	switch ws {
	case CLEAN:
		return "CLEAN"
	case DIRTY:
		return "DIRTY"
	default:
		// this is never reached, but in case...
		panic("unknown WorkspaceStatus. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the workspace status corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown workspace status is passed
*/
func ValueOfWorkspaceStatus(s string) (WorkspaceStatus, error) {
	switch s {
	case "CLEAN":
		return CLEAN, nil
	case "DIRTY":
		return DIRTY, nil
	default:
		return CLEAN, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal workspace status '%s'", s)}
	}
}
