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

package version

import (
	"fmt" // https://pkg.go.dev/fmt
)

/*
The identifiers used for core version numbers.
*/
type CoreIdentifiers struct {
	// The name of the identifier.
	name string

	// The relative position of this identifier (starting from 0).
	position int
}

// we can't have these instances as constants so we back up to a variables block
var (
	/*
	   The major number.
	*/
	MAJOR CoreIdentifiers = CoreIdentifiers{name: "major", position: 0}

	/*
		The minor number.
	*/
	MINOR CoreIdentifiers = CoreIdentifiers{name: "minor", position: 1}

	/*
		The patch number.
	*/
	PATCH CoreIdentifiers = CoreIdentifiers{name: "patch", position: 2}
)

/*
Returns the name of the identifier.
*/
func (ci CoreIdentifiers) GetName() string {
	return ci.name
}

/*
Returns the relative position of this identifier (starting from 0.
*/
func (ci CoreIdentifiers) GetPosition() int {
	return ci.position
}

/*
Returns the string representation of the value
*/
func (ci CoreIdentifiers) String() string {
	switch ci {
	case MAJOR:
		return "MAJOR"
	case MINOR:
		return "MINOR"
	case PATCH:
		return "PATCH"
	default:
		// this is never reached, but in case...
		panic("unknown CoreIdentifiers. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the value corresponding to the given string.

Errors can be returned:

- in case an unknown value is passed
*/
func ValueOfCoreIdentifiers(s string) (CoreIdentifiers, error) {
	switch s {
	case "major":
		return MAJOR, nil
	case "minor":
		return MINOR, nil
	case "patch":
		return PATCH, nil
	default:
		return PATCH, fmt.Errorf("illegal value '%s'", s)
	}
}

/*
Returns true if there is a value with the given name. This method can be used to invoke
ValueOfCoreIdentifiers safely.
*/
func HasCoreIdentifierWithName(s string) bool {
	switch s {
	case "major":
		return true
	case "minor":
		return true
	case "patch":
		return true
	default:
		return false
	}
}
