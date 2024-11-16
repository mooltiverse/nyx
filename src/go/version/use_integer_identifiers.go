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
This enum tells when to use integers instead of strings as Identifiers
*/
type UseIntegerIdentifiers int8

const (
	/*
	   Always use Integer identifiers, Strings are not allowed
	*/
	ALWAYS UseIntegerIdentifiers = 0

	/*
		Never use Integer identifiers, only Strings are allowed
	*/
	NEVER UseIntegerIdentifiers = 1

	/*
		Use Integers when possible (when Strings can be converted to Integers), otherwise fallback to Strings.
	*/
	WHEN_POSSIBLE UseIntegerIdentifiers = 2
)

/*
Returns the string representation of the value
*/
func (uii UseIntegerIdentifiers) String() string {
	switch uii {
	case ALWAYS:
		return "ALWAYS"
	case NEVER:
		return "NEVER"
	case WHEN_POSSIBLE:
		return "WHEN_POSSIBLE"
	default:
		// this is never reached, but in case...
		panic("unknown UseIntegerIdentifiers. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the value corresponding to the given string.

Errors can be returned:

- in case an unknown value is passed
*/
func ValueOfUseIntegerIdentifiers(s string) (UseIntegerIdentifiers, error) {
	switch s {
	case "ALWAYS":
		return ALWAYS, nil
	case "NEVER":
		return NEVER, nil
	case "WHEN_POSSIBLE":
		return WHEN_POSSIBLE, nil
	default:
		return ALWAYS, fmt.Errorf("illegal value '%s'", s)
	}
}
