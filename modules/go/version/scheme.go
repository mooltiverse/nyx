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
The values of this enum are used to select the versioning scheme to use.
*/
type Scheme string

const (
	// The Maven versioning scheme.
	// TODO: uncomment this value as per https://github.com/mooltiverse/nyx/issues/4. As of now this is just a placeholder.
	//MAVEN Scheme = "MAVEN"

	// The Semantic Versioning (https://semver.org/) scheme.
	SEMVER Scheme = "SEMVER"
)

/*
Returns the string representation of the scheme
*/
func (s Scheme) String() string {
	switch s {
	//case MAVEN:
	//	return "MAVEN"
	case SEMVER:
		return "SEMVER"
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the scheme corresponding to the given string.

Errors can be returned:

- in case an unknown scheme is passed
*/
func ValueOfScheme(s string) (Scheme, error) {
	switch s {
	//case "MAVEN":
	//	return MAVEN, nil
	case "SEMVER":
		return SEMVER, nil
	default:
		return SEMVER, fmt.Errorf("illegal scheme '%s'", s)
	}
}
