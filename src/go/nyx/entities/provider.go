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

	errs "github.com/mooltiverse/nyx/src/go/errors"
)

/*
These are the constants representing the available service providers.
*/
type Provider string

const (
	// The GitHub (https://github.com) service provider.
	GITHUB Provider = "GITHUB"

	// The GitLab https://gitlab.com/) service provider.
	GITLAB Provider = "GITLAB"
)

/*
Returns the string representation of the provider
*/
func (p Provider) String() string {
	switch p {
	case GITHUB:
		return "GITHUB"
	case GITLAB:
		return "GITLAB"
	default:
		// this is never reached, but in case...
		panic("unknown Provider. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the service corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown service is passed
*/
func ValueOfProvider(s string) (Provider, error) {
	switch s {
	case "GITHUB":
		return GITHUB, nil
	case "GITLAB":
		return GITLAB, nil
	default:
		return GITHUB, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal service '%s'", s)}
	}
}
