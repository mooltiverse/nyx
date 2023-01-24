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

	// https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

/*
This type maps allowed values for autentication methods.
*/
type AuthenticationMethod string

const (
	// Public key authentication (SSH).
	PUBLIC_KEY AuthenticationMethod = "PUBLIC_KEY"

	// User name and password.
	USER_PASSWORD AuthenticationMethod = "USER_PASSWORD"
)

/*
Returns the string representation of the authentication method
*/
func (am AuthenticationMethod) String() string {
	switch am {
	case PUBLIC_KEY:
		return "PUBLIC_KEY"
	case USER_PASSWORD:
		return "USER_PASSWORD"
	default:
		// this is never reached, but in case...
		panic("unknown AuthenticationMethod. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the authentication method corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown authentication method is passed
*/
func ValueOfAuthenticationMethod(s string) (AuthenticationMethod, error) {
	switch s {
	case "PUBLIC_KEY":
		return PUBLIC_KEY, nil
	case "USER_PASSWORD":
		return USER_PASSWORD, nil
	default:
		return USER_PASSWORD, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal authentication method '%s'", s)}
	}
}
