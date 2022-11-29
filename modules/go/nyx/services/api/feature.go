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

package api

import (
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

/*
These are the constants representing the features that may or may not be supported by services.

When invoking an operation method against a service and the operation is not supported then
the method throws an UnsupportedOperationError.

In order to safely know if an operation is supported you can query the service object via
the Supports method.
*/
type Feature string

const (
	// When this feature is supported then the implementation class implements the GitHostingService interface
	// (so it can be safely cast to it) and the service specific methods can be safely invoked without an
	// UnsupportedOperationError being thrown.
	GIT_HOSTING Feature = "GIT_HOSTING"

	// When this feature is supported then the implementation class implements the ReleaseService interface
	// (so it can be safely cast to it) and the service specific methods can be safely invoked without an
	// UnsupportedOperationError being thrown.
	RELEASES Feature = "RELEASES"

	// When this feature is supported then the implementation class implements the ReleaseService interface
	// and the PublishReleaseAssets method can be safely published
	// without errors. See for more details on the supported assets on the service implementation class.
	RELEASE_ASSETS Feature = "RELEASE_ASSETS"

	// When this feature is supported then the implementation class implements the UserService interface
	// (so it can be safely cast to it) and the service specific methods can be safely invoked without an
	// UnsupportedOperationError being thrown.
	USERS Feature = "USERS"
)

/*
Returns the string representation of the feature
*/
func (f Feature) String() string {
	switch f {
	case GIT_HOSTING:
		return "GIT_HOSTING"
	case RELEASES:
		return "RELEASES"
	case RELEASE_ASSETS:
		return "RELEASE_ASSETS"
	case USERS:
		return "USERS"
	default:
		// this is never reached, but in case...
		panic("unknown Feature. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the feature corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown feature is passed
*/
func ValueOfFeature(s string) (Feature, error) {
	switch s {
	case "GIT_HOSTING":
		return GIT_HOSTING, nil
	case "RELEASES":
		return RELEASES, nil
	case "RELEASE_ASSETS":
		return RELEASE_ASSETS, nil
	case "USERS":
		return USERS, nil
	default:
		return USERS, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal workspace status '%s'", s)}
	}
}
