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
This object models the fields used to configure the Git service.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type GitConfiguration struct {
	// The map of remotes configuration options.
	Remotes *map[string]*GitRemoteConfiguration `json:"remotes,omitempty" yaml:"remotes,omitempty"`
}

/*
Default constructor
*/
func NewGitConfiguration() *GitConfiguration {
	gc := GitConfiguration{}
	gc.setDefaults()
	return &gc
}

/*
Standard constructor.

Arguments are as follows:

- remotes the map of remotes configuration options.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func NewGitConfigurationWith(remotes *map[string]*GitRemoteConfiguration) (*GitConfiguration, error) {
	gc := GitConfiguration{}

	if remotes == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "remotes")}
	}

	gc.Remotes = remotes

	return &gc, nil
}

/*
Loads default values on the target instance
*/
func (gc *GitConfiguration) setDefaults() {
	gc.Remotes = &map[string]*GitRemoteConfiguration{}
}

/*
Returns the map of remotes configuration options.
*/
func (gc *GitConfiguration) GetRemotes() *map[string]*GitRemoteConfiguration {
	return gc.Remotes
}

/*
Sets the map of remotes configuration options.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (gc *GitConfiguration) SetRemotes(remotes *map[string]*GitRemoteConfiguration) error {
	if remotes == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "remotes")}
	}
	gc.Remotes = remotes
	return nil
}
