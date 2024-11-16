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
A value holder that models a section containing a map of release types.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type ReleaseTypes struct {
	// The private list of enabled items.
	Enabled *[]*string `json:"enabled,omitempty" yaml:"enabled,omitempty"`

	// The private list of publication services.
	PublicationServices *[]*string `json:"publicationServices,omitempty" yaml:"publicationServices,omitempty"`

	// The private list of remote repositories.
	RemoteRepositories *[]*string `json:"remoteRepositories,omitempty" yaml:"remoteRepositories,omitempty"`

	// The private map of the items.
	// Due to the lack of an (acceptable) implementation of generics in Go, that doesn't allow
	// to define T in a way that is not known upfront, this map needs to be
	// redefined here along with getters/setters instead of the 'enabledItemsMap' struct
	Items *map[string]*ReleaseType `json:"items,omitempty" yaml:"items,omitempty"`
}

/*
Default constructor
*/
func NewReleaseTypes() *ReleaseTypes {
	rt := ReleaseTypes{}
	rt.setDefaults()
	return &rt
}

/*
Standard constructor.

Arguments are as follows:

- enabled the list of names of enabled items
- publicationServices the list of names of publication services
- remoteRepositories the list of remote repositories. It may be nil
- items the map of items

Errors can be:

- NilPointerError in case enabled, publicationServices or items is nil
*/
func NewReleaseTypesWith(enabled *[]*string, publicationServices *[]*string, remoteRepositories *[]*string, items *map[string]*ReleaseType) (*ReleaseTypes, error) {
	rt := ReleaseTypes{}

	if enabled == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "enabled")}
	}
	if publicationServices == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "publicationServices")}
	}
	if items == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "items")}
	}

	rt.Enabled = enabled
	rt.PublicationServices = publicationServices
	rt.RemoteRepositories = remoteRepositories
	rt.Items = items

	return &rt, nil
}

/*
Loads default values on the target instance
*/
func (rt *ReleaseTypes) setDefaults() {
	rt.Enabled = &[]*string{}
	rt.PublicationServices = &[]*string{}
	rt.Items = &map[string]*ReleaseType{}
}

/*
Returns the list of enabled items. A nil value means undefined.
*/
func (rt *ReleaseTypes) GetEnabled() *[]*string {
	return rt.Enabled
}

/*
Sets the list of enabled items. A nil value means undefined.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (rt *ReleaseTypes) SetEnabled(enabled *[]*string) error {
	if enabled == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "enabled")}
	}
	rt.Enabled = enabled
	return nil
}

/*
Returns the map of the items configured in this section, where keys are item names
and values are actual item objects. A nil value means undefined.
*/
func (rt *ReleaseTypes) GetItems() *map[string]*ReleaseType {
	return rt.Items
}

/*
Sets the map of the items configured in this section, where keys are item names
and values are actual item objects. A nil value means undefined.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (rt *ReleaseTypes) SetItems(items *map[string]*ReleaseType) error {
	if items == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "items")}
	}
	rt.Items = items
	return nil
}

/*
Returns the list of publication services. A nil value means undefined.
*/
func (rt *ReleaseTypes) GetPublicationServices() *[]*string {
	return rt.PublicationServices
}

/*
Sets the list of publication services. A nil value means undefined.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (rt *ReleaseTypes) SetPublicationServices(publicationServices *[]*string) error {
	if publicationServices == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "publicationServices")}
	}
	rt.PublicationServices = publicationServices
	return nil
}

/*
Returns the list of remote repositories. A nil value means undefined.
*/
func (rt *ReleaseTypes) GetRemoteRepositories() *[]*string {
	return rt.RemoteRepositories
}

/*
Sets the list of remote repositories. A nil value means undefined.

Errors can be:

- none
*/
func (rt *ReleaseTypes) SetRemoteRepositories(remoteRepositories *[]*string) error {
	rt.RemoteRepositories = remoteRepositories
	return nil
}
