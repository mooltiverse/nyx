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
A value holder that models a section containing a map of commit message conventions.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type CommitMessageConventions struct {
	// The private list of enabled items.
	Enabled *[]*string `json:"enabled,omitempty" yaml:"enabled,omitempty"`

	// The private map of the items.
	// Due to the lack of an (acceptable) implementation of generics in Go, that doesn't allow
	// to define T in a way that is not known upfront, this map needs to be
	// redefined here along with getters/setters instead of the 'enabledItemsMap' struct
	Items *map[string]*CommitMessageConvention `json:"items,omitempty" yaml:"items,omitempty"`
}

/*
Default constructor
*/
func NewCommitMessageConventions() *CommitMessageConventions {
	return &CommitMessageConventions{}
}

/*
Standard constructor.

Arguments are as follows:

- enabled the list of names of enabled items
- items the map of items

Errors can be:

- NilPointerError in case any parameter is nil
*/
func NewCommitMessageConventionsWith(enabled *[]*string, items *map[string]*CommitMessageConvention) (*CommitMessageConventions, error) {
	cmc := CommitMessageConventions{}

	if enabled == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "enabled")}
	}
	if items == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "items")}
	}

	cmc.Enabled = enabled
	cmc.Items = items

	return &cmc, nil
}

/*
Returns the list of enabled items. A nil value means undefined.
*/
func (cmc *CommitMessageConventions) GetEnabled() *[]*string {
	return cmc.Enabled
}

/*
Sets the list of enabled items. A nil value means undefined.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (cmc *CommitMessageConventions) SetEnabled(enabled *[]*string) error {
	if enabled == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "enabled")}
	}
	cmc.Enabled = enabled
	return nil
}

/*
Returns the map of the items configured in this section, where keys are item names
and values are actual item objects. A nil value means undefined.
*/
func (cmc *CommitMessageConventions) GetItems() *map[string]*CommitMessageConvention {
	return cmc.Items
}

/*
Sets the map of the items configured in this section, where keys are item names
and values are actual item objects. A nil value means undefined.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (cmc *CommitMessageConventions) SetItems(items *map[string]*CommitMessageConvention) error {
	if items == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "items")}
	}
	cmc.Items = items
	return nil
}
