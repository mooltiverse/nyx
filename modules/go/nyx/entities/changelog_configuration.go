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
This object models the fields used to configure the changelog generation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type ChangelogConfiguration struct {
	// The flag instructing if and when to append contents to the existing changelog file.
	Append *string `json:"append,omitempty" yaml:"append,omitempty"`

	// The path to the destination file.
	Path *string `json:"path,omitempty" yaml:"path,omitempty"`

	// The map of sections and commit types.
	Sections *map[string]string `json:"sections,omitempty" yaml:"sections,omitempty"`

	// The map of substitution strings.
	Substitutions *map[string]string `json:"substitutions,omitempty" yaml:"substitutions,omitempty"`

	// The path to the optional template file.
	Template *string `json:"template,omitempty" yaml:"template,omitempty"`
}

/*
Default constructor
*/
func NewChangelogConfiguration() *ChangelogConfiguration {
	cl := ChangelogConfiguration{}

	sections := make(map[string]string)
	substitutions := make(map[string]string)
	cl.Sections = &sections
	cl.Substitutions = &substitutions

	return &cl
}

/*
Standard constructor.

Arguments are as follows:

- append the flag instructing if and when to append contents to the existing changelog file. It may be nil
- path the path to the destination file. It may be nil
- sections the map of sections and commit types.
- template the path to the optional template file. It may be nil
- substitutions the map of substitution strings.

Errors can be:

- NilPointerError in case sections is nil
*/
func NewChangelogConfigurationWith(append *string, path *string, sections *map[string]string, template *string, substitutions *map[string]string) (*ChangelogConfiguration, error) {
	cl := ChangelogConfiguration{}

	if sections == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "sections")}
	}

	cl.Append = append
	cl.Path = path
	cl.Sections = sections
	cl.Substitutions = substitutions
	cl.Template = template

	if cl.Sections == nil {
		s := make(map[string]string)
		cl.Sections = &s
	}
	if cl.Substitutions == nil {
		s := make(map[string]string)
		cl.Substitutions = &s
	}

	return &cl, nil
}

/*
Returns the flag instructing if and when to append contents to the existing changelog file.
*/
func (cl *ChangelogConfiguration) GetAppend() *string {
	return cl.Append
}

/*
Sets the flag instructing if and when to append contents to the existing changelog file.

Errors can be:

- none
*/
func (cl *ChangelogConfiguration) SetAppend(append *string) error {
	cl.Append = append
	return nil
}

/*
Returns the path to the destination file.
*/
func (cl *ChangelogConfiguration) GetPath() *string {
	return cl.Path
}

/*
Sets the path to the destination file.

Errors can be:

- none
*/
func (cl *ChangelogConfiguration) SetPath(path *string) error {
	cl.Path = path
	return nil
}

/*
Returns the map of sections and commit types.
*/
func (cl *ChangelogConfiguration) GetSections() *map[string]string {
	return cl.Sections
}

/*
Sets the map of sections and commit types.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (cl *ChangelogConfiguration) SetSections(sections *map[string]string) error {
	if sections == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "sections")}
	}
	cl.Sections = sections
	return nil
}

/*
Returns the map of substitution strings.
*/
func (cl *ChangelogConfiguration) GetSubstitutions() *map[string]string {
	return cl.Substitutions
}

/*
Sets the map of substitution strings.

Errors can be:

- NilPointerError in case the given parameter is nil
*/
func (cl *ChangelogConfiguration) SetSubstitutions(substitutions *map[string]string) error {
	if substitutions == nil {
		return &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "substitutions")}
	}
	cl.Substitutions = substitutions
	return nil
}

/*
Returns the path to the optional template file.
*/
func (cl *ChangelogConfiguration) GetTemplate() *string {
	return cl.Template
}

/*
Sets the path to the optional template file.

Errors can be:

- none
*/
func (cl *ChangelogConfiguration) SetTemplate(template *string) error {
	cl.Template = template
	return nil
}
