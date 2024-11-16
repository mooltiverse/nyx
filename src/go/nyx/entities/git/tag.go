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

package git

/*
This object is a Git tag value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Tag struct {
	// The annotated or lightweight flag.
	Annotated bool `json:"annotated,omitempty" yaml:"annotated,omitempty"`

	// The name.
	Name string `json:"name,omitempty" yaml:"name,omitempty"`

	// The tagged object ID.
	Target string `json:"target,omitempty" yaml:"target,omitempty"`
}

/*
Standard constructor.

Arguments are as follows:

- name the simple name (without prefix)
- target the ID (SHA-1) of the tagged object
- annotated make it true for annotated tags, false for lightweight tags
*/
func NewTagWith(name string, target string, annotated bool) *Tag {
	t := Tag{}

	t.Name = name
	t.Target = target
	t.Annotated = annotated

	return &t
}

/*
Returns true if this is an annotated tag, false if it's a lightweight tag.
*/
func (t Tag) IsAnnotated() bool {
	return t.Annotated
}

/*
Returns the name.
*/
func (t Tag) GetName() string {
	return t.Name
}

/*
Returns the ID (SHA-1) of the tagged object.
*/
func (t Tag) GetTarget() string {
	return t.Target
}

/*
Returns the name.
*/
func (t Tag) String() string {
	return t.Name
}
