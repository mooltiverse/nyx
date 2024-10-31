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
This object is a Git identity value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Identity struct {
	// The email.
	Email string `json:"email,omitempty" yaml:"email,omitempty"`

	// The name.
	Name string `json:"name" yaml:"name"`
}

/*
Standard constructor.

Arguments are as follows:

- name the name
- email the email
*/
func NewIdentityWith(name string, email string) *Identity {
	i := Identity{}

	i.Name = name
	i.Email = email

	return &i
}

/*
Returns the email.
*/
func (i Identity) GetEmail() string {
	return i.Email
}

/*
Returns the name.
*/
func (i Identity) GetName() string {
	return i.Name
}

/*
Returns the string representation of the identity.
*/
func (i Identity) String() string {
	if i.Email == "" {
		return i.Name
	} else {
		return i.Name + " <" + i.Email + ">"
	}
}
