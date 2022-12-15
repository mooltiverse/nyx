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

/*
This object models the fields used to configure the remote Git repository.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type GitRemoteConfiguration struct {
	// The remote user name.
	User *string `json:"user,omitempty" yaml:"user,omitempty"`

	// The remote password.
	Password *string `json:"password,omitempty" yaml:"password,omitempty"`
}

/*
Default constructor
*/
func NewGitRemoteConfiguration() *GitRemoteConfiguration {
	return &GitRemoteConfiguration{}
}

/*
Standard constructor.

Arguments are as follows:

- user the remote user name.
- password the remote password.
*/
func NewGitRemoteConfigurationWith(user *string, password *string) *GitRemoteConfiguration {
	grc := GitRemoteConfiguration{}

	grc.User = user
	grc.Password = password

	return &grc
}

/*
Returns the remote user name.
*/
func (grc *GitRemoteConfiguration) GetUser() *string {
	return grc.User
}

/*
Sets the remote user name.
*/
func (grc *GitRemoteConfiguration) SetUser(user *string) {
	grc.User = user
}

/*
Returns the remote password.
*/
func (grc *GitRemoteConfiguration) GetPassword() *string {
	return grc.Password
}

/*
Sets the remote password.
*/
func (grc *GitRemoteConfiguration) SetPassword(password *string) {
	grc.Password = password
}
