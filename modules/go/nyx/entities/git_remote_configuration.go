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
	// The authentication method.
	AuthenticationMethod *AuthenticationMethod `json:"authenticationMethod,omitempty" yaml:"authenticationMethod,omitempty"`

	// The remote user name.
	User *string `json:"user,omitempty" yaml:"user,omitempty"`

	// The remote password.
	Password *string `json:"password,omitempty" yaml:"password,omitempty"`

	// The private key.
	PrivateKey *string `json:"privateKey,omitempty" yaml:"privateKey,omitempty"`

	// The passphrase for the private key.
	Passphrase *string `json:"passphrase,omitempty" yaml:"passphrase,omitempty"`
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

- authenticationMethod the authentication method.
- user the remote user name.
- password the remote password.
- privateKey the private key.
- passphrase the passphrase for the private key.
*/
func NewGitRemoteConfigurationWith(authenticationMethod *AuthenticationMethod, user *string, password *string, privateKey *string, passphrase *string) *GitRemoteConfiguration {
	grc := GitRemoteConfiguration{}

	grc.AuthenticationMethod = authenticationMethod
	grc.User = user
	grc.Password = password
	grc.PrivateKey = privateKey
	grc.Passphrase = passphrase

	return &grc
}

/*
Returns the authentication method.
*/
func (grc *GitRemoteConfiguration) GetAuthenticationMethod() *AuthenticationMethod {
	return grc.AuthenticationMethod
}

/*
Sets the authentication method.
*/
func (grc *GitRemoteConfiguration) SetAuthenticationMethod(authenticationMethod *AuthenticationMethod) {
	grc.AuthenticationMethod = authenticationMethod
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

/*
Returns the private key.
*/
func (grc *GitRemoteConfiguration) GetPrivateKey() *string {
	return grc.PrivateKey
}

/*
Sets the private key.
*/
func (grc *GitRemoteConfiguration) SetPrivateKey(privateKey *string) {
	grc.PrivateKey = privateKey
}

/*
Returns the passphrase for the private key.
*/
func (grc *GitRemoteConfiguration) GetPassphrase() *string {
	return grc.Passphrase
}

/*
Sets the passphrase for the private key.
*/
func (grc *GitRemoteConfiguration) SetPassphrase(passphrase *string) {
	grc.Passphrase = passphrase
}
