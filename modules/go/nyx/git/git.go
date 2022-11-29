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

/*
This is the Git package for Nyx, encapsulating the underlying Git implementation.
*/
package git

/*
The entry point to the Git local and remote service. This is also the main entry point to retrieve Repository instances
*/
type Git struct {
	// The user name to authenticate to the remote service.
	user *string

	// The password to authenticate to the remote service.
	password *string
}

/*
Returns an instance using default options.
*/
func GitInstance() Git {
	return Git{}
}

/*
Returns a repository instance working in the given directory after cloning from the given URI.
If the instance has some credentials set, those are used to perform the operation, otherwise
anonymous access will be used.

Arguments are as follows:

- directory the directory where the repository has to be cloned. It is created if it doesn't exist.
- uri the URI of the remote repository to clone.

Errors can be:

- NilPointerError if any of the required objects is nil
- IllegalArgumentError if a given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func (g Git) Clone(directory *string, uri *string) (Repository, error) {
	return g.CloneWithCredentials(directory, uri, g.GetUser(), g.GetPassword())
}

/*
Returns a repository instance working in the given directory after cloning from the given URI.

Arguments are as follows:

- directory the directory where the repository has to be cloned. It is created if it doesn't exist.
- uri the URI of the remote repository to clone.
- user the user name to use when credentials are required. If this and password are both nil
  then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
  this value may be the token or something other than a token, depending on the remote provider.
- password the password to use when credentials are required. If this and user are both nil
  then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
  this value may be the token or something other than a token, depending on the remote provider.

Errors can be:

- NilPointerError if any of the required objects is nil
- IllegalArgumentError if a given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func (g Git) CloneWithCredentials(directory *string, uri *string, user *string, password *string) (Repository, error) {
	return cloneWithCredentials(directory, uri, user, password)
}

/*
Returns the user name to be used when connecting to remote repositories.
*/
func (g Git) GetUser() *string {
	return g.user
}

/*
Returns the password to be used when connecting to remote repositories.
*/
func (g Git) GetPassword() *string {
	return g.password
}

/*
Returns a repository instance working in the given directory.

Arguments are as follows:

- directory the directory where the repository is.

Errors can be:

- IllegalArgumentError if a given object is illegal for some reason, like referring to an illegal repository
- GitError in case the operation fails for some reason, including when authentication fails
*/
func (g Git) Open(directory string) (Repository, error) {
	return open(directory)
}

/*
Sets the user name to be used when connecting to remote repositories.

Arguments are as follows:

- user the user name to be used when connecting to remote repositories.
  When nil the remote repositories are meant to allow anonymous access.
*/
func (g Git) SetUser(user *string) {
	g.user = user
}

/*
Sets the password to be used when connecting to remote repositories.

Arguments are as follows:

-  password the password to be used when connecting to remote repositories.
*/
func (g Git) SetPassword(password *string) {
	g.password = password
}
