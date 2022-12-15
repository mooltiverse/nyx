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

/*
A service that supports the GIT_HOSTING feature to manage hosted repositories.
*/
type GitHostingService interface {
	/*
		Creates a new Git repository for the currently authenticated user.

		Please note that if the service has been configured with repository owner and name those attributes are ignored
		by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
		name is always the name attribute.

		Arguments are as follows:

		- name the repository name. Cannot be nil
		- description the repository description. It may be nil
		- restricted when true the repository will have private visibility, otherwise it will be public
		- initialize when true the repository is also initialized (usually with a default README file)

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the GIT_HOSTING feature.
	*/
	CreateGitRepository(name string, description *string, restricted bool, initialize bool) (*GitHostedRepository, error)

	/*
		Deletes a Git repository for the currently authenticated user.

		Please note that if the service has been configured with repository owner and name those attributes are ignored
		by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
		name is always the name attribute.

		Arguments are as follows:

		- name the repository name. Cannot be nil

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the GIT_HOSTING feature.
	*/
	DeleteGitRepository(name string) error
}
