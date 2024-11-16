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
A user belonging to a service. These entities are managed through services implementing the
UserService interface and supporting the USERS feature.
*/
type UserService interface {
	/*
		Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the USERS feature.
	*/
	GetAuthenticatedUser() (*User, error)
}
