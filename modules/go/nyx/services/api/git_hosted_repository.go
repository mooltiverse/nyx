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
A remote Git repository hosted on some service. These entities are managed through services implementing the
GitHostingService interface and supporting the GIT_HOSTING feature.
*/
type GitHostedRepository interface {
	/*
		Returns the repository default branch.
	*/
	GetDefaultBranch() string

	/*
		Returns the repository description.
	*/
	GetDescription() string

	/*
		Returns the repository full name.
	*/
	GetFullName() string

	/*
		Returns the repository HTTP URL.
	*/
	GetHTTPURL() string

	/*
		Returns the repository SSH URL.
	*/
	GetSSHURL() string

	/*
		Returns the repository ID.
	*/
	GetID() string

	/*
		Returns the repository name.
	*/
	GetName() string
}
