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

package gitlab

import (
	"strconv" // https://pkg.go.dev/strconv

	gl "gitlab.com/gitlab-org/api/client-go" // https://pkg.go.dev/gitlab.com/gitlab-org/api/client-go
)

/*
A user for a remote GitLab service.
*/
type GitLabUser struct {
	// The ID for the user on this service.
	id string

	// The name for the user on this service.
	userName string

	// The full name for the user on this service.
	fullName string
}

/*
Creates the user object modelled by the attributed from the given reference.

Arguments are as follows:

  - user the object to read the attributes from
*/
func newGitLabUser(user gl.User) *GitLabUser {
	res := &GitLabUser{}
	res.id = strconv.FormatInt(user.ID, 10)
	res.userName = user.Username
	res.fullName = user.Name
	return res
}

/*
Returns the ID for the user on this service.
*/
func (u *GitLabUser) GetID() string {
	return u.id
}

/*
Returns the name for the user on this service.
*/
func (u *GitLabUser) GetUserName() string {
	return u.userName
}

/*
Returns the full name for the user on this service.
*/
func (u *GitLabUser) GetFullName() string {
	return u.fullName
}
