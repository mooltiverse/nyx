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

package github

import (
	"strconv" // https://pkg.go.dev/strconv

	gh "github.com/google/go-github/github" // https://pkg.go.dev/github.com/google/go-github/github
)

/*
A user for a remote GitHub service.
*/
type GitHubUser struct {
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
func newGitHubUser(user gh.User) *GitHubUser {
	res := &GitHubUser{}
	res.id = strconv.FormatInt(*user.ID, 10)
	res.userName = *user.Login
	res.fullName = *user.Name
	return res
}

/*
Returns the ID for the user on this service.
*/
func (u *GitHubUser) GetID() string {
	return u.id
}

/*
Returns the name for the user on this service.
*/
func (u *GitHubUser) GetUserName() string {
	return u.userName
}

/*
Returns the full name for the user on this service.
*/
func (u *GitHubUser) GetFullName() string {
	return u.fullName
}
