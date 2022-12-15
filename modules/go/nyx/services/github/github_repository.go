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
A user for a remote GitHub repository.
*/
type GitHubRepository struct {
	// The repository default branch.
	defaultBranch string

	// The repository description.
	description string

	// The repository full name.
	fullName string

	// The repository HTTP URL.
	httpURL string

	// The repository ID.
	id string

	// The repository name.
	name string
}

/*
Creates the user object modelled by the attributed from the given reference.

Arguments are as follows:

  - repository the object to read the attributes from
*/
func newGitHubRepository(repository gh.Repository) *GitHubRepository {
	res := &GitHubRepository{}
	res.defaultBranch = *repository.DefaultBranch
	res.description = *repository.Description
	res.fullName = *repository.FullName
	res.httpURL = *repository.CloneURL
	res.id = strconv.FormatInt(*repository.ID, 10)
	res.name = *repository.Name
	return res
}

/*
Returns the repository default branch.
*/
func (r *GitHubRepository) GetDefaultBranch() string {
	return r.defaultBranch
}

/*
Returns the repository description.
*/
func (r *GitHubRepository) GetDescription() string {
	return r.description
}

/*
Returns the repository full name.
*/
func (r *GitHubRepository) GetFullName() string {
	return r.fullName
}

/*
Returns the repository HTTP URL.
*/
func (r *GitHubRepository) GetHTTPURL() string {
	return r.httpURL
}

/*
Returns the repository ID.
*/
func (r *GitHubRepository) GetID() string {
	return r.id
}

/*
Returns the repository name.
*/
func (r *GitHubRepository) GetName() string {
	return r.name
}
