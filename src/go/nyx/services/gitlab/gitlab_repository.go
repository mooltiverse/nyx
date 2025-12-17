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
A user for a remote GitLab repository.
*/
type GitLabRepository struct {
	// The repository default branch.
	defaultBranch string

	// The repository description.
	description string

	// The repository full name.
	fullName string

	// The repository HTTP URL.
	httpURL string

	// The repository SSH URL.
	sshURL string

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
func newGitLabRepository(repository gl.Project) *GitLabRepository {
	res := &GitLabRepository{}
	res.defaultBranch = repository.DefaultBranch
	res.description = repository.Description
	res.fullName = repository.Name
	res.httpURL = repository.HTTPURLToRepo
	res.sshURL = repository.SSHURLToRepo
	res.id = strconv.FormatInt(repository.ID, 10)
	res.name = repository.Path
	return res
}

/*
Returns the repository default branch.
*/
func (r *GitLabRepository) GetDefaultBranch() string {
	return r.defaultBranch
}

/*
Returns the repository description.
*/
func (r *GitLabRepository) GetDescription() string {
	return r.description
}

/*
Returns the repository full name.
*/
func (r *GitLabRepository) GetFullName() string {
	return r.fullName
}

/*
Returns the repository HTTP URL.
*/
func (r *GitLabRepository) GetHTTPURL() string {
	return r.httpURL
}

/*
Returns the repository SSH URL.
*/
func (r *GitLabRepository) GetSSHURL() string {
	return r.sshURL
}

/*
Returns the repository ID.
*/
func (r *GitLabRepository) GetID() string {
	return r.id
}

/*
Returns the repository name.
*/
func (r *GitLabRepository) GetName() string {
	return r.name
}
