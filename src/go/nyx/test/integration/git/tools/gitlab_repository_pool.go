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

package tools

import (
	"errors"   // https://pkg.go.dev/errors
	"fmt"      // https://pkg.go.dev/fmt
	"net/http" // https://pkg.go.dev/net/http
	"strconv"  // https://pkg.go.dev/strconv
	"strings"  // https://pkg.go.dev/strings

	ggithttp "github.com/go-git/go-git/v5/plumbing/transport/http" // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/transport/http
	log "github.com/sirupsen/logrus"                               // https://pkg.go.dev/github.com/sirupsen/logrus
	gl "gitlab.com/gitlab-org/api/client-go"                       // https://pkg.go.dev/gitlab.com/gitlab-org/api/client-go

	api "github.com/mooltiverse/nyx/src/go/nyx/services/api"
)

/*
A pool of long-lived GitLab repositories (projects) for integration tests, used instead of creating (and deleting) a new
repository for every test, to avoid rate limits on content creation.

See hostedRepositoryPool for how repositories are reset and verified. On GitLab the reset also deletes all releases and
packages (release assets are uploaded as packages, which survive the deletion of releases), removes branch and tag
protections (GitLab protects the default branch on the first push, which prevents force pushes), sets the default branch
and the repository visibility.
*/
type GitLabRepositoryPool struct {
	hostedRepositoryPool[*gl.Project]

	// The GitLab client.
	client *gl.Client

	// The owner of the pooled repositories (the authenticated user), lazily initialized.
	owner string

	// The authentication token.
	token string
}

/*
A pooled GitLab repository, implementing the GitHostedRepository interface.
*/
type pooledGitLabRepository struct {
	project *gl.Project
}

func (r pooledGitLabRepository) GetDefaultBranch() string { return r.project.DefaultBranch }
func (r pooledGitLabRepository) GetDescription() string   { return r.project.Description }
func (r pooledGitLabRepository) GetFullName() string      { return r.project.Name }
func (r pooledGitLabRepository) GetHTTPURL() string       { return r.project.HTTPURLToRepo }
func (r pooledGitLabRepository) GetSSHURL() string        { return r.project.SSHURLToRepo }
func (r pooledGitLabRepository) GetID() string            { return strconv.FormatInt(r.project.ID, 10) }
func (r pooledGitLabRepository) GetName() string          { return r.project.Path }

/*
Returns a GitLab pool scoped to the given environment.

Arguments are as follows:

  - token the GitLab authentication token. It can't be empty.
  - environment the environment the pool is scoped to. It's sanitized so it can be used in repository names and it
    can't be empty after sanitization.

Errors:

  - an error if any argument is not valid
*/
func NewGitLabRepositoryPool(token string, environment string) (*GitLabRepositoryPool, error) {
	if strings.TrimSpace(token) == "" {
		return nil, errors.New("the GitLab token can't be empty")
	}
	client, err := gl.NewClient(token)
	if err != nil {
		return nil, err
	}
	p := &GitLabRepositoryPool{token: token, client: client}
	base, err := newHostedRepositoryPool[*gl.Project](environment, p)
	if err != nil {
		return nil, err
	}
	p.hostedRepositoryPool = base
	return p, nil
}

/*
Returns a GitLab pool scoped to the default environment (see HostedRepositoryPoolDefaultEnvironment()).

Arguments are as follows:

  - token the GitLab authentication token. It can't be empty.

Errors:

  - an error if the token is not valid
*/
func GitLabRepositoryPoolInstance(token string) (*GitLabRepositoryPool, error) {
	return NewGitLabRepositoryPool(token, HostedRepositoryPoolDefaultEnvironment())
}

/*
Returns the owner of the pooled repositories, which is the authenticated user.
*/
func (p *GitLabRepositoryPool) getOwner() (string, error) {
	if p.owner == "" {
		u, _, err := p.client.Users.CurrentUser()
		if err != nil {
			return "", fmt.Errorf("unable to retrieve the authenticated user: %w", err)
		}
		p.owner = u.Username
	}
	return p.owner, nil
}

/*
Returns the given visibility as a GitLab visibility value.
*/
func gitLabVisibility(restricted bool) gl.VisibilityValue {
	if restricted {
		return gl.PrivateVisibility
	}
	return gl.PublicVisibility
}

/*
Returns all the releases of the given project.
*/
func (p *GitLabRepositoryPool) listReleases(project *gl.Project) ([]*gl.Release, error) {
	res := []*gl.Release{}
	options := &gl.ListReleasesOptions{ListOptions: gl.ListOptions{PerPage: 100}}
	for {
		releases, response, err := p.client.Releases.ListReleases(project.ID, options)
		if err != nil {
			return nil, fmt.Errorf("unable to list the releases of the pooled repository '%s': %w", project.Path, err)
		}
		res = append(res, releases...)
		if response.NextPage == 0 {
			return res, nil
		}
		options.Page = response.NextPage
	}
}

/*
Returns all the packages of the given project.
*/
func (p *GitLabRepositoryPool) listPackages(project *gl.Project) ([]*gl.Package, error) {
	res := []*gl.Package{}
	options := &gl.ListProjectPackagesOptions{ListOptions: gl.ListOptions{PerPage: 100}}
	for {
		packages, response, err := p.client.Packages.ListProjectPackages(project.ID, options)
		if err != nil {
			return nil, fmt.Errorf("unable to list the packages of the pooled repository '%s': %w", project.Path, err)
		}
		res = append(res, packages...)
		if response.NextPage == 0 {
			return res, nil
		}
		options.Page = response.NextPage
	}
}

func (p *GitLabRepositoryPool) find(name string) (*gl.Project, bool, error) {
	owner, err := p.getOwner()
	if err != nil {
		return nil, false, err
	}
	project, response, err := p.client.Projects.GetProject(owner+"/"+name, nil)
	if err != nil {
		if response != nil && response.StatusCode == http.StatusNotFound {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("unable to retrieve the repository '%s/%s': %w", owner, name, err)
	}
	// projects pending deletion are not usable
	if project.MarkedForDeletionOn != nil {
		return nil, false, nil
	}
	return project, true, nil
}

func (p *GitLabRepositoryPool) create(name string, restricted bool) error {
	_, _, err := p.client.Projects.CreateProject(&gl.CreateProjectOptions{Name: gl.Ptr(name), Path: gl.Ptr(name), Description: gl.Ptr(hostedRepositoryPoolDescription), Visibility: gl.Ptr(gitLabVisibility(restricted)), InitializeWithReadme: gl.Ptr(false)})
	return err
}

/*
Deletes the given project. If the project is only marked for deletion (delayed deletion), it's then removed permanently
so that its name can be used again.
*/
func (p *GitLabRepositoryPool) delete(project *gl.Project) error {
	if _, err := p.client.Projects.DeleteProject(project.ID, nil); err != nil {
		return err
	}
	deleted, response, err := p.client.Projects.GetProject(project.ID, nil)
	if err != nil {
		if response != nil && response.StatusCode == http.StatusNotFound {
			return nil
		}
		return err
	}
	if deleted.MarkedForDeletionOn != nil {
		_, err = p.client.Projects.DeleteProject(project.ID, &gl.DeleteProjectOptions{FullPath: gl.Ptr(deleted.PathWithNamespace), PermanentlyRemove: gl.Ptr(true)})
	}
	return err
}

func (p *GitLabRepositoryPool) httpURL(project *gl.Project) string {
	return project.HTTPURLToRepo
}

func (p *GitLabRepositoryPool) gitAuth() *ggithttp.BasicAuth {
	return &ggithttp.BasicAuth{Username: "PRIVATE-TOKEN", Password: p.token}
}

/*
Deletes all releases and packages and removes all branch and tag protections.
*/
func (p *GitLabRepositoryPool) beforePush(project *gl.Project) error {
	releases, err := p.listReleases(project)
	if err != nil {
		return err
	}
	for _, release := range releases {
		log.Debugf("deleting release '%s' from the pooled repository '%s'", release.TagName, project.Path)
		if _, _, err = p.client.Releases.DeleteRelease(project.ID, release.TagName); err != nil {
			return fmt.Errorf("unable to delete the release '%s' from the pooled repository '%s': %w", release.TagName, project.Path, err)
		}
	}
	packages, err := p.listPackages(project)
	if err != nil {
		return err
	}
	for _, pkg := range packages {
		log.Debugf("deleting package '%s' from the pooled repository '%s'", pkg.Name, project.Path)
		if _, err = p.client.Packages.DeleteProjectPackage(project.ID, pkg.ID); err != nil {
			return fmt.Errorf("unable to delete the package '%s' from the pooled repository '%s': %w", pkg.Name, project.Path, err)
		}
	}
	branches, _, err := p.client.ProtectedBranches.ListProtectedBranches(project.ID, &gl.ListProtectedBranchesOptions{ListOptions: gl.ListOptions{PerPage: 100}})
	if err != nil {
		return fmt.Errorf("unable to list the protected branches of the pooled repository '%s': %w", project.Path, err)
	}
	for _, branch := range branches {
		if _, err = p.client.ProtectedBranches.UnprotectRepositoryBranches(project.ID, branch.Name); err != nil {
			return fmt.Errorf("unable to unprotect the branch '%s' of the pooled repository '%s': %w", branch.Name, project.Path, err)
		}
	}
	tags, _, err := p.client.ProtectedTags.ListProtectedTags(project.ID, &gl.ListProtectedTagsOptions{ListOptions: gl.ListOptions{PerPage: 100}})
	if err != nil {
		return fmt.Errorf("unable to list the protected tags of the pooled repository '%s': %w", project.Path, err)
	}
	for _, tag := range tags {
		if _, err = p.client.ProtectedTags.UnprotectRepositoryTags(project.ID, tag.Name); err != nil {
			return fmt.Errorf("unable to unprotect the tag '%s' of the pooled repository '%s': %w", tag.Name, project.Path, err)
		}
	}
	return nil
}

func (p *GitLabRepositoryPool) afterPush(project *gl.Project, restricted bool) error {
	if project.DefaultBranch == HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH && project.Visibility == gitLabVisibility(restricted) {
		return nil
	}
	return retryHostedRepositoryPoolOperation(func() error {
		_, _, err := p.client.Projects.EditProject(project.ID, &gl.EditProjectOptions{DefaultBranch: gl.Ptr(HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH), Visibility: gl.Ptr(gitLabVisibility(restricted))})
		return err
	})
}

/*
Branch protections are not inspected as they don't affect tests and GitLab adds them asynchronously after the first push
to the default branch. They are removed by beforePush() anyway.
*/
func (p *GitLabRepositoryPool) inspect(project *gl.Project, restricted bool) ([]string, error) {
	problems := []string{}
	if project.DefaultBranch != HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH {
		problems = append(problems, fmt.Sprintf("unexpected default branch: '%s', expected: '%s'", project.DefaultBranch, HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH))
	}
	if project.Visibility != gitLabVisibility(restricted) {
		problems = append(problems, fmt.Sprintf("unexpected visibility: %s, expected: %s", project.Visibility, gitLabVisibility(restricted)))
	}
	releases, err := p.listReleases(project)
	if err != nil {
		return nil, err
	}
	if len(releases) > 0 {
		tags := []string{}
		for _, release := range releases {
			tags = append(tags, release.TagName)
		}
		problems = append(problems, fmt.Sprintf("unexpected releases: %v", tags))
	}
	packages, err := p.listPackages(project)
	if err != nil {
		return nil, err
	}
	if len(packages) > 0 {
		names := []string{}
		for _, pkg := range packages {
			names = append(names, pkg.Name+"/"+pkg.Version)
		}
		problems = append(problems, fmt.Sprintf("unexpected packages: %v", names))
	}
	return problems, nil
}

func (p *GitLabRepositoryPool) wrap(project *gl.Project) api.GitHostedRepository {
	return pooledGitLabRepository{project: project}
}
