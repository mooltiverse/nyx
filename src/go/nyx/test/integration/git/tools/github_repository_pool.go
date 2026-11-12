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
	"context"  // https://pkg.go.dev/context
	"errors"   // https://pkg.go.dev/errors
	"fmt"      // https://pkg.go.dev/fmt
	"net/http" // https://pkg.go.dev/net/http
	"strconv"  // https://pkg.go.dev/strconv
	"strings"  // https://pkg.go.dev/strings

	ggithttp "github.com/go-git/go-git/v5/plumbing/transport/http" // https://pkg.go.dev/github.com/go-git/go-git/v5/plumbing/transport/http
	gh "github.com/google/go-github/github"                        // https://pkg.go.dev/github.com/google/go-github/github
	log "github.com/sirupsen/logrus"                               // https://pkg.go.dev/github.com/sirupsen/logrus
	oauth2 "golang.org/x/oauth2"                                   // https://pkg.go.dev/golang.org/x/oauth2

	api "github.com/mooltiverse/nyx/src/go/nyx/services/api"
)

/*
A pool of long-lived GitHub repositories for integration tests, used instead of creating (and deleting) a new repository
for every test, to avoid GitHub's secondary rate limit on content creation.

See hostedRepositoryPool for how repositories are reset and verified. On GitHub the reset also deletes all releases,
sets the default branch and the repository visibility.
*/
type GitHubRepositoryPool struct {
	hostedRepositoryPool[*gh.Repository]

	// The GitHub client.
	client *gh.Client

	// The owner of the pooled repositories (the authenticated user), lazily initialized.
	owner string

	// The authentication token, also used as the user name for Git operations.
	token string
}

/*
A pooled GitHub repository, implementing the GitHostedRepository interface.
*/
type pooledGitHubRepository struct {
	repository *gh.Repository
}

func (r pooledGitHubRepository) GetDefaultBranch() string { return r.repository.GetDefaultBranch() }
func (r pooledGitHubRepository) GetDescription() string   { return r.repository.GetDescription() }
func (r pooledGitHubRepository) GetFullName() string      { return r.repository.GetFullName() }
func (r pooledGitHubRepository) GetHTTPURL() string       { return r.repository.GetCloneURL() }
func (r pooledGitHubRepository) GetSSHURL() string        { return r.repository.GetSSHURL() }
func (r pooledGitHubRepository) GetID() string            { return strconv.FormatInt(r.repository.GetID(), 10) }
func (r pooledGitHubRepository) GetName() string          { return r.repository.GetName() }

/*
Returns a GitHub pool scoped to the given environment.

Arguments are as follows:

  - token the GitHub authentication token. It can't be empty.
  - environment the environment the pool is scoped to. It's sanitized so it can be used in repository names and it
    can't be empty after sanitization.

Errors:

  - an error if any argument is not valid
*/
func NewGitHubRepositoryPool(token string, environment string) (*GitHubRepositoryPool, error) {
	if strings.TrimSpace(token) == "" {
		return nil, errors.New("the GitHub token can't be empty")
	}
	p := &GitHubRepositoryPool{token: token, client: gh.NewClient(oauth2.NewClient(context.Background(), oauth2.StaticTokenSource(&oauth2.Token{AccessToken: token})))}
	base, err := newHostedRepositoryPool[*gh.Repository](environment, p)
	if err != nil {
		return nil, err
	}
	p.hostedRepositoryPool = base
	return p, nil
}

/*
Returns a GitHub pool scoped to the default environment (see HostedRepositoryPoolDefaultEnvironment()).

Arguments are as follows:

  - token the GitHub authentication token. It can't be empty.

Errors:

  - an error if the token is not valid
*/
func GitHubRepositoryPoolInstance(token string) (*GitHubRepositoryPool, error) {
	return NewGitHubRepositoryPool(token, HostedRepositoryPoolDefaultEnvironment())
}

/*
Returns the owner of the pooled repositories, which is the authenticated user.
*/
func (p *GitHubRepositoryPool) getOwner() (string, error) {
	if p.owner == "" {
		u, _, err := p.client.Users.Get(context.Background(), "")
		if err != nil {
			return "", fmt.Errorf("unable to retrieve the authenticated user: %w", err)
		}
		p.owner = u.GetLogin()
	}
	return p.owner, nil
}

/*
Returns all the releases of the given repository.
*/
func (p *GitHubRepositoryPool) listReleases(repository *gh.Repository) ([]*gh.RepositoryRelease, error) {
	owner, err := p.getOwner()
	if err != nil {
		return nil, err
	}
	res := []*gh.RepositoryRelease{}
	options := &gh.ListOptions{PerPage: 100}
	for {
		releases, response, err := p.client.Repositories.ListReleases(context.Background(), owner, repository.GetName(), options)
		if err != nil {
			return nil, fmt.Errorf("unable to list the releases of the pooled repository '%s': %w", repository.GetName(), err)
		}
		res = append(res, releases...)
		if response.NextPage == 0 {
			return res, nil
		}
		options.Page = response.NextPage
	}
}

func (p *GitHubRepositoryPool) find(name string) (*gh.Repository, bool, error) {
	owner, err := p.getOwner()
	if err != nil {
		return nil, false, err
	}
	repository, response, err := p.client.Repositories.Get(context.Background(), owner, name)
	if err != nil {
		if response != nil && response.StatusCode == http.StatusNotFound {
			return nil, false, nil
		}
		return nil, false, fmt.Errorf("unable to retrieve the repository '%s/%s': %w", owner, name, err)
	}
	return repository, true, nil
}

func (p *GitHubRepositoryPool) create(name string, restricted bool) error {
	_, _, err := p.client.Repositories.Create(context.Background(), "", &gh.Repository{Name: gh.String(name), Description: gh.String(hostedRepositoryPoolDescription), Private: gh.Bool(restricted), AutoInit: gh.Bool(false)})
	return err
}

func (p *GitHubRepositoryPool) delete(repository *gh.Repository) error {
	owner, err := p.getOwner()
	if err != nil {
		return err
	}
	_, err = p.client.Repositories.Delete(context.Background(), owner, repository.GetName())
	return err
}

func (p *GitHubRepositoryPool) httpURL(repository *gh.Repository) string {
	return repository.GetCloneURL()
}

func (p *GitHubRepositoryPool) gitAuth() *ggithttp.BasicAuth {
	return &ggithttp.BasicAuth{Username: p.token, Password: ""}
}

/*
Deletes all releases, as they survive the deletion of their tags.
*/
func (p *GitHubRepositoryPool) beforePush(repository *gh.Repository) error {
	owner, err := p.getOwner()
	if err != nil {
		return err
	}
	releases, err := p.listReleases(repository)
	if err != nil {
		return err
	}
	for _, release := range releases {
		log.Debugf("deleting release '%s' from the pooled repository '%s'", release.GetTagName(), repository.GetName())
		if _, err = p.client.Repositories.DeleteRelease(context.Background(), owner, repository.GetName(), release.GetID()); err != nil {
			return fmt.Errorf("unable to delete the release '%s' from the pooled repository '%s': %w", release.GetTagName(), repository.GetName(), err)
		}
	}
	return nil
}

func (p *GitHubRepositoryPool) afterPush(repository *gh.Repository, restricted bool) error {
	if repository.GetDefaultBranch() == HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH && repository.GetPrivate() == restricted {
		return nil
	}
	owner, err := p.getOwner()
	if err != nil {
		return err
	}
	return retryHostedRepositoryPoolOperation(func() error {
		_, _, err := p.client.Repositories.Edit(context.Background(), owner, repository.GetName(), &gh.Repository{DefaultBranch: gh.String(HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH), Private: gh.Bool(restricted)})
		return err
	})
}

func (p *GitHubRepositoryPool) inspect(repository *gh.Repository, restricted bool) ([]string, error) {
	problems := []string{}
	if repository.GetDefaultBranch() != HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH {
		problems = append(problems, fmt.Sprintf("unexpected default branch: '%s', expected: '%s'", repository.GetDefaultBranch(), HOSTED_REPOSITORY_POOL_DEFAULT_BRANCH))
	}
	if repository.GetPrivate() != restricted {
		problems = append(problems, fmt.Sprintf("unexpected private flag: %t, expected: %t", repository.GetPrivate(), restricted))
	}
	releases, err := p.listReleases(repository)
	if err != nil {
		return nil, err
	}
	if len(releases) > 0 {
		tags := []string{}
		for _, release := range releases {
			tags = append(tags, release.GetTagName())
		}
		problems = append(problems, fmt.Sprintf("unexpected releases: %v", tags))
	}
	return problems, nil
}

func (p *GitHubRepositoryPool) wrap(repository *gh.Repository) api.GitHostedRepository {
	return pooledGitHubRepository{repository: repository}
}
