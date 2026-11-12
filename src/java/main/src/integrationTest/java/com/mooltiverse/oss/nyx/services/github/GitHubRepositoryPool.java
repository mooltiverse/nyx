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
package com.mooltiverse.oss.nyx.services.github;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.kohsuke.github.GHFileNotFoundException;
import org.kohsuke.github.GHRelease;
import org.kohsuke.github.GHRepository;
import org.kohsuke.github.GitHubBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.git.tools.HostedRepositoryPool;

/**
 * A pool of long-lived GitHub repositories for integration tests, used instead of creating (and deleting) a new
 * repository for every test, to avoid GitHub's secondary rate limit on content creation.
 * <br>
 * See {@link HostedRepositoryPool} for how repositories are reset and verified. On GitHub the reset also deletes all
 * releases, sets the default branch and the repository visibility.
 */
public final class GitHubRepositoryPool extends HostedRepositoryPool<GHRepository, GitHubRepository> {
    /**
     * The private logger instance.
     */
    private static final Logger logger = LoggerFactory.getLogger(GitHubRepositoryPool.class);

    /**
     * The GitHub client, lazily initialized.
     */
    private org.kohsuke.github.GitHub client = null;

    /**
     * The owner of the pooled repositories (the authenticated user), lazily initialized.
     */
    private String owner = null;

    /**
     * The authentication token, also used as the user name for Git operations.
     */
    private final String token;

    /**
     * Builds a pool scoped to the given environment.
     *
     * @param token the GitHub authentication token. It can't be {@code null} or blank.
     * @param environment the environment the pool is scoped to. It can't be {@code null} or blank. It's sanitized
     * so it can be used in repository names.
     */
    GitHubRepositoryPool(String token, String environment) {
        super(environment);
        Objects.requireNonNull(token, "The GitHub token can't be null");
        if (token.isBlank())
            throw new IllegalArgumentException("The GitHub token can't be blank");
        this.token = token;
    }

    /**
     * Returns a pool scoped to the {@link #defaultEnvironment() default environment}.
     *
     * @param token the GitHub authentication token. It can't be {@code null} or blank.
     *
     * @return the pool
     */
    public static GitHubRepositoryPool instance(String token) {
        return new GitHubRepositoryPool(token, defaultEnvironment());
    }

    /**
     * Returns a pool scoped to the {@link #defaultEnvironment() default environment} and the given scope, so that
     * different groups of tests (i.e. integration and functional tests) never share a repository, even if they
     * run at the same time.
     * 
     * @param token the GitHub authentication token. It can't be {@code null} or blank.
     * @param scope the scope, appended to the default environment (i.e. {@code functional}). It can't be
     * {@code null}.
     * 
     * @return the pool
     */
    public static GitHubRepositoryPool instance(String token, String scope) {
        return new GitHubRepositoryPool(token, scopedEnvironment(scope));
    }

    /**
     * Returns the GitHub client, initializing it the first time.
     *
     * @return the GitHub client
     *
     * @throws IOException if the client can't be built
     */
    private org.kohsuke.github.GitHub getClient()
        throws IOException {
        if (Objects.isNull(client))
            client = new GitHubBuilder().withOAuthToken(token).build();
        return client;
    }

    /**
     * Returns the owner of the pooled repositories, which is the authenticated user.
     *
     * @return the owner of the pooled repositories
     *
     * @throws IOException if the authenticated user can't be retrieved
     */
    private String getOwner()
        throws IOException {
        if (Objects.isNull(owner))
            owner = getClient().getMyself().getLogin();
        return owner;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected GHRepository find(String name)
        throws Exception {
        try {
            return getClient().getRepository(getOwner().concat("/").concat(name));
        }
        catch (GHFileNotFoundException fnfe) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void create(String name, boolean restricted)
        throws Exception {
        getClient().createRepository(name).description(REPOSITORY_DESCRIPTION).visibility(restricted ? GHRepository.Visibility.PRIVATE : GHRepository.Visibility.PUBLIC).autoInit(false).create();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void delete(GHRepository repository)
        throws Exception {
        repository.delete();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String httpURL(GHRepository repository) {
        return repository.getHttpTransportUrl();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected CredentialsProvider credentials() {
        return new UsernamePasswordCredentialsProvider(token, "");
    }

    /**
     * {@inheritDoc}
     * Deletes all releases, as they survive the deletion of their tags.
     */
    @Override
    protected void beforePush(GHRepository repository)
        throws Exception {
        for (GHRelease release: repository.listReleases().toList()) {
            logger.debug("Deleting release '{}' from the pooled repository '{}'", release.getTagName(), repository.getName());
            release.delete();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void afterPush(GHRepository repository, boolean restricted)
        throws Exception {
        if (!DEFAULT_BRANCH.equals(repository.getDefaultBranch()))
            retry(() -> { repository.setDefaultBranch(DEFAULT_BRANCH); return null; });
        if (repository.isPrivate() != restricted)
            retry(() -> { repository.setVisibility(restricted ? GHRepository.Visibility.PRIVATE : GHRepository.Visibility.PUBLIC); return null; });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected List<String> inspect(GHRepository repository, boolean restricted)
        throws Exception {
        List<String> problems = new ArrayList<String>();
        if (!DEFAULT_BRANCH.equals(repository.getDefaultBranch()))
            problems.add(String.format("Unexpected default branch: '%s', expected: '%s'", repository.getDefaultBranch(), DEFAULT_BRANCH));
        if (repository.isPrivate() != restricted)
            problems.add(String.format("Unexpected visibility: %s, expected: %s", repository.isPrivate() ? "private" : "public", restricted ? "private" : "public"));
        List<String> releases = new ArrayList<String>();
        for (GHRelease release: repository.listReleases().toList())
            releases.add(release.getTagName());
        if (!releases.isEmpty())
            problems.add(String.format("Unexpected releases: %s", releases));
        return problems;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected GitHubRepository wrap(GHRepository repository)
        throws Exception {
        return new GitHubRepository(repository);
    }
}
