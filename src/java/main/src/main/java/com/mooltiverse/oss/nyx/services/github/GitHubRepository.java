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

import java.util.Objects;

import org.kohsuke.github.GHRepository;

import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.GitHostedRepository;

/**
 * A Git repository hosted on GitHub.
 */
public class GitHubRepository extends GitHubEntity implements GitHostedRepository {
    /**
     * The repository name
     */
    private final String name;

    /**
     * The repository full name
     */
    private final String fullName;

    /**
     * The repository description
     */
    private final String description;

    /**
     * The repository default branch
     */
    private final String defaultBranch;

    /**
     * The repository HTTP URL
     */
    private final String httpUrl;

    /**
     * The repository SSH URL
     */
    private final String sshUrl;

    /**
     * Creates the repository object modelled by the given reference.
     * 
     * @param repo the reference to the backing object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given reference is {@code null}
     * @throws TransportException if the given reference can't be read for some reasons
     */
    GitHubRepository(GHRepository repo)
        throws TransportException {
        super(repo);
        Objects.requireNonNull(repo, "The repository reference cannot be null");
        this.name = repo.getName();
        this.fullName = repo.getFullName();
        this.description = repo.getDescription();
        this.defaultBranch = repo.getDefaultBranch();
        this.httpUrl = repo.getHttpTransportUrl();
        this.sshUrl = repo.getSshUrl();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultBranch() {
        return defaultBranch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return description;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getFullName() {
        return fullName;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getHTTPURL() {
        return httpUrl;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSSHURL() {
        return sshUrl;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        return name;
    }
}