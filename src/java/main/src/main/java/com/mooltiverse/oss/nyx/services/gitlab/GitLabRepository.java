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
package com.mooltiverse.oss.nyx.services.gitlab;

import java.util.Map;

import com.mooltiverse.oss.nyx.services.GitHostedRepository;

/**
 * A Git repository hosted on GitLab.
 */
public class GitLabRepository extends GitLabEntity implements GitHostedRepository {
    /**
     * Creates the user object modelled by the given attributes.
     * 
     * @param api the reference to the API used to communicate with the remote end. Can't be {@code null}
     * @param attributes the map of attributes for this object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given attributes map is {@code null}
     * @throws IllegalArgumentException if the map of attributes is empty
     */
    GitLabRepository(API api, Map<String, Object> attributes) {
        super(api, attributes);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultBranch() {
        return getAttributes().get("default_branch").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return getAttributes().get("description").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getFullName() {
        return getAttributes().get("name").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getHTTPURL() {
        return getAttributes().get("http_url_to_repo").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSSHURL() {
        return getAttributes().get("ssh_url_to_repo").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getID() {
        return getAttributes().get("id").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        return getAttributes().get("path").toString();
    }
}