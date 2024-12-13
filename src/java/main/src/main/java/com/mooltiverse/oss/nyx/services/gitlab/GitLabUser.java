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

import com.mooltiverse.oss.nyx.services.User;

/**
 * A user for a remote GitLab service.
 */
public class GitLabUser extends GitLabEntity implements User {
    /**
     * Creates the user object modelled by the given attributes.
     * 
     * @param api the reference to the API used to communicate with the remote end. Can't be {@code null}
     * @param attributes the map of attributes for this object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given attributes map is {@code null}
     * @throws IllegalArgumentException if the map of attributes is empty
     */
     GitLabUser(API api, Map<String, Object> attributes) {
        super(api, attributes);
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
    public String getUserName() {
        return getAttributes().get("username").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String geFullName() {
        // this is not a mandatory attribute so we need to check for nulls
        Object res = getAttributes().get("name");
        return res == null ? null : res.toString();
    }
}