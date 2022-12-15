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

import java.util.Objects;

import org.kohsuke.github.GHUser;

import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.User;

/**
 * A user for a remote GitHub service.
 */
public class GitHubUser extends GitHubEntity implements User {
    /**
     * The user name
     */
    private final String userName;

    /**
     * The user full name
     */
    private final String fullName;

    /**
     * Creates the user object modelled by the given reference.
     * 
     * @param user the reference to the backing object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given reference is {@code null}
     * @throws TransportException if the given reference can't be read for some reasons
     */
    GitHubUser(GHUser user)
        throws TransportException {
        super(user);
        Objects.requireNonNull(user, "The user reference cannot be null");
        try {
            this.userName = user.getLogin();
            this.fullName = user.getName();
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the user full name"), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getUserName() {
        return userName;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String geFullName() {
        return fullName;
    }
}