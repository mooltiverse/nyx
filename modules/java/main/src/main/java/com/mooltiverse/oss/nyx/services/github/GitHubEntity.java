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

import org.kohsuke.github.GHObject;

import com.mooltiverse.oss.nyx.io.TransportException;

/**
 * An abstract base class for GitHub entities.
 */
abstract class GitHubEntity {
    /**
     * The user ID
     */
    private final long id;

    /**
     * Creates the object modelled by the given reference.
     * 
     * @param entity the reference to the backing object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given reference is {@code null}
     * @throws TransportException if the given reference can't be read for some reasons
     */
    protected GitHubEntity(GHObject entity)
        throws TransportException {
        Objects.requireNonNull(entity, "The entity reference cannot be null");
        this.id = entity.getId();
    }

    /**
     * Returns the ID for the entity on this service.
     * 
     * @return the ID for the entity on this service.
     */
    public String getID() {
        return Long.valueOf(id).toString();
    }
}