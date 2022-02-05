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
package com.mooltiverse.oss.nyx.entities;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * A value holder that models a section containing a map of release types.
 */
public class ReleaseTypes extends EnabledItemsMap<ReleaseType> {
    /**
     * The private list of publication services.
     */
    private List<String> publicationServices = null;

    /**
     * The private list of remote repositories.
     */
    private List<String> remoteRepositories = null;

    /**
     * Default constructor. Constructs a new object with no items.
     */
    public ReleaseTypes() {
        super();
    }

    /**
     * Standard constructor. Constructs a new object with the given items.
     * 
     * @param enabled the list of names of enabled items
     * @param publicationServices the list of names of publication services
     * @param remoteRepositories the list of remote repositories
     * @param items the map of items
     * 
     * @throws NullPointerException if some argument is {@code null}
     */
    public ReleaseTypes(List<String> enabled, List<String> publicationServices, List<String> remoteRepositories, Map<String,ReleaseType> items) {
        super(enabled, items);
        Objects.requireNonNull(publicationServices);
        this.publicationServices = publicationServices;
        this.remoteRepositories = remoteRepositories;
    }

    /**
     * Returns the list of publication services.
     * 
     * @return the list of publication services. It may be empty but not {@code null} if not configured.
     */
    public List<String> getPublicationServices() {
        return publicationServices;
    }

    /**
     * Returns the list of remote repositories.
     * 
     * @return the list of remote repositories. It may be empty but not {@code null} if not configured.
     */
    public List<String> getRemoteRepositories() {
        return remoteRepositories;
    }
}
