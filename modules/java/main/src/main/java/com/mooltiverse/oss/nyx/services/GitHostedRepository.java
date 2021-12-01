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
package com.mooltiverse.oss.nyx.services;

/**
 * A remote Git repository hosted on some service. These entities are managed through services implementing the
 * {@link GitHostingService} interface and {@link Service#supports(Feature) supporting} the
 * {@link Service.Feature#GIT_HOSTING} feature.
 */
public interface GitHostedRepository {
    /**
     * Returns the repository default branch.
     * 
     * @return the repository default branch.
     */
    public String getDefaultBranch();

    /**
     * Returns the repository description.
     * 
     * @return the repository description.
     */
    public String getDescription();

    /**
     * Returns the repository full name.
     * 
     * @return the repository full name.
     */
    public String getFullName();

    /**
     * Returns the repository HTTP URL.
     * 
     * @return the repository HTTP URL.
     */
    public String getHTTPURL();

    /**
     * Returns the repository ID.
     * 
     * @return the repository ID.
     */
    public String getID();

    /**
     * Returns the repository name.
     * 
     * @return the repository name.
     */
    public String getName();
}