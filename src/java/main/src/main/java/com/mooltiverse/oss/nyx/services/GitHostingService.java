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

import com.mooltiverse.oss.nyx.io.TransportException;

/**
 * A service that {@link Service#supports(Feature) supports} the {@link Service.Feature#GIT_HOSTING} feature
 * to manage hosted repositories.
 */
public interface GitHostingService extends Service {
    /**
     * Creates a new Git repository for the currently authenticated user.
     * <br>
     * Please note that if the service has been configured with repository owner and name those attributes are ignored
     * by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
     * name is always the {@code name} attribute.
     * 
     * @param name the repository name. Cannot be {@code null}
     * @param description the repository description. It may be {@code null}
     * @param restricted when {@code true} the repository will have private visibility, otherwise it will be public
     * @param initialize when {@code true} the repository is also initialized (usually with a default README file)
     * 
     * @return the object representing the newly created repository
     * 
     * @throws SecurityException if authentication or authorization fails or there is no currently authenticated user
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#GIT_HOSTING} feature.
     */
    public GitHostedRepository createGitRepository(String name, String description, boolean restricted, boolean initialize)
        throws SecurityException, TransportException;

    /**
     * Deletes a Git repository for the currently authenticated user.
     * <br>
     * Please note that if the service has been configured with repository owner and name those attributes are ignored
     * by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
     * name is always the {@code name} attribute.
     * 
     * @param name the repository name. Cannot be {@code null}
     * 
     * @throws SecurityException if authentication or authorization fails or there is no currently authenticated user
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#GIT_HOSTING} feature.
     */
    public void deleteGitRepository(String name)
        throws SecurityException, TransportException;
}