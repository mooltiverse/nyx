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
 * A service that {@link Service#supports(Feature) supports} the {@link Service.Feature#USERS} feature
 * to manage users.
 */
public interface UserService extends Service {
    /**
     * Retrieves informations about the currently authenticated user. The authenticated user is the one owning the configured credentials.
     * 
     * @return the authenticated user
     * 
     * @throws SecurityException if authentication or authorization fails or there is no currently authenticated user
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#USERS} feature.
     */
    public User getAuthenticatedUser()
        throws SecurityException, TransportException;
}