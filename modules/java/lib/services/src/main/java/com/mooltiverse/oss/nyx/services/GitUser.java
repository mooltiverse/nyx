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

import java.util.Map;

/**
 * A user for a remote Git service.
 */
public interface GitUser {
    /**
     * Returns a read-only map with all the raw properties for the user.
     * 
     * @return a read-only map with all the raw properties for the user.
     */
    public Map<String, Object> getAttributes();

    /**
     * Returns the ID of this user.
     * 
     * @return the ID of this user.
     */
    public String getID();

    /**
     * Returns the user name of this user.
     * 
     * @return the user name of this user.
     */
    public String getUserName();

    /**
     * Returns the full name of this user.
     * 
     * @return the full name of this user. If not vailable then {@code null} is returned.
     */
    public String geFullName();

    /**
     * Updates the user data and attributes. This method might be useful when the remote data has changed
     * and you need to update the local status.
     * 
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws GitAuthenticationException if authentication fails
     */
    public void update()
        throws GitTransportException, GitAuthenticationException;
}