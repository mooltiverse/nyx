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

import com.mooltiverse.oss.nyx.services.Service.Feature;

/**
 * A user belonging to a service. These entities are managed through services implementing the
 * {@link UserService} interface and {@link Service#supports(Feature) supporting} the
 * {@link Service.Feature#USERS} feature.
 */
public interface User {
    /**
     * Returns the ID for the user on this service.
     * 
     * @return the ID for the user on this service.
     */
    public String getID();

    /**
     * Returns the name for the user on this service.
     * 
     * @return the name for the user on this service.
     */
    public String getUserName();

    /**
     * Returns the full name for the user on this service.
     * 
     * @return the full name for the user on this service.
     */
    public String geFullName();
}