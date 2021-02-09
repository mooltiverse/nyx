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
package com.mooltiverse.oss.nyx.git.hosted.services;

/**
 * An (authenticated) session for a remote Git service.
 */
public interface GitSession {
    /**
     * Returns the service instance backing this session.
     * 
     * @return the service instance backing this session.
     */
    public GitService getService();

    /**
     * Returns the authenticated user associated with this session.
     * 
     * @return the authenticated user associated with this session.
     */
    public GitUser getAuthenticatedUser();
}