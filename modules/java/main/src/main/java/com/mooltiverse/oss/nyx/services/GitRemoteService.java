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

import java.util.List;

/**
 * A service that {@link Service#supports(Feature) supports} the {@link Service.Feature#GIT_REMOTE} feature
 * to manage remote repositories.
 */
public interface GitRemoteService extends Service {
    /**
     * Returns the list of the remote repositories supported by this service. The names returned are simple names
     * like those returned by a {@code git remote} command.
     * <br>
     * The returned list is meant to be used as a filter so when it's {@code null} it means that any remote is
     * supported.
     * 
     * @return the list of the remote repositories supported by this service. When {@code null} or empty any remote
     * is supported by this service.
     * 
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#GIT_REMOTE} feature.
     */
    public List<String> getSupportedRemoteNames();

    /**
     * Returns the user name to be used when connecting to remote repositories.
     * 
     * @return the user name to be used when connecting to remote repositories.
     * When {@code null} the remote repositories are meant to allow anonymous access and,
     * in this case, also the {@link #getPassword() password} is {@code null}.
     * 
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#GIT_REMOTE} feature.
     */
    public String getUser();

    /**
     * Returns the password to be used when connecting to remote repositories.
     * 
     * @return the password to be used when connecting to remote repositories.
     * When {@link #getUser()} returns {@code null} this method also returns
     * {@code null} amd the remote repositories are meant to allow anonymous access.
     * 
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#GIT_REMOTE} feature.
     */
    public String getPassword();
}