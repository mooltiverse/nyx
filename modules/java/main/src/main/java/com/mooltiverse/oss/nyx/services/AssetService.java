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

import java.net.URI;

import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.git.Repository;
import com.mooltiverse.oss.nyx.state.State;

/**
 * A service that {@link Service#supports(Feature) supports} the {@link Service.Feature#ASSET} feature
 * to build project assets.
 */
public interface AssetService extends Service {
    /**
     * Builds the asset.
     * 
     * @param destination the asset destination. It can't be {@code null}.
     * @param state the state reference. It can't be {@code null}.
     * @param repository the repository reference. It can't be {@code null}.
     * 
     * @return the path to the asset that has been created.
     * 
     * @throws NullPointerException if a given argument is {@code null}
     * @throws SecurityException if authentication or authorization fails or there is no currently authenticated user
     * @throws TransportException if communication to the remote endpoint fails
     * @throws DataAccessException if files can't be read or written
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#ASSET} feature.
     */
    public URI buildAsset(String destination, State state, Repository repository)
        throws SecurityException, TransportException, DataAccessException;
}