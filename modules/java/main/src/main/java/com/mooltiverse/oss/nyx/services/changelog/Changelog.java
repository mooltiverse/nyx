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
package com.mooltiverse.oss.nyx.services.changelog;

import java.net.URI;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.AssetService;
import com.mooltiverse.oss.nyx.services.Service;
import com.mooltiverse.oss.nyx.services.git.Repository;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The entry point to the service producing changelogs.
 */
public class Changelog implements AssetService {
    /**
     * The {@code SERVICE} marker, used when logging command events.
     */
    static final Marker SERVICE = MarkerFactory.getMarker("SERVICE");

    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(Changelog.class);

    /**
     * Builds an instance.
     */
    private Changelog() {
        super();
    }

    /**
     * Returns an instance using the given options.
     * 
     * @param options the map of options for the requested service. It can't be {@code null}.
     * Valid options are documented as constants on this class.
     * 
     * @return an instance using the given options.
     * 
     * @throws NullPointerException if the given options map is {@code null}
     * @throws IllegalArgumentException if some entries in the given options map are missing or illegal for some reason
     */
    public static Changelog instance(Map<String,String> options) {
        Objects.requireNonNull(options, "Can't create a new instance with a null options map");
        return new Changelog();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URI buildAsset(String destination, State state, Repository repository)
        throws SecurityException, TransportException, DataAccessException {
        Objects.requireNonNull(state, "Can't create a new instance with a null state reference");
        Objects.requireNonNull(repository, "Can't create a new instance with a null repository reference");

        // TODO: implement this method
        logger.info(SERVICE, "Changelog.buildAsset()");
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean supports(Service.Feature feature) {
        Objects.requireNonNull(feature, "Can't check if the feature is supported for a null feature");
        switch (feature)
        {
            case ASSET:         return true;
            default:            return false;
        }
    }
}
