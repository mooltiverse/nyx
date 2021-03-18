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
package com.mooltiverse.oss.nyx.configuration.mock;

import java.io.File;

import com.mooltiverse.oss.nyx.configuration.ConfigurationLayer;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.Version;

/**
 * A fake configuration layer, with null or empty values to be set for specific purposes.
 */
public class ConfigurationLayerMock implements ConfigurationLayer {
    /**
     * The value returned by this mock object.
     */
    public String bump = null;

    /**
     * The value returned by this mock object. This is an abstract path and does not exists on the file system.
     */
    public File directory = null;

    /**
     * The value returned by this mock object.
     */
    public Boolean dryRun = null;

    /**
     * The value returned by this mock object.
     */
    public Version initialVersion = null;

    /**
     * The value returned by this mock object.
     */
    public String releasePrefix = null;

    /**
     * The value returned by this mock object.
     */
    public Boolean releaseLenient = null;

    /**
     * The value returned by this mock object.
     */
    public Scheme scheme = null;

    /**
     * The value returned by this mock object.
     */
    public Verbosity verbosity = null;

    /**
     * The value returned by this mock object.
     */
    public Version version = null;

    /**
     * Default constructor
     */
    public ConfigurationLayerMock() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return bump;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory() {
        return directory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return dryRun;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getInitialVersion(){
        return initialVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return releasePrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient() {
        return releaseLenient;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme() {
        return scheme;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity(){
        return verbosity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getVersion(){
        return version;
    }
}
