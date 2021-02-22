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
package com.mooltiverse.oss.nyx.configuration;

import java.io.File;
import java.util.Objects;

import org.slf4j.event.Level;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The default configuration layer. This is a singleton class so instances are to be
 * retrieved via the static {@link #getInstance()} method.
 */
class DefaultLayer implements ConfigurationLayer, Defaults {
    private static DefaultLayer instance = null;
    /**
     * Default constructor is private on purpose.
     */
    private DefaultLayer() {
        super();
    }

    /**
     * Returns the singleton instance of this class.
     * 
     * @return the singleton instance of this class
     */
    static DefaultLayer getInstance() {
        if (Objects.isNull(instance))
            instance = new DefaultLayer();
        return instance;
    }

    /**
     * {@inheritDoc}
     */
    public String getBump()
        throws ConfigurationException {
        return BUMP;
    }

    /**
     * {@inheritDoc}
     */
    public File getDirectory()
        throws ConfigurationException {
        return DIRECTORY;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getDryRun()
        throws ConfigurationException {
        return DRY_RUN;
    }

    /**
     * {@inheritDoc}
     */
    public String getReleasePrefix()
        throws ConfigurationException {
        return RELEASE_PREFIX;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getReleasePrefixLenient()
        throws ConfigurationException {
        return RELEASE_PREFIX_LENIENT;
    }

    /**
     * {@inheritDoc}
     */
    public Scheme getScheme()
        throws ConfigurationException {
        return SCHEME;
    }

    /**
     * {@inheritDoc}
     */
    public Level getVerbosity()
        throws ConfigurationException {
        return VERBOSITY;
    }
}
