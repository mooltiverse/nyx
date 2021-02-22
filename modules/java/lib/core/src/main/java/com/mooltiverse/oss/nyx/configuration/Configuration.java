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

import java.util.EnumMap;
import java.util.Objects;

import org.slf4j.event.Level;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The Nyx configuration.
 * 
 * Instances of this configuration used by Nyx can be retrieved by {@link com.mooltiverse.oss.nyx.Nyx#configuration()}.
 */
public class Configuration implements Root {
    /**
     * The internal representation of the configuration layers.
     * 
     * Map entries are ordered by the natural order of the {@link LayerPriority} keys.
     */
    private final EnumMap<LayerPriority, ConfigurationLayer> layers = new EnumMap<LayerPriority, ConfigurationLayer>(LayerPriority.class){
        // The default serial version UID
        private static final long serialVersionUID = 1L;
        {
            // Initialize the layers with the default values
            put(LayerPriority.DEFAULT, DefaultLayer.getInstance());
        }
    };

    /**
     * Default constructor hidden on purpose.
     */
    private Configuration() {
        super();
        // TODO: add the code to load standard local (see LayerPriority#CUSTOM_LOCAL_FILE) and shared (see LayerPriority#STANDARD_SHARED_FILE) configuration files, if found
    }

    /**
     * Returns a new configuration object at its initial state.
     * 
     * @return a new configuration object at its initial state
     * 
     * See {@link com.mooltiverse.oss.nyx.Nyx#configuration()}
     */
    public static Configuration initial() {
        return new Configuration();
    }

    /**
     * {@inheritDoc}
     */
    public String getBump()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            String bump = layer.getBump();
            if (!Objects.isNull(bump))
                return bump;
        }
        return DefaultLayer.getInstance().getBump();
    }

    /**
     * {@inheritDoc}
     */
    public File getDirectory()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            File directory = layer.getDirectory();
            if (!Objects.isNull(directory))
                return directory;
        }
        return DefaultLayer.getInstance().getDirectory();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getDryRun()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            Boolean dryRun = layer.getDryRun();
            if (!Objects.isNull(dryRun))
                return dryRun;
        }
        return DefaultLayer.getInstance().getDryRun();
    }

    /**
     * {@inheritDoc}
     */
    public String getReleasePrefix()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            String releasePrefix = layer.getReleasePrefix();
            if (!Objects.isNull(releasePrefix))
                return releasePrefix;
        }
        return DefaultLayer.getInstance().getReleasePrefix();
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getReleasePrefixLenient()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            Boolean releasePrefixLenient = layer.getReleasePrefixLenient();
            if (!Objects.isNull(releasePrefixLenient))
                return releasePrefixLenient;
        }
        return DefaultLayer.getInstance().getReleasePrefixLenient();
    }

    /**
     * {@inheritDoc}
     */
    public Scheme getScheme()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            Scheme scheme = layer.getScheme();
            if (!Objects.isNull(scheme))
                return scheme;
        }
        return DefaultLayer.getInstance().getScheme();
    }

    /**
     * {@inheritDoc}
     */
    public Level getVerbosity()
        throws ConfigurationException {
        // TODO: add TRACE log events here to tell how the resolution happens
        for (ConfigurationLayer layer: layers.values()) {
            Level verbosity = layer.getVerbosity();
            if (!Objects.isNull(verbosity))
                return verbosity;
        }
        return DefaultLayer.getInstance().getVerbosity();
    }
}
