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

import static com.mooltiverse.oss.nyx.log.Markers.CONFIGURATION;

import java.io.File;

import java.util.EnumMap;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The Nyx configuration.
 * 
 * Instances of this configuration used by Nyx can be retrieved by {@link com.mooltiverse.oss.nyx.Nyx#configuration()}.
 */
public class Configuration implements Root {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Configuration.class);

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
     * Default constructor. Returns a new configuration object at its initial state.
     * Instances if this class are created using the {@link com.mooltiverse.oss.nyx.Nyx#configuration()} method.
     * 
     * See {@link com.mooltiverse.oss.nyx.Nyx#configuration()}
     * 
     * @throws ConfigurationException in case default configuration objects prevent this configuration to be created correctly
     */
    public Configuration()
        throws ConfigurationException {
        super();
        logger.debug(CONFIGURATION, "New configuration object");
        // TODO: add the code to load standard local (see LayerPriority#CUSTOM_LOCAL_FILE) and shared (see LayerPriority#STANDARD_SHARED_FILE) configuration files, if found
    }

    /**
     * Adds, replaces or removes the layer at the {@link LayerPriority#PLUGIN} level.
     * 
     * @param layer the configuration layer to set at the {@link LayerPriority#PLUGIN} level.
     * If {@code null} any existing configuration layer at the same level is removed (if any).
     * 
     * @return a reference to this object.
     * 
     * @throws ConfigurationException in case the given configuration has issues preventing
     * it from being used.
     */
    public synchronized Configuration withPluginConfiguration(ConfigurationLayer layer)
        throws ConfigurationException {
        if (Objects.isNull(layer)) {
            logger.debug(CONFIGURATION, "Removing the existing {} configuration layer, if any", LayerPriority.PLUGIN);
            if (layers.containsKey(LayerPriority.PLUGIN))
                layers.remove(LayerPriority.PLUGIN);
        }
        else {
            logger.debug(CONFIGURATION, "Adding or replacing the {} configuration layer", LayerPriority.PLUGIN);
            layers.put(LayerPriority.PLUGIN, layer);
        }

        // TODO: invoke a method (yet to implement) to set overall values like logging verbosity, current directory and dry run

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "bump");
        for (ConfigurationLayer layer: layers.values()) {
            String bump = layer.getBump();
            if (!Objects.isNull(bump)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "bump", bump);
                return bump;
            }
        }
        return DefaultLayer.getInstance().getBump();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "directory");
        for (ConfigurationLayer layer: layers.values()) {
            File directory = layer.getDirectory();
            if (!Objects.isNull(directory)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "directory", directory.getAbsolutePath());
                return directory;
            }
        }
        return DefaultLayer.getInstance().getDirectory();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "dryRun");
        for (ConfigurationLayer layer: layers.values()) {
            Boolean dryRun = layer.getDryRun();
            if (!Objects.isNull(dryRun)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "dryRun", dryRun);
                return dryRun;
            }
        }
        return DefaultLayer.getInstance().getDryRun();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "releasePrefix");
        for (ConfigurationLayer layer: layers.values()) {
            String releasePrefix = layer.getReleasePrefix();
            if (!Objects.isNull(releasePrefix)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "releasePrefix", releasePrefix);
                return releasePrefix;
            }
        }
        return DefaultLayer.getInstance().getReleasePrefix();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleasePrefixLenient()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "releasePrefixLenient");
        for (ConfigurationLayer layer: layers.values()) {
            Boolean releasePrefixLenient = layer.getReleasePrefixLenient();
            if (!Objects.isNull(releasePrefixLenient)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "releasePrefixLenient", releasePrefixLenient);
                return releasePrefixLenient;
            }
        }
        return DefaultLayer.getInstance().getReleasePrefixLenient();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "scheme");
        for (ConfigurationLayer layer: layers.values()) {
            Scheme scheme = layer.getScheme();
            if (!Objects.isNull(scheme)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "scheme", scheme);
                return scheme;
            }
        }
        return DefaultLayer.getInstance().getScheme();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws ConfigurationException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "verbosity");
        for (ConfigurationLayer layer: layers.values()) {
            Verbosity verbosity = layer.getVerbosity();
            if (!Objects.isNull(verbosity)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "verbosity", verbosity);
                return verbosity;
            }
        }
        return DefaultLayer.getInstance().getVerbosity();
    }
}
