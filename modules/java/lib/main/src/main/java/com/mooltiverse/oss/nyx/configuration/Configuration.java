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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.CommitMessageConventions;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.Version;

/**
 * The Nyx configuration. The configuration is a live object that resolves each option lazily, only when required.
 * This not only improves the overall performances but is also safer as in case of malformed configuration options, only
 * those actually needed are resolved.
 * 
 * This means that even if the configuration sources don't change throughout a release process, the state of the
 * configuration may change every time a not yet resolved oprion is requested and evaluated.
 * 
 * The configuration is layered, where each layer represents a source of configuration options. There is a clear definition
 * of priorities among different layers so there is a clear precedence of options coming from one layer or another.
 * Thanks to this, each option can be overridden by other layer with higher priority.
 * 
 * There must be only one instance of this class for every execution and it's retrieved by {@link Nyx#configuration()}.
 */
public class Configuration implements Root {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Configuration.class);

    /**
     * The private singleton instance of the commit message convention configuration block.
     */
    private CommitMessageConventionsBlock commitMessageConventionsBlock = null;

    /**
     * The internal representation of the configuration layers and their priorities.
     * 
     * Map entries are ordered by the natural order of the {@link LayerPriority} keys so by iterating over the
     * entries in this map we are assured that the evaluation order is safe.
     * 
     * This instance is initialized with the {@link DefaultLayer} (which has the least priority) as a last resort
     * when looking up configuration options.
     */
    private final EnumMap<LayerPriority, ConfigurationLayer> layers = new EnumMap<LayerPriority, ConfigurationLayer>(LayerPriority.class){
        // The default serial version UID
        private static final long serialVersionUID = 1L;
        {
            // Initialize the layers with the default values, which have the least priority
            put(LayerPriority.DEFAULT, DefaultLayer.getInstance());
        }
    };

    /**
     * Default constructor. Returns a new configuration object at its initial state.
     * 
     * Instances if this class are created using the {@link Nyx#configuration()} method so this constructor should never be used alone.
     * 
     * See {@link Nyx#configuration()}
     * 
     * @throws DataAccessException in case data cannot be read or accessed.
     * @throws IllegalPropertyException in case some option has been defined but has incorrect values or it can't be resolved.
     */
    public Configuration()
        throws DataAccessException, IllegalPropertyException {
        super();
        logger.debug(CONFIGURATION, "New configuration object");
        // TODO: add the code to load standard local (see LayerPriority#CUSTOM_LOCAL_FILE) and shared (see LayerPriority#STANDARD_SHARED_FILE) configuration files, if found
    }

    /**
     * Resets the cache of resolved options.
     */
    private synchronized void resetCache() {
        if (!Objects.isNull(commitMessageConventionsBlock))
        commitMessageConventionsBlock.resolvedItems = null;
    }

    /**
     * Adds, replaces or removes the layer at the given priority level.
     * 
     * @param layer the configuration layer to set at the given priority level.
     * If {@code null} any existing configuration layer at the same level is removed (if any).
     * @param priority the priority level
     * 
     * @return a reference to this same object.
     * 
     * @throws DataAccessException in case data cannot be read or accessed.
     * @throws IllegalPropertyException in case some option has been defined but has incorrect values or it can't be resolved.
     */
    private synchronized void setConfigurationLayer(ConfigurationLayer layer, LayerPriority priority) {
        if (Objects.isNull(layer)) {
            logger.debug(CONFIGURATION, "Removing the existing {} configuration layer, if any", priority);
            if (layers.containsKey(priority))
                layers.remove(priority);
        }
        else {
            logger.debug(CONFIGURATION, "Adding or replacing the {} configuration layer", priority);
            layers.put(priority, layer);
        }
        resetCache();
    }

    /**
     * Adds, replaces or removes the layer at the {@link LayerPriority#COMMAND_LINE} level.
     * 
     * @param layer the configuration layer to set at the {@link LayerPriority#COMMAND_LINE} level.
     * If {@code null} any existing configuration layer at the same level is removed (if any).
     * 
     * @return a reference to this same object.
     * 
     * @throws DataAccessException in case data cannot be read or accessed.
     * @throws IllegalPropertyException in case some option has been defined but has incorrect values or it can't be resolved.
     */
    public Configuration withCommandLineConfiguration(ConfigurationLayer layer)
        throws DataAccessException, IllegalPropertyException {
        
        setConfigurationLayer(layer, LayerPriority.COMMAND_LINE);
        return this;
    }

    /**
     * Adds, replaces or removes the layer at the {@link LayerPriority#PLUGIN} level.
     * 
     * @param layer the configuration layer to set at the {@link LayerPriority#PLUGIN} level.
     * If {@code null} any existing configuration layer at the same level is removed (if any).
     * 
     * @return a reference to this same object.
     * 
     * @throws DataAccessException in case data cannot be read or accessed.
     * @throws IllegalPropertyException in case some option has been defined but has incorrect values or it can't be resolved.
     */
    public Configuration withPluginConfiguration(ConfigurationLayer layer)
        throws DataAccessException, IllegalPropertyException {
        
        setConfigurationLayer(layer, LayerPriority.PLUGIN);
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump()
        throws DataAccessException, IllegalPropertyException {
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
    public CommitMessageConventions getCommitMessageConventions()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(commitMessageConventionsBlock)) {
            commitMessageConventionsBlock = new CommitMessageConventionsBlock();
        }
        return commitMessageConventionsBlock;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory()
        throws DataAccessException, IllegalPropertyException {
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
     * This method allows to override the default directory that will be returned by {@link #getDirectory()}.
     * 
     * @param directory the new default directory. If {@code null} then the standard default directory will be used.
     */
    public static void setDefaultDirectory(File directory) {
        DefaultLayer.getInstance().setDirectory(directory);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun()
        throws DataAccessException, IllegalPropertyException {
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
    public Version getInitialVersion()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "initialVersion");
        for (ConfigurationLayer layer: layers.values()) {
            Version initialVersion = layer.getInitialVersion();
            if (!Objects.isNull(initialVersion)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "initialVersion", initialVersion);
                return initialVersion;
            }
        }
        return DefaultLayer.getInstance().getInitialVersion();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix()
        throws DataAccessException, IllegalPropertyException {
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
    public Boolean getReleaseLenient()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "releaseLenient");
        for (ConfigurationLayer layer: layers.values()) {
            Boolean releaseLenient = layer.getReleaseLenient();
            if (!Objects.isNull(releaseLenient)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "releaseLenient", releaseLenient);
                return releaseLenient;
            }
        }
        return DefaultLayer.getInstance().getReleaseLenient();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException {
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
        throws DataAccessException, IllegalPropertyException {
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

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getVersion()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "version");
        for (ConfigurationLayer layer: layers.values()) {
            Version version = layer.getVersion();
            if (!Objects.isNull(version)) {
                logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "version", version);
                return version;
            }
        }
        return DefaultLayer.getInstance().getVersion();
    }

    /**
     * The class implementing the {@link CommitMessageConventions} confliguration block.
     */
    private class CommitMessageConventionsBlock implements CommitMessageConventions {
        /**
         * The private map of the items resolved among the layers.
         */
        private Map<String,CommitMessageConvention> resolvedItems = null;

        /**
         * Default constructor is private on purpose.
         */
        private CommitMessageConventionsBlock() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<String> getEnabled()
            throws DataAccessException, IllegalPropertyException {
            logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "commitMessageConventions.enabled");
            for (ConfigurationLayer layer: layers.values()) {
                List<String> enabled = layer.getCommitMessageConventions().getEnabled();
                if (!Objects.isNull(enabled)) {
                    logger.trace(CONFIGURATION, "The {} configuration option value is: {}", "commitMessageConventions.enabled", String.join(", ", enabled));
                    return enabled;
                }
            }
            return DefaultLayer.getInstance().getCommitMessageConventions().getEnabled();
        }

        /**
         * Returns the map of resolved items, resolving them over all the layers. Each field of each item needs to be 
         * resolved independently as it may be overridden in some layer.
         * 
         * @return the map of resolved items, resolving them over all the layers. {@code null} of no items are enabled.
         * 
         * @throws DataAccessException in case the option cannot be read or accessed.
         * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
         */
        private Map<String,CommitMessageConvention> getResolvedItems()
            throws DataAccessException, IllegalPropertyException {
            if (Objects.isNull(resolvedItems)) {
                logger.trace(CONFIGURATION, "Resolving the {} configuration option", "commitMessageConventions.items");
                List<String> enabled = getEnabled();
                if (Objects.isNull(enabled)) {
                    logger.trace(CONFIGURATION, "No enabled {} to resolve", "commitMessageConventions.items");
                    return null;
                }
                else {
                    logger.trace(CONFIGURATION, "Resolving {} {}: ", enabled.size(), "commitMessageConventions.items", String.join(", ", enabled));
                    resolvedItems = new HashMap<String,CommitMessageConvention>(enabled.size());
                    for (String enabledItem: enabled) {
                        resolvedItems.put(enabledItem, getResolvedItem(enabledItem));
                    }
                }
            }
            return resolvedItems;
        }

        /**
         * Returns the resolved item with the given name, resolving it over all the layers. Each field needs to be 
         * resolved independently as it may be overridden in some layer.
         * 
         * @param name the name of the item to retrieve. Cannot be {@code null}.
         * 
         * @return the resolved item, if any. {@code null} of no item with such name is available.
         * 
         * @throws DataAccessException in case the option cannot be read or accessed.
         * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
         */
        private CommitMessageConvention getResolvedItem(String name)
            throws DataAccessException, IllegalPropertyException {
            logger.trace(CONFIGURATION, "Resolving the {}[{}] configuration option", "commitMessageConventions.items", name);
            for (ConfigurationLayer layer: layers.values()) {
                CommitMessageConvention item = layer.getCommitMessageConventions().getItem(name);
                if (!Objects.isNull(item)) {
                    logger.trace(CONFIGURATION, "The {}[{}] configuration option has been resolved", "commitMessageConventions.items", name);
                    return item;
                }
            }
            logger.error(CONFIGURATION, "Unable to resolve the {}[{}] configuration option", "commitMessageConventions.items", name);
            throw new IllegalPropertyException(String.format("Unable to resolve the {}[{}] configuration option", "commitMessageConventions.items", name));
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String,CommitMessageConvention> getItems()
            throws DataAccessException, IllegalPropertyException {
            logger.trace(CONFIGURATION, "Retrieving the {} configuration option", "commitMessageConventions.items");
            return getResolvedItems();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public CommitMessageConvention getItem(String name)
            throws DataAccessException, IllegalPropertyException {
            return getResolvedItem(name);
        }
    }
}
