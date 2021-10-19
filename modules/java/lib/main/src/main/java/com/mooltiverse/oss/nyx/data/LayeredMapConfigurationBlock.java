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
package com.mooltiverse.oss.nyx.data;

import static com.mooltiverse.oss.nyx.log.Markers.CONFIGURATION;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The class modelling a layered nested configuration block whose content is a map of nested items.
 * 
 * @param <T> the type of the items managed by the map.
 */
public abstract class LayeredMapConfigurationBlock<T> implements MapConfigurationBlock<T> {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(LayeredMapConfigurationBlock.class);

    /**
     * The private map of the items resolved among the layers.
     */
    private final Map<String,T> resolvedItems = new HashMap<String,T>();

    /**
     * The flag telling if the items have been already initialized.
     */
    private boolean itemsInitialized = false;

    /**
     * The name of the configuration block.
     */
    protected final String configurationBlockName;

    /**
     * Standard constructor.
     * 
     * @param configurationBlockName the name of the configuration block that is modelled by this instance.
     */
    protected LayeredMapConfigurationBlock(String configurationBlockName) {
        super();
        this.configurationBlockName = configurationBlockName;
    }

    /**
     * Resets the resolved items and clears the cache.
     */
    public void resetCache() {
        resolvedItems.clear();
        itemsInitialized = false;
    }

    /**
     * Returns the map of resolved items, resolving them over all the layers. Each field of each item needs to be 
     * resolved independently as it may be overridden in some layer.
     * 
     * @return the map of resolved items, resolving them over all the layers. Empty if no items are enabled.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    private Map<String,T> getResolvedItems()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Resolving the {}.{} configuration option", configurationBlockName, "items");
        if (!itemsInitialized) {
            List<String> enabled = getEnabled();
            if (Objects.isNull(enabled)) {
                logger.trace(CONFIGURATION, "No enabled {}.{} to resolve", configurationBlockName, "items");
            }
            else {
                logger.trace(CONFIGURATION, "Resolving {}.{} {}: ", enabled.size(), configurationBlockName, "items", String.join(", ", enabled));
                resolvedItems.clear();
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
    protected abstract T getResolvedItem(String name)
        throws DataAccessException, IllegalPropertyException;

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,T> getItems()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Retrieving the {}.{} configuration option", configurationBlockName, "items");
        return getResolvedItems();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getItem(String name)
        throws DataAccessException, IllegalPropertyException {
        logger.trace(CONFIGURATION, "Retrieving the {}.{}[{}] configuration option", configurationBlockName, "items", name);
        return getResolvedItem(name);
    }
}