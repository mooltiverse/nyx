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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The class modelling a simple nested configuration block whose content is a map of nested items.
 * 
 * @param <T> the type of the items managed by the map.
 */
public abstract class SimpleMapConfigurationBlock<T> implements MapConfigurationBlock<T> {
    /**
     * The private list of enabled items.
     */
    private List<String> enabled = new ArrayList<String>();

    /**
     * The private map of the items.
     */
    private Map<String,T> items = new HashMap<String,T>();

    /**
     * Default constructor is protected on purpose.
     */
    protected SimpleMapConfigurationBlock() {
        super();
    }

    /**
     * Constructor is protected on purpose.
     * 
     * @param enabled the list of enabled items. It may be {@code null} if not configured.
     * @param items the map of the items configured in this block, where keys are item names
     * and values are actual item objects. It may be empty but not {@code null} if not configured.
     */
    protected SimpleMapConfigurationBlock(List<String> enabled, Map<String,T> items) {
        super();
        this.enabled = enabled;
        this.items = items;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getEnabled() {
        return enabled;
    }

    /**
     * Sets the list of enabled items.
     * 
     * @param enabled the list of enabled items. It may be {@code null} if not configured.
     */
    public void setEnabled(List<String> enabled) {
        this.enabled = enabled;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,T> getItems()
        throws DataAccessException, IllegalPropertyException {
        return items;
    }

    /**
     * Sets the map of the items configured in this block, where keys are item names
     * and values are actual item objects.
     * 
     * @param items the map of the items configured in this block, where keys are item names
     * and values are actual item objects. It may be empty but not {@code null} if not configured.
     */
    public void setItems(Map<String,T> items) {
        this.items = items;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getItem(String name)
        throws DataAccessException, IllegalPropertyException {
        return items.get(name);
    }

    /**
     * Sets the item with the given name configured in this block, if any.
     * 
     * @param name the name of the item to set. Cannot be {@code null}.
     * @param item the item to set. Cannot be {@code null}.
     */
    public void setItem(String name, T item) {
        this.items.put(name, item);
    }
}