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
package com.mooltiverse.oss.nyx.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A value holder that models a section containing a map of items that need to be enabled
 * by means of a list of names in order to be active.
 * 
 * @param <T> the type of the items managed by the map.
 */
class EnabledItemsMap<T> {
    /**
     * The private list of enabled items.
     */
    private List<String> enabled = null;

    /**
     * The private map of the items.
     */
    private Map<String,T> items = null;

    /**
     * Default constructor. Constructs a new object with no items.
     */
    public EnabledItemsMap() {
        super();
        this.enabled = new ArrayList<String>();
        this.items = new HashMap<String,T>();
    }

    /**
     * Standard constructor. Constructs a new object with the given items.
     * 
     * @param enabled the list of names of enabled items
     * @param items the map of items
     * 
     * @throws NullPointerException if some argument is {@code null}
     */
    public EnabledItemsMap(List<String> enabled, Map<String,T> items) {
        super();
        Objects.requireNonNull(enabled);
        Objects.requireNonNull(items);
        this.enabled = enabled;
        this.items = items;
    }

    /**
     * Returns the list of enabled items.
     * 
     * @return the list of enabled items. It may be empty but not {@code null} if not configured.
     */
    public List<String> getEnabled() {
        return enabled;
    }

    /**
     * Returns the map of the items configured in this section, where keys are item names
     * and values are actual item objects.
     * 
     * @return the map of the items configured in this section, where keys are item names
     * and values are actual item objects. It may be empty but not {@code null} if not configured.
     */
    public Map<String,T> getItems() {
        return items;
    }
}
