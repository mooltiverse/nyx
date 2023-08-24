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

import java.util.List;
import java.util.Map;

/**
 * A value holder that models a section containing a map of substitutions.
 */
public class Substitutions extends EnabledItemsMap<Substitution> {
    /**
     * Default constructor. Constructs a new object with no items.
     */
    public Substitutions() {
        super();
    }

    /**
     * Standard constructor. Constructs a new object with the given items.
     * 
     * @param enabled the list of names of enabled items
     * @param items the map of items
     * 
     * @throws NullPointerException if some argument is {@code null}
     */
    public Substitutions(List<String> enabled, Map<String,Substitution> items) {
        super(enabled, items);
    }
}
