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

import java.util.List;
import java.util.Map;

/**
 * The class modelling the {@code identifiers} configuration block.
 */
public class Identifiers extends SimpleMapConfigurationBlock<Identifier> {
    /**
     * Default constructor.
     */
    public Identifiers() {
        super();
    }

    /**
     * Constructor is protected on purpose.
     * 
     * @param enabled the list of enabled items. It may be {@code null} if not configured.
     * @param items the map of the items configured in this block, where keys are item names
     * and values are actual item objects. It may be empty but not {@code null} if not configured.
     */
    public Identifiers(List<String> enabled, Map<String,Identifier> items) {
        super(enabled, items);
    }
}