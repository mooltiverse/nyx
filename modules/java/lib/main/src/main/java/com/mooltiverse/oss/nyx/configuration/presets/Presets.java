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
package com.mooltiverse.oss.nyx.configuration.presets;

import com.mooltiverse.oss.nyx.configuration.ConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;

/**
 * The presets factory class.
 */
public class Presets {
    /**
     * Default constructor is hidden on purpose.
     */
    public Presets() {
        super();
    }

    /**
     * Returns a configuration layer instance by its name.
     * 
     * @param name the name of the configuration layer to be returned.
     * 
     * @return the requested configuration layer.
     * 
     * @throws IllegalPropertyException in case the given name can't be resolved to a preset.
     */
    public static ConfigurationLayer byName(String name)
        throws IllegalPropertyException {
        switch (name) {
            case Extended.NAME: return new Extended();
            case Simple.NAME:   return new Simple();
            default: throw new IllegalPropertyException(String.format("Preset '%s' does not exists", name));
        }
    }
}
