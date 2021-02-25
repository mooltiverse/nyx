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

/**
 * This enumeration models configuration layer levels and their priorities. The order used to define
 * each item matters as it also determines the layer priority.
 * 
 * This means that the {@link Enum#ordinal()} can be queried to retrieve the layer priority for each value,
 * provided it returns {@code 0} for the highest priority item.
 */
enum LayerPriority {
    /**
     * The layer that reads values from command line options
     */
    COMMAND_LINE(),

    /**
     * The layer that reads values from environment variables
     */
    ENVIRONMENT(),

    /**
     * The layer that models a configuration from a script or plugin
     */
    PLUGIN(),

    /**
     * The layer that models a local configuration file from a custom location
     */
    CUSTOM_LOCAL_FILE(),

    /**
     * The layer that models a local configuration file from a standard location
     */
    STANDARD_LOCAL_FILE(),

    /**
     * The layer that models a shared configuration file from a custom location
     */
    CUSTOM_SHARED_FILE(),

    /**
     * The layer that models a shared configuration file from a standard location
     */
    STANDARD_SHARED_FILE(),

    /**
     * The layer that models the optional preset configuration.
     */
    PRESET(),

    /**
     * The layer that models default options.
     * 
     * @see Defaults
     * @see DefaultLayer
     */
    DEFAULT();
}
