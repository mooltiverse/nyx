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
 * This enumeration models configuration layer levels and their priorities.
 */
enum LayerPriority {
    /**
     * The layer that reads values from command line options
     */
    COMMAND_LINE(8),

    /**
     * The layer that reads values from environment variables
     */
    ENVIRONMENT(7),

    /**
     * The layer that models a configuration from a script or plugin
     */
    PLUGIN(6),

    /**
     * The layer that models a local configuration file from a custom location
     */
    CUSTOM_LOCAL_FILE(5),

    /**
     * The layer that models a local configuration file from a standard location
     */
    STANDARD_LOCAL_FILE(4),

    /**
     * The layer that models a shared configuration file from a custom location
     */
    CUSTOM_SHARED_FILE(3),

    /**
     * The layer that models a shared configuration file from a standard location
     */
    STANDARD_SHARED_FILE(2),

    /**
     * The layer that models the optional preset configuration.
     */
    PRESET(1),

    /**
     * The layer that models default options.
     * 
     * @see Defaults
     * @see DefaultLayer
     */
    DEFAULT(0);

    /**
     * The level priority. Lower numbers have lower priority.
     */
    private final int priority;

    /**
     * Constructor
     * 
     * @param priority the level priority
     */
    private LayerPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Returns the level priority. Lower numbers mean lower priority.
     * 
     * @return the level priority
     */
    public int getPriority() {
        return priority;
    }
}
