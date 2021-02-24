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
package com.mooltiverse.oss.nyx.log;

import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

/**
 * An utility class with the defined set of markers used. Markers are used by {@link Logger} methods to better
 * qualify log output, colorize it, filter it etc, based on the runtime configuration of the log framework in use.
 */
public class Markers {
    /**
     * The {@code CONFIGURATION} marker, used when logging configuration events.
     */
    public static Marker CONFIGURATION = MarkerFactory.getMarker("CONFIGURATION");

    /**
     * The {@code DEFAULT} marker, used when logging default configuration.
     */
    public static Marker DEFAULT = MarkerFactory.getMarker("DEFAULT");

    /**
     * The {@code MAIN} marker, used when logging events from the main classes.
     */
    public static Marker MAIN = MarkerFactory.getMarker("MAIN");

    /**
     * Default constructor is hidden on purpose
     */
    private Markers() {
        super();
    }
}
