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
package com.mooltiverse.oss.nyx.version;

/**
 * The identifiers used for core version numbers.
 */
enum CoreIdentifiers {
    /**
     * The major number.
     */
    MAJOR("major", 0),

    /**
     * The minor number.
     */
    MINOR("minor", 1),

    /**
     * The patch number.
     */
    PATCH("patch", 2);

    /**
     * The name of the identifier.
     */
    private final String name;

    /**
     * The relative position of this identifier (starting from <code>0</code>).
     */
    private final int position;

    /**
     * Constructor.
     *
     * @param name the identifier name.
     * @param position the relative position of this identifier (starting from <code>0</code>).
     */
    private CoreIdentifiers(String name, int position) {
        this.name = name;
        this.position = position;
    }

    /**
     * Returns the name of the identifier.
     *
     * @return the name of the identifier.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the relative position of this identifier (starting from <code>0</code>).
     *
     * @return the relative position of this identifier.
     */
    int getPosition() {
        return position;
    }
}
