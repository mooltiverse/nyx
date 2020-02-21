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
public enum CoreIdentifiers {
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

    /**
     * Returns the value with the given name.
     *
     * @param name the name of the identifier to search
     *
     * @return the value with the given name.
     *
     * @throws NullPointerException if <code>null</code> is passed
     * @throws IllegalArgumentException if there is no identifier with the given name
     */
    static CoreIdentifiers byName(String name) {
        for (CoreIdentifiers ci: CoreIdentifiers.values())
            if (ci.getName().equals(name))
                return ci;
        // if none is found fall back to valueOf()
        return CoreIdentifiers.valueOf(name);
    }
}
