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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * The specialization of a Build version number as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 */
class SemanticBuildVersionHandler extends CompositeStringValueHandler {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Builds the value handler with the given children values.
     *
     * @param children the children of this composite handler. There must be at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given values are illegal or there isn't any non <code>null</code> item
     */
    private SemanticBuildVersionHandler(List<StringValueHandler> children) {
        super(DEFAULT_SEPARATOR, children);
    }

    /**
     * Returns an handler instance representing the specified String value.
     *
     * @param s the string to parse
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string contains illegal characters or there isn't any non
     * <code>null</code> item
     */
    static SemanticBuildVersionHandler valueOf(String s) {
        Objects.requireNonNull(s);
        List<StringValueHandler> handlers = new ArrayList<StringValueHandler>();
        if (s.isEmpty())
            throw new IllegalArgumentException("Can't build the list of identifiers from an empty string");
        for (String part: split(s, DEFAULT_SEPARATOR))
            handlers.add(new StringValueHandler(part));
        return new SemanticBuildVersionHandler(handlers);
    }

    /**
     * Returns an handler instance representing the specified String values.
     *
     * @param s the strings to parse
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string contains illegal characters or there isn't any non
     * <code>null</code> item
     */
    static SemanticBuildVersionHandler valueOf(String... s) {
        Objects.requireNonNull(s);
        if (s.length == 0)
            throw new IllegalArgumentException("Can't build the list of identifiers from an empty list");
        List<StringValueHandler> handlers = new ArrayList<StringValueHandler>();
        for (String item: s)
            if (!item.isEmpty())
                for (String part: split(item, DEFAULT_SEPARATOR))
                    handlers.add(new StringValueHandler(part));
        return new SemanticBuildVersionHandler(handlers);
    }
}
