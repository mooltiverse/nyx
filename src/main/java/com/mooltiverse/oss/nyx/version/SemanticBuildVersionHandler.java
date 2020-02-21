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

/**
 * The specialization of a Build version number as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 */
class SemanticBuildVersionHandler extends CompositeStringValueHandler {
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
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static SemanticBuildVersionHandler valueOf(String s) {
        List<StringValueHandler> handlers = new ArrayList<StringValueHandler>();
        for (String part: split(s, DEFAULT_SEPARATOR))
            handlers.add(new StringValueHandler(part));
        return new SemanticBuildVersionHandler(handlers);
    }
}
