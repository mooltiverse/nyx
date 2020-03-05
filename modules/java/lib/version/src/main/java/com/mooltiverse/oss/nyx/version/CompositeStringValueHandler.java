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
 * A composite value handler that only accepts {@link StringValueHandler}s as children.
 */
class CompositeStringValueHandler extends CompositeValueHandler<StringValueHandler> {
    /**
     * Builds the value handler with the given separator and children values.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite handler
     *
     * @throws NullPointerException if the given list of children is <code>null</code> doesn't contain any non
     * <code>null</code> item
     * @throws IllegalArgumentException if the given values are illegal
     */
    protected CompositeStringValueHandler(char separator, StringValueHandler... children) {
        super(separator, children);
    }

    /**
     * Builds the value handler with the given children values.
     *
     * @param children the children of this composite handler
     *
     * @throws NullPointerException if the given list of children is <code>null</code> doesn't contain any non
     * <code>null</code> item
     * @throws IllegalArgumentException if the given values are illegal
     */
    protected CompositeStringValueHandler(StringValueHandler... children) {
        super(children);
    }

    /**
     * Builds the value handler with the given separator and children values.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite handler
     *
     * @throws NullPointerException if the given list of children is <code>null</code> doesn't contain any non
     * <code>null</code> item
     * @throws IllegalArgumentException if the given values are illegal
     */
    protected CompositeStringValueHandler(char separator, List<StringValueHandler> children) {
        super(separator, children);
    }

    /**
     * Builds the value handler with the given children values.
     *
     * @param children the children of this composite handler
     *
     * @throws NullPointerException if the given list of children is <code>null</code> doesn't contain any non
     * <code>null</code> item
     * @throws IllegalArgumentException if the given values are illegal
     */
    protected CompositeStringValueHandler(List<StringValueHandler> children) {
        super(children);
    }

    /**
     * Returns the string at the given position (the first being at position 0)
     *
     * @param i the position to retrieve the value at (starting from 0)
     *
     * @return the string at the given position
     *
     * @throws IndexOutOfBoundsException if the given index is out of range
     */
    public String get(int i) {
        return children.get(i).getValue();
    }

    /**
     * Returns an handler instance representing the specified String value. The string is split using the
     * default separator.
     *
     * @param s the string to parse
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static CompositeStringValueHandler valueOf(String s) {
        return valueOf(s, DEFAULT_SEPARATOR);
    }

    /**
     * Returns an handler instance representing the specified String value.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static CompositeStringValueHandler valueOf(String s, char separator) {
        List<StringValueHandler> handlers = new ArrayList<StringValueHandler>();
        for (String part: split(s, separator))
            handlers.add(new StringValueHandler(part));
        return new CompositeStringValueHandler(separator, handlers);
    }
}
