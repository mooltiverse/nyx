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
 * A composite value handler that only accepts {@link IntegerValueHandler}s as children.
 */
class CompositeIntegerValueHandler extends CompositeValueHandler {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
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
    protected CompositeIntegerValueHandler(char separator, IntegerValueHandler... children) {
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
    protected CompositeIntegerValueHandler(IntegerValueHandler... children) {
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
    protected CompositeIntegerValueHandler(char separator, List<IntegerValueHandler> children) {
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
    protected CompositeIntegerValueHandler(List<IntegerValueHandler> children) {
        super(children);
    }

    /**
     * Returns the number at the given position (the first being at position 0)
     *
     * @param i the position to retrieve the value at (starting from 0)
     *
     * @return the number at the given position
     *
     * @throws IndexOutOfBoundsException if the given index is out of range
     */
    public int get(int i) {
        return Integer.class.cast(children.get(i).getValue()).intValue();
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
     * @throws IllegalArgumentException if the given string doesn't hold positive integers
     */
    static CompositeIntegerValueHandler valueOf(String s) {
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
     * @throws IllegalArgumentException if the given string doesn't hold positive integers
     */
    static CompositeIntegerValueHandler valueOf(String s, char separator) {
        List<IntegerValueHandler> handlers = new ArrayList<IntegerValueHandler>();
        for (String part: split(s, separator))
            handlers.add(new IntegerValueHandler(part));
        return new CompositeIntegerValueHandler(handlers);
    }
}
