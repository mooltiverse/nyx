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

import java.util.List;

/**
 * A composite identifier that only accepts {@link StringIdentifier}s as children.
 */
class CompositeStringIdentifier extends CompositeIdentifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * Builds the identifier with the given nested identifiers and separator.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeStringIdentifier(char separator, StringIdentifier... children) {
        super(separator, children);
    }

    /**
     * Builds the identifier with the given nested identifiers and the default separator.
     *
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeStringIdentifier(StringIdentifier... children) {
        super(children);
    }

    /**
     * Builds the identifier with the given nested identifiers and separator.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeStringIdentifier(char separator, List<StringIdentifier> children) {
        super(separator, children);
    }

    /**
     * Builds the identifier with the given nested identifiers and the default separator.
     *
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeStringIdentifier(List<StringIdentifier> children) {
        super(children);
    }

    /**
     * Returns an identifier instance representing the specified String value. The string is split using the
     * default separator.
     *
     * @param s the string to parse
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static CompositeStringIdentifier valueOf(String s) {
        return valueOf(s, DEFAULT_SEPARATOR);
    }

    /**
     * Returns an identifier instance representing the specified String value.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static CompositeStringIdentifier valueOf(String s, char separator) {
        return new CompositeStringIdentifier(separator, Parser.toStringIdentifiers(s, separator));
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
        return String.class.cast(children.get(i).getValue());
    }
}
