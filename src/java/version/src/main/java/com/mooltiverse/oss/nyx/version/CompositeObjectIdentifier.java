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
 * A composite identifier that only accepts {@link SimpleIdentifier}s as children.
 */
class CompositeObjectIdentifier extends CompositeIdentifier {
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
    protected CompositeObjectIdentifier(char separator, SimpleIdentifier... children) {
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
    protected CompositeObjectIdentifier(SimpleIdentifier... children) {
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
    protected CompositeObjectIdentifier(char separator, List<SimpleIdentifier> children) {
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
    protected CompositeObjectIdentifier(List<SimpleIdentifier> children) {
        super(children);
    }

    /**
     * Returns an identifier representing the specified String value. The string is split using the
     * default separator.
     *
     * @param multipleIdentifiers when {@code true} the given string is parsed as it (may) contain multiple
     * identifiers, separated by the default separator, so this method may yield to multiple identifiers.
     * When {@code false} the given string is expected to have a single identifier so if the given
     * string has multiple identifiers an exception is thrown.
     * @param s the string to parse
     * @param integersPolicy the flag specifying how to parse integers
     *
     * @return the new identifier representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static CompositeObjectIdentifier valueOf(boolean multipleIdentifiers, String s, UseIntegerIdentifiers integersPolicy) {
        return valueOf(multipleIdentifiers, s, DEFAULT_SEPARATOR, integersPolicy);
    }

     /**
     * Returns an identifier representing the specified String value.
     *
     * @param multipleIdentifiers when {@code true} the given string is parsed as it (may) contain multiple
     * identifiers, separated by the given separator, so this method may yield to multiple identifiers.
     * When {@code false} the given string is expected to have a single identifier so if the given
     * string has multiple identifiers an exception is thrown.
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     * @param integersPolicy the flag specifying how to parse integers
     *
     * @return the new identifier representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static CompositeObjectIdentifier valueOf(boolean multipleIdentifiers, String s, char separator, UseIntegerIdentifiers integersPolicy) {
        if (multipleIdentifiers)
            return new CompositeObjectIdentifier(Parser.toIdentifiers(s, separator, integersPolicy));
        else return new CompositeObjectIdentifier(List.<SimpleIdentifier>of(Parser.toIdentifier(s, integersPolicy)));
    }

    /**
     * Returns the object at the given position (the first being at position 0)
     *
     * @param i the position to retrieve the value at (starting from 0)
     *
     * @return the object at the given position
     *
     * @throws IndexOutOfBoundsException if the given index is out of range
     */
    public Object get(int i) {
        return children.get(i).getValue();
    }
}
