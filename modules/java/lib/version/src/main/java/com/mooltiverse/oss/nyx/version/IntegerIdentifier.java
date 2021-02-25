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
 * A simple identifier holding an {@link Integer} value.
 * 
 * In <a href="https://semver.org/">Semantic Versioning 2.0.0</a> parlance, an integer identifier is
 * restricted to be a <b>positive</b> integer, with no leading zeroes, otherwise it's handled as a
 * {@link StringIdentifier}.
 */
class IntegerIdentifier extends SimpleIdentifier implements Comparable<Integer> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The value held by this identifier
     */
    private final Integer value;
    
    /**
     * Builds the identifier with the given value.
     *
     * @param value the identifier value
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters
     */
    protected IntegerIdentifier(Integer value) {
        super();
        this.value = Parser.validateIntegerIdentifier(value, false);
    }

    /**
     * Builds the value identifier with the given value.
     *
     * @param value the identifier value
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal.
     * When the string cannot be converted to an integer because it's not an integer representation the exception will
     * also carry a {@link NumberFormatException} as its {@link Throwable#getCause() root cause}, otherwise the root
     * cause may me {@code null}.
     */
    protected IntegerIdentifier(String value) {
        super();
        this.value = Parser.validateIntegerIdentifier(value, false, false);
    }
    
    /**
     * Creates an identifier instance on the given value.
     *
     * @param value the identifier value
     * 
     * @return the identifier instance
     *
     * @throws IllegalArgumentException if the given value is illegal for some reason, like when it's negative
     */
    public static IntegerIdentifier valueOf(Integer value) {
        return new IntegerIdentifier(value);
    }

    /**
     * Creates an identifier instance by parsing the given string.
     *
     * @param value the identifier value
     * 
     * @return the identifier instance
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters.
     * When the string cannot be converted to an integer because it's not an integer representation the exception will
     * also carry a {@link NumberFormatException} as its {@link Throwable#getCause() root cause}, otherwise the root
     * cause may me {@code null}.
     */
    public static IntegerIdentifier valueOf(String value) {
        return new IntegerIdentifier(value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Integer o) {
        return value.compareTo(o);
    }

    /**
     * {@inheritDoc}
     */
    public final Integer getValue() {
        return value;
    }
}
