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
 * A simple identifier holding a {@link String} value.
 * 
 * In <a href="https://semver.org/">Semantic Versioning 2.0.0</a> parlance, a string identifier can be
 * alphanumeric or even numeric, where leading zeroes are allowed. In other words, all non positive
 * integer identifiers in <a href="https://semver.org/">Semantic Versioning 2.0.0</a> are to be considered
 * as string identifiers, while positive integers are handled as {@link IntegerIdentifier}.
 */
class StringIdentifier extends SimpleIdentifier implements Comparable<String> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The value held by this identifier
     */
    private final String value;

    /**
     * Builds the identifier with the given value.
     *
     * @param value the identifier value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters
     */
    protected StringIdentifier(String value) {
        super();
        this.value = Parser.validateStringIdentifier(value);
    }

    /**
     * Creates an identifier instance by parsing the given string.
     *
     * @param value the identifier value
     * 
     * @return the identifier instance
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters
     */
    public static StringIdentifier valueOf(String value) {
        return new StringIdentifier(value);
    }

    /**
     * {@inheritDoc}
     */
    public int compareTo(String o) {
        return value.compareTo(o);
    }

    /**
     * {@inheritDoc}
     */
    public final String getValue() {
        return value;
    }
}
