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
 * A simple handler holding a {@link String} value.
 */
class StringValueHandler extends AbstractSimpleValueHandler<String> {
    /**
     * The range of allowed characters in string identifiers.
     *
     * {@value}
     */
    static final String ALLOWED_CHARACTERS = "[0-9a-zA-Z-]";

    /**
     * The regexp pattern that can be used to validate characters in string identifiers.
     *
     * {@value}
     */
    static final String ALLOWED_CHARACTERS_REGEXP_PATTERN = "[0-9a-zA-Z-]+";

    /**
     * Builds the value handler with the given value.
     *
     * @param value the handler value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    protected StringValueHandler(String value) {
        super(validate(value));
    }

    /**
     * Validates the given value to make sure it's legal.
     *
     * @param value the value to validate
     *
     * @return the same value passed as input
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    static String validate(String value) {
        AbstractSimpleValueHandler.validate(value);
        if (value.isEmpty())
            throw new IllegalArgumentException("Handler string value cannot be empty");
        if (!value.matches(ALLOWED_CHARACTERS_REGEXP_PATTERN))
            throw new IllegalArgumentException(String.format("Illegal characters found in handler value %s. Only characters in range "+ALLOWED_CHARACTERS+" are allowed", value));
        return value;
    }

    /**
     * {@inheritDoc}
     */
    public int compareTo(String o) {
        return value.compareTo(o);
    }
}
