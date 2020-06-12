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

import java.util.Objects;

/**
 * A simple handler holding an {@link Integer} value.
 */
class IntegerValueHandler extends SimpleValueHandler implements Comparable<Integer> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The value held by this handler
     */
    private final Integer value;
    
    /**
     * Builds the value handler with the given value.
     *
     * @param value the handler value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    protected IntegerValueHandler(Integer value) {
        super();
        this.value = validate(value);
    }

    /**
     * Builds the value handler with the given value.
     *
     * @param value the handler value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    protected IntegerValueHandler(String value) {
        super();
        this.value = validate(value);
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
    private static Integer validate(Integer value) {
        Objects.requireNonNull(value, "Handler value cannot be null");
        if (value.intValue() < 0)
            throw new IllegalArgumentException(String.format("Integer value cannot be negative. %d was passed.", value));
        return value;
    }

    /**
     * Validates the given value to make sure it's legal.
     *
     * @param value the value to validate
     *
     * @return the same value passed as input, converted to Integer
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    private static Integer validate(String value) {
        Objects.requireNonNull(value, "Handler value cannot be null");

        Integer intValue = null;
        try {
            intValue = Integer.valueOf(value);
        }
        catch (NumberFormatException nfe) {
            throw new IllegalArgumentException(String.format("The value %s is not a valid integer", value), nfe);
        }

        // check that the string representation of the parsed value is the same as the input value
        // they may differ if the input value had leading zeroes
        if (!value.equals(intValue.toString()))
            throw new IllegalArgumentException(String.format("The value %s is not the same as %d, which makes it illegal. Consider sanitizing the string before parsing.", value, intValue));

        // let the other version of the validate() method check if it's positive
        return validate(intValue);
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
