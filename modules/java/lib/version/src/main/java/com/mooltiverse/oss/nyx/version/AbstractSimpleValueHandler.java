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
 * A value handler that holds a simple value like {@link String} or {@link Integer}.
 *
 * @param <V> the type of value represented by this handler. It must be a class around a primitive type like
 * {@link String} or {@link Integer}.
 */
abstract class AbstractSimpleValueHandler<V extends Object> extends AbstractValueHandler implements SimpleValueHandler<V> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The value held by this handler
     */
    protected final V value;

    /**
     * Builds the value handler with the given value.

     * @param value the handler value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    protected AbstractSimpleValueHandler(V value) {
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
     *
     * @param <V> the type of value to validate
     */
    public static <V> V validate(V value) {
        Objects.requireNonNull(value, "Handler value cannot be null");
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 31 * value.hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (this == obj)
            return true;
        if (!this.getClass().isInstance(obj))
            return false;
        return value.equals(AbstractSimpleValueHandler.class.cast(obj).value);
    }

    /**
     * {@inheritDoc}
     */
    public String toString() {
        return value.toString();
    }

    /**
     * {@inheritDoc}
     */
    public V getValue() {
        return value;
    }
}
