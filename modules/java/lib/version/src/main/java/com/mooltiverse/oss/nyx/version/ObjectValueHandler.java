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
 * A simple handler holding an {@link Object} value.
 *
 * @param <V> the type of value represented by this handler. It must be a class around a primitive type like
 * {@link String} or {@link Integer}.
 */
class ObjectValueHandler<V extends Object> extends AbstractSimpleValueHandler<V> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Builds the value handler with the given value.
     *
     * @param value the handler value
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal
     */
    protected ObjectValueHandler(V value) {
        super(value);
    }

    /**
     * {@inheritDoc}
     */
    public int compareTo(V o) {
        if (o == null)
            return 1;
        return value.toString().compareTo(o.toString());
    }
}
