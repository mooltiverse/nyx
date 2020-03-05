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
 * A value handler that holds a simple value like {@link String} or {@link Integer}.
 *
 * @param <V> the type of value represented by this handler. It must be a class around a primitive type like
 * {@link String} or {@link Integer}.
 */
interface SimpleValueHandler<V> extends ValueHandler, Comparable<V> {
    /**
     * Returns the value held by the handler.
     *
     * @return the value held by the handler.
     */
    public V getValue();
}
