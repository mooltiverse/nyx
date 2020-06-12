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
 * The superclass of Version classes.
 */
abstract class AbstractVersion extends AbstractIdentifier implements Version {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The value handler used internally by this version class
     */
    private final ValueHandler handler;

    /**
     * Builds the version with the given value handler.
     *
     * @param handler value handler used internally by this version class
     *
     * @throws NullPointerException if the given value handler is <code>null</code>
     * @throws IllegalArgumentException if the given value handler is illegal or contains illegal values illegal
     */
    protected AbstractVersion(ValueHandler handler) {
        super();
        Objects.requireNonNull(handler, "Value handler can't be null");
        this.handler = handler;
    }

    /**
     * Returns a hash code value for the object.
     *
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        return 19 * handler.hashCode();
    }
}
