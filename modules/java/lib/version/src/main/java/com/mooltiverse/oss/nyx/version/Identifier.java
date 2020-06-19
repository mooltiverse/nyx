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

import java.io.Serializable;

/**
 * An identifier is a simple value, part of a composite value.
 */
abstract class Identifier implements Cloneable, Serializable {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * Default contstructor, protected on purpose.
     */
    protected Identifier() {
        super();
    }

    /**
     * Returns the underlying value held by this identifier.
     * 
     * @return the underlying value held by this identifier
     */
    public abstract Object getValue();

    /**
     * Returns a has code value for this object.
     * 
     * @return a has code value for this object.
     * 
     * @see Object#hashCode()
     */
    public abstract int hashCode();

    /**
     * Tests the given object for equality against this instance.
     * 
     * @return <code>true</code> if this object is the same as the obj argument; <code>false</code> otherwise.
     * 
     * @see Object#equals(Object)
     */
    @Override
    public abstract boolean equals(Object obj);

    /**
     * Returns a string representation of the object, coherent with the identigfier scope,
     * suitable for generating a version string.
     * 
     * @return a string representation of the object
     * 
     * @see Object#toString()
     */
    @Override
    public abstract String toString();
}
