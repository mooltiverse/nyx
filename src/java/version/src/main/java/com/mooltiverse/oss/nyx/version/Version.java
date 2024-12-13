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
 * The superclass of Version classes.
 */
public abstract class Version implements Cloneable, Serializable {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * Builds the version.
     */
    protected Version() {
        super();
    }

    /**
     * Returns the hash code for this object
     * 
     * @return the hash code for this object
     */
    @Override
    public abstract int hashCode();

    /**
     * Returns {@code true} if this version is equal to the given object, {@code false} otherwise
     * 
     * @param obj the object to compare to
     * 
     * @return {@code true} if this version is equal to the given object, {@code false} otherwise
     */
    @Override
    public abstract boolean equals(Object obj);

    /**
     * Returns the scheme that identifies the implementation
     * 
     * @return the scheme that identifies the implementation
     */
    public abstract Scheme getScheme();

    /**
     * Returns the string representation of this version
     * 
     * @return the string representation of this version
     */
    @Override
    public abstract String toString();

    /**
     * Returns a new instance with the number identified by the given value bumped. The supported identifiers depend
     * on the concrete subclass.
     *
     * @param id the name of the identifier to bump
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if {@code null} is passed
     * @throws IllegalArgumentException if the given string is empty, contains illegal characters or does not represent
     * a valid identifier to be bumped
     */
    public abstract Version bump(String id);
}
