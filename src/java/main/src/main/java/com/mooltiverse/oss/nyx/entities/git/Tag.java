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
package com.mooltiverse.oss.nyx.entities.git;

import java.io.Serializable;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This object is a Git tag value holder independent from the underlying Git implementation.
 */
public class Tag implements Comparable<Tag>, Cloneable, Serializable {
    /**
     * The annotated or lightweight flag.
     */
    private final boolean annotated;

    /**
     * The name.
     */
    private final String name;

    /**
     * The tagged object ID.
     */
    private final String target;

    /**
     * Constructor.
     * 
     * @param name the simple name (without prefix). Cannot be {@code null}
     * @param target the ID (SHA-1) of the tagged object. Cannot be {@code null}
     * @param annotated make it {@code true} for annotated tags, {@code false} for lightweight tags
     */
    @JsonCreator
    public Tag(@JsonProperty("name") String name, @JsonProperty("target") String target, @JsonProperty("annotated") boolean annotated) {
        super();
        Objects.requireNonNull(name);
        Objects.requireNonNull(target);
        this.name = name;
        this.target = target;
        this.annotated = annotated;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 83 * Boolean.hashCode(annotated) * 79 * name.hashCode() * 73 * target.hashCode();
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

        Tag other = Tag.class.cast(obj);
        return Boolean.valueOf(isAnnotated()).equals(Boolean.valueOf(other.isAnnotated())) && getName().equals(other.getName()) && getTarget().equals(other.getTarget());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Tag t) {
        if (t == null)
            return 1;

        if (getName().compareTo(t.getName()) == 0) {
            if (getTarget().compareTo(t.getTarget()) == 0) {
                return Boolean.valueOf(isAnnotated()).compareTo(Boolean.valueOf(t.isAnnotated()));
            }
            else return getTarget().compareTo(t.getTarget());
        }
        else return getName().compareTo(t.getName());
    }

    /**
     * Returns {@code true} if this is an annotated tag, {@code false} if it's a lightweight tag.
     * 
     * @return {@code true} if this is an annotated tag, {@code false} if it's a lightweight tag.
     */
    public boolean isAnnotated() {
        return annotated;
    }

    /**
     * Returns the name.
     * 
     * @return the name. Never {@code null}.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the ID (SHA-1) of the tagged object.
     * 
     * @return the ID (SHA-1) of the tagged object. Never {@code null}.
     */
    public String getTarget() {
        return target;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return name;
    }
}
