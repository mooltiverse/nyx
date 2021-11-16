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
 * This object is a Git identity value holder independent from the underlying Git implementation.
 */
public class Identity implements Comparable<Identity>, Cloneable, Serializable {
    /**
     * The email.
     */
    private final String email;

    /**
     * The name.
     */
    private final String name;

    /**
     * Constructor.
     * 
     * @param name the name. Cannot be {@code null}
     * @param email the email. May be {@code null}
     */
    @JsonCreator
    public Identity(@JsonProperty("name") String name, @JsonProperty("email") String email) {
        super();
        Objects.requireNonNull(name);
        this.name = name;
        this.email = email;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 59 * name.hashCode() * (email == null ? 1 : 53 * email.hashCode());
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

        Identity other = Identity.class.cast(obj);
        return getName().equals(other.getName()) && Objects.isNull(getEmail()) ? Objects.isNull(other.getEmail()) : getEmail().equals(other.getEmail());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Identity i) {
        if (i == null)
            return 1;

        if (getName().compareTo(i.getName()) == 0) {
            return Objects.isNull(getEmail()) ? (Objects.isNull(i.getEmail()) ? 0 : 1) : getEmail().compareTo(i.getEmail());
        }
        else return getName().compareTo(i.getName());
    }

    /**
     * Returns the email.
     * 
     * @return the email. May be {@code null}.
     */
    public String getEmail() {
        return email;
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
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return name.concat(Objects.isNull(email) ? "" : " <".concat(email).concat(">"));
    }
}
