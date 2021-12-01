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
 * This object is a Git action value holder independent from the underlying Git implementation.
 */
public class Action implements Comparable<Action>, Cloneable, Serializable {
    /**
     * The identity.
     */
    private final Identity identity;

    /**
     * The time stamp.
     */
    private final TimeStamp timeStamp;

    /**
     * Constructor.
     * 
     * @param identity the identity. Cannot be {@code null}
     * @param timeStamp the time stamp. May be {@code null}
     */
    @JsonCreator
    public Action(@JsonProperty("identity") Identity identity, @JsonProperty("timeStamp") TimeStamp timeStamp) {
        super();
        Objects.requireNonNull(identity);
        this.identity = identity;
        this.timeStamp = timeStamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 47 * identity.hashCode() * (timeStamp == null ? 1 : 43 * timeStamp.hashCode());
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

        Action other = Action.class.cast(obj);
        return getIdentity().equals(other.getIdentity()) && Objects.isNull(getTimeStamp()) ? Objects.isNull(other.getTimeStamp()) : getTimeStamp().equals(other.getTimeStamp());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Action a) {
        if (a == null)
            return 1;

        if (getIdentity().compareTo(a.getIdentity()) == 0) {
            return Objects.isNull(getTimeStamp()) ? (Objects.isNull(a.getTimeStamp()) ? 0 : 1) : getTimeStamp().compareTo(a.getTimeStamp());
        }
        else return getIdentity().compareTo(a.getIdentity());
    }

    /**
     * Returns the identity.
     * 
     * @return the identity. Never {@code null}.
     */
    public Identity getIdentity() {
        return identity;
    }

    /**
     * Returns the time stamp.
     * 
     * @return the time stamp. May be {@code null}.
     */
    public TimeStamp getTimeStamp() {
        return timeStamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return identity.toString().concat(Objects.isNull(timeStamp) ? "" : " ".concat(timeStamp.toString()));
    }
}
