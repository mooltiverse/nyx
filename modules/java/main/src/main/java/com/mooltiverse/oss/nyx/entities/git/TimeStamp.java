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
import java.util.Date;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This object is a Git timestamp value holder independent from the underlying Git implementation.
 */
public class TimeStamp implements Comparable<TimeStamp>, Cloneable, Serializable {
    /**
     * The time stamp.
     */
    private final Date timeStamp;

    /**
     * The time zone offset in minutes relative to UTC.
     */
    private final Integer offset;

    /**
     * Constructor.
     * 
     * @param timeStamp the time stamp. Cannot be {@code null}
     * @param offset the time zone offset in minutes relative to UTC. May be {@code null}
     */
    @JsonCreator
    public TimeStamp(@JsonProperty("timeStamp") Date timeStamp, @JsonProperty("offset") Integer offset) {
        super();
        Objects.requireNonNull(timeStamp);
        this.timeStamp = timeStamp;
        this.offset = offset;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 97 * timeStamp.hashCode() * (offset == null ? 1 : 89 * offset.hashCode());
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

        TimeStamp other = TimeStamp.class.cast(obj);
        return getTimeStamp().equals(other.getTimeStamp()) && Objects.isNull(getOffset()) ? Objects.isNull(other.getOffset()) : getOffset().equals(other.getOffset());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(TimeStamp t) {
        if (t == null)
            return 1;

        return getTimeStamp().compareTo(t.getTimeStamp());
    }

    /**
     * Returns the time stamp.
     * 
     * @return the time stamp. Never {@code null}.
     */
    public Date getTimeStamp() {
        return timeStamp;
    }

    /**
     * Returns the time zone offset in minutes relative to UTC.
     * 
     * @return the time zone offset in minutes relative to UTC. May be {@code null}.
     */
    public Integer getOffset() {
        return offset;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return timeStamp.toString().concat(Objects.isNull(offset) ? "" : " ".concat(offset.toString()));
    }
}
