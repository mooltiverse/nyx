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
package com.mooltiverse.oss.nyx.data;

import java.util.Date;
import java.util.Objects;
import java.util.TimeZone;

/**
 * This object is a Git timestamp value holder independent from the underlying Git implementation.
 */
public class TimeStamp {
    /**
     * The time stamp.
     */
    private final Date timeStamp;

    /**
     * The time zone.
     */
    private final TimeZone timeZone;

    /**
     * Constructor.
     * 
     * @param timeStamp the time stamp. Cannot be {@code null}
     * @param timeZone the time zone. May be {@code null}
     */
    public TimeStamp(Date timeStamp, TimeZone timeZone) {
        super();
        Objects.requireNonNull(timeStamp);
        this.timeStamp = timeStamp;
        this.timeZone = timeZone;
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
     * Returns the time zone.
     * 
     * @return the time zone. May be {@code null}.
     */
    public TimeZone getTimeZone() {
        return timeZone;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return timeStamp.toString().concat(Objects.isNull(timeZone) ? "" : " ".concat(timeZone.getDisplayName()));
    }
}
