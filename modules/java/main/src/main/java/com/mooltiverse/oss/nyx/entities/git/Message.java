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
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This object is a Git commit message value holder independent from the underlying Git implementation.
 */
public class Message implements Comparable<Message>, Cloneable, Serializable {
    /**
     * The message footer lines, where keys are names and values are values.
     */
    private final Map<String,String> footers;

    /**
     * The full message.
     */
    private final String fullMessage;

    /**
     * The short message.
     */
    private final String shortMessage;

    /**
     * Constructor.
     * 
     * @param fullMessage the full message. Cannot be {@code null}
     * @param shortMessage the short message. Cannot be {@code null}
     * @param footers the map of message footers, where keys are names and values are values. May be {@code null}
     */
    @JsonCreator
    public Message(@JsonProperty("fullMessage") String fullMessage, @JsonProperty("shortMessage") String shortMessage, @JsonProperty("footers") Map<String,String> footers) {
        super();
        Objects.requireNonNull(fullMessage);
        Objects.requireNonNull(shortMessage);
        this.fullMessage = fullMessage;
        this.shortMessage = shortMessage;
        this.footers = footers == null ? null : Collections.unmodifiableMap(footers);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 71 * fullMessage.hashCode() * 67 * shortMessage.hashCode() * 61 * (footers == null ? 1 : footers.hashCode());
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

        Message other = Message.class.cast(obj);
        return getFullMessage().equals(other.getFullMessage()) && getShortMessage().equals(other.getShortMessage()) && (getFooters() == null ? other.getFooters() == null : getFooters().equals(other.getFooters()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Message m) {
        if (m == null)
            return 1;

        if (getFullMessage().compareTo(m.getFullMessage()) == 0) {
            if (getShortMessage().compareTo(m.getShortMessage()) == 0) {
                if (getFooters() == null) {
                    return m.getFooters() == null ? 0 : m.getFooters().size() * -1;
                } else {
                    return m.getFooters() == null ? getFooters().size() : getFooters().size()-m.getFooters().size();
                }
            }
            else return getShortMessage().compareTo(m.getShortMessage());
        }
        else return getFullMessage().compareTo(m.getFullMessage());
    }

    /**
     * Returns the immutable list of footers, where keys are names and values are values.
     * 
     * @return the immutable list of footers, where keys are names and values are values. May be {@code null}.
     */
    public Map<String,String> getFooters() {
        return footers;
    }

    /**
     * Returns the full message.
     * 
     * @return the full message. Never {@code null}.
     */
    public String getFullMessage() {
        return fullMessage;
    }

    /**
     * Returns the short message.
     * 
     * @return the short message. Never {@code null}.
     */
    public String getShortMessage() {
        return shortMessage;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return shortMessage.concat(shortMessage.equals(fullMessage) ? "" : " ...");
    }
}
