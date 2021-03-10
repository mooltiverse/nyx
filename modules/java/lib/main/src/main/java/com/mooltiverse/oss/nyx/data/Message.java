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

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * This object is a Git commit message value holder independent from the underlying Git implementation.
 */
public class Message {
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
     * @param footers the map of message footers, where keys are names and values are values. Cannot be {@code null}
     */
    public Message(String fullMessage, String shortMessage, Map<String,String> footers) {
        super();
        Objects.requireNonNull(fullMessage);
        Objects.requireNonNull(shortMessage);
        Objects.requireNonNull(footers);
        this.fullMessage = fullMessage;
        this.shortMessage = shortMessage;
        this.footers = Collections.unmodifiableMap(footers);
    }

    /**
     * Returns the immutable list of footers, where keys are names and values are values.
     * 
     * @return the immutable list of footers, where keys are names and values are values. May be empty but not {@code null}.
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
