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

import java.util.Objects;

/**
 * This object is a Git identity value holder independent from the underlying Git implementation.
 */
public class Identity {
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
    public Identity(String name, String email) {
        super();
        Objects.requireNonNull(name);
        this.name = name;
        this.email = email;
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
