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
 * This object is a Git tag value holder independent from the underlying Git implementation.
 */
public class Tag {
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
    public Tag(String name, String target, boolean annotated) {
        super();
        Objects.requireNonNull(name);
        Objects.requireNonNull(target);
        this.name = name;
        this.target = target;
        this.annotated = annotated;
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
