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
package com.mooltiverse.oss.nyx.services;

/**
 * These are the constants representing the features that may or may not be supported by providers.
 * <br>
 * When invoking an operation method against a provider and the operation is not supported then
 * the method throws an {@link UnsupportedOperationException}.
 * <br>
 * In order to safely know if an operation is supported you can query the service object via
 * the {@code supports} method.
 */
public enum GitServiceFeature {
    /**
     * When this feature is supported then the implementation class supports authentication with
     * tokens (Personal Access Tokens, OAuth).
     */
    TOKEN_AUTHENTICATION(false),

    /**
     * When this feature is supported then the implementation class supports custom API endpoints
     * (i.e. for on premises installations).
     */
    CUSTOM_ENDPOINT(true),

    /**
     * When this feature is supported then the implementation class supports the {@link GitService#ping()}
     * method.
     */
    PING(false);

    /**
     * The optional flag
     */
    private final boolean optional;

    /**
     * Builds the enum constant with the given optional flag.
     * 
     * @param optional the optional flag for the feature
     */
    private GitServiceFeature(boolean optional) {
        this.optional = optional;
    }

    /**
     * Returns {@code true} if the feature is optional.
     * 
     * @return {@code true} if the feature is optional
     */
    public boolean isOptional() {
        return optional;
    }
}