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
package com.mooltiverse.oss.nyx.services.github;

import java.util.Objects;

import com.mooltiverse.oss.nyx.services.GitServiceFeature;

/**
 * Package level utility class that collects supported features for this specific provider.
 */
class FeatureSupport {
    /**
     * Default constructor is hidden on purpose.
     */
    private FeatureSupport() {
        super();
    }

    /**
     * Safely checks if the implementation supports the given operation.
     * 
     * @param feature the feature to check for support.
     * 
     * @return {@code true} if the operation is supported, {@code false} otherwise
     * 
     * @throws NullPointerException if a {@code null} feature is passed
     */
    static boolean supports(GitServiceFeature feature) {
        Objects.requireNonNull(feature, "Can't check if the feature is supported for a null feature");
        switch (feature)
        {
            case CUSTOM_ENDPOINT:       return true;
            case PING:                  return true;
            case TOKEN_AUTHENTICATION:  return true;
            default:                    return false;
        }
    }
}