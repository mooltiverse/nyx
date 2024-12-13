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
package com.mooltiverse.oss.nyx.gradle;

import org.gradle.api.Project;

/**
 * An interface with constant declarations.
 */
public interface Constants {
    /**
     * The name of the Gradle project property that brings the project version. This property can be retrieved using
     * {@link Project#findProperty(String)} and set with {@link Project#setProperty(String, Object)}.
     * 
     * @see Project#DEFAULT_VERSION for the default value this property is assigned with when user doesn't
     * set the property.
     */
    public static final String GRADLE_VERSION_PROPERTY_NAME = "version";
}