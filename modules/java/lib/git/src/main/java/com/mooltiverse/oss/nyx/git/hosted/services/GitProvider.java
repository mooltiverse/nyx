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
package com.mooltiverse.oss.nyx.git.hosted.services;

import com.mooltiverse.oss.nyx.git.hosted.services.github.GitHub;
import com.mooltiverse.oss.nyx.git.hosted.services.gitlab.GitLab;

/**
 * These are the constants representing the available services and their implementation classes.
 */
public enum GitProvider {
    /**
     * The <a href="https://github.com/">GitHub</a> service provider.
     */
    GITHUB(GitHub.class),

    /**
     * The <a href="https://gitlab.com/">GitLab</a> service provider.
     */
    GITLAB(GitLab.class);

    /**
     * The class implementing the service
     */
    private final Class<?> serviceClass;

    /**
     * Builds the enum constant with the given implementation class.
     * 
     * @param clazz the implementation class for the service
     */
    private GitProvider(Class<?> clazz) {
        this.serviceClass = clazz;
    }

    /**
     * Returns the class implementing the service.
     * 
     * @return the class implementing the service.
     */
    public Class<?> getServiceClass() {
        return serviceClass;
    }
}