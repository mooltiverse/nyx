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
package com.mooltiverse.oss.nyx.entities;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * This object models the fields used to configure the Git service.
 */
public class GitConfiguration {
    /**
     * The map of remotes configuration options.
     */
    private Map<String,GitRemoteConfiguration> remotes = null;

    /**
     * Default constructor.
     */
    public GitConfiguration() {
        super();
        this.remotes = new HashMap<String,GitRemoteConfiguration>();
    }

    /**
     * Standard constructor.
     * 
     * @param remotes the map of remotes configuration options.
     * 
     * @throws NullPointerException if some argument is {@code null}
     */
    public GitConfiguration(Map<String,GitRemoteConfiguration> remotes) {
        super();
        Objects.requireNonNull(remotes);
        this.remotes = remotes;
    }

    /**
     * Returns the map of remotes configuration options.
     * 
     * @return the map of remotes configuration options.
     */
    public Map<String,GitRemoteConfiguration> getRemotes() {
        return remotes;
    }

    /**
     * Sets the map of remotes configuration options.
     * 
     * @param remotes the map of remotes configuration options.
     */
    public void setRemotes(Map<String,GitRemoteConfiguration> remotes) {
        this.remotes = remotes;
    }
}
