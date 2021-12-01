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

import java.util.Map;

import com.mooltiverse.oss.nyx.services.Provider;

/**
 * This object models the fields used to configure a generic service.
 */
public class ServiceConfiguration {
    /**
     * The map of service configuration options.
     */
    private Map<String,String> options = null;

    /**
     * The type of service.
     */
    private Provider type = null;

    /**
     * Default constructor.
     */
    public ServiceConfiguration() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param type the type of service.
     * @param options the map of service configuration options.
     */
    public ServiceConfiguration(Provider type, Map<String,String> options) {
        super();
        this.type = type;
        this.options = options;
    }

    /**
     * Returns the map of service configuration options.
     * 
     * @return the map of service configuration options.
     */
    public Map<String,String> getOptions() {
        return options;
    }

    /**
     * Sets the map of service configuration options.
     * 
     * @param options the map of service configuration options.
     */
    public void setOptions(Map<String,String> options) {
        this.options = options;
    }

    /**
     * Returns the service type
     * 
     * @return the service type
     */
    public Provider getType() {
        return type;
    }

    /**
     * Sets the service type.
     * 
     * @param type the service type.
     */
    public void setType(Provider type) {
        this.type = type;
    }
}
