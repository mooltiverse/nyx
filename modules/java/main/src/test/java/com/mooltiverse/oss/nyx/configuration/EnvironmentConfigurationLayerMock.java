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
package com.mooltiverse.oss.nyx.configuration;

import java.util.HashMap;
import java.util.Map;

/**
 * This mock is used to workaround the limitation in setting environment variables.
 * This allows to pass a map of values that will be used in place of {@link System#getenv()}
 * and {@link System#getenv(String)}.
 * <br>
 * 
 * This object allows read only operations.
 */
class EnvironmentConfigurationLayerMock extends EnvironmentConfigurationLayer {
    /**
     * The map used in place of the environment variables normally returned by {@link System#getenv()}.
     */
    public final Map<String,String> environment = new HashMap<String,String>();

    /**
     * Default constructor is private on purpose.
     */
    private EnvironmentConfigurationLayerMock() {
        super();
    }

    /**
     * Returns a new instance of this class. Although this looks like a singleton factory method,
     * it just mimicks the superclass factory but in this case always returns a new instance.
     * 
     * @return a new instance of this class.
     */
    static EnvironmentConfigurationLayerMock getInstance() {
        return new EnvironmentConfigurationLayerMock();
    }

    /**
     * Returns the {@link #environment} map, used for mocking the {@link System#getenv()} map.
     * 
     * @return the {@link #environment} map.
     */
    @Override
    protected Map<String,String> getenv() {
        return environment;
    }

    /**
     * Returns the value of the entry with the given key from {@link System#getenv(String)}.
     * 
     * @return the value of the entry with the given key from {@link System#getenv(String)}.
     * 
     * @throws NullPointerException if thrown by the underlying map.
     */
    @Override
    protected String getenv(String name)
        throws NullPointerException {
        return environment.get(name);
    }
}
