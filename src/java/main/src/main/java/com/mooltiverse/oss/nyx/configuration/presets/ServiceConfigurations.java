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
package com.mooltiverse.oss.nyx.configuration.presets;

import java.util.Map;

import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;

/**
 * This class provides reusable configuration chunks for service configurations.
 */
public class ServiceConfigurations {
    /**
     * The GitHub service configuration
     */
    public static final ServiceConfiguration GITHUB = new ServiceConfiguration() {
        {
            setType(Provider.GITHUB);
            setOptions(Map.<String,String>of("AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}"));
        }
    };

    /**
     * The GitLab service configuration
     */
    public static final ServiceConfiguration GITLAB = new ServiceConfiguration() {
        {
            setType(Provider.GITLAB);
            setOptions(Map.<String,String>of("AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}"));
        }
    };

    /**
     * Default constructor is hidden on purpose.
     */
    public ServiceConfigurations() {
        super();
    }
}
