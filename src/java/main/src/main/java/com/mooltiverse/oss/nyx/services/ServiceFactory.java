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

import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.services.Service.Feature;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.gitlab.GitLab;

/**
 * The generic entry point to inspect and retrieve service implementations.
 */
public class ServiceFactory {
    /**
     * Default constructor is hidden on purpose.
     */
    private ServiceFactory() {
        super();
    }

    /**
     * Returns an instance for the given provider using the given options.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param options the map of options for the requested service. It may be {@code null} if the requested
     * service does not require the options map. To know if the service needs rhese options and, if so, which
     * entries are to be present please check with the specific service.
     * 
     * @return a service instance initialized with the given options.
     * 
     * @throws NullPointerException if the given provider is {@code null} or the given options map is {@code null}
     * and the service instance does not allow {@code null} options
     * @throws IllegalArgumentException if the given provider is not supported or some entries in the given options
     * map are illegal for some reason
     * @throws UnsupportedOperationException if the service provider does not {@link Service#supports(Service.Feature)
     * support} the {@link Service.Feature#GIT_HOSTING} feature.
     */
    public static GitHostingService gitHostingServiceInstance(Provider provider, Map<String,String> options){
        Service instance = instance(provider, options);
        if (instance.supports(Feature.GIT_HOSTING))
            try {
                return GitHostingService.class.cast(instance);
            }
            catch (ClassCastException cce) {
                throw new UnsupportedOperationException(String.format("The %s provider supports the %s feature but instances do not implement the %s interface", provider.toString(), Feature.GIT_HOSTING.toString(), ReleaseService.class.getName()), cce);
            }
        else throw new UnsupportedOperationException(String.format("The %s provider does not support the %s feature", provider.toString(), Feature.GIT_HOSTING.toString()));
    }

    /**
     * Returns an instance for the given provider using the given options.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param options the map of options for the requested service. It may be {@code null} if the requested
     * service does not require the options map. To know if the service needs rhese options and, if so, which
     * entries are to be present please check with the specific service.
     * 
     * @return a service instance initialized with the given options.
     * 
     * @throws NullPointerException if the given provider is {@code null} or the given options map is {@code null}
     * and the service instance does not allow {@code null} options
     * @throws IllegalArgumentException if some entries in the given options map are missing or illegal for some reason
     */
    public static Service instance(Provider provider, Map<String,String> options) {
        Objects.requireNonNull(provider, "Can't create a provider instance from a null provider spec");
        switch (provider)
        {
            case GITHUB:    return GitHub.instance(options);
            case GITLAB:    return GitLab.instance(options);
            default:        throw new IllegalArgumentException(String.format("Illegal provider: '%s'", provider));
        }
    }

    /**
     * Returns an instance for the given provider using the given options.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param options the map of options for the requested service. It may be {@code null} if the requested
     * service does not require the options map. To know if the service needs rhese options and, if so, which
     * entries are to be present please check with the specific service.
     * 
     * @return a service instance initialized with the given options.
     * 
     * @throws NullPointerException if the given provider is {@code null} or the given options map is {@code null}
     * and the service instance does not allow {@code null} options
     * @throws IllegalArgumentException if the given provider is not supported or some entries in the given options
     * map are illegal for some reason
     * @throws UnsupportedOperationException if the service provider does not {@link Service#supports(Service.Feature)
     * support} the {@link Service.Feature#RELEASES} feature.
     */
    public static ReleaseService releaseServiceInstance(Provider provider, Map<String,String> options) {
        Service instance = instance(provider, options);
        if (instance.supports(Feature.RELEASES))
            try {
                return ReleaseService.class.cast(instance);
            }
            catch (ClassCastException cce) {
                throw new UnsupportedOperationException(String.format("The %s provider supports the %s feature but instances do not implement the %s interface", provider.toString(), Feature.RELEASES.toString(), ReleaseService.class.getName()), cce);
            }
        else throw new UnsupportedOperationException(String.format("The %s provider does not support the %s feature", provider.toString(), Feature.RELEASES.toString()));
    }

    /**
     * Returns an instance for the given provider using the given options.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param options the map of options for the requested service. It may be {@code null} if the requested
     * service does not require the options map. To know if the service needs rhese options and, if so, which
     * entries are to be present please check with the specific service.
     * 
     * @return a service instance initialized with the given options.
     * 
     * @throws NullPointerException if the given provider is {@code null} or the given options map is {@code null}
     * and the service instance does not allow {@code null} options
     * @throws IllegalArgumentException if the given provider is not supported or some entries in the given options
     * map are illegal for some reason
     * @throws UnsupportedOperationException if the service provider does not {@link Service#supports(Service.Feature)
     * support} the {@link Service.Feature#USERS} feature.
     */
    public static UserService userServiceInstance(Provider provider, Map<String,String> options){
        Service instance = instance(provider, options);
        if (instance.supports(Feature.USERS))
            try {
                return UserService.class.cast(instance);
            }
            catch (ClassCastException cce) {
                throw new UnsupportedOperationException(String.format("The %s provider supports the %s feature but instances do not implement the %s interface", provider.toString(), Feature.USERS.toString(), ReleaseService.class.getName()), cce);
            }
        else throw new UnsupportedOperationException(String.format("The %s provider does not support the %s feature", provider.toString(), Feature.USERS.toString()));
    }
}