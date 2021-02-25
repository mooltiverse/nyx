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

import java.net.URI;

import java.util.Objects;

/**
 * The generic entry point to retrieve service implementations.
 */
public class GitServiceFactory {
    /**
     * Default constructor is hidden on purpose.
     */
    private GitServiceFactory() {
        super();
    }

    /**
     * Returns an instance for the given provider using the default API URI.
     * 
     * @param provider the provider to retrieve the instance for.
     * 
     * @return an instance using the default API URI.
     * 
     * @throws GitTransportException if the default URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given provider is {@code null}
     * @throws IllegalArgumentException if the given provider is not supported
     */
    public static GitService instance(GitProvider provider)
        throws GitTransportException {
        Objects.requireNonNull(provider, "Can't create a provider instance from a null provider spec");
        switch (provider)
        {
            case GITHUB: return GitHub.instance();
            case GITLAB: return GitLab.instance();
            default:     throw new IllegalArgumentException(String.format("Illegal provider: %s", provider));
        }
    }

    /**
     * Returns an instance for the given provider using the given API URI.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param apiURI the API URI, which is usually the endpoint to a private or on premise installation
     * 
     * @return an instance using the given API URI
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI or provider is {@code null}
     * @throws IllegalArgumentException if the given provider is not supported or the given URI is illegal (i.e. empty or malformed)
     * @throws UnsupportedOperationException if the requested provider does not support the {@link GitServiceFeature#CUSTOM_ENDPOINT} feature
     */
    public static GitService instance(GitProvider provider, String apiURI)
        throws GitTransportException {
        Objects.requireNonNull(provider, "Can't create a provider instance from a null provider spec");
        switch (provider)
        {
            case GITHUB: return GitHub.instance(apiURI);
            case GITLAB: return GitLab.instance(apiURI);
            default:     throw new IllegalArgumentException(String.format("Illegal provider: %s", provider));
        }
    }

    /**
     * Returns an instance for the given provider using the given API URI.
     * 
     * @param provider the provider to retrieve the instance for.
     * @param apiURI the API URI, which is usually the endpoint to a private or on premise installation
     * 
     * @return an instance using the given API URI
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI provider is {@code null}
     * @throws IllegalArgumentException if the given provider is not supported or the given URI is illegal (i.e. empty or malformed)
     * @throws UnsupportedOperationException if the requested provider does not support the {@link GitServiceFeature#CUSTOM_ENDPOINT} feature
     */
    public static GitService instance(GitProvider provider, URI apiURI)
        throws GitTransportException {
        Objects.requireNonNull(provider, "Can't create a provider instance from a null provider spec");
        switch (provider)
        {
            case GITHUB: return GitHub.instance(apiURI);
            case GITLAB: return GitLab.instance(apiURI);
            default:     throw new IllegalArgumentException(String.format("Illegal provider: %s", provider));
        }
    }
}