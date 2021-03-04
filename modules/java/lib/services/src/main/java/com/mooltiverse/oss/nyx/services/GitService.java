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

import java.net.URI;

/**
 * An abstraction over Git services.
 */
public interface GitService {
    /**
     * Safely checks if the underlying implementation supports the given operation. If this
     * method returns {@code true} then the underlying class will not raise any
     * {@link UnsupportedOperationException} when invoking the specific methods.
     * 
     * @param feature the feature to check for support.
     * 
     * @return {@code true} if the operation is supported, {@code false} otherwise
     */
    public boolean supports(GitServiceFeature feature);

    /**
     * Returns the API base URI for the service instance.
     * 
     * @return the API base URI for the service instance
     */
    public URI getBaseURI();

    /**
     * Tries to contact the remote server to verify it's reachable and healthy. Please note that this method
     * sends unauthenticated requests that may fail because of rate limits so use it with care.
     * 
     * @return {@code true} if the operation succeeds, {@code false} otherwise
     * 
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(GitServiceFeature) support} the {@link GitServiceFeature#PING} feature.
     */
    public boolean ping();

    /**
     * Authenticates by using the given token (Personal Access Token, OAuth)
     * 
     * @param token the token to use for authentication.
     * 
     * @return a session object that can be used to perform operations as an authenticated user
     * 
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws GitAuthenticationException if authentication fails
     * @throws NullPointerException if the given token is {@code null}
     * @throws IllegalArgumentException if given token is illegal for some reason (i.e. is an empry string)
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(GitServiceFeature) support} the {@link GitServiceFeature#TOKEN_AUTHENTICATION} feature.
     */
    public GitSession authenticateWithToken(String token)
        throws GitTransportException, GitAuthenticationException;
}