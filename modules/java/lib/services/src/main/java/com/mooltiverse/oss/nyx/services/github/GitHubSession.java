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

import java.net.http.HttpRequest;

import java.util.Objects;

import com.mooltiverse.oss.nyx.services.GitAuthenticationException;
import com.mooltiverse.oss.nyx.services.GitSession;
import com.mooltiverse.oss.nyx.services.GitTransportException;

/**
 * An (authenticated) session for a remote GitHub service.
 */
public class GitHubSession implements GitSession {
    /**
     * The private instance of the backing service
     */
    private final GitHub serviceInstance;

    /**
     * The instance of a standard request builder with all reusable attributes set. Each request can
     * be created from a copy of this builder.
     */
    private HttpRequest.Builder requestBuilderTemplate = null;

    /**
     * The authentication token
     */
    private final String ghToken;

    /**
     * The authenticated user for this session
     */
    private final GitHubUser authenticatedUser;

    /**
     * Default constructor is hidden on purpose.
     */
    private GitHubSession() {
        super();
        serviceInstance = null;
        ghToken = null;
        authenticatedUser = null;
    }

    /**
     * Builds the session using the given backing service instance and authentication token.
     * 
     * @param service the backing service instance
     * @param token the authentication token
     * 
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws GitAuthenticationException if authentication fails
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if any of the given objects is illegal for some reason (i.e. the token is an empry string)
     */
    private GitHubSession(GitHub service, String token) 
        throws GitTransportException, GitAuthenticationException {
        super();
        Objects.requireNonNull(service, "Cannot create a new session with a null backing service instance");
        Objects.requireNonNull(token, "Cannot create a new session with a null authentication token");
        if (token.isBlank())
            throw new IllegalArgumentException("Cannot create a new session with an empty authentication token");
        serviceInstance = service;
        ghToken = token;
        authenticatedUser = GitHubUser.getAuthenticatedUser(this);
    }

    /**
     * Opens a new session using the given token for authentication.
     * 
     * @param service the backing service instance
     * @param token the authentication token
     * 
     * @return the new authenticated session
     * 
     * @throws GitAuthenticationException if authentication fails
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if any of the given objects is illegal for some reason (i.e. the token is an empry string)
     */
    static GitHubSession authenticateWithToken(GitHub service, String token)
        throws GitTransportException, GitAuthenticationException {
        return new GitHubSession(service, token);
    }

    /**
     * {@inheritDoc}
     */
    public GitHub getService() {
        return serviceInstance;
    }

    /**
     * {@inheritDoc}
     */
    public GitHubUser getAuthenticatedUser() {
        return authenticatedUser;
    }

    /**
     * Returns an HTTP request builder with all reusable attributes set. The returned object is authenticated.<br>
     * After retrieving the request builder with this method you can set custom properties if you need to and then get a new request
     * instance with {@link HttpRequest.Builder#build()}.
     * 
     * @return an HTTP request builder with all reusable attributes set.
     */
    synchronized HttpRequest.Builder getRequestBuilder() {
        if (requestBuilderTemplate == null) {
            // See https://developer.github.com/v3/ for details
            requestBuilderTemplate = serviceInstance.getRequestBuilder().setHeader("Authorization", "token "+ghToken); // Set the authentication token
        }
        return requestBuilderTemplate.copy();
    }
}