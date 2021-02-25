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
package com.mooltiverse.oss.nyx.git.hosted.services.gitlab;

import java.net.http.HttpRequest;

import java.util.Objects;

import com.mooltiverse.oss.nyx.git.hosted.services.GitAuthenticationException;
import com.mooltiverse.oss.nyx.git.hosted.services.GitSession;
import com.mooltiverse.oss.nyx.git.hosted.services.GitTransportException;

/**
 * An (authenticated) session for a remote GitLab service.
 */
public class GitLabSession implements GitSession {
    /**
     * The private instance of the backing service
     */
    private final GitLab serviceInstance;

    /**
     * The instance of a standard request builder with all reusable attributes set. Each request can
     * be created from a copy of this builder.
     */
    private HttpRequest.Builder requestBuilderTemplate = null;

    /**
     * The authentication token
     */
    private final String glToken;

    /**
     * The authenticated user for this session
     */
    private final GitLabUser authenticatedUser;

    /**
     * Default constructor is hidden on purpose.
     */
    private GitLabSession() {
        super();
        serviceInstance = null;
        glToken = null;
        authenticatedUser = null;
    }

    /**
     * Builds the session using the given backing service instance and authentication token.
     * 
     * @param service the backing service instance
     * @param token the authentication token
     * 
     * @throws GitAuthenticationException if authentication fails
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if any of the given objects is illegal for some reason (i.e. the token is an empry string)
     */
    private GitLabSession(GitLab service, String token) 
    throws GitTransportException, GitAuthenticationException {
        super();
        Objects.requireNonNull(service, "Cannot create a new session with a null backing service instance");
        Objects.requireNonNull(token, "Cannot create a new session with a null authentication token");
        if (token.isBlank())
            throw new IllegalArgumentException("Cannot create a new session with an empty authentication token");
        serviceInstance = service;
        glToken = token;
        authenticatedUser = GitLabUser.getAuthenticatedUser(this);
    }

    /**
     * Opens a new session using the given token for authentication.
     * 
     * @param service the backing service instance
     * @param token the authentication token
     * 
     * @return the new session
     * 
     * @throws GitAuthenticationException if authentication fails
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if any of the given objects is illegal for some reason (i.e. the token is an empry string)
     */
    static GitLabSession authenticateWithToken(GitLab service, String token)
        throws GitTransportException, GitAuthenticationException {
        return new GitLabSession(service, token);
    }

    /**
     * {@inheritDoc}
     */
    public GitLab getService() {
        return serviceInstance;
    }

    /**
     * {@inheritDoc}
     */
    public GitLabUser getAuthenticatedUser() {
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
            // See https://docs.gitlab.com/ee/api/README.html
            requestBuilderTemplate = serviceInstance.getRequestBuilder().setHeader("Authorization", "Bearer "+glToken); // Set the authentication token
        }
        return requestBuilderTemplate.copy();
    }
}