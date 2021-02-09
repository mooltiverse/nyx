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

import java.io.IOException;

import java.net.URI;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.git.hosted.services.GitAuthenticationException;
import com.mooltiverse.oss.nyx.git.hosted.services.GitUser;
import com.mooltiverse.oss.nyx.git.hosted.services.GitTransportException;

/**
 * A user for a remote GitLab service.
 */
public class GitLabUser extends GitLabEntity implements GitUser {
    /**
     * The private instance of the user attributes map.
     */
    private final Map<String, Object> attributes;

    /**
     * Creates the user object associated with the given session and attributes.
     * 
     * @param session the session associated with this object
     * @param attributes the map of attributes for this object
     * 
     * @throws NullPointerException if the given session or attributes map is <code>null</code>
     * @throws IllegalArgumentException if the map of attributes is empty
     */
    private GitLabUser(GitLabSession session, Map<String, Object> attributes) {
        super(session);
        Objects.requireNonNull(attributes, "Can't create a user with a null map of attributes");
        if (attributes.isEmpty())
            throw new IllegalArgumentException("Can't create a user with an empty attributes map");
        this.attributes = attributes;
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Object> getAttributes() {
        return Collections.unmodifiableMap(attributes);
    }

    /**
     * {@inheritDoc}
     */
    public String getID() {
        return attributes.get("id").toString();
    }

    /**
     * {@inheritDoc}
     */
    public String getUserName() {
        return attributes.get("username").toString();
    }

    /**
     * {@inheritDoc}
     */
    public String geFullName() {
        // this is not a mandatory attribute so we need to check for nulls
        Object res = attributes.get("name");
        return res == null ? null : res.toString();
    }

    /**
     * Retrieves the user attributes using the given session. If the given used ID is <code>null</code>
     * then the current authenticated user for the given session is retrieved, otherwise the user with the
     * given ID is fetched.
     * 
     * @param session the session to use to fetch the user data
     * @param userID the ID of the user to retrieve, or 
     * 
     * @return the attributes of the requested user <code>null</code> to fetch the data for the authenticated
     * user in the given session
     * 
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws GitAuthenticationException if authentication fails
     * @throws NullPointerException if the given session is <code>null</code>
     */
    private static Map<String, Object> getUserAttributes(GitLabSession session, String userID)
        throws GitTransportException, GitAuthenticationException {
        //  See: https://docs.gitlab.com/ee/api/users.html#list-current-user-for-normal-users
        URI uri = userID == null ? session.getService().newRequestURI("/user") : session.getService().newRequestURI("/users/"+userID);
        Logger logger = LoggerFactory.getLogger(GitLabUser.class);

        logger.debug(String.format("HTTP request %s %s", "GET", uri));

        HttpResponse<String> response = null;
        try {
            response = session.getService().newClient().send(session.getRequestBuilder().uri(uri).GET().build(), BodyHandlers.ofString());
        }
        catch (IOException | InterruptedException e) {
            throw new GitTransportException(e);
        }

        logger.debug(String.format("Request to %s returned a response code %d", uri, response.statusCode()));
        logger.trace(response.body());

        if (response.statusCode() != 200) {
            logger.error(String.format("Request failed with result code %d", response.statusCode()));
            throw new GitTransportException(String.format("Request returned a status code %d", response.statusCode()));
        }

        try {
            ObjectMapper mapper = new ObjectMapper();
            JsonNode rootNode = mapper.readTree(response.body());
            if (rootNode == null)
                throw new GitTransportException("Unmarshalling JSON content returned a null object");

            return toAttributeMap(rootNode);
        }
        catch (JsonProcessingException jpe) {
            throw new GitTransportException("An error occurred while unmarshalling JSON response", jpe);
        }
    }

    /**
     * {@inheritDoc}
     */
    public synchronized void update()
        throws GitTransportException, GitAuthenticationException
    {
        Map<String, Object> userAttributes = getUserAttributes(getSession(), getID());
        attributes.clear();
        attributes.putAll(userAttributes);
    }

    /**
     * Retrieves the authenticated user for the given session.
     * 
     * @param session the session to fetch the authenticated user for
     * 
     * @return the authenticated user for the given session
     * 
     * @throws GitTransportException if a transport related error occurs while communicating with the server
     * @throws GitAuthenticationException if authentication fails
     * @throws NullPointerException if the given session is <code>null</code>
     */
    public static GitLabUser getAuthenticatedUser(GitLabSession session)
        throws GitTransportException, GitAuthenticationException {
        return new GitLabUser(session, getUserAttributes(session, null));
    }
}