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
package com.mooltiverse.oss.nyx.services.gitlab;

import static com.mooltiverse.oss.nyx.log.Markers.SERVICE;
import static com.mooltiverse.oss.nyx.services.gitlab.GitLab.logger;

import java.io.IOException;

import java.net.URI;

import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.SecurityException;

/**
 * A superclass for various API versions.
 */
abstract class API {
    /**
     * The instance of the remote base URI.
     */
    protected final URI uri;

    /**
     * Builds this object using the given uri to use as the remote endpoint.
     * 
     * @param uri the remote API endpoint. It can't be {@code null}
     */
    protected API(URI uri) {
        super();
        Objects.requireNonNull(uri, "API base URI cannot be null");
        this.uri = uri;
    }

    /**
     * Creates a new repository for the currently authenticated user.
     * <br>
     * Please note that if the service has been configured with repository owner and name those attributes are ignored
     * by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
     * name is always the {@code name} attribute.
     * 
     * @param name the repository name. Cannot be {@code null}
     * @param description the repository description. It may be {@code null}
     * @param restricted when {@code true} the repository will have private visibility, otherwise it will be public
     * @param initialize when {@code true} the repository is also initialized with a default README file
     * 
     * @return the attributes of the new repository. Never {@code null}
     * 
     * @throws TransportException if a transport related error occurs while communicating with the server
     * @throws SecurityException if authentication fails
     */
    abstract Map<String, Object> createRepository(String name, String description, boolean restricted, boolean initialize)
        throws TransportException, SecurityException;

    /**
     * Deletes a repository for the currently authenticated user.
     * <br>
     * Please note that if the service has been configured with repository owner and name those attributes are ignored
     * by this method as the owner is always the authenticated user (the one owning the configured credentials) and the
     * name is always the {@code name} attribute.
     * 
     * @param name the repository name. Cannot be {@code null}
     * 
     * @throws TransportException if a transport related error occurs while communicating with the server
     * @throws SecurityException if authentication fails
     */
    abstract void deleteRepository(String name)
        throws TransportException, SecurityException;

    /**
     * Returns an HTTP client builder with all reusable attributes set. The returned object is not aware of any authentication
     * method.<br>
     * After retrieving the client builder with this method you can set custom properties if you need to and then get a new client
     * instance with {@link HttpClient.Builder#build()}.
     * 
     * @return an HTTP client builder with all reusable attributes set.
     */
    abstract HttpClient.Builder getClientBuilder();

    /**
     * Finds the release in the given repository by the release tag.
     * 
     * @param owner the name of the owner of the repository to get the release for. It can't be {@code null}
     * @param repository the name of the repository to get the release for. It can't be {@code null}
     * @param tag the release tag (i.e. {@code 1.2.3}, {@code v4.5.6}). It can't be {@code null}
     * 
     * @return the attributes of the requested release, if available, or {@code null} otherwise
     * 
     * @throws SecurityException if authentication or authorization fails
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#RELEASES} feature.
     */
    abstract Map<String, Object> getReleaseByTag(String owner, String repository, String tag)
        throws SecurityException, TransportException;

    /**
     * Returns an HTTP request builder with all reusable attributes set.<br>
     * After retrieving the request builder with this method you can set custom properties if you need to and then get a new request
     * instance with {@link HttpRequest.Builder#build()}.
     * 
     * @param authenticate if {@code true} the returned object will be able to send authenticated requests
     * 
     * @return an HTTP request builder with all reusable attributes set.
     * 
     * @throws SecurityException if {@code authenticate} is {@code true} and authentication fails
     */
    abstract HttpRequest.Builder getRequestBuilder(boolean authenticate)
        throws SecurityException;

    /**
     * Retrieves the attributes of the user with the given ID. If the given used ID is {@code null}
     * then the current authenticated user for the given session is retrieved, otherwise the user with the
     * given ID is fetched. The authenticated user is the one owning the configured credentials.
     * 
     * @param userID the ID of the user to retrieve the attributes for. When {@code null} the attributes
     * of the authenticated user are returned
     * 
     * @return the attributes of the requested user. When {@code userID} is {@code null} the result value
     * is never {@code null}, otherwise it may be {@code null} if no used with the given ID is available
     * 
     * @throws TransportException if a transport related error occurs while communicating with the server
     * @throws SecurityException if authentication fails
     */
    abstract Map<String, Object> getUserAttributes(String userID)
        throws TransportException, SecurityException;

    /**
     * Returns an HTTP client with all reusable attributes set. The returned object is not aware of any authentication method.<br>
     * 
     * @return an HTTP client with all reusable attributes set.
     */
    HttpClient newClient() {
        return getClientBuilder().build();
    }

    /**
     * Publishes a new release.
     * 
     * @param owner the name of the owner of the repository to create the release for. It can't be {@code null}
     * @param repository the name of the repository to create the release for. It can't be {@code null}
     * @param title the release title, it may be the same of {@code tag} but not necessarily. It may be {@code null}
     * @param tag tag to publish the release for (i.e. {@code 1.2.3}, {@code v4.5.6}). It can't be {@code null}
     * @param description the release description. This is usually a Markdown text containing release notes or a changelog
     * or something like that giving an overall description of the release
     * 
     * @return the attributes of the new release
     * 
     * @throws SecurityException if authentication or authorization fails
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#RELEASES} feature.
     */
    abstract Map<String, Object> publishRelease(String owner, String repository, String title, String tag, String description)
        throws SecurityException, TransportException;

    /**
     * Sends the given request and returns the response, logging as needed.
     * 
     * @param request the request to send
     * 
     * @return the response
     * 
     * @throws IOException if thrown by the underlying implementation
     * @throws InterruptedException if thrown by the underlying implementation
     * @throws IllegalArgumentException if thrown by the underlying implementation
     */
    protected HttpResponse<String> sendRequest(HttpRequest request)
        throws IOException, InterruptedException, IllegalArgumentException {
        logger.debug(SERVICE, "HTTP request: '{}' '{}'", request.method(), request.uri());
        logger.trace(SERVICE, "HTTP request headers: '{}'", request.headers().toString());
        HttpResponse<String> response = newClient().send(request, BodyHandlers.ofString());
        logger.debug(SERVICE, "HTTP response code: '{}'", response.statusCode());
        logger.trace(SERVICE, "HTTP response headers: '{}'", response.headers().toString());
        logger.trace(SERVICE, "HTTP response body:");
        logger.trace(SERVICE, response.body());
        return response;
    }

    /**
     * Parses the given string as a JSON object tree and returns each item in the resulting map.
     * 
     * @param body the JSON string to parse
     * 
     * @return the map of properties parsed from the given body
     * 
     * @throws TransportException in case unmarshalling fails
     */
    protected Map<String, Object> unmarshalJSONBody(String body)
        throws TransportException {
        try {
            JsonNode rootNode = new ObjectMapper().readTree(body);
            if (rootNode == null)
                throw new TransportException("Unmarshalling JSON content returned a null object");

            return GitLabEntity.toAttributeMap(rootNode);
        }
        catch (JsonProcessingException jpe) {
            throw new TransportException("An error occurred while unmarshalling JSON response", jpe);
        }
    }
}
