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
package com.mooltiverse.oss.nyx.git.hosted.services.github;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpClient.Version;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.git.hosted.services.GitAuthenticationException;
import com.mooltiverse.oss.nyx.git.hosted.services.GitService;
import com.mooltiverse.oss.nyx.git.hosted.services.GitServiceFeature;
import com.mooltiverse.oss.nyx.git.hosted.services.GitTransportException;

/**
 * The entry point to the <a href="https://github.com/">GitHub</a> service.
 */
public class GitHub implements GitService {
    /**
     * The API base URL, as per <a href="https://developer.github.com/v3/">GitHub API</a>
     */
    public static final String API_BASE_URL = "https://api.github.com";

    /**
     * The suggested content type to set in the 'Accept' header, as per https://developer.github.com/v3/
     */
    private static final String API_ACCEPT_CONTENT_TYPE = "application/vnd.github.v3+json";

    /**
     * The URI to the API.
     */
    private final URI apiBaseURI;

    /**
     * The instance of a standard client builder with all reusable attributes set. Each client can
     * be created from a copy of this builder.
     */
    private HttpClient.Builder clientBuilderTemplate = null;

    /**
     * The instance of a standard request builder with all reusable attributes set. Each request can
     * be created from a copy of this builder.
     */
    private HttpRequest.Builder requestBuilderTemplate = null;

    /**
     * The private logger instance
     */
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    /**
     * Default constructor is hidden on purpose.
     */
    private GitHub() {
        super();
        apiBaseURI = null;
    }

    /**
     * Builds an instance pointing at the given API URI.
     * 
     * @param uri the URI to use as the base for API invocations
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI is <code>null</code>
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty, malformed etc)
     */
    private GitHub(URI uri) 
        throws GitTransportException {
        super();
        Objects.requireNonNull(uri, "Can't create a new instance with a null URI");
        if (uri.toString().isBlank())
            throw new IllegalArgumentException("Can't create a new instance with an empty URI");
        apiBaseURI = uri;
        
        // Since ping requests are not authenticated they may incur into errors due to rate limits so we need to avoid it here
        //if (!ping())
        //    throw new IOException(String.format("Ping failed using the given URI %s", uri));
    }

    /**
     * Returns an instance using the default API URI ({@link #API_BASE_URL}).
     * 
     * @throws GitTransportException if the default URI can't be reached or does not expose valid APIs
     * @return an instance using the default API URI.
     */
    public static GitHub instance()
        throws GitTransportException {
        return instance(API_BASE_URL);
    }

    /**
     * Returns an instance using the given API URI.
     * 
     * @param apiURI the API URI, which is usually the endpoint to a private or on premises installation
     * 
     * @return an instance using the given API URI
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI is <code>null</code>
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty or malformed)
     */
    public static GitHub instance(String apiURI)
        throws GitTransportException {
        Objects.requireNonNull(apiURI, "Can't create a new instance with a null URI");
        try {
            return new GitHub(new URI(apiURI));
        }
        catch (URISyntaxException use) {
            throw new IllegalArgumentException(String.format("The given URI %s is not a valid URI", apiURI), use);
        }
    }

    /**
     * Returns an instance using the given API URI.
     * 
     * @param apiURI the API URI, which is usually the endpoint to a private or on premises installation
     * 
     * @return an instance using the given API URI
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI is <code>null</code>
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty or malformed)
     */
    public static GitHub instance(URI apiURI)
        throws GitTransportException {
        return new GitHub(apiURI);
    }

    /**
     * {@inheritDoc}
     */
    public boolean supports(GitServiceFeature feature) {
        return FeatureSupport.supports(feature);
    }

    /**
     * {@inheritDoc}
     */
    public URI getBaseURI() {
        return apiBaseURI;
    }

    /**
     * {@inheritDoc}
     */
    public boolean ping() {
        URI uri = getBaseURI();
        logger.debug(String.format("HTTP request %s %s", "GET", uri));
        try {
            HttpResponse<String> response = newClient().send(getRequestBuilder().uri(uri).GET().build(), BodyHandlers.ofString());
            
            logger.debug(String.format("Request to %s returned a response code %d", uri, response.statusCode()));
            logger.trace(response.body());

            if (response.statusCode() != 200) {
                logger.error(String.format("Ping failed with result code %d", response.statusCode()));
                return false;
            }
        }
        catch (IOException | InterruptedException | IllegalArgumentException | SecurityException e) {
            logger.error(String.format("HTTP request %s %s has thrown an exception", "GET", uri), e);
            return false;
        }

        logger.debug("Ping succeeded");
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public GitHubSession authenticateWithToken(String token)
        throws GitTransportException, GitAuthenticationException {
        return GitHubSession.authenticateWithToken(this, token);
    }

    /**
     * Returns an HTTP client builder with all reusable attributes set. The returned object is not aware of any authentication
     * method.<br>
     * After retrieving the client builder with this method you can set custom properties if you need to and then get a new client
     * instance with {@link HttpClient.Builder#build()}.
     * 
     * @return an HTTP client builder with all reusable attributes set.
     */
    synchronized HttpClient.Builder getClientBuilder() {
        if (clientBuilderTemplate == null) {
            clientBuilderTemplate = HttpClient.newBuilder().version(Version.HTTP_1_1).followRedirects(Redirect.NORMAL);
        }
        return clientBuilderTemplate;
    }

    /**
     * Returns an HTTP client with all reusable attributes set. The returned object is not aware of any authentication method.<br>
     * 
     * @return an HTTP client with all reusable attributes set.
     */
    HttpClient newClient() {
        return getClientBuilder().build();
    }

    /**
     * Returns an HTTP request builder with all reusable attributes set. The returned object is not aware of any authentication
     * method.<br>
     * After retrieving the request builder with this method you can set custom properties if you need to and then get a new request
     * instance with {@link HttpRequest.Builder#build()}.
     * 
     * @return an HTTP request builder with all reusable attributes set.
     */
    synchronized HttpRequest.Builder getRequestBuilder() {
        if (requestBuilderTemplate == null) {
            // See https://developer.github.com/v3/ for details
            requestBuilderTemplate = HttpRequest.newBuilder()
                .version(Version.HTTP_1_1)
                .uri(getBaseURI())
                .setHeader("Accept", API_ACCEPT_CONTENT_TYPE);
        }
        return requestBuilderTemplate.copy();
    }

    /**
     * Returns a new URI by appending the given path to the service URI
     * 
     * @param path the path to append to the service URI
     * 
     * @return the new URI
     */
    URI newRequestURI(String path) {
        Objects.requireNonNull(path, "Can't build a URI with a null path");
        if (path.isBlank())
            return getBaseURI();

        String baseURI = getBaseURI().toString();
        // Avoid having a double //
        if (baseURI.endsWith("/") && path.startsWith("/"))
            path = path.substring(1);
        return URI.create(baseURI+path);
    }
}