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

import com.mooltiverse.oss.nyx.services.GitAuthenticationException;
import com.mooltiverse.oss.nyx.services.GitService;
import com.mooltiverse.oss.nyx.services.GitServiceFeature;
import com.mooltiverse.oss.nyx.services.GitTransportException;

/**
 * The entry point to the <a href="https://gitlab.com/">GitLab</a> service.
 */
public class GitLab implements GitService {
    /**
     * The API base URL, as per <a href="https://docs.gitlab.com/ee/api/README.html">GitLab API</a>
     */
    public static final String API_BASE_URL = "https://gitlab.com/api/v4";
    
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
    private GitLab() {
        super();
        apiBaseURI = null;
    }

    /**
     * Builds an instance pointing at the given API URI.
     * 
     * @param uri the URI to use as the base for API invocations
     * 
     * @throws GitTransportException if the given URI can't be reached or does not expose valid APIs
     * @throws NullPointerException if the given URI is {@code null}
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty, malformed etc)
     */
    private GitLab(URI uri)
        throws GitTransportException {
        super();
        Objects.requireNonNull(uri, "Can't create a new instance with a null URI");
        if (uri.toString().isBlank())
            throw new IllegalArgumentException("Can't create a new instance with an empty URI");
        apiBaseURI = uri;

        // Since ping requests are not authenticated they may incur into errors due to rate limits so we need to avoid it here
        //if (!ping())
        //    throw new IOException(String.format("Ping failed using the given URI '%s'", uri));
    }

    /**
     * Returns an instance using the default API URI ({@link #API_BASE_URL}).
     * 
     * @return an instance using the default API URI.
     * 
     * @throws GitTransportException if the default URI can't be reached or does not expose valid APIs
     */
    public static GitLab instance()
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
     * @throws NullPointerException if the given URI is {@code null}
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty or malformed)
     */
    public static GitLab instance(String apiURI)
        throws GitTransportException {
        Objects.requireNonNull(apiURI, "Can't create a new instance with a null URI");
        try {
            return new GitLab(new URI(apiURI));
        }
        catch (URISyntaxException use) {
            throw new IllegalArgumentException(String.format("The given URI '%s' is not a valid URI", apiURI), use);
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
     * @throws NullPointerException if the given URI is {@code null}
     * @throws IllegalArgumentException if the given URI is illegal (i.e. empty or malformed)
     */
    public static GitLab instance(URI apiURI)
        throws GitTransportException {
        return new GitLab(apiURI);
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
        // GitLab does not expose any URL to be invoked without authentication so it will always return an
        // error message. Let's just check if the error message is what we expect
        URI uri = getBaseURI();
        logger.debug(String.format("HTTP request '%s' '%s'", "GET", uri));
        try {
            HttpResponse<String> response = newClient().send(getRequestBuilder().uri(uri).GET().build(), BodyHandlers.ofString());

            logger.debug(String.format("Request to '%s' returned a response code '%d'", uri, response.statusCode()));
            logger.trace(response.body());

            // Expected response is 404 but in case future releases answer with a 200 let's foresee that
            // This is quite a lose check but let's hope GitLab gives an health check path in the future
            if (!((response.statusCode() >= 200) && (response.statusCode() < 300)) && !((response.statusCode() >= 400) && (response.statusCode() < 500))) {
                logger.error(String.format("Ping failed with result code '%d'", response.statusCode()));
                return false;
            }
        }
        catch (IOException | InterruptedException | IllegalArgumentException | SecurityException e) {
            logger.error(String.format("HTTP request '%s' '%s' has thrown an exception", "GET", uri), e);
            return false;
        }

        logger.debug("Ping succeeded");
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public GitLabSession authenticateWithToken(String token)
        throws GitTransportException, GitAuthenticationException {
        return GitLabSession.authenticateWithToken(this, token);
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
            // See https://docs.gitlab.com/ee/api/README.html
            requestBuilderTemplate = HttpRequest.newBuilder()
                .version(Version.HTTP_1_1)
                .uri(getBaseURI());
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