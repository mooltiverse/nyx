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

import static com.mooltiverse.oss.nyx.log.Markers.SERVICE;
import static com.mooltiverse.oss.nyx.services.github.GitHub.logger;

import java.io.File;
import java.io.IOException;

import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;

import java.net.URI;
import java.net.URLEncoder;

import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpClient.Version;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;

import java.util.List;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.SecurityException;

/**
 * The REST API v3 implementation as per <a href="https://docs.github.com/en/rest">GitHub API</a>.
 */
class RESTv3 extends API {
    /**
     * The suggested content type to set in the 'Accept' header, as per <a href="https://docs.github.com/en/rest/overview/media-types">GitHub API</a>.
     */
    private static final String API_ACCEPT_CONTENT_TYPE = "application/vnd.github.v3+json";

    /**
     * The security token to use to authenticate to the remote service.
     * If {@code null} or invalid all authentication protected operations will fail with a
     * {@link SecurityException}
     */
    private final String authenticationToken;

    /**
     * The instance of a standard client builder with all reusable attributes set. Each client can
     * be created from a copy of this builder.
     */
    protected HttpClient.Builder clientBuilderTemplate = null;

    /**
     * The default API base URL, as per <a href="https://docs.github.com/en/rest/overview/resources-in-the-rest-api">GitHub API</a>.
     * This value can be overridden by passing an entry with the {@link #BASE_URI_OPTION_NAME} key
     * in the options map used to get an instance of this service.
     * Value: {@value}
     */
    public static final String BASE_URI_DEFAULT_VALUE = "https://api.github.com";

    /**
     * Builds this object using the given uri to use as the remote endpoint.
     * 
     * @param uri the remote API endpoint. It can't be {@code null}
     * @param authenticationToken the token (Personal Access Token, OAuth) to use for authentication. It may be {@code null}.
     * If {@code null} the service will not be able to authenticate and perform any of the authentication protected operations.
     */
    RESTv3(URI uri, String authenticationToken) {
        super(uri);
        this.authenticationToken = authenticationToken;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> createRepository(String name, String description, boolean restricted, boolean initialize)
        throws TransportException, SecurityException {
        // See: https://docs.github.com/en/rest/reference/repos#create-a-repository-for-the-authenticated-user
        Objects.requireNonNull(name, "The name of the repository to create cannot be null");
        URI uri = newRequestURI("/user/repos");

        Map<String,Object> requestParameters = new HashMap<String,Object>();
        requestParameters.put("name", name);
        requestParameters.put("description", description);
        requestParameters.put("private", restricted);
        requestParameters.put("auto_init", initialize);

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).POST(BodyPublishers.ofString(new ObjectMapper().writeValueAsString(requestParameters))).build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 201) {
            if (response.statusCode() == 401 || response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }

        return unmarshalJSONBody(response.body());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void deleteRepository(String name)
        throws TransportException, SecurityException {
        // See: https://docs.github.com/en/rest/reference/repos#delete-a-repository
        Objects.requireNonNull(name, "The name of the repository to delete cannot be null");
        // we need the owner, so let's fetch the current user ID
        String currentUserID = getUserAttributes(null).get("login").toString();
        URI uri = newRequestURI("/repos/"+URLEncoder.encode(currentUserID, StandardCharsets.UTF_8)+"/"+URLEncoder.encode(name, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).DELETE().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 204) {
            if (response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized HttpClient.Builder getClientBuilder() {
        if (clientBuilderTemplate == null) {
            clientBuilderTemplate = HttpClient.newBuilder().version(Version.HTTP_1_1).followRedirects(Redirect.NORMAL);
        }
        return clientBuilderTemplate;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> getReleaseByTag(String owner, String repository, String tag)
        throws SecurityException, TransportException {
        // See: https://docs.github.com/en/rest/reference/repos#get-a-release
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(tag, "The release tag cannot be null");
        URI uri = newRequestURI("/repos/"+URLEncoder.encode(owner, StandardCharsets.UTF_8)+"/"+URLEncoder.encode(repository, StandardCharsets.UTF_8)+"/releases/tags/"+URLEncoder.encode(tag, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).GET().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 200) {
            if (response.statusCode() == 404)
                return null; // the object just doesn't exist
            else if (response.statusCode() == 401 || response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }

        return unmarshalJSONBody(response.body());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    synchronized HttpRequest.Builder getRequestBuilder(boolean authenticate)
        throws SecurityException {
        // See https://docs.github.com/en/rest/overview/resources-in-the-rest-api for details
        HttpRequest.Builder builder = HttpRequest.newBuilder()
            .version(Version.HTTP_1_1)
            .uri(uri)
            .setHeader("Accept", API_ACCEPT_CONTENT_TYPE);

        if (authenticate) {
            if (Objects.isNull(authenticationToken))
                throw new SecurityException("No authentication token provided");
            builder.setHeader("Authorization", "token "+authenticationToken); // Set the authentication token
        }

        return builder.copy();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> getUserAttributes(String userID)
        throws TransportException, SecurityException {
        // See:
        // - https://docs.github.com/en/rest/reference/users#get-the-authenticated-user
        // - https://docs.github.com/en/rest/reference/users#get-a-user
        URI uri = userID == null ? newRequestURI("/user") : newRequestURI("/users/"+URLEncoder.encode(userID, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).GET().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 200) {
            if (response.statusCode() == 404)
                return null; // no such user
            else if (response.statusCode() == 401 || response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }

        return unmarshalJSONBody(response.body());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Set<Attachment> listReleaseAssets(String owner, String repository, String id)
        throws SecurityException, TransportException {
        // See: https://docs.github.com/en/rest/releases/assets#list-release-assets
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(id, "The release ID cannot be null");
        URI uri = newRequestURI("/repos/"+URLEncoder.encode(owner, StandardCharsets.UTF_8)+"/"+URLEncoder.encode(repository, StandardCharsets.UTF_8)+"/releases/"+URLEncoder.encode(id, StandardCharsets.UTF_8)+"/assets");

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).GET().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 200) {
            if (response.statusCode() == 404)
                return null; // the object just doesn't exist
            else if (response.statusCode() == 401 || response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }

        List<Map<String, Object>> releaseAssets = unmarshalJSONBodyAsCollection(response.body());
        if (Objects.isNull(releaseAssets))
            return null;
        Set<Attachment> result = new HashSet<Attachment>();
        for (Map<String, Object> releaseAssetMap: releaseAssets) {
            result.add(new Attachment(releaseAssetMap.get("name").toString(), releaseAssetMap.get("label").toString(), releaseAssetMap.get("content_type").toString(), releaseAssetMap.get("url").toString()));
        }
        return result;
    }

    /**
     * Returns a new URI by appending the given path to the service URI
     * 
     * @param path the path to append to the service URI
     * 
     * @return the new URI
     */
    private URI newRequestURI(String path) {
        Objects.requireNonNull(path, "Can't build a URI with a null path");
        if (path.isBlank())
            return uri;

        String baseURI = uri.toString();
        // Avoid having a double //
        if (baseURI.endsWith("/") && path.startsWith("/"))
            path = path.substring(1);
        return URI.create(baseURI.concat(path));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> publishRelease(String owner, String repository, String title, String tag, String description)
        throws SecurityException, TransportException {
        // See: https://docs.github.com/en/rest/reference/repos#create-a-release
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(tag, "The release tag cannot be null");
        URI uri = newRequestURI("/repos/"+URLEncoder.encode(owner, StandardCharsets.UTF_8)+"/"+URLEncoder.encode(repository, StandardCharsets.UTF_8)+"/releases");

        Map<String,Object> requestParameters = new HashMap<String,Object>();
        if (!Objects.isNull(title))
            requestParameters.put("name", title);
        requestParameters.put("tag_name", tag);
        if (!Objects.isNull(description))
            requestParameters.put("body", description);

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).POST(BodyPublishers.ofString(new ObjectMapper().writeValueAsString(requestParameters))).build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        if (response.statusCode() != 201) {
            if (response.statusCode() == 401 || response.statusCode() == 403)
                throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
        }

        return unmarshalJSONBody(response.body());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Set<Attachment> publishReleaseAssets(String owner, String repository, String uploadURL, Set<Attachment> assets)
        throws SecurityException, TransportException {
        if (Objects.isNull(assets) || assets.isEmpty())
            return assets;
        Objects.requireNonNull(uploadURL, "The upload_url cannot be null");

        // As per https://docs.github.com/en/rest/releases/assets, the upload URL comes with the "{?name,label}", which we need to remove before adding query parameters
        uploadURL = uploadURL.replace("{?name,label}", "");
        logger.debug(SERVICE, "Uploading '{}' assets to base URL '{}'", assets.size(), uploadURL);

        Set<Attachment> result = new HashSet<Attachment>();
        for (Attachment asset: assets) {
            File assetFile = new File(asset.getPath());
            if (assetFile.exists()) {
                logger.debug(SERVICE, "Uploading asset '{}' (description: '{}', type: '{}', path: '{}') to URL '{}'", asset.getName(), asset.getDescription(), asset.getType(), asset.getPath(), uri.toString());
                // See: https://docs.github.com/en/rest/releases/assets
                URI uri = null;
                try {
                    uri = URI.create(uploadURL+"?name="+URLEncoder.encode(asset.getName(), StandardCharsets.UTF_8)+"&"+"label="+URLEncoder.encode(asset.getDescription(), StandardCharsets.UTF_8));
                }
                catch (IllegalArgumentException iae) {
                    throw new TransportException(String.format("The '%s' attribute '%s' returned by the release doesn't seem to be a valid URI", "upload_url", uploadURL), iae);
                }

                HttpResponse<String> response = null;
                try {
                    response = sendRequest(getRequestBuilder(true).setHeader("Content-Type", asset.getType()).uri(uri).POST(BodyPublishers.ofFile(Paths.get(assetFile.getPath()))).build());
                }
                catch (IOException | InterruptedException e) {
                    throw new TransportException(e);
                }
        
                if (response.statusCode() != 201) {
                    throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
                }

                String assetURL = unmarshalJSONBody(response.body()).get("url").toString();
                logger.debug(SERVICE, "Asset '{}' (type: '{}', path: '{}') has been uploaded and is available to URL '{}'", asset.getName(), asset.getType(), asset.getPath(), assetURL);
                result.add(new Attachment(asset.getName(), asset.getDescription(), asset.getType(), assetURL));
            }
            else 
            {
                logger.warn(SERVICE, "The path '{}' for the asset '{}' cannot be resolved to a local file and will be skipped", asset.getPath(), asset.getName());
            }
        }

        logger.debug(SERVICE, "Uploaded '{}' assets", result.size());

        return result;
    }
}
