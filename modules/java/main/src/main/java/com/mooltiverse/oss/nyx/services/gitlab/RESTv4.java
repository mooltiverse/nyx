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

import java.io.File;
import java.io.IOException;

import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;

import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpClient.Version;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.SecurityException;

/**
 * The REST API v4 implementation as per <a href="https://docs.gitlab.com/ee/api/api_resources.html">GitLab API</a>.
 */
class RESTv4 extends API {
    /**
     * The suggested content type to set in the 'content-type' header, as per <a href="https://docs.gitlab.com/ee/api/">GitHub API</a>.
     */
    private static final String CONTENT_TYPE = "application/json";

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
     * The default API base URL, as per <a href="https://docs.gitlab.com/ee/api/">GitLab API</a>.
     * This value can be overridden by passing an entry with the {@link #BASE_URI_OPTION_NAME} key
     * in the options map used to get an instance of this service.
     * Value: {@value}
     */
    public static final String BASE_URI_DEFAULT_VALUE = "https://gitlab.com/api/v4";

    /**
     * Builds this object using the given uri to use as the remote endpoint.
     * 
     * @param uri the remote API endpoint. It can't be {@code null}
     * @param authenticationToken the token (Personal Access Token, OAuth) to use for authentication. It may be {@code null}.
     * If {@code null} the service will not be able to authenticate and perform any of the authentication protected operations.
     */
    RESTv4(URI uri, String authenticationToken) {
        super(uri);
        this.authenticationToken = authenticationToken;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> createRepository(String name, String description, boolean restricted, boolean initialize)
        throws TransportException, SecurityException {
        // See: https://docs.gitlab.com/ee/api/projects.html#create-project
        Objects.requireNonNull(name, "The name of the repository to create cannot be null");
        URI uri = newRequestURI("/projects");

        Map<String,Object> requestParameters = new HashMap<String,Object>();
        requestParameters.put("name", name);
        requestParameters.put("description", description);
        requestParameters.put("visibility", restricted ? "private" : "public");
        requestParameters.put("initialize_with_readme", initialize);

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).POST(BodyPublishers.ofString(new ObjectMapper().writeValueAsString(requestParameters))).build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        // See: https://docs.gitlab.com/ee/api/#status-codes
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
        // See: https://docs.gitlab.com/ee/api/projects.html#delete-project
        Objects.requireNonNull(name, "The name of the repository to delete cannot be null");
        URI uri = newRequestURI("/projects/"+URLEncoder.encode(name, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).DELETE().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        // See: https://docs.gitlab.com/ee/api/#status-codes
        if (response.statusCode() != 202 && response.statusCode() != 204) {
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
        // See: https://docs.gitlab.com/ee/api/releases/#get-a-release-by-a-tag-name
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(tag, "The release tag cannot be null");
        URI uri = newRequestURI("/projects/"+URLEncoder.encode(owner+"/"+repository, StandardCharsets.UTF_8)+"/releases/"+URLEncoder.encode(tag, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).GET().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        // See: https://docs.gitlab.com/ee/api/#status-codes
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
        // See https://docs.gitlab.com/ee/api/
        HttpRequest.Builder builder = HttpRequest.newBuilder()
            .version(Version.HTTP_1_1)
            .uri(uri)
            .setHeader("content-type", CONTENT_TYPE);

        if (authenticate) {
            if (Objects.isNull(authenticationToken))
                throw new SecurityException("No authentication token provided");
            builder.setHeader("Authorization", "Bearer "+authenticationToken); // Set the authentication token
        }

        return builder.copy();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Map<String, Object> getUserAttributes(String userID)
        throws TransportException, SecurityException {
        //  See: https://docs.gitlab.com/ee/api/users.html
        URI uri = userID == null ? newRequestURI("/user") : newRequestURI("/users/"+URLEncoder.encode(userID, StandardCharsets.UTF_8));

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).GET().build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        // See: https://docs.gitlab.com/ee/api/#status-codes
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
    Set<Attachment> listReleaseAssets(String owner, String repository, String tag)
        throws SecurityException, TransportException {
        // See: https://docs.gitlab.com/ee/api/releases/links.html#get-links
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(tag, "The release ID cannot be null");
        URI uri = newRequestURI("/projects/"+URLEncoder.encode(owner+"/"+repository, StandardCharsets.UTF_8)+"/releases/"+URLEncoder.encode(tag, StandardCharsets.UTF_8)+"/assets/links");

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
            result.add(new Attachment(releaseAssetMap.get("name").toString(), "" /* the description is not available by this API */, "" /* the content type is not available by this API */, releaseAssetMap.get("url").toString()));
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
        // See: https://docs.gitlab.com/ee/api/releases/#create-a-release
        Objects.requireNonNull(owner, "The release repository owner cannot be null");
        Objects.requireNonNull(repository, "The release repository name cannot be null");
        Objects.requireNonNull(tag, "The release tag cannot be null");
        URI uri = newRequestURI("/projects/"+URLEncoder.encode(owner+"/"+repository, StandardCharsets.UTF_8)+"/releases");

        Map<String,Object> requestParameters = new HashMap<String,Object>();
        if (!Objects.isNull(title))
            requestParameters.put("name", title);
        requestParameters.put("tag_name", tag);
        if (!Objects.isNull(description))
            requestParameters.put("description", description);

        HttpResponse<String> response = null;
        try {
            response = sendRequest(getRequestBuilder(true).uri(uri).POST(BodyPublishers.ofString(new ObjectMapper().writeValueAsString(requestParameters))).build());
        }
        catch (IOException | InterruptedException e) {
            throw new TransportException(e);
        }

        // See: https://docs.gitlab.com/ee/api/#status-codes
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
    Set<Attachment> publishReleaseAssets(String owner, String repository, String version, Set<Attachment> assets)
        throws SecurityException, TransportException {
        if (Objects.isNull(assets) || assets.isEmpty())
            return assets;

        // The process here consists of 2 steps:
        // 1 - if the asset path is a local file, upload it to the package registry and get the returned URL from the API.
        //     If it's a URL skip this step and go to step 2
        // 2 - attach the link to the asset to the release

        // See: https://docs.gitlab.com/ee/user/packages/generic_packages/index.html
        Objects.requireNonNull(owner, "The assets repository owner cannot be null");
        Objects.requireNonNull(repository, "The assets repository name cannot be null");
        Objects.requireNonNull(version, "The assets version cannot be null");

        // step 1: upload local files to the package repository
        Set<Attachment> result = new HashSet<Attachment>();
        for (Attachment asset: assets) {
            File assetFile = new File(asset.getPath());
            if (assetFile.exists()) {
                logger.debug(SERVICE, "Uploading asset '{}' (description: '{}', type: '{}', path: '{}') to URL '{}'", asset.getName(), asset.getDescription(), asset.getType(), asset.getPath(), uri.toString());
                // See: https://docs.gitlab.com/ee/user/packages/generic_packages/index.html
                // we always upload to the 'generic' package registry here
                URI uri = newRequestURI("/projects/"+URLEncoder.encode(owner+"/"+repository, StandardCharsets.UTF_8)+"/packages/generic/"+URLEncoder.encode(asset.getDescription(), StandardCharsets.UTF_8)+"/"+URLEncoder.encode(version, StandardCharsets.UTF_8)+"/"+URLEncoder.encode(asset.getName(), StandardCharsets.UTF_8)+"?select=package_file");

                HttpResponse<String> response = null;
                try {
                    response = sendRequest(getRequestBuilder(true).uri(uri).PUT(BodyPublishers.ofFile(Paths.get(assetFile.getPath()))).build());
                }
                catch (IOException | InterruptedException e) {
                    throw new TransportException(e);
                }
        
                // See: https://docs.gitlab.com/ee/api/#status-codes
                if ((response.statusCode() != 200) && (response.statusCode() != 201)) {
                    if (response.statusCode() == 401 || response.statusCode() == 403)
                        throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
                    else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
                }

                String assetURL = unmarshalJSONBodyElement(response.body(), "file").get("url").toString();
                logger.debug(SERVICE, "Asset '{}' (type: '{}', path: '{}') has been uploaded and is available to URL '{}'", asset.getName(), asset.getType(), asset.getPath(), assetURL);
                result.add(new Attachment(asset.getName(), asset.getDescription(), asset.getType(), assetURL));
            }
            else 
            {
                logger.debug(SERVICE, "The path '{}' for the asset '{}' cannot be resolved to a local file", asset.getPath(), asset.getName());

                try {
                    // just check if it's a valid URL and if it is add it to the result assets
                    new URL(asset.getPath());
                    result.add(asset);
                }
                catch (MalformedURLException mue) {
                    logger.warn(SERVICE, "The path '{}' for the asset '{}' cannot be resolved to a local file and is not a valid URL and will be skipped", asset.getPath(), asset.getName());
                }
            }
        }

        // step 2: upload asset links to the release
        for (Attachment asset: result) {
            logger.debug(SERVICE, "Updating release '{}' with asset '{}' (description: '{}', type: '{}', path: '{}') to URL '{}'", version, asset.getName(), asset.getDescription(), asset.getType(), asset.getPath(), uri.toString());
            // See: https://docs.gitlab.com/ee/user/packages/generic_packages/index.html
            // we always upload to the 'generic' package registry here
            URI uri = newRequestURI("/projects/"+URLEncoder.encode(owner+"/"+repository, StandardCharsets.UTF_8)+"/releases/"+URLEncoder.encode(version, StandardCharsets.UTF_8)+"/assets/links");

            Map<String,Object> requestParameters = new HashMap<String,Object>();
            //requestParameters.put("name", asset.getName());
            requestParameters.put("name", asset.getName()); // maybe the description could be used here?
            requestParameters.put("url", asset.getPath());

            HttpResponse<String> response = null;
            try {
                response = sendRequest(getRequestBuilder(true).uri(uri).POST(BodyPublishers.ofString(new ObjectMapper().writeValueAsString(requestParameters))).build());
            }
            catch (IOException | InterruptedException e) {
                throw new TransportException(e);
            }
    
            // See: https://docs.gitlab.com/ee/api/#status-codes
            if (response.statusCode() != 201) {
                if (response.statusCode() == 401 || response.statusCode() == 403)
                    throw new SecurityException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
                else throw new TransportException(String.format("Request returned a status code '%d': %s", response.statusCode(), response.body()));
            }

            logger.debug(SERVICE, "Asset '{}' (type: '{}', path: '{}') has been uploaded to release '{}'", asset.getName(), asset.getType(), asset.getPath(), version);
        }

        logger.debug(SERVICE, "Uploaded '{}' assets", result.size());

        return result;
    }
}
