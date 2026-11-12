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

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.mooltiverse.oss.nyx.git.tools.HostedRepositoryPool;

/**
 * A pool of long-lived GitLab repositories (projects) for integration tests, used instead of creating (and deleting)
 * a new repository for every test, to avoid rate limits on content creation.
 * <br>
 * See {@link HostedRepositoryPool} for how repositories are reset and verified. On GitLab the reset also deletes all
 * releases and packages (release assets are uploaded as packages, which survive the deletion of releases), removes
 * branch and tag protections (GitLab protects the default branch on the first push, which prevents force pushes),
 * sets the default branch and the repository visibility.
 */
public final class GitLabRepositoryPool extends HostedRepositoryPool<Map<String,Object>, GitLabRepository> {
    /**
     * The private logger instance.
     */
    private static final Logger logger = LoggerFactory.getLogger(GitLabRepositoryPool.class);

    /**
     * The JSON mapper.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * The type reference used to unmarshal JSON objects.
     */
    private static final TypeReference<Map<String,Object>> OBJECT_TYPE = new TypeReference<Map<String,Object>>() {};

    /**
     * The type reference used to unmarshal JSON arrays of objects.
     */
    private static final TypeReference<List<Map<String,Object>>> ARRAY_TYPE = new TypeReference<List<Map<String,Object>>>() {};

    /**
     * The base URI of the GitLab REST API.
     */
    private final String baseURI = RESTv4.BASE_URI_DEFAULT_VALUE;

    /**
     * The HTTP client.
     */
    private final HttpClient client = HttpClient.newHttpClient();

    /**
     * The owner of the pooled repositories (the authenticated user), lazily initialized.
     */
    private String owner = null;

    /**
     * The authentication token.
     */
    private final String token;

    /**
     * Builds a pool scoped to the given environment.
     *
     * @param token the GitLab authentication token. It can't be {@code null} or blank.
     * @param environment the environment the pool is scoped to. It can't be {@code null} or blank. It's sanitized
     * so it can be used in repository names.
     */
    GitLabRepositoryPool(String token, String environment) {
        super(environment);
        Objects.requireNonNull(token, "The GitLab token can't be null");
        if (token.isBlank())
            throw new IllegalArgumentException("The GitLab token can't be blank");
        this.token = token;
    }

    /**
     * Returns a pool scoped to the {@link #defaultEnvironment() default environment}.
     *
     * @param token the GitLab authentication token. It can't be {@code null} or blank.
     *
     * @return the pool
     */
    public static GitLabRepositoryPool instance(String token) {
        return new GitLabRepositoryPool(token, defaultEnvironment());
    }

    /**
     * Returns a pool scoped to the {@link #defaultEnvironment() default environment} and the given scope, so that
     * different groups of tests (i.e. integration and functional tests) never share a repository, even if they
     * run at the same time.
     * 
     * @param token the GitLab authentication token. It can't be {@code null} or blank.
     * @param scope the scope, appended to the default environment (i.e. {@code functional}). It can't be
     * {@code null}.
     * 
     * @return the pool
     */
    public static GitLabRepositoryPool instance(String token, String scope) {
        return new GitLabRepositoryPool(token, scopedEnvironment(scope));
    }

    /**
     * Sends a request to the GitLab REST API.
     *
     * @param method the HTTP method
     * @param path the path, relative to the API base URI
     * @param body the optional JSON body, may be {@code null}
     *
     * @return the response
     *
     * @throws IOException if the request fails or returns an unexpected status code (anything other than 2xx, 3xx or
     * 404). Redirects are not followed.
     * @throws InterruptedException if the request is interrupted
     */
    private HttpResponse<String> send(String method, String path, Map<String,Object> body)
        throws IOException, InterruptedException {
        HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(baseURI.concat(path)))
            .setHeader("Authorization", "Bearer ".concat(token))
            .setHeader("Accept", "application/json")
            .method(method, Objects.isNull(body) ? HttpRequest.BodyPublishers.noBody() : HttpRequest.BodyPublishers.ofString(MAPPER.writeValueAsString(body)));
        if (!Objects.isNull(body))
            builder.setHeader("Content-Type", "application/json");
        HttpResponse<String> response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString());
        if ((response.statusCode() < 200 || response.statusCode() > 399) && response.statusCode() != 404)
            throw new IOException(String.format("GitLab request %s %s failed with status %d: %s", method, path, response.statusCode(), response.body()));
        return response;
    }

    /**
     * Returns all the items of a paginated collection.
     *
     * @param path the collection path, relative to the API base URI
     *
     * @return all the items of the collection
     *
     * @throws Exception if the collection can't be retrieved
     */
    private List<Map<String,Object>> list(String path)
        throws Exception {
        List<Map<String,Object>> res = new ArrayList<Map<String,Object>>();
        for (int page = 1; ; page++) {
            HttpResponse<String> response = send("GET", String.format("%s%sper_page=100&page=%d", path, path.contains("?") ? "&" : "?", page), null);
            if (response.statusCode() == 404)
                return res;
            List<Map<String,Object>> items = MAPPER.readValue(response.body(), ARRAY_TYPE);
            res.addAll(items);
            if (items.size() < 100)
                return res;
        }
    }

    /**
     * Returns the URL encoded value.
     *
     * @param value the value to encode
     *
     * @return the URL encoded value
     */
    private static String encode(String value) {
        return URLEncoder.encode(value, UTF_8);
    }

    /**
     * Returns the API path of the given project.
     *
     * @param project the project
     *
     * @return the API path of the given project
     */
    private static String projectPath(Map<String,Object> project) {
        return "/projects/".concat(Objects.toString(project.get("id")));
    }

    /**
     * Returns the owner of the pooled repositories, which is the authenticated user.
     *
     * @return the owner of the pooled repositories
     *
     * @throws Exception if the authenticated user can't be retrieved
     */
    private String getOwner()
        throws Exception {
        if (Objects.isNull(owner)) {
            HttpResponse<String> response = send("GET", "/user", null);
            if (response.statusCode() == 404)
                throw new IOException("Unable to retrieve the authenticated user");
            owner = Objects.toString(MAPPER.readValue(response.body(), OBJECT_TYPE).get("username"));
        }
        return owner;
    }

    /**
     * {@inheritDoc}
     * A redirect means that the project that had the given name was renamed, i.e. when it was marked for deletion
     * (delayed deletion), so there is no project with the given name and the name can be used for a new project.
     */
    @Override
    protected Map<String,Object> find(String name)
        throws Exception {
        HttpResponse<String> response = send("GET", "/projects/".concat(encode(getOwner().concat("/").concat(name))), null);
        if (response.statusCode() == 404 || (response.statusCode() >= 300 && response.statusCode() <= 399))
            return null;
        Map<String,Object> project = MAPPER.readValue(response.body(), OBJECT_TYPE);
        // projects pending deletion are not usable
        return Objects.isNull(project.get("marked_for_deletion_on")) ? project : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void create(String name, boolean restricted)
        throws Exception {
        send("POST", "/projects", Map.<String,Object>of("name", name, "path", name, "description", REPOSITORY_DESCRIPTION, "visibility", restricted ? "private" : "public", "initialize_with_readme", Boolean.FALSE));
    }

    /**
     * {@inheritDoc}
     * If the project is only marked for deletion (delayed deletion), it's then removed permanently so that its name
     * can be used again.
     */
    @Override
    protected void delete(Map<String,Object> repository)
        throws Exception {
        send("DELETE", projectPath(repository), null);
        HttpResponse<String> response = send("GET", projectPath(repository), null);
        if (response.statusCode() != 404) {
            Map<String,Object> project = MAPPER.readValue(response.body(), OBJECT_TYPE);
            if (!Objects.isNull(project.get("marked_for_deletion_on")))
                send("DELETE", projectPath(repository).concat("?permanently_remove=true&full_path=").concat(encode(Objects.toString(project.get("path_with_namespace")))), null);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String httpURL(Map<String,Object> repository) {
        return Objects.toString(repository.get("http_url_to_repo"));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected CredentialsProvider credentials() {
        return new UsernamePasswordCredentialsProvider("PRIVATE-TOKEN", token);
    }

    /**
     * {@inheritDoc}
     * Deletes all releases and packages and removes all branch and tag protections.
     */
    @Override
    protected void beforePush(Map<String,Object> repository)
        throws Exception {
        String path = projectPath(repository);
        for (Map<String,Object> release: list(path.concat("/releases"))) {
            logger.debug("Deleting release '{}' from the pooled repository '{}'", release.get("tag_name"), repository.get("path"));
            send("DELETE", path.concat("/releases/").concat(encode(Objects.toString(release.get("tag_name")))), null);
        }
        for (Map<String,Object> pkg: list(path.concat("/packages"))) {
            logger.debug("Deleting package '{}' from the pooled repository '{}'", pkg.get("name"), repository.get("path"));
            send("DELETE", path.concat("/packages/").concat(Objects.toString(pkg.get("id"))), null);
        }
        for (Map<String,Object> branch: list(path.concat("/protected_branches")))
            send("DELETE", path.concat("/protected_branches/").concat(encode(Objects.toString(branch.get("name")))), null);
        for (Map<String,Object> tag: list(path.concat("/protected_tags")))
            send("DELETE", path.concat("/protected_tags/").concat(encode(Objects.toString(tag.get("name")))), null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void afterPush(Map<String,Object> repository, boolean restricted)
        throws Exception {
        String visibility = restricted ? "private" : "public";
        if (!DEFAULT_BRANCH.equals(repository.get("default_branch")) || !visibility.equals(repository.get("visibility")))
            retry(() -> send("PUT", projectPath(repository), Map.<String,Object>of("default_branch", DEFAULT_BRANCH, "visibility", visibility)));
    }

    /**
     * {@inheritDoc}
     * Branch protections are not inspected as they don't affect tests and GitLab adds them asynchronously after the
     * first push to the default branch. They are removed by {@link #beforePush(Map)} anyway.
     */
    @Override
    protected List<String> inspect(Map<String,Object> repository, boolean restricted)
        throws Exception {
        List<String> problems = new ArrayList<String>();
        String visibility = restricted ? "private" : "public";
        if (!DEFAULT_BRANCH.equals(repository.get("default_branch")))
            problems.add(String.format("Unexpected default branch: '%s', expected: '%s'", repository.get("default_branch"), DEFAULT_BRANCH));
        if (!visibility.equals(repository.get("visibility")))
            problems.add(String.format("Unexpected visibility: %s, expected: %s", repository.get("visibility"), visibility));
        String path = projectPath(repository);
        List<String> releases = new ArrayList<String>();
        for (Map<String,Object> release: list(path.concat("/releases")))
            releases.add(Objects.toString(release.get("tag_name")));
        if (!releases.isEmpty())
            problems.add(String.format("Unexpected releases: %s", releases));
        List<String> packages = new ArrayList<String>();
        for (Map<String,Object> pkg: list(path.concat("/packages")))
            packages.add(Objects.toString(pkg.get("name")).concat("/").concat(Objects.toString(pkg.get("version"))));
        if (!packages.isEmpty())
            problems.add(String.format("Unexpected packages: %s", packages));
        return problems;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected GitLabRepository wrap(Map<String,Object> repository)
        throws Exception {
        return new GitLabRepository(new RESTv4(URI.create(baseURI), token), repository);
    }
}
