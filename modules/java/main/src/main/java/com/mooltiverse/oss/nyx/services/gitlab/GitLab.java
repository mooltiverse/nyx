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

import java.net.URI;
import java.net.URISyntaxException;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.GitHostingService;
import com.mooltiverse.oss.nyx.services.SecurityException;
import com.mooltiverse.oss.nyx.services.Release;
import com.mooltiverse.oss.nyx.services.ReleaseService;
import com.mooltiverse.oss.nyx.services.Service;
import com.mooltiverse.oss.nyx.services.UserService;

/**
 * The entry point to the <a href="https://gitlab.com/">GitLab</a> remote service.
 */
public class GitLab implements GitHostingService, ReleaseService, UserService {
    /**
     * The name of the repository owner, used when using APIs that require the name of the repository
     * owner (individual or organization). It may be {@code null}, but some operations may fail.
     */
    private String repositoryOwner = null;

    /**
     * The name of the repository, used when using APIs that require the name of the repository
     * (simple or hierarchical, separated by '/'). It may be {@code null}, but some operations may fail.
     */
    private String repositoryName = null;

    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(GitLab.class);

    /**
     * The private API instance.
     */
    private final API api;

    /**
     * The name of the option used to pass the base URI to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the default URI is used.
     * Value: {@value}
     */
    public static final String BASE_URI_OPTION_NAME = "BASE_URI";

    /**
     * The name of the option used to pass the authentication token (Personal Access Token, OAuth)
     * to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to authenticate and perform any
     * of the authentication protected operations.
     * Value: {@value}
     */
    public static final String AUTHENTICATION_TOKEN_OPTION_NAME = "AUTHENTICATION_TOKEN";

    /**
     * The name of the option used to pass the list of supported remotes to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REMOTES_OPTION_NAME = "REMOTES";

    /**
     * The name of the option used to pass the name of the Git repository to this object instance.
     * If the repository is {@code https://gitlab.com/jdoe/project}, the value to pass for
     * this option is {@code project}, while if it's a hierarchical project like
     * {@code https://gitlab.com/acme/project/at/some/depth}, the value to pass for
     * this option is {@code project/at/some/depth}. Leading and trailing slashes must be omitted, if any.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REPOSITORY_NAME_OPTION_NAME = "REPOSITORY_NAME";

    /**
     * The name of the option used to pass the owner of the Git repository to this object instance.
     * If the repository is {@code https://gitlab.com/jdoe/project}, the value to pass for
     * this option is {@code jdoe}, and if it's a hierarchical project like
     * {@code https://gitlab.com/acme/project/at/some/depth}, the value to pass for
     * this option is {@code acme}. Leading and trailing slashes must be omitted, if any.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REPOSITORY_OWNER_OPTION_NAME = "REPOSITORY_OWNER";

    /**
     * Builds an instance using the given API.
     * 
     * @param api the API to be used internally. It can't be {@code null}
     * If {@code null} the service will not be able to authenticate and perform any of the authentication protected operations.
     * @param repositoryOwner the name of the repository owner, used when using APIs that require the
     * name of the repository owner (individual or organization). It may be {@code null}, but some operations may fail
     * @param repositoryName the name of the repository, used when using APIs that require the
     * name of the repository (simple or hierarchical, separated by '/'). It may be {@code null}, but some operations may fail
     * 
     * @throws NullPointerException if the given API is {@code null}.
     */
    private GitLab(API api, String repositoryOwner, String repositoryName) {
        super();
        Objects.requireNonNull(api, "Can't create a new instance with a null API");
        this.api = api;
        this.repositoryOwner = repositoryOwner;
        this.repositoryName = repositoryName;
    }

    /**
     * Returns an instance using the given options.
     * 
     * @param options the map of options for the requested service. It can't be {@code null}.
     * Valid options are documented as constants on this class.
     * 
     * @return an instance using the given options.
     * 
     * @throws NullPointerException if the given options map is {@code null}
     * @throws IllegalArgumentException if some entries in the given options map are missing or illegal for some reason
     */
    public static GitLab instance(Map<String,String> options) {
        Objects.requireNonNull(options, "Can't create a new instance with a null options map");

        String uriString = Objects.isNull(options.get(BASE_URI_OPTION_NAME)) ? RESTv4.BASE_URI_DEFAULT_VALUE : options.get(BASE_URI_OPTION_NAME);
        URI baseURI = null;
        try {
            baseURI = new URI(uriString);
        }
        catch (URISyntaxException use) {
            throw new IllegalArgumentException(String.format("The given URI '%s' is not a valid URI", uriString), use);
        }

        String authenticationToken = null;
        if (Objects.isNull(options.get(AUTHENTICATION_TOKEN_OPTION_NAME)))
            logger.warn(SERVICE, "No authentication token passed to the '{}' service, no authentication protected operation will be available. Use the '{}' option to set this value", GitLab.class.getSimpleName(), AUTHENTICATION_TOKEN_OPTION_NAME);
        else authenticationToken = options.get(AUTHENTICATION_TOKEN_OPTION_NAME);

        String repositoryName = null;
        if (Objects.isNull(options.get(REPOSITORY_NAME_OPTION_NAME)))
            logger.warn(SERVICE, "No repository name passed to the '{}' service, some features may not work. Use the '{}' option to set this value", GitLab.class.getSimpleName(), REPOSITORY_NAME_OPTION_NAME);
        else repositoryName = options.get(REPOSITORY_NAME_OPTION_NAME);

        String repositoryOwner = null;
        if (Objects.isNull(options.get(REPOSITORY_OWNER_OPTION_NAME)))
            logger.warn(SERVICE, "No repository owner passed to the '{}' service, some features may not work. Use the '{}' option to set this value", GitLab.class.getSimpleName(), REPOSITORY_OWNER_OPTION_NAME);
        else repositoryOwner = options.get(REPOSITORY_OWNER_OPTION_NAME);
        
        logger.trace(SERVICE, "Instantiating new service using the base URI: '{}'", baseURI.toString());

        // we have only one API and version so far so we just use that, otherwise we'd need to read additional
        // options to understand which API and version to use
        return new GitLab(new RESTv4(baseURI, authenticationToken), repositoryOwner, repositoryName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitLabRepository createGitRepository(String name, String description, boolean restricted, boolean initialize)
        throws SecurityException, TransportException {
        return new GitLabRepository(api, api.createRepository(name, description, restricted, initialize));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteGitRepository(String name)
        throws SecurityException, TransportException {
        api.deleteRepository(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitLabUser getAuthenticatedUser()
        throws TransportException, SecurityException {
        return new GitLabUser(api, api.getUserAttributes(null));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitLabRelease getReleaseByTag(String owner, String repository, String tag)
        throws SecurityException, TransportException {
            if (Objects.isNull(owner) && Objects.isNull(repositoryOwner))
            logger.warn(SERVICE, "The repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME);
        if (Objects.isNull(repository) && Objects.isNull(repositoryName))
            logger.warn(SERVICE, "The repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME);
        Map<String, Object> releaseAttributes = api.getReleaseByTag(Objects.isNull(owner) ? repositoryOwner : owner, Objects.isNull(repository) ? this.repositoryName : repository, tag);
        return Objects.isNull(releaseAttributes) ? null : new GitLabRelease(api, releaseAttributes, api.listReleaseAssets(owner, repository, tag));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitLabRelease publishRelease(String owner, String repository, String title, String tag, String description)
        throws SecurityException, TransportException {
        if (Objects.isNull(owner) && Objects.isNull(repositoryOwner))
            logger.warn(SERVICE, "The repository owner was not passed as a service option nor overridden as an argument, creating the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME);
        if (Objects.isNull(repository) && Objects.isNull(repositoryName))
            logger.warn(SERVICE, "The repository name was not passed as a service option nor overridden as an argument, creating the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME);
        return new GitLabRelease(api, api.publishRelease(Objects.isNull(owner) ? repositoryOwner : owner, Objects.isNull(repository) ? this.repositoryName : repository, title, tag, description));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitLabRelease publishReleaseAssets(String owner, String repository, Release release, Set<Attachment> assets)
        throws SecurityException, TransportException {
        Objects.requireNonNull(release);
        if (Objects.isNull(owner) && Objects.isNull(repositoryOwner))
            logger.warn(SERVICE, "The repository owner was not passed as a service option nor overridden as an argument, creating the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME);
        if (Objects.isNull(repository) && Objects.isNull(repositoryName))
            logger.warn(SERVICE, "The repository name was not passed as a service option nor overridden as an argument, creating the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME);
        try {
            GitLabRelease glRelease = GitLabRelease.class.cast(release);
            return new GitLabRelease(api, glRelease.getAttributes(), api.publishReleaseAssets(Objects.isNull(owner) ? repositoryOwner : owner, Objects.isNull(repository) ? this.repositoryName : repository, glRelease.getTag(), assets));
        }
        catch (ClassCastException cce) {
            throw new IllegalArgumentException("The given release was not created by this service", cce);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean supports(Service.Feature feature) {
        Objects.requireNonNull(feature, "Can't check if the feature is supported for a null feature");
        switch (feature)
        {
            case GIT_HOSTING:       return true;
            case RELEASES:          return true;
            case RELEASE_ASSETS:    return true;
            case USERS:             return true;
            default:                return false;
        }
    }
}
