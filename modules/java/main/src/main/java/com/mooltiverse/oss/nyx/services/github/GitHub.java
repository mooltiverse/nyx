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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.kohsuke.github.GitHubBuilder;
import org.kohsuke.github.GHAsset;
import org.kohsuke.github.GHRelease;
import org.kohsuke.github.GHRepository;

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
 * The entry point to the <a href="https://github.com/">GitHub</a> remote service.
 */
public class GitHub implements GitHostingService, ReleaseService, UserService {
    /**
     * The name of the repository owner, used when using APIs that require the name of the repository
     * owner (individual or organization). It may be {@code null}, but some operations may fail.
     */
    private String repositoryOwner = null;

    /**
     * The name of the repository, used when using APIs that require the name of the repository.
     * It may be {@code null}, but some operations may fail.
     */
    private String repositoryName = null;

    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(GitHub.class);

    /**
     * The private API client instance.
     */
    private final org.kohsuke.github.GitHub client;

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
     * If the repository is {@code https://github.com/octocat/hello-world}, the value to pass for
     * this option is {@code hello-world}.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REPOSITORY_NAME_OPTION_NAME = "REPOSITORY_NAME";

    /**
     * The name of the option used to pass the owner of the Git repository to this object instance.
     * If the repository is {@code https://github.com/octocat/hello-world}, the value to pass for
     * this option is {@code octocat}. This option accepts individual and organization names.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REPOSITORY_OWNER_OPTION_NAME = "REPOSITORY_OWNER";

    /**
     * Builds an instance using the given API.
     * 
     * @param client the API client to be used internally. It can't be {@code null}
     * If {@code null} the service will not be able to authenticate and perform any of the authentication protected operations.
     * @param repositoryOwner the name of the repository owner, used when using APIs that require the
     * name of the repository owner (individual or organization). It may be {@code null}, but some operations may fail
     * @param repositoryName the name of the repository, used when using APIs that require the
     * name of the repository. It may be {@code null}, but some operations may fail
     * 
     * @throws NullPointerException if the given API is {@code null}.
     */
    private GitHub(org.kohsuke.github.GitHub client, String repositoryOwner, String repositoryName) {
        super();
        Objects.requireNonNull(client, "Can't create a new instance with a null API client");
        this.client = client;
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
    public static GitHub instance(Map<String,String> options) {
        Objects.requireNonNull(options, "Can't create a new instance with a null options map");

        String baseURI = null;
        if (Objects.isNull(options.get(BASE_URI_OPTION_NAME)))
            logger.debug(SERVICE, "No custom base URI passed to the '{}' service, service will use the default URI", GitHub.class.getSimpleName());
        else baseURI = options.get(BASE_URI_OPTION_NAME);

        String authenticationToken = null;
        if (Objects.isNull(options.get(AUTHENTICATION_TOKEN_OPTION_NAME)))
            logger.warn(SERVICE, "No authentication token passed to the '{}' service, no authentication protected operation will be available. Use the '{}' option to set this value", GitHub.class.getSimpleName(), AUTHENTICATION_TOKEN_OPTION_NAME);
        else authenticationToken = options.get(AUTHENTICATION_TOKEN_OPTION_NAME);

        String repositoryName = null;
        if (Objects.isNull(options.get(REPOSITORY_NAME_OPTION_NAME)))
            logger.warn(SERVICE, "No repository name passed to the '{}' service, some features may not work. Use the '{}' option to set this value", GitHub.class.getSimpleName(), REPOSITORY_NAME_OPTION_NAME);
        else repositoryName = options.get(REPOSITORY_NAME_OPTION_NAME);

        String repositoryOwner = null;
        if (Objects.isNull(options.get(REPOSITORY_OWNER_OPTION_NAME)))
            logger.warn(SERVICE, "No repository owner passed to the '{}' service, some features may not work. Use the '{}' option to set this value", GitHub.class.getSimpleName(), REPOSITORY_OWNER_OPTION_NAME);
        else repositoryOwner = options.get(REPOSITORY_OWNER_OPTION_NAME);

        GitHubBuilder ghBuilder = new GitHubBuilder();
        if (!Objects.isNull(baseURI) && !baseURI.isBlank())
            ghBuilder = ghBuilder.withEndpoint(baseURI);
        if (!Objects.isNull(authenticationToken) && !authenticationToken.isBlank())
            ghBuilder = ghBuilder.withOAuthToken(authenticationToken);

        try {
            return new GitHub(ghBuilder.build(), repositoryOwner, repositoryName);
        }
        catch (IOException ioe) {
            throw new IllegalArgumentException("Unable to build a GitHub client instance", ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitHubRepository createGitRepository(String name, String description, boolean restricted, boolean initialize)
        throws SecurityException, TransportException {
        try {
            GHRepository ghRepository = client.createRepository(name).description(description).autoInit(initialize).visibility(restricted ? GHRepository.Visibility.PRIVATE : GHRepository.Visibility.PUBLIC).create();
            // it looks like setting the visibility above against the builder doesn't work, so let's repeat it here
            ghRepository.setVisibility(restricted ? GHRepository.Visibility.PRIVATE : GHRepository.Visibility.PUBLIC);
            return new GitHubRepository(ghRepository);
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to create the GitHub repository '%s'", name), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteGitRepository(String name)
        throws SecurityException, TransportException {
        // the repository must be in the form owner/repo, and this method always deletes repositories for the currently authenticated user
        String repoName = getAuthenticatedUser().getUserName().concat("/").concat(name);
        try {
            client.getRepository(repoName).delete();
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to find or delete the GitHub repository '%s'", repoName), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitHubUser getAuthenticatedUser()
        throws TransportException, SecurityException {
        try {
            if (client.isAnonymous())
                throw new SecurityException("The current session is not authenticated");
            else return new GitHubUser(client.getMyself());
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the authenticated user"), ioe);
        }
    }

    /**
     * Returns the repository with the given name and beloging to the optional owner.
     * 
     * @param owner the name of the repository owner to create the release for. It may be {@code null}, in which case,
     * the repository owner must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * @param repositoryName the name of the repository to create the release for. It may be {@code null}, in which case,
     * the repository name must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * 
     * @return the repository with the given name and beloging to the optional owner.
     * 
     * @throws SecurityException if authentication or authorization fails
     * @throws TransportException if communication to the remote endpoint fails
     */
    private GHRepository getRepository(String owner, String repository) 
        throws SecurityException, TransportException {
        String repoName = "";
        if (Objects.isNull(owner) && Objects.isNull(repositoryOwner))
            logger.warn(SERVICE, "The repository owner was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_OWNER_OPTION_NAME);
        else repoName = Objects.isNull(owner) ? (Objects.isNull(repositoryOwner) ? "" : repositoryOwner) : owner;
        if (Objects.isNull(repository) && Objects.isNull(repositoryName))
            logger.warn(SERVICE, "The repository name was not passed as a service option nor overridden as an argument, getting the release may fail. Use the '{}' option to set this option or override it when invoking this method.", REPOSITORY_NAME_OPTION_NAME);
        else repoName = Objects.isNull(repository) ? (Objects.isNull(repositoryName) ? repoName : repoName+"/"+repositoryName) : repoName+"/"+repository;

        try {
            return client.getRepository(repoName);
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the repository '%s'", repoName), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitHubRelease getReleaseByTag(String owner, String repository, String tag)
        throws SecurityException, TransportException {
        try {
            GHRelease ghRelease = getRepository(owner, repository).getReleaseByTagName(tag);
            return Objects.isNull(ghRelease) ? null : new GitHubRelease(ghRelease);
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the release '%s'", tag), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitHubRelease publishRelease(String owner, String repository, String title, String tag, String description)
        throws SecurityException, TransportException {
        try {
            return new GitHubRelease(getRepository(owner, repository).createRelease(tag).name(title).body(description).create());
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to publish the release '%s'", tag), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitHubRelease publishReleaseAssets(String owner, String repository, Release release, Set<Attachment> assets)
        throws SecurityException, TransportException {
        Objects.requireNonNull(release);
        if (Objects.isNull(assets) || assets.isEmpty()) {
            try {
                return GitHubRelease.class.cast(release);
            }
            catch (ClassCastException cce) {
                throw new IllegalArgumentException("The given release was not created by this service", cce);
            }
        }

        GHRelease ghRelease = null;
        try {
            ghRelease = getRepository(owner, repository).getReleaseByTagName(release.getTag());
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the release '%s'", release.getTag()), ioe);
        }

        for (Attachment asset: assets) {
            File assetFile = new File(asset.getPath());
            if (assetFile.exists()) {
                try {
                    GHAsset ghAsset = ghRelease.uploadAsset(asset.getFileName(), new FileInputStream(assetFile), asset.getType());
                    ghAsset.setLabel(asset.getDescription());
                }
                catch (IOException ioe) {
                    throw new TransportException(String.format("Could not upload release asset '%s'", asset.getPath()), ioe);
                }
            }
            else 
            {
                logger.warn(SERVICE, "The path '{}' for the asset '{}' cannot be resolved to a local file and will be skipped", asset.getPath(), asset.getFileName());
            }
        }
        return new GitHubRelease(ghRelease);
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
