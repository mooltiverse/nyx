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
package com.mooltiverse.oss.nyx.command;

import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.services.Release;
import com.mooltiverse.oss.nyx.services.ReleaseService;
import com.mooltiverse.oss.nyx.services.SecurityException;

/**
 * The Publish command takes care of publishing a release.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Publish extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Publish.class);

    /**
     * The common prefix used for all the internal state attributes managed by this class.
     */
    private static final String INTERNAL_ATTRIBUTE_PREFIX = "publish";

    /**
     * The common prefix used for all the internal state attributes managed by this class, representing an input.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_PREFIX = INTERNAL_ATTRIBUTE_PREFIX.concat(".").concat("input");

    /**
     * The common prefix used for all the internal state attributes managed by this class, representing an output.
     */
    private static final String INTERNAL_OUTPUT_ATTRIBUTE_PREFIX = INTERNAL_ATTRIBUTE_PREFIX.concat(".").concat("output");

    /**
     * The name used for the internal state attribute where we store the version.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("version");

    /**
     * The name used for the internal state attribute where we store the last version
     * that was published by this command.
     */
    private static final String INTERNAL_OUPUT_ATTRIBUTE_STATE_VERSION = INTERNAL_OUTPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("version");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Publish(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Publish command object");
    }

    /**
     * Publishes the release to remotes.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void publish()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        if (state().getConfiguration().getReleaseTypes().getPublicationServices().isEmpty())
            logger.debug(COMMAND, "No publication services have been configured");
        else {
            String description = renderTemplate(state().getReleaseType().getDescription());
            for (String serviceName: state().getConfiguration().getReleaseTypes().getPublicationServices()) {
                logger.debug(COMMAND, "Publishing version '{}' to '{}'", state().getVersion(), serviceName);
                if (state().getConfiguration().getDryRun()) {
                    logger.info(COMMAND, "Publish to '{}' skipped due to dry run", serviceName);
                }
                else {
                    try {
                        ReleaseService service = resolveReleaseService(serviceName);
                        if (Objects.isNull(service))
                            throw new IllegalPropertyException(String.format("The release type uses the '%s' publication service but no such service has been configured in the 'services' section", serviceName));
                        String releaseName = renderTemplate(state().getReleaseType().getReleaseName());
                        if (Objects.isNull(releaseName) || releaseName.isBlank()) {
                            // if no release name template was specified then fall-back to the version for the release title
                            releaseName = state().getVersion();
                        }
                        // evaluate the release options
                        Boolean publishDraft = renderTemplateAsBoolean(state().getReleaseType().getPublishDraft());
                        Boolean publishPreRelease = renderTemplateAsBoolean(state().getReleaseType().getPublishPreRelease());
                        Map<String,Object> releaseOptions = Map.<String,Object>of(ReleaseService.RELEASE_OPTION_DRAFT, publishDraft.booleanValue(), ReleaseService.RELEASE_OPTION_PRE_RELEASE, publishPreRelease.booleanValue());

                        // The first two parameters here are null because the repository owner and name are expected to be passed
                        // along with service options. This is just a place where we could override them.
                        Release release = service.publishRelease(null, null, releaseName, state().getVersion(), description, releaseOptions);
                        putInternalAttribute(INTERNAL_OUPUT_ATTRIBUTE_STATE_VERSION, state().getVersion());

                        // publish release assets now
                        if (Objects.isNull(state().getConfiguration().getReleaseAssets()) || state().getConfiguration().getReleaseAssets().isEmpty()) {
                            logger.debug(COMMAND, "No release asset has been configured for publication");
                        }
                        else {
                            for (Map.Entry<String,Attachment> configuredAsset: state().getConfiguration().getReleaseAssets().entrySet()) {
                                // if the release type has configured the release types, that is considered a filter over the global release types
                                // so only the ones enabled in the release type must be published
                                if (Objects.isNull(state().getReleaseType().getAssets()) || state().getReleaseType().getAssets().contains(configuredAsset.getKey())) {
                                    logger.debug(COMMAND, "Publishing release asset '{}'", configuredAsset.getKey());

                                    // we need to render each asset's field before we publish, so we create a new Attachment instance with all the fields rendered from the configured asset
                                    Attachment asset = new Attachment(renderTemplate(configuredAsset.getValue().getFileName()), renderTemplate(configuredAsset.getValue().getDescription()), renderTemplate(configuredAsset.getValue().getType()), renderTemplate(configuredAsset.getValue().getPath()));

                                    // now actually publish the asset
                                    release = service.publishReleaseAssets(null, null, release, Set.<Attachment>of(asset));
                                    state().getReleaseAssets().add(asset);
                                    logger.debug(COMMAND, "Release asset '{}' has been published to '{}' for release '{}'", configuredAsset.getKey(), serviceName, release.getTag());
                                }
                                else {
                                    logger.debug(COMMAND, "Release asset '{}' has been configured globally but the current release type is configured to skip it", configuredAsset.getKey());
                                }
                            }
                        }
                    }
                    catch (SecurityException se) {
                        throw new ReleaseException(String.format("A security error occurred while publishing the release to service '%s'", serviceName), se);
                    }
                    catch (TransportException te) {
                        throw new ReleaseException(te);
                    }
                    catch (UnsupportedOperationException uoe) {
                        throw new IllegalPropertyException(String.format("Service '%s' does not support the release feature", serviceName));
                    }
                    
                    logger.debug(COMMAND, "Version '{}' has been published to '{}'", state().getVersion(), serviceName);
                }
            }
        }
    }

    /**
     * This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
     * of the {@link #isUpToDate()} method can find them and determine if the command is already up to date.
     * 
     * This method is meant to be invoked at the end of a successful {@link #run()}.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #isUpToDate()
     * @see State#getInternals()
     */
    private void storeStatusInternalAttributes()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Storing the Publish command internal attributes to the State");
        if (!state().getConfiguration().getDryRun()) {
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, state().getVersion());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Publish command is up to date");

        // Never up to date if this command hasn't stored a version yet into the state or the stored version is different than the state version
        if (Objects.isNull(state().getVersion()) || !isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, state().getVersion())) {
            logger.debug(COMMAND, "The Publish command is not up to date because the internal state has no version yet or the state version doesn't match the version previously published by Publish");
            return false;
        }
        
        return isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, state().getVersion());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Publish command...");

        if (state().getNewVersion()) {
            if (renderTemplateAsBoolean(state().getReleaseType().getPublish())) {
                logger.debug(COMMAND, "The release type has the publish flag enabled");
                if ((renderTemplateAsBoolean(state().getReleaseType().getGitTag())) && (renderTemplateAsBoolean(state().getReleaseType().getGitPush()))) {
                    logger.debug(COMMAND, "The release type also has the tag and push flags enabled");
                    publish();
                } else {
                    logger.warn(COMMAND, "The release type has the publish flag enabled but the tag and push flags are not (or at least one of them is not) so the release can't be published. Please make sure the tag and push flags are enabled or disable the publish flag.");
                }
            }
            else logger.debug(COMMAND, "The release type has the publish flag disabled");
        }
        else {
            logger.debug(COMMAND, "No version change detected. Nothing to publish.");
        }

        storeStatusInternalAttributes();
        
        return state();
    }
}