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

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.services.GitException;
import com.mooltiverse.oss.nyx.services.ReleaseService;
import com.mooltiverse.oss.nyx.services.SecurityException;
import com.mooltiverse.oss.nyx.services.git.Repository;

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
     * The name used for the internal state attribute where we store the last version
     * that was published by this command.
     */
    private static final String INTERNAL_LAST_PUBLISHED_VERSION = Mark.class.getSimpleName().concat(".").concat("last").concat(".").concat("published").concat(".").concat("version");

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
            logger.info(COMMAND, "No publication services have been configured");
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
                        // The first two parameters here are null because the repository owner and name are expected to be passed
                        // along with service options. This is just a place where we could override them.
                        service.publishRelease(null, null, state().getVersion(), state().getVersion(), description);
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
            putInternalAttribute(INTERNAL_LAST_PUBLISHED_VERSION, state().getVersion());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Publish command is up to date");

        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion()))
            return false;
        
        return isInternalAttributeUpToDate(INTERNAL_LAST_PUBLISHED_VERSION, state().getVersion());
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
                publish();
            }
            else logger.debug(COMMAND, "The release type has the publish flag disabled");
        }
        else {
            logger.info(COMMAND, "No version change detected. Nothing to publish.");
        }

        storeStatusInternalAttributes();
        return state();
    }
}