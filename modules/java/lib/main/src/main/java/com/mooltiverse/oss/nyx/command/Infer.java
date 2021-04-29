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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Tag;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.version.Version;
import com.mooltiverse.oss.nyx.version.VersionFactory;

/**
 * The Infer command takes care of inferring and computing informations in order to make a new release.
 * 
 * After this task is executed the state object has:<br>
 * - the {@code version} attribute set to the release version
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Infer extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Infer.class);

    /**
     * The name used for the internal state attribute where we store the configured bump.
     */
    private static final String CONFIGURED_BUMP = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("bump");

    /**
     * The name used for the internal state attribute where we store the configured initial version.
     */
    private static final String CONFIGURED_INITIAL_VERSION = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("initialVersion");

    /**
     * The name used for the internal state attribute where we store the configured release lenient.
     */
    private static final String CONFIGURED_RELEASE_LENIENT = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("releaseLenient");

    /**
     * The name used for the internal state attribute where we store the configured release prefix.
     */
    private static final String CONFIGURED_RELEASE_PREFIX = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("releasePrefix");

    /**
     * The name used for the internal state attribute where we store the configured scheme.
     */
    private static final String CONFIGURED_SCHEME = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("scheme");

    /**
     * The name used for the internal state attribute where we store the configured version.
     */
    private static final String CONFIGURED_VERSION = Infer.class.getSimpleName().concat(".").concat("configured").concat(".").concat("version");

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_LAST_COMMIT = Infer.class.getSimpleName().concat(".").concat("last").concat(".").concat("commit");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Infer(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Infer command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Infer command is up to date");
        // The command is never considered up to date when the repository is not clean
        if (!isRepositoryClean())
            return false;
        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion()))
            return false;

        // The command is never considered up to date when the repository last commit has changed
        if (!isInternalAttributeUpToDate(INTERNAL_LAST_COMMIT, getLatestCommit()))
            return false;
        // Check if configuration parameters have changed
        return isInternalAttributeUpToDate(CONFIGURED_VERSION, state().getConfiguration().getVersion()) &&
            isInternalAttributeUpToDate(CONFIGURED_BUMP, state().getConfiguration().getBump()) &&
            isInternalAttributeUpToDate(CONFIGURED_SCHEME, state().getConfiguration().getScheme()) &&
            isInternalAttributeUpToDate(CONFIGURED_INITIAL_VERSION, state().getConfiguration().getInitialVersion()) &&
            isInternalAttributeUpToDate(CONFIGURED_RELEASE_PREFIX, state().getConfiguration().getReleasePrefix()) &&
            isInternalAttributeUpToDate(CONFIGURED_RELEASE_LENIENT, state().getConfiguration().getReleaseLenient());
    }

    /**
     * This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
     * of the {@link #isUpToDate()} method can find them and determine if the command is already up to date.
     * 
     * This method is meant to be invoked at the end of a succesful {@link #run()}.
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
        logger.debug(COMMAND, "Storing the Infer command internal attributes to the State");
        putInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
        putInternalAttribute(CONFIGURED_VERSION, state().getConfiguration().getVersion());
        putInternalAttribute(CONFIGURED_BUMP, state().getConfiguration().getBump());
        putInternalAttribute(CONFIGURED_SCHEME, state().getConfiguration().getScheme());
        putInternalAttribute(CONFIGURED_INITIAL_VERSION, state().getConfiguration().getInitialVersion());
        putInternalAttribute(CONFIGURED_RELEASE_PREFIX, state().getConfiguration().getReleasePrefix());
        putInternalAttribute(CONFIGURED_RELEASE_LENIENT, state().getConfiguration().getReleaseLenient());
    }

    /**
     * Infers all the required informations to produce a new release from the Git repository.
     * <br>
     * Inputs to this task are:<br>
     * - the Git repository and the commit history;<br>
     * - the configuration;<br>
     * <br>
     * Outputs from this task are all stored in the State object, with more detail:<br>
     * - {@code bump} is the version identifier that was bumped to get the new version; it's undefined if the user
     *   has overridden the version by configuration or no previous versions to bump can be found in the history or
     *   the release scope doesn't have significant commits<br>
     * - the {@code version} is defined with the new version identifier for the new release; if the user has overridden
     *   the version by configuration that value is simply used and no inference is done; if the version is not overridden
     *   by configuration and no previous versions can be found in the history the initial version from the
     *   configuration is used<br>
     * - the {@code releaseScope/previousVersion} and {@code releaseScope/previousVersionCommit} are defined with the
     *   tag and SHA-1 of the previous release, if any; if no previous release is found or the version was overridden
     *   by the user configuration they will be {@code null};<br>
     * - the {@code releaseScope/initialCommit} is defined with the SHA-1 of the commit right after the
     *   {@code releaseScope/previousVersionCommit} or, when {@code releaseScope/previousVersionCommit} can't be
     *   inferred, the repository root commit SHA-1 is used; if the user overrides the version by configuration
     *   this value remains {@code null}
     * - {@code releaseScope/significant} is {@code true} if the release scope contains significant commits (commits
     *   whose messages bring informations about new versions)
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @return the updated reference to the state object. The returned object is the same instance passed in the constructor.
     * 
     * @see #isUpToDate()
     * @see #state()
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Infer command...");

        if (Objects.isNull(state().getConfiguration().getVersion())) {
            // these objects are fetched in advance from the state because they may throw an exception that we can't handle in the lambda expression
            final boolean releaseLenient = state().getConfiguration().getReleaseLenient().booleanValue();
            final String releasePrefix = state().getConfiguration().getReleasePrefix();
            final Scheme scheme = state().getScheme();
            final Map<String,CommitMessageConvention> commitMessageConventions = state().getConfiguration().getCommitMessageConventions().getItems();

            // this list contains the identifiers to bump, according to each single commit
            final List<String> bumpComponents = new ArrayList<String>();

            if (!Objects.isNull(state().getConfiguration().getBump())) {
                logger.debug(COMMAND, "Bump component overridden by user, skipping commit message convention evaluation");
                state().setBump(state().getConfiguration().getBump());
            }

            logger.debug(COMMAND, "Walking the commit history...");
            repository().walkHistory(null, null, c -> {
                logger.debug(COMMAND, "Stepping by commit {}", c.getSHA());
                logger.debug(COMMAND, "Commit {} has {} tags: {}", c.getSHA(), c.getTags().size(), c.getTags());

                // inspect the tags in order to define the release scope
                for (Tag tag: c.getTags()) {
                    if (releaseLenient ? VersionFactory.isLegal(scheme.getScheme(), tag.getName(), releaseLenient) : VersionFactory.isLegal(scheme.getScheme(), tag.getName(), releasePrefix)) {
                        logger.debug(COMMAND, "Tag {} is a valid {} version and is used as the previousVersion. Likewise, {} is used as the previousVersionCommit", tag.getName(), scheme.toString(), c.getSHA());
                        state().getReleaseScope().setPreviousVersion(tag.getName());
                        state().getReleaseScope().setPreviousVersionCommit(c.getSHA());
                        break;
                    }
                    else {
                        logger.debug(COMMAND, "Tag {} is not a valid {} version", tag.getName(), scheme.toString());
                    }
                }

                if (!state().getReleaseScope().hasPreviousVersion() || !state().getReleaseScope().hasPreviousVersionCommit()) {
                    logger.debug(COMMAND, "Commit {} has no valid version tags so it's added to the release scope", c.getSHA());
                    state().getReleaseScope().setInitialCommit(c.getSHA());
                }

                // if the 'bump' was not overridden by user, evaluate the commit message against the configured conventions to see which identifier must be dumped, if any
                if (!state().hasBump()) {
                    if (!Objects.isNull(commitMessageConventions)) {
                        for (Map.Entry<String,CommitMessageConvention> cmcEntry: commitMessageConventions.entrySet()) {
                            logger.debug(COMMAND, "Evaluating commit {} against message convention {}", c.getSHA(), cmcEntry.getKey());
                            Matcher messageMatcher = Pattern.compile(cmcEntry.getValue().getExpression()).matcher(c.getMessage().getFullMessage());
                            if (messageMatcher.matches()) {
                                logger.debug(COMMAND, "Commit message convention {} matches commit {}", cmcEntry.getKey(), c.getSHA());
                                for (Map.Entry<String,String> bumpExpression: cmcEntry.getValue().getBumpExpressions().entrySet()) {
                                    logger.debug(COMMAND, "Matching commit {} against bump expression {} of message convention {}", c.getSHA(), bumpExpression.getKey(), cmcEntry.getKey());
                                    Matcher bumpMatcher = Pattern.compile(bumpExpression.getValue()).matcher(c.getMessage().getFullMessage());
                                    if (bumpMatcher.matches()) {
                                        logger.debug(COMMAND, "Bump expression {} of message convention {} matches commit {}, meaning that the {} identifier has to be bumped, according to this commit", bumpExpression.getKey(), cmcEntry.getKey(), c.getSHA(), bumpExpression.getKey());
                                        bumpComponents.add(bumpExpression.getKey());
                                    }
                                    else logger.debug(COMMAND, "Bump expression {} of message convention {} doesn't match commit {}", bumpExpression.getKey(), cmcEntry.getKey(), c.getSHA());
                                }
                            }
                            else logger.debug(COMMAND, "Commit message convention {} doesn't match commit {}, skipping", cmcEntry.getKey(), c.getSHA());
                        }
                    }
                }

                // stop walking the commit history if we already have the previous version (and its commit), otherwise keep walking
                return !(state().getReleaseScope().hasPreviousVersion() && state().getReleaseScope().hasPreviousVersionCommit());
            });
            logger.debug(COMMAND, "Walking the commit history finished.");

            // if we couldn't infer the initial version and its commit, set the state attributes to the configured initial values
            if (!state().getReleaseScope().hasPreviousVersion() || !state().getReleaseScope().hasPreviousVersionCommit()) {
                logger.debug(COMMAND, "The commit history had no information about the previousVersion and previousVersionCommit, using default initial value {} for the previousVersion", state().getConfiguration().getInitialVersion());
                // use the configured initial version as the previous version
                state().getReleaseScope().setPreviousVersion(state().getConfiguration().getInitialVersion());
                state().getReleaseScope().setPreviousVersionCommit(null);
            }
            // parse the previous version now
            Version previousVersion = releaseLenient ? VersionFactory.valueOf(scheme.getScheme(), state().getReleaseScope().getPreviousVersion(), releaseLenient) : VersionFactory.valueOf(scheme.getScheme(), state().getReleaseScope().getPreviousVersion(), releasePrefix);

            // determine the bump component, if not overridden by user
            if (!state().hasBump()) {
                if (bumpComponents.isEmpty()) {
                    logger.debug(COMMAND, "Unable to infer the identifier to bump from the commits within the release scope. This may be due to the configured commit message conventions not matching any commit or because the release scope has no significant commits");
                }
                else {
                    // the bump component is now the most significant among those detected on each commit within the scope
                    VersionFactory.sortIdentifiers(scheme.getScheme(), bumpComponents);
                    String bumpComponent = bumpComponents.get(0);
                    logger.debug(COMMAND, "The version identifier to bump from the commits in the release scope, according to the configured commit message convenstions, is {}", bumpComponent);
                    state().setBump(bumpComponent);
                }
            }

            // finally compute the version
            Version version = null;
            if (state().hasBump()) {
                logger.info(COMMAND, "Bumping component {} on version {}", state().getBump(), previousVersion.toString());
                version = previousVersion.bump(state().getBump());
            }
            else {
                logger.info(COMMAND, "The release scope does not contain any significant commit, version remains unchanged: {}", previousVersion.toString());
                version = previousVersion;
            }

            logger.info(COMMAND, "Inferred version is: {}", version.toString());

            // store values to the state object
            state().setVersion(Objects.isNull(releasePrefix) ? version.toString() : releasePrefix.concat(version.toString()));
            state().getReleaseScope().setSignificant(Boolean.valueOf(!bumpComponents.isEmpty()));
        }
        else {
            // the version was overridden by user
            logger.info(COMMAND, "Version overridden by user: {}", state().getConfiguration().getVersion());
            state().setVersion(state().getConfiguration().getVersion());
        }

        storeStatusInternalAttributes();
        return state();
    }
}