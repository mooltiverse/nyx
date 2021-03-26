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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
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
        storeInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
        storeInternalAttribute(CONFIGURED_VERSION, state().getConfiguration().getVersion());
        storeInternalAttribute(CONFIGURED_BUMP, state().getConfiguration().getBump());
        storeInternalAttribute(CONFIGURED_SCHEME, state().getConfiguration().getScheme());
        storeInternalAttribute(CONFIGURED_INITIAL_VERSION, state().getConfiguration().getInitialVersion());
        storeInternalAttribute(CONFIGURED_RELEASE_PREFIX, state().getConfiguration().getReleasePrefix());
        storeInternalAttribute(CONFIGURED_RELEASE_LENIENT, state().getConfiguration().getReleaseLenient());
    }

    /**
     * Infers all the required informations to produce a new release from the Git repository.
     * <br>
     * Inputs to this task are:<br>
     * - the Git repository and the commit history;<br>
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
        logger.info(COMMAND, "Infer.run()");

        Version version = state().getConfiguration().getVersion();

        if (Objects.isNull(version)) {
            // these objects are fetched in advance from the state because they may throw an exception that we can't handle in the lambda expression
            final boolean releaseLenient = state().getConfiguration().getReleaseLenient().booleanValue();
            final String releasePrefix = state().getConfiguration().getReleasePrefix();
            final Scheme scheme = state().getScheme();

            // this map is used to work around the 'local variables referenced from a lambda expression must be final or effectively final'
            // that would be raised by compiler if we change simple non final values inside the lambda function
            final Map<String,String> findings = new HashMap<String,String>();
            // and these are the attribute names we use in the map
            final String BUMP_COMPONENT = "bumpComponent";
            final String PREVIOUS_VERSION = "previousVersion";
            final String PREVIOUS_VERSION_COMMIT = "previousVersionCommit";
            final String PREVIOUS_VERSION_COMMIT_PLUS_1 = "previousVersionCommitPlusOne";
            final String SIGNIFICANT = "significant"; // simply defining this object means it's true

            if (!Objects.isNull(state().getConfiguration().getBump()))
                findings.put(BUMP_COMPONENT, state().getConfiguration().getBump());

            logger.debug(COMMAND, "Walking the commit history...");
            repository().walkHistory(null, null, c -> {
                logger.debug(COMMAND, "Stepping by commit {}", c.getSHA());
                logger.debug(COMMAND, "Commit {} has {} tags: {}", c.getSHA(), c.getTags().size(), c.getTags());

                // inspect the tags in order to define the release scope
                for (Tag tag: c.getTags()) {
                    if (releaseLenient ? VersionFactory.isLegal(scheme.getScheme(), tag.getName(), releaseLenient) : VersionFactory.isLegal(scheme.getScheme(), tag.getName(), releasePrefix)) {
                        logger.debug(COMMAND, "Tag {} is a valid {} version and is used as the previousVersion. Likewise, {} is used as the previousVersionCommit", tag.getName(), scheme.toString(), c.getSHA());
                        findings.put(PREVIOUS_VERSION, tag.getName());
                        findings.put(PREVIOUS_VERSION_COMMIT, c.getSHA());
                        break;
                    }
                    else {
                        logger.debug(COMMAND, "Tag {} is not a valid {} version", tag.getName(), scheme.toString());
                    }
                }

                if (findings.containsKey(BUMP_COMPONENT)) {
                    // TODO: only set the SIGNIFICANT attribute if the commit really contains significant contents.
                    // Until we are not able to inspect the commit messages let's just assume that if the user forces the 'bump' we have significant commits.
                    findings.put(SIGNIFICANT, Boolean.TRUE.toString());
                }
                else {
                    // TODO: if 'bump' was not overridden by user config, inspect the commit message to figure out if we have to bump a specific version component based on that

                    // each time we need to compare the 'bump' resulting from the previous commits with the one of the current commit and only store
                    // the greatest in the findings map
                }

                // decide whether or not we need to keep walking to the next (previous) commit
                // if we keep walking, store this curent commit as the previousVersionCommitPlusOne, which MAY become the initialCommit in the next loop
                if (findings.containsKey(PREVIOUS_VERSION) && findings.containsKey(PREVIOUS_VERSION_COMMIT)) {
                    return false;
                }
                else {
                    findings.put(PREVIOUS_VERSION_COMMIT_PLUS_1, c.getSHA());
                    return true;
                }
            });
            logger.debug(COMMAND, "Walking the commit history finished.");

            // if we found the previous version commit the initial commit in the scope is the one next to it, otherwise the scope will start at the root commit
            if (findings.containsKey(PREVIOUS_VERSION_COMMIT)) {
                logger.debug(COMMAND, "Setting the initialCommit state value to {}", findings.get(PREVIOUS_VERSION_COMMIT_PLUS_1));
                state().getReleaseScope().setInitialCommit(findings.get(PREVIOUS_VERSION_COMMIT_PLUS_1));
            }
            else {
                String rootCommitSHA = repository().getRootCommit();
                logger.debug(COMMAND, "The commit history had no information about the previousVersion and previousVersionCommit so the initialCommit is the repository root commit {}", rootCommitSHA);
                state().getReleaseScope().setInitialCommit(rootCommitSHA);
            }

            // set the state attributes about the previous version and its related commit
            if (findings.containsKey(PREVIOUS_VERSION) && findings.containsKey(PREVIOUS_VERSION_COMMIT)) {
                logger.debug(COMMAND, "Setting the previousVersion and previousVersionCommit state values to {} and {} respectively", findings.get(PREVIOUS_VERSION), findings.get(PREVIOUS_VERSION_COMMIT));
                try {
                    state().getReleaseScope().setPreviousVersion(releaseLenient ? VersionFactory.valueOf(scheme.getScheme(), findings.get(PREVIOUS_VERSION), releaseLenient) : VersionFactory.valueOf(scheme.getScheme(), findings.get(PREVIOUS_VERSION), releasePrefix));
                    state().getReleaseScope().setPreviousVersionCommit(findings.get(PREVIOUS_VERSION_COMMIT));
                }
                catch (IllegalArgumentException iae) {
                    throw new ReleaseException(String.format("The previous version %s cannot be parsed as a valid version identifier", findings.get(PREVIOUS_VERSION)), iae);
                }
            }
            else {
                logger.debug(COMMAND, "The commit history had no information about the previousVersion and previousVersionCommit so they will be null");
                // set them to null in the state to make sure previous runs didn't leave stale values
                state().getReleaseScope().setPreviousVersion(null);
                state().getReleaseScope().setPreviousVersionCommit(null);
            }

            // finally compute the version
            // the previous version has been stored in the state above
            if (Objects.isNull(state().getReleaseScope().getPreviousVersion())) {
                version = state().getConfiguration().getInitialVersion();
                logger.debug(COMMAND, "No previous version detected. Using the initial version {}", version.toString());
            }
            else {
                Version previousVersion = state().getReleaseScope().getPreviousVersion();
                if (findings.containsKey(BUMP_COMPONENT)) {
                    logger.debug(COMMAND, "Bumping component {} on version {}", findings.get(BUMP_COMPONENT), previousVersion.toString());
                    version = previousVersion.bump(findings.get(BUMP_COMPONENT));
                }
                else {
                    logger.info(COMMAND, "The release scope does not contain any significant commit, version remains unchanged: {}", previousVersion.toString());
                    version = previousVersion;
                }
            }

            logger.info(COMMAND, "Inferred version is: {}", version.toString());

            // store values to the state object
            state().setVersionInternal(version);
            state().setBump(findings.containsKey(BUMP_COMPONENT) ? findings.get(BUMP_COMPONENT) : null);
            state().getReleaseScope().setSignificant(Boolean.valueOf(findings.containsKey(SIGNIFICANT)));
        }
        else {
            // the version was overridden by user
            logger.info(COMMAND, "Version overridden by user: {}", version.toString());
            state().setVersionInternal(version);
        }

        storeStatusInternalAttributes();
        return state();
    }
}