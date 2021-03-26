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

import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The Mark command takes care of tagging and committing into the Git repository.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Mark extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Mark.class);

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_LAST_COMMIT = Mark.class.getSimpleName().concat(".").concat("last").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the initial commit.
     */
    private static final String STATE_INITIAL_COMMIT = Mark.class.getSimpleName().concat(".").concat("state").concat(".").concat("initialCommit");

    /**
     * The name used for the internal state attribute where we store the significant commits flag.
     */
    private static final String STATE_SIGNIFICANT = Mark.class.getSimpleName().concat(".").concat("state").concat(".").concat("significant");

    /**
     * The name used for the internal state attribute where we store the version.
     */
    private static final String STATE_VERSION = Mark.class.getSimpleName().concat(".").concat("state").concat(".").concat("version");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Mark(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Mark command object");
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
        return isInternalAttributeUpToDate(STATE_VERSION, state().getVersion()) &&
            isInternalAttributeUpToDate(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit()) &&
            isInternalAttributeUpToDate(STATE_SIGNIFICANT, state().getReleaseScope().getSignificant());
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
        if (!state().getConfiguration().getDryRun()) {
            storeInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
            storeInternalAttribute(STATE_VERSION, state().getVersion());
            storeInternalAttribute(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit());
            storeInternalAttribute(STATE_SIGNIFICANT, state().getReleaseScope().getSignificant());
        }
    }

    /**
     * Commits pending changes to the Git repository, applies a release tags and pushes changes to remotes.
     * <br>
     * Inputs to this task are:<br>
     * - the Git repository and the commit history;<br>
     * - the {@code releaseScope/initialCommit} with the SHA-1 of the initial commit in the release scope; if {@code null}
     *   this task just exits taking no act
     * - {@code releaseScope/significant}, when {@code true} the release scope contains significant commits (commits
     *   whose messages bring informations about new versions) and this task creates a new tag and, optionally,
     *   a commit. When this is {@code false} and the {@code releaseScope/initialCommit} is {@code null}
     *   this task just exits taking no actions; if this is {@code false} but {@code releaseScope/initialCommit} is
     *   not {@code null} the {@code releaseScope/finalCommit} is set to the latest commit in the current branch
     * <br>
     * Outputs from this task are all stored in the State object, with more detail:<br>
     * - the {@code releaseScope/finalCommit} is defined with the SHA-1 of the last commit, which may be a new
     *   commit created by this task (if pending changes are found and if configured to do so) or the most recent
     *   commit that in the current branch; if the user overrides the version by configuration
     *   this value remains {@code null}
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
        logger.info(COMMAND, "Mark.run()");

        if (Objects.isNull(state().getReleaseScope().getInitialCommit())) {
            logger.info(COMMAND, "Release scope is empty. Nothing to release.");
        }
        else {
            String finalCommit = repository().getLatestCommit();

            if (Objects.isNull(state().getReleaseScope().getSignificant()) || Boolean.FALSE.equals(state().getReleaseScope().getSignificant())) {
                logger.info(COMMAND, "Release scope does not contain significant changes. Nothing to release.");
            }
            else {
                // COMMIT
                // TODO: make the commit step conditional, depending on the configuration and the release type. Not all release types may have the commit enabled
                if (repository().isClean()) {
                    // TODO: customize the commit message
                    finalCommit = repository().getLatestCommit();
                    logger.debug(COMMAND, "Repository is clean, no commits need to be made. finalCommit updated to the latest commit {} in current branch", finalCommit);
                }
                else {
                    if (state().getConfiguration().getDryRun()) {
                        logger.info(COMMAND, "Git commit skipped due to dry run");

                        finalCommit = repository().getLatestCommit();
                        logger.debug(COMMAND, "Dry run prevents from committing. Updating the finalCommit at the latest commit in local branch: {}", finalCommit);
                    }
                    else {
                        logger.debug(COMMAND, "Committing local changes");

                        // TODO: not all changes may need to be committed so replace "." here with the paths of the files to commit, in case only a subset has to be committed
                        // TODO: make the commit message customizeable. Now we use the version number also for the message
                        // TODO: use the other version of the commit() method that also accepts identities, to optionally set the Author and Committer. This could be used to add Nyx as the committer
                        finalCommit = repository().commit(List.<String>of("."), state().getVersion()).getSHA();
                        logger.debug(COMMAND, "Local changes committed at {}", finalCommit);
                    }
                }

                // TAG
                // TODO: make the tag step conditional, depending on the configuration and the release type. Not all release types may have the tag enabled
                if (state().getConfiguration().getDryRun()) {
                    logger.info(COMMAND, "Git tag skipped due to dry run");
                }
                else {
                    // TODO: make the lightweight/annotated tag customizeable here and optionally add the Tagger Identity
                    logger.debug(COMMAND, "Tagging latest commit {} with tag {}", finalCommit, state().getVersion());
                    repository().tag(state().getVersion());
                    logger.debug(COMMAND, "Tag {} applied to commit {}", state().getVersion(), finalCommit);
                }

                // PUSH
                // TODO: make the push step conditional, depending on the configuration and the release type. Not all release types may have the push enabled
                if (state().getConfiguration().getDryRun()) {
                    logger.info(COMMAND, "Git push skipped due to dry run");
                }
                else {
                    // TODO: here we push to the default remote only (origin). The remotes to push to should be customizeable
                    logger.debug(COMMAND, "Pushing local changes to remotes");
                    String remote = repository().push();
                    logger.debug(COMMAND, "Local changes pushed to remote {}", remote);
                }
            }

            logger.debug(COMMAND, "Setting the finalCommit state value to {}", finalCommit);
            state().getReleaseScope().setFinalCommit(finalCommit);
        }

        storeStatusInternalAttributes();
        return state();
    }
}