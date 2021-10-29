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
import com.mooltiverse.oss.nyx.configuration.Defaults;
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
     * The name used for the internal state attribute where we store current branch name.
     */
    private static final String INTERNAL_BRANCH = Mark.class.getSimpleName().concat(".").concat("repository").concat(".").concat("current").concat(".").concat("branch");

    /**
     * The name used for the internal state attribute where we store the commit flag.
     */
    private static final String INTERNAL_GIT_COMMIT = Mark.class.getSimpleName().concat(".").concat("git").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the push flag.
     */
    private static final String INTERNAL_GIT_PUSH = Mark.class.getSimpleName().concat(".").concat("git").concat(".").concat("push");

    /**
     * The name used for the internal state attribute where we store the tag flag.
     */
    private static final String INTERNAL_GIT_TAG = Mark.class.getSimpleName().concat(".").concat("git").concat(".").concat("tag");

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
     * The flag telling if the current version is new.
     */
    private static final String STATE_NEW_VERSION = Mark.class.getSimpleName().concat(".").concat("state").concat(".").concat("newVersion");

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
        logger.debug(COMMAND, "Checking whether the Mark command is up to date");
        // The command is never considered up to date when the repository is not clean
        if (!isRepositoryClean())
            return false;
        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion()))
            return false;

        // The command is never considered up to date when the repository branch or last commit has changed
        if ((!isInternalAttributeUpToDate(INTERNAL_BRANCH, getCurrentBranch())) || (!isInternalAttributeUpToDate(INTERNAL_LAST_COMMIT, getLatestCommit())))
            return false;
        // The command is never considered up to date when the commit, tag or push configurantion flags have changed
        if ((!isInternalAttributeUpToDate(INTERNAL_GIT_COMMIT, state().getReleaseType().getGitCommit())) || (!isInternalAttributeUpToDate(INTERNAL_GIT_PUSH, state().getReleaseType().getGitPush())) || (!isInternalAttributeUpToDate(INTERNAL_GIT_TAG, state().getReleaseType().getGitTag())))
            return false;
        // Check if configuration parameters have changed
        return isInternalAttributeUpToDate(STATE_VERSION, state().getVersion()) &&
            isInternalAttributeUpToDate(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit()) &&
            isInternalAttributeUpToDate(STATE_NEW_VERSION, state().getNewVersion());
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
        logger.debug(COMMAND, "Storing the Mark command internal attributes to the State");
        if (!state().getConfiguration().getDryRun()) {
            putInternalAttribute(INTERNAL_BRANCH, getCurrentBranch());
            putInternalAttribute(INTERNAL_GIT_COMMIT, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitCommit());
            putInternalAttribute(INTERNAL_GIT_PUSH, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitPush());
            putInternalAttribute(INTERNAL_GIT_TAG, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitTag());
            putInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
            putInternalAttribute(STATE_VERSION, state().getVersion());
            putInternalAttribute(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit());
            putInternalAttribute(STATE_NEW_VERSION, state().getNewVersion());
        }
    }

    /**
     * Commits pending changes to the Git repository, applies a release tags and pushes changes to remotes.
     * <br>
     * Inputs to this task are:<br>
     * - the Git repository and the commit history;<br>
     * - the {@code releaseType} {@link #state()} attribute group, bringing flags for operations (commit, tag, push)
     *   to be performed or not. More specifically these attributes are {@code releaseType/gitCommit}, {@code releaseType/gitTag},
     *   and {@code releaseType/gitPush}, plus {@code releaseType/gitCommitMessage} and {@code releaseType/gitTagMessage}
     *   telling the format of messages
     * - the {@code releaseScope/initialCommit} with the SHA-1 of the initial commit in the release scope; if {@code null}
     *   this task just exits taking no act
     * - the {@code newVersion} {@link #state()} flag, that must be {@code true} for this task to run, otherwise it just skips
     * <br>
     * Outputs from this task are operations executed on the Git repository plus some attributes stored in the State object
     * with more detail:<br>
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
        logger.debug(COMMAND, "Running the Mark command...");

        if (state().getNewVersion()) {
            if (state().hasReleaseType()) {
                // COMMIT
                if (renderTemplateAsBoolean(state().getReleaseType().getGitCommit())) {
                    logger.debug(COMMAND, "The release type has the git commit flag enabled");
                    if (repository().isClean())
                        logger.debug(COMMAND, "Repository is clean, no commits need to be made");
                    else {
                        if (state().getConfiguration().getDryRun())
                            logger.info(COMMAND, "Git commit skipped due to dry run");
                        else {
                            logger.debug(COMMAND, "Committing local changes");

                            String commitMessage = renderTemplate(state().getReleaseType().getGitCommitMessage());
                            if (Objects.isNull(commitMessage) || commitMessage.isBlank()) {
                                logger.debug(COMMAND, "The configured commit message template yields to an empty commit message. Using default template '{}'", Defaults.ReleaseType.GIT_COMMIT_MESSAGE);
                                commitMessage = renderTemplate(Defaults.ReleaseType.GIT_COMMIT_MESSAGE);
                            }

                            // Here we commit all uncommitted files (of course if they're not ignored by .gitignore). Should we pick a specific subset instead? Maybe among the artifacts produced by Nyx?
                            // Here we can also specify the Author and Committer Identity as per https://github.com/mooltiverse/nyx/issues/65
                            String finalCommit = repository().commit(List.<String>of("."), commitMessage).getSHA();
                            logger.debug(COMMAND, "Local changes committed at '{}'", finalCommit);

                            logger.debug(COMMAND, "Adding commit '{}' to the release scope", finalCommit);
                            state().getReleaseScope().getCommits().add(0, finalCommit);
                        }
                    }
                }
                else logger.debug(COMMAND, "The release type has the git commit flag disabled");

                // TAG
                if (renderTemplateAsBoolean(state().getReleaseType().getGitTag())) {
                    logger.debug(COMMAND, "The release type has the git tag flag enabled");
                    if (state().getConfiguration().getDryRun())
                        logger.info(COMMAND, "Git tag skipped due to dry run");
                    else {
                        String tagMessage = renderTemplate(state().getReleaseType().getGitTagMessage());
                        logger.debug(COMMAND, "Tagging latest commit '{}' with tag '{}'", repository().getLatestCommit(), state().getVersion());
                        // Here we can also specify the Tagger Identity as per https://github.com/mooltiverse/nyx/issues/65
                        repository().tag(state().getVersion(), Objects.isNull(tagMessage) || tagMessage.isBlank() ? null : tagMessage);
                        logger.debug(COMMAND, "Tag '{}' applied to commit '{}'", state().getVersion(), repository().getLatestCommit());
                    }
                }
                else logger.debug(COMMAND, "The release type has the git tag flag disabled");

                // PUSH
                if (renderTemplateAsBoolean(state().getReleaseType().getGitPush())) {
                    logger.debug(COMMAND, "The release type has the git push flag enabled");
                    if (state().getConfiguration().getDryRun())
                        logger.info(COMMAND, "Git push skipped due to dry run");
                    else {
                        // Here we should make the Git remote repositories configurable as per https://github.com/mooltiverse/nyx/issues/66
                        logger.debug(COMMAND, "Pushing local changes to remotes");
                        String remote = repository().push();
                        logger.debug(COMMAND, "Local changes pushed to remote '{}'", remote);
                    }
                }
                else logger.debug(COMMAND, "The release type has the git push flag disabled");
            }
            else logger.warn(COMMAND, "No release type available. Nothing to release.");
        }
        else logger.info(COMMAND, "No version change detected. Nothing to release.");

        storeStatusInternalAttributes();
        return state();
    }
}