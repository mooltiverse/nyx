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
import com.mooltiverse.oss.nyx.entities.AuthenticationMethod;
import com.mooltiverse.oss.nyx.entities.Defaults;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
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
     * The common prefix used for all the internal state attributes managed by this class.
     */
    private static final String INTERNAL_ATTRIBUTE_PREFIX = "mark";

    /**
     * The common prefix used for all the internal state attributes managed by this class, representing an input.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_PREFIX = INTERNAL_ATTRIBUTE_PREFIX.concat(".").concat("input");

    /**
     * The common prefix used for all the internal state attributes managed by this class, representing an output.
     */
    private static final String INTERNAL_OUTPUT_ATTRIBUTE_PREFIX = INTERNAL_ATTRIBUTE_PREFIX.concat(".").concat("output");

    /**
     * The name used for the internal state attribute where we store the commit flag.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("configured").concat(".").concat("git").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the push flag.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("configured").concat(".").concat("git").concat(".").concat("push");

    /**
     * The name used for the internal state attribute where we store the tag flag.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("configured").concat(".").concat("git").concat(".").concat("tag");

    /**
     * The name used for the internal state attribute where we store current branch name.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("repository").concat(".").concat("current").concat(".").concat("branch");

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("repository").concat(".").concat("last").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the initial commit.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("initialCommit");

    /**
     * The flag telling if the current version is new.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("newVersion");

    /**
     * The name used for the internal state attribute where we store the version.
     */
    private static final String INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION = INTERNAL_INPUT_ATTRIBUTE_PREFIX.concat(".").concat("state").concat(".").concat("version");

    /**
     * The name used for the internal state attribute where we store the last commit created by this command.
     */
    private static final String INTERNAL_OUPUT_ATTRIBUTE_COMMIT = INTERNAL_OUTPUT_ATTRIBUTE_PREFIX.concat(".").concat("commit");

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
     * Commits pending changes to the Git repository, applies a release tags and pushes changes to remotes.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void commit()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
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
                Commit finalCommit = repository().commit(List.<String>of("."), commitMessage);
                logger.debug(COMMAND, "Local changes committed at '{}'", finalCommit.getSHA());
                putInternalAttribute(INTERNAL_OUPUT_ATTRIBUTE_COMMIT, finalCommit.getSHA());

                logger.debug(COMMAND, "Adding commit '{}' to the release scope", finalCommit.getSHA());
                state().getReleaseScope().getCommits().add(0, finalCommit);
            }
        }
    }

    /**
     * Applies release tags to the latest commit.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void tag()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        if (state().getConfiguration().getDryRun())
            logger.info(COMMAND, "Git tag skipped due to dry run");
        else {
            String tagMessage = renderTemplate(state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(state().getReleaseType().getGitTagNames()) || state().getReleaseType().getGitTagNames().isEmpty()) {
                logger.debug(COMMAND, "No tag name has been configured for this release type so no tag is applied");
            }
            else {
                for (String tagTemplate: state().getReleaseType().getGitTagNames()) {
                    String tag = renderTemplate(tagTemplate);
                    logger.trace(COMMAND, "Tag template '{}' renders to '{}'", tagTemplate, tag);
                    logger.debug(COMMAND, "Tagging latest commit '{}' with tag '{}'", repository().getLatestCommit(), tag);
                    // Here we can also specify the Tagger Identity as per https://github.com/mooltiverse/nyx/issues/65
                    repository().tag(tag, Objects.isNull(tagMessage) || tagMessage.isBlank() ? null : tagMessage);
                    logger.debug(COMMAND, "Tag '{}' applied to commit '{}'", tag, repository().getLatestCommit());
                }
            }
        }
    }

    /**
     * Pushes changes to remotes.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void push()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        if (state().getConfiguration().getDryRun())
            logger.info(COMMAND, "Git push skipped due to dry run");
        else {
            logger.debug(COMMAND, "Pushing local changes to remotes");

            List<String> remotes = state().getConfiguration().getReleaseTypes().getRemoteRepositories();
            if (Objects.isNull(remotes) || remotes.isEmpty()) {
                logger.debug(COMMAND, "The list of remotes is not defined. Using the default remote '{}'", Repository.DEFAULT_REMOTE_NAME);
                remotes = List.<String>of(Repository.DEFAULT_REMOTE_NAME);
            }
            for (String remote: remotes) {
                logger.debug(COMMAND, "Pushing local changes to remote '{}'", remote);

                // Now we need to find the credentials by going through all the configured remotes and finding
                // the one that supports the target remote.
                logger.debug(COMMAND, "Looking up credentials for remote '{}'", remote);
                AuthenticationMethod authenticationMethod = null;
                String user = null;
                String password = null;
                String privateKey = null;
                byte[] passphrase = null;
                if (Objects.isNull(state().getConfiguration().getGit()) || Objects.isNull(state().getConfiguration().getGit().getRemotes())) 
                    logger.debug(COMMAND, "No Git remote repository has been configured");
                else {
                    if (state().getConfiguration().getGit().getRemotes().containsKey(remote)) {
                        logger.debug(COMMAND, "Using configured credentials for remote '{}'", remote);
                        authenticationMethod = state().getConfiguration().getGit().getRemotes().get(remote).getAuthenticationMethod();
                        user = renderTemplate(state().getConfiguration().getGit().getRemotes().get(remote).getUser());
                        password = renderTemplate(state().getConfiguration().getGit().getRemotes().get(remote).getPassword());
                        privateKey = renderTemplate(state().getConfiguration().getGit().getRemotes().get(remote).getPrivateKey());
                        String passphraseString = renderTemplate(state().getConfiguration().getGit().getRemotes().get(remote).getPassphrase());
                        if (!Objects.isNull(passphraseString))
                            passphrase = passphraseString.getBytes();
                    }
                    else {
                        logger.debug(COMMAND, "No configuration available for remote '{}'", remote);
                    }
                }

                // finally push
                if (AuthenticationMethod.PUBLIC_KEY.equals(authenticationMethod)) {
                    logger.debug(COMMAND, "Attempting push to '{}' using public key credentials.", remote);
                    repository().push(remote, privateKey, passphrase);
                }
                else {
                    if (Objects.isNull(user) && Objects.isNull(password))
                        logger.debug(COMMAND, "No credentials were configured for remote '{}'. Attempting anonymous push.", remote);
                    else logger.debug(COMMAND, "Attempting push to '{}' using user name and password credentials.", remote);
                    repository().push(remote, user, password);
                }

                logger.debug(COMMAND, "Local changes pushed to remote '{}'", remote);
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
        logger.debug(COMMAND, "Storing the Mark command internal attributes to the State");
        if (!state().getConfiguration().getDryRun()) {
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitCommit());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitPush());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG, Objects.isNull(state().getReleaseType()) ? null : state().getReleaseType().getGitTag());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, getCurrentBranch());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, getLatestCommit());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, state().getNewVersion());
            putInternalAttribute(INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, state().getVersion());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        // Never up to date if this command hasn't stored a version yet into the state or the stored version is different than the state version
        if (Objects.isNull(state().getVersion()) || !isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, state().getVersion())) {
            logger.debug(COMMAND, "The Mark command is not up to date because the internal state has no version yet or the state version doesn't match the version previously generated by Mark");
            return false;
        }

        // The command is never considered up to date when the repository branch or last commit has changed
        if ((!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_CURRENT_BRANCH, getCurrentBranch())) || (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_REPOSITORY_LAST_COMMIT, getLatestCommit()))) {
            logger.debug(COMMAND, "The Mark command is not up to date because the last commit or the current branch has changed");
            return false;
        }
        // The command is never considered up to date when the commit, tag or push configurantion flags have changed
        if ((!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_COMMIT, state().getReleaseType().getGitCommit())) || (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_PUSH, state().getReleaseType().getGitPush())) || (!isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_CONFIGURED_GIT_TAG, state().getReleaseType().getGitTag()))) {
            logger.debug(COMMAND, "The Mark command is not up to date because the configuration of Git flags has changed");
            return false;
        }
            
        // Check if configuration parameters have changed
        boolean res = isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit()) &&
            isInternalAttributeUpToDate(INTERNAL_INPUT_ATTRIBUTE_STATE_NEW_VERSION, state().getNewVersion());
        if (res) {
            logger.debug(COMMAND, "The Mark command is up to date");
        }
        else {
            logger.debug(COMMAND, "The Mark command is not up to date because the configuration or the internal state has changed");
        }
        return res;
    }

    /**
     * Commits pending changes to the Git repository, applies release tags and pushes changes to remotes.
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
                    commit();
                }
                else logger.debug(COMMAND, "The release type has the git commit flag disabled");

                // TAG
                if (renderTemplateAsBoolean(state().getReleaseType().getGitTag())) {
                    logger.debug(COMMAND, "The release type has the git tag flag enabled");
                    tag();
                }
                else logger.debug(COMMAND, "The release type has the git tag flag disabled");

                // PUSH
                if (renderTemplateAsBoolean(state().getReleaseType().getGitPush())) {
                    logger.debug(COMMAND, "The release type has the git push flag enabled");
                    push();
                }
                else logger.debug(COMMAND, "The release type has the git push flag disabled");
            }
            else logger.warn(COMMAND, "No release type available. Nothing to release.");
        }
        else logger.debug(COMMAND, "No version change detected. Nothing to release.");

        storeStatusInternalAttributes();
        
        return state();
    }
}