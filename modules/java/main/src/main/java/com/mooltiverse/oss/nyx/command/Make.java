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

import java.io.File;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.changelog.Changelog;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The Make command takes care of building the release artifacts.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Make extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Make.class);

    /**
     * The name used for the internal state attribute where we store current branch name.
     */
    private static final String INTERNAL_BRANCH = Make.class.getSimpleName().concat(".").concat("repository").concat(".").concat("current").concat(".").concat("branch");

    /**
     * The name used for the internal state attribute where we store the path to the changelog file.
     */
    private static final String INTERNAL_CHANGELOG_FILE = Make.class.getSimpleName().concat(".").concat("changelog").concat(".").concat("file");

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     */
    private static final String INTERNAL_LAST_COMMIT = Make.class.getSimpleName().concat(".").concat("last").concat(".").concat("commit");

    /**
     * The name used for the internal state attribute where we store the initial commit.
     */
    private static final String STATE_INITIAL_COMMIT = Make.class.getSimpleName().concat(".").concat("state").concat(".").concat("initialCommit");

    /**
     * The flag telling if the current version is new.
     */
    private static final String STATE_NEW_VERSION = Make.class.getSimpleName().concat(".").concat("state").concat(".").concat("newVersion");

    /**
     * The name used for the internal state attribute where we store the version.
     */
    private static final String STATE_VERSION = Make.class.getSimpleName().concat(".").concat("state").concat(".").concat("version");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Make(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Make command object");
    }

    /**
     * Returns the reference to the configured changelog file, if configured, or {@code null}
     * of no destination file has been set by the configuration.
     * 
     * @return the reference to the configured changelog file, if configured
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     */
    protected File getChangelogFile() 
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(state().getConfiguration().getChangelog()) || Objects.isNull(state().getConfiguration().getChangelog().getPath()) || Objects.isNull(state().getConfiguration().getChangelog().getPath().isBlank()))
            return null;

        File changelogFile = new File(state().getConfiguration().getChangelog().getPath());
        // if the file path is relative make it relative to the configured directory
        if (!changelogFile.isAbsolute())
            changelogFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getChangelog().getPath());
        
        return changelogFile;
    }

    /**
     * Builds the configured assets.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void buildAssets()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        // The only asset to build is the changelog
        // The destination path is also used as a flag to enable or disable the changelog generation, so if it's not configured the changelog is not generated
        File changelogFile = getChangelogFile();

        if (Objects.isNull(changelogFile))
            logger.debug(COMMAND, "Changelog has not been configured or it has no destination path. Skipping the changelog generation.");
        else {
            logger.debug(COMMAND, "Building the changelog to '{}'", changelogFile.getAbsolutePath());

            Changelog.instance(state().getConfiguration().getChangelog()).saveTo(changelogFile);

            logger.debug(COMMAND, "The changelog has been saved to '{}'", changelogFile.getAbsolutePath());
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
        logger.debug(COMMAND, "Storing the Make command internal attributes to the State");
        if (!state().getConfiguration().getDryRun()) {
            File changelogFile = getChangelogFile();
            putInternalAttribute(INTERNAL_BRANCH, getCurrentBranch());
            putInternalAttribute(INTERNAL_CHANGELOG_FILE, Objects.isNull(changelogFile) ? "null" : changelogFile.getAbsolutePath());
            putInternalAttribute(INTERNAL_LAST_COMMIT, getLatestCommit());
            putInternalAttribute(STATE_VERSION, state().getVersion());
            putInternalAttribute(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit());
            putInternalAttribute(STATE_NEW_VERSION, state().getNewVersion());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Make command is up to date");
        // Never up to date if this command hasn't stored a version yet into the state
        if (Objects.isNull(state().getVersion()))
            return false;

        // The command is never considered up to date when the repository branch or last commit has changed
        if ((!isInternalAttributeUpToDate(INTERNAL_BRANCH, getCurrentBranch())) || (!isInternalAttributeUpToDate(INTERNAL_LAST_COMMIT, getLatestCommit())))
            return false;

        // The command is never considered up to date when the changelog file hasn't been saved yet or it has changed
        File changelogFile = getChangelogFile();
        if ((!isInternalAttributeUpToDate(INTERNAL_CHANGELOG_FILE, Objects.isNull(changelogFile) ? "null" : changelogFile.getAbsolutePath())))
            return false;
        if (!Objects.isNull(changelogFile) && !changelogFile.exists())
            return false;

        // Check if configuration parameters have changed
        return isInternalAttributeUpToDate(STATE_VERSION, state().getVersion()) &&
            isInternalAttributeUpToDate(STATE_INITIAL_COMMIT, state().getReleaseScope().getInitialCommit()) &&
            isInternalAttributeUpToDate(STATE_NEW_VERSION, state().getNewVersion());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Make command...");

        buildAssets();

        storeStatusInternalAttributes();
        return state();
    }
}