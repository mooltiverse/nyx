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
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The Clean command takes care of cleaning the release process and reverting the repository state to its initial state.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Clean extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Clean.class);

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Clean(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Clean command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(COMMAND, "Checking whether the Clean command is up to date");

        // Check if there a State file
        String stateFilePath = state().getConfiguration().getStateFile();
        if (!Objects.isNull(stateFilePath) && !stateFilePath.isBlank()) {
            // if the file path is relative make it relative to the configured directory
            File stateFile = new File(stateFilePath);
            if (!stateFile.isAbsolute())
                stateFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getStateFile());

            if (stateFile.exists()) {
                logger.debug(COMMAND, "The Clean command is not up to date because the state file has been configured ('{}') and is present on the file system so it can be deleted", stateFilePath);
                return false;
            }
        }

        // Check if there a summary file
        String summaryFilePath = state().getConfiguration().getSummaryFile();
        if (!Objects.isNull(summaryFilePath) && !summaryFilePath.isBlank()) {
            // if the file path is relative make it relative to the configured directory
            File summaryFile = new File(summaryFilePath);
            if (!summaryFile.isAbsolute())
                summaryFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getSummaryFile());

            if (summaryFile.exists()) {
                logger.debug(COMMAND, "The Clean command is not up to date because the summary file has been configured ('{}') and is present on the file system so it can be deleted", summaryFilePath);
                return false;
            }
        }

        // Check if there a Changelog file
        String changelogFilePath = state().getConfiguration().getChangelog().getPath();
        if (!Objects.isNull(changelogFilePath) && !changelogFilePath.isBlank()) {
            // if the file path is relative make it relative to the configured directory
            File changelogFile = new File(changelogFilePath);
            if (!changelogFile.isAbsolute())
                changelogFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getChangelog().getPath());

            if (changelogFile.exists()) {
                logger.debug(COMMAND, "The Clean command is not up to date because the changelog file has been configured ('{}') and is present on the file system so it can be deleted", changelogFilePath);
                return false;
            }
        }

        // Otherwise return true
        return true;
    }

    /**
     * Reverts the workspace to its initial state and returns {@code null}.
     * Specifically, the generated artifacts and the state file, if any, are deleted.
     * 
     * @return {@code null} as the state has to be invalidated for all commands by this command.
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(COMMAND, "Running the Clean command...");

        // Delete the state file, if any
        String stateFilePath = state().getConfiguration().getStateFile();
        if (!Objects.isNull(stateFilePath) && !stateFilePath.isBlank()) {
            logger.debug(COMMAND, "Deleting state file '{}', if present", stateFilePath);
            File stateFile = new File(stateFilePath);
            // if the file path is relative make it relative to the configured directory
            if (!stateFile.isAbsolute())
                stateFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getStateFile());
            if (stateFile.exists()) {
                stateFile.delete();
            }
        }

        // Delete the summary file, if any
        String summaryFilePath = state().getConfiguration().getSummaryFile();
        if (!Objects.isNull(summaryFilePath) && !summaryFilePath.isBlank()) {
            logger.debug(COMMAND, "Deleting summary file '{}', if present", summaryFilePath);
            File summaryFile = new File(summaryFilePath);
            // if the file path is relative make it relative to the configured directory
            if (!summaryFile.isAbsolute())
                summaryFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getSummaryFile());
            if (summaryFile.exists()) {
                summaryFile.delete();
            }
        }

        // Delete the changelog file, if any
        String changelogFilePath = state().getConfiguration().getChangelog().getPath();
        if (!Objects.isNull(changelogFilePath) && !changelogFilePath.isBlank()) {
            logger.debug(COMMAND, "Deleting changelog file '{}', if present", changelogFilePath);
            File changelogFile = new File(changelogFilePath);
            // if the file path is relative make it relative to the configured directory
            if (!changelogFile.isAbsolute())
                changelogFile = new File(state().getConfiguration().getDirectory(), state().getConfiguration().getChangelog().getPath());
            if (changelogFile.exists()) {
                changelogFile.delete();
            }
        }
            
        return null;
    }
}