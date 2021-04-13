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
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
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
        // Check if there a State file
        String stateFilePath = state().getConfiguration().getStateFile();

        if (!Objects.isNull(stateFilePath) && !stateFilePath.isEmpty() && new File(stateFilePath).exists())
            return false;

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
        logger.info(COMMAND, "Clean.run()");

        // Delete the state file, if any
        String stateFilePath = state().getConfiguration().getStateFile();
        if (!Objects.isNull(stateFilePath) && !stateFilePath.isEmpty()) {
            logger.debug(COMMAND, "Deleting state file {}, if present", stateFilePath);
            File stateFile = new File(stateFilePath);
            if (stateFile.exists()) {
                stateFile.delete();
            }
        }
            
        return null;
    }
}