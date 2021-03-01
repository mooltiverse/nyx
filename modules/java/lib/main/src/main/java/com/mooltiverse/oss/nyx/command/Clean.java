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

import com.mooltiverse.oss.nyx.git.local.Repository;
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
     * The name used for the internal state attribute where we store the timestamp
     * of the last execution of this command.
     * 
     * The name is prefixed with this class name to avoid clashes with other attributes.
     * 
     * @see State#getInternals()
     */
    private static final String INTERNAL_EXECUTED = Clean.class.getSimpleName().concat(".").concat("executed").concat("last");

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
    public boolean isUpToDate() {
        // TODO: implement the up-to-date checks here
        // for now let's just check if the task has executed by seeing if we have stored the last
        // execution time. Also see where the attribute is stored in the run() method
        return !Objects.isNull(state().getInternals().get(INTERNAL_EXECUTED));
    }

    /**
     * Reverts the workspace to its initial state and returns {@code null}.
     * 
     * @return {@code null} as the state has to be invalidated for all commands by this command.
     */
    @Override
    public State run() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(COMMAND, "Clean.run()");

        // store the last execution time, used in the up-to-date checks
        state().getInternals().put(INTERNAL_EXECUTED, Long.toString(System.currentTimeMillis()));

        return null;
    }
}