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
 * The Mark command takes care of tagging and committing into the Git repository.
 */
public class Mark extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Publish.class);

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
     * Runs the command and returns the updated reference to the state object.
     * 
     * @return the updated reference to the state object. The returned object is the same instance passed
     * in the constructor.
     */
    @Override
    public State run() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(COMMAND, "Mark.run()");
        return getState();
    }
}