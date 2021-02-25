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

import com.mooltiverse.oss.nyx.state.State;

/**
 * The Clean command takes care of cleaning the release process and reverting the repository state to its initial state.
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
     * 
     * @throws NullPointerException if the given argument is {@code null}
     */
    public Clean(State state) {
        super(state);
        logger.debug(COMMAND, "New Clean command object");
    }

    /**
     * Runs the command and returns the updated reference to the state object.
     * 
     * @return {@code null} as the state has to be invalidated for all commands by this command.
     */
    @Override
    public State run() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(COMMAND, "Clean.run()");
        return null;
    }
}