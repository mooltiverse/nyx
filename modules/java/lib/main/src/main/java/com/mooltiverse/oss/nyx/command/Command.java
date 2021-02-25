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
 * The Command interface must be implemented by all Nyx commands.
 */
public interface Command {
    /**
     * Returns the state object.
     * 
     * @return the state object that was passed in the constructor (never {@code null}, even when or after {@link #run()} returns {@code null}).
     */
    public State getState();

    /**
     * Runs the command and returns the updated reference to the state object.
     * 
     * @return the updated reference to the state object. The returned object is the same instance passed in the constructor
     * or {@code null} if the command has cleared the state.
     */
    public State run();
}