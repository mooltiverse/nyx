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

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.services.GitException;
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
    public State state();

    /**
     * Returns {@code true} if this command is up to date, which means that the internal state ({@link #state()}) would not
     * change by running ({@link #run()}) the command again. It other words, when this method returns {@code true} any
     * invocation of the ({@link #run()}) method is needless and idempotent about the state.
     * 
     * This method uses the quickest method to verify whether the state is up to date or not. This method must not rely on
     * dependencies and it must always evaluate its own status independently.
     * 
     * As a general rule this method checks if its inputs (i.e. from the configuration) have changed since the last run.
     * 
     * @return {@code true} if this command is up to date
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #state()
     * @see #run()
     */
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException;

    /**
     * Runs the command and returns the updated reference to the state object. In order to improve performances you should only
     * invoke this method when {@link #isUpToDate()} returns {@code false}.
     * 
     * @return the updated reference to the state object. The returned object is the same instance passed in the constructor
     * or {@code null} if the command has cleared the state.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see #isUpToDate()
     * @see #state()
     */
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException;
}