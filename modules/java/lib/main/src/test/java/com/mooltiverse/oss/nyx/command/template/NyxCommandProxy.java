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
package com.mooltiverse.oss.nyx.command.template;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * This is a proxy implementation to be used in test templates and allows to run a command
 * through the {@link Nyx} class business methods.
 */
public class NyxCommandProxy implements CommandProxy {
    /**
     * The Nyx class private instance.
     */
    private final Nyx nyx;

    /**
     * The command to be invoked by this class.
     */
    private final Commands command;

    /**
     * The context name returned by this proxy.
     */
    public static final String CONTEXT_NAME = "nyx";

    /**
     * Constructor.
     * 
     * @param nyx the Nyx instance to use to run the command
     * @param command the command to run when the {@link #run()} method is invoked.
     * 
     * @throws Exception in case of any issue
     */
    public NyxCommandProxy(Nyx nyx, Commands command)
        throws Exception {
        super();
        this.nyx = nyx;
        this.command = command;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getContextName() {
        return CONTEXT_NAME;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State state() {
        try {
            return nyx.state();
        }
        catch (DataAccessException | IllegalPropertyException e) {
            // wrap any exception to an unchecked exception
            throw new RuntimeException("Couldn't get the state from the Nyx class", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        return nyx.isUpToDate(command);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        nyx.run(command);
        return nyx.state();
    }
}
