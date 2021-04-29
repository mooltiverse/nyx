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

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * This is a proxy implementation to be used in test templates and allows to run a command
 * standalone, so each method invocation is dispatched directly to the backing {@link Command} instance.
 */
public class StandaloneCommandProxy implements CommandProxy {
    /**
     * The backing command instance
     */
    private final Command command;

    /**
     * The context name returned by this proxy.
     */
    public static final String CONTEXT_NAME = "standalone";

    /**
     * Constructor.
     * 
     * @param command the backing standalone command instance
     * 
     * @throws Exception in case of any issue
     */
    public StandaloneCommandProxy(Command command)
        throws Exception {
        super();
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
        return command.state();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        return command.isUpToDate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        return command.run();
    }
}
