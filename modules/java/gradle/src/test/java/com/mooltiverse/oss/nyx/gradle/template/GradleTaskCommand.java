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
package com.mooltiverse.oss.nyx.gradle.template;

import org.gradle.api.Action;
import org.gradle.api.Task;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.gradle.CoreTask;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.services.GitException;
import com.mooltiverse.oss.nyx.state.State;

/**
 * This is a proxy implementation to be used in test templates and allows to run a command
 * as a Gradle {@link Task}.
 */
public class GradleTaskCommand implements CommandProxy {
    /**
     * The Gradle task private instance.
     */
    private final CoreTask task;

    /**
     * The context name returned by this proxy.
     */
    public static final String CONTEXT_NAME = "gradle";

    /**
     * Constructor.
     * 
     * @param task the Gradle task instance to use to run the command
     * 
     * @throws Exception in case of any issue
     */
    public GradleTaskCommand(CoreTask task) {
        super();
        this.task = task;
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
            return task.state();
        }
        catch (DataAccessException | IllegalPropertyException e) {
            // wrap any exception to an unchecked exception
            throw new RuntimeException("Couldn't get the state from the Nyx Gradle task", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, GitException {
        return task.getState().getUpToDate();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        for (Action<? super Task> action: task.getActions()) {
            action.execute(task);
        }
        return task.state();
    }
}
