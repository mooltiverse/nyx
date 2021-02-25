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
package com.mooltiverse.oss.nyx.gradle;

import javax.inject.Inject;

import org.gradle.api.Action;
import org.gradle.api.Project;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.TaskProvider;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.NyxException;

/**
 * The task running the Amend command by invoking the {@link Nyx#amend()} method on the backing Nyx instance.
 */
public abstract class AmendTask extends CoreTask {
    /**
     * The description of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Integrates the local repository history with extra information from remotes";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "nyxAmend";

    /**
     * Default constructor
     */
    @Inject
    public AmendTask() {
        super();
    }

    /**
     * Registers the task into the given project. The task is lazily registered, for deferred creation.
     * 
     * @param project the project to define the task for
     * 
     * @return the task provider used for the deferred task instantiation
     * 
     * @see #define(Project, String, Class, Action)
     */
    public static TaskProvider<AmendTask> define(Project project) {
        return define(project, NAME, AmendTask.class, task -> configure(task));
    }

    /**
     * Configures the task (group, description, dependencies, properties).
     * 
     * This method is lazily invoked by Gradle (only when actually needed) as its reference is passed as an {@link Action} during the
     * {@link #define(Project, String, Class, Action)} phase.
     * 
     * @param task the task to configure
     * 
     * @see #define(Project)
     */
    protected static void configure(AmendTask task) {
        task.getLogger().debug("Configuring task: {} - {}", task.getName(), AmendTask.NAME);

        CoreTask.configure(task);
        task.setDescription(DESCRIPTION);

        // This task has no dependencies to configure

        task.getLogger().debug("Task: {} - {} configured", task.getName(), AmendTask.NAME);
    }

    /**
     * The actual business method for this task. This method runs the {@link Nyx#amend()} method on the shared
     * singleton Nyx instance.
     * 
     * Gradle knows this is the method to run upon task execution thanks to the {@link TaskAction} annotation.
     * 
     * @throws NyxException in case of any exception when invoking the backing instance
     */
    @TaskAction
    public void amend()
        throws NyxException {
        getLogger().info("Running AmendTask: {}", NAME);

        // just a draft to test the wireframing between objects
        nyx().amend();
    }
}
