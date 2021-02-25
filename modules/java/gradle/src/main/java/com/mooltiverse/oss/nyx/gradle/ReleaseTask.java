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

import java.util.Objects;

import javax.inject.Inject;

import org.gradle.api.Action;
import org.gradle.api.Project;
import org.gradle.api.tasks.TaskProvider;

/**
 * The Release <a href="https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks">lifecycle task</a>.
 * 
 * This task does not provide any concrete action but is used to group together all the release-related tasks so they can be
 * invoked with just this one simple task.
 */
public class ReleaseTask extends LifecycleTask {
    /**
     * The description of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Runs all the release tasks";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "release";

    /**
     * Default constructor
     */
    @Inject
    public ReleaseTask() {
        super();
    }

    /**
     * Registers the task into the given project if no task with the same name has already been registered.
     * The task is lazily registered, for deferred creation. Either way, when this method returns, a task with this
     * name exists within the project, be it this one or another defined elsewhere (i.e. by user or other plugins).
     * 
     * @param project the project to define the task for
     * 
     * @return the task provider used for the deferred task instantiation or {@code null} if a task with the same
     * name was already registered
     * 
     * @see #define(Project, String, Class, Action)
     */
    public static TaskProvider<ReleaseTask> conditionallyDefine(Project project) {
        if (Objects.isNull(findTask(project, NAME))) {
            return define(project, NAME, ReleaseTask.class, task -> configure(task));
        }
        else {
            project.getLogger().debug("A task with name {} was already defined. Skipping the definition of the new task", ReleaseTask.NAME);
            return null;
        }
    }

    /**
     * Configures the task.
     * 
     * This method is lazily invoked by Gradle (only when actually needed) as its reference is passed as an {@link Action} during the
     * {@link #define(Project, String, Class, Action)} phase.
     * 
     * @param task the task to configure
     * 
     * @see #conditionallyDefine(Project)
     * @see NyxPlugin#defineTasks(Project)
     */
    protected static void configure(ReleaseTask task) {
        task.getLogger().debug("Configuring task: {} - {}", task.getName(), ReleaseTask.NAME);

        AbstractTask.configure(task);
        task.setDescription(DESCRIPTION);

        task.getLogger().debug("Task: {} - {} configured", task.getName(), ReleaseTask.NAME);

        // Dependencies for lifecycle tasks can't be defined here as it may lead to unexpected behavior
        // (see https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:task_configuration_avoidance_general, bullet #2).
        // Dependencies for this task are defined in the NyxPlugin#defineTasks(Project) method
    }
}
