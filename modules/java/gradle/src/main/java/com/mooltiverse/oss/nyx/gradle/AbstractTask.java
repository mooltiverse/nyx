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

import javax.annotation.Nullable;

import org.gradle.api.Action;
import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.UnknownTaskException;
import org.gradle.api.tasks.TaskCollection;
import org.gradle.api.tasks.TaskContainer;
import org.gradle.api.tasks.TaskProvider;

/**
 * The abstract superclass for all Nyx tasks.
 */
abstract class AbstractTask extends DefaultTask {
    /**
     * The group the tasks belong to.
     */
    public static final String GROUP = "Release";

    /**
     * Default constructor.
     */
    public AbstractTask() {
        super();
    }

    /**
     * Returns the provider for the task with the given name or <code>null</code> if no task has been registered or created
     * with such name in the given project.
     * 
     * This is the same as {@link TaskCollection#named(String)} but in case there is no such task returns <code>null</code>
     * instead of throwing an exception. For this reason this method does not trigger the creation of the task if it was
     * only registered, according to <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html">Configuration Avoidance</a>.
     * 
     * @param project the project to look up the task into
     * @param name the name of the task to look up
     * 
     * @return the provider for the given task or <code>null</code> if there is no such task
     */
    protected static TaskProvider<Task> findTask(Project project, String name) {
        try {
            return project.getTasks().named(name);
        }
        catch (UnknownTaskException ute) {
            return null;
        }
    }

    /**
     * Registers the task into the given project.
     * 
     * The task is defined lazily so it will be actually created by Gradle only when needed, according to the 
     * <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html">Configuration Avoidance</a>.
     * 
     * For this reason {@link TaskContainer#register(String, Class)} is used instead of {@link TaskContainer#create(String, Class)} internally.
     * 
     * @param <T> the task class
     * 
     * @param project the project to define the task for
     * @param name the name to register the task with
     * @param type the task class
     * @param configurationAction the optional action used to configure the task upon creation. It may be <code>null</code>
     * 
     * @return the task provider used for the task definition
     */
    protected static <T extends Task> TaskProvider<T> define(Project project, String name, Class<T> type, @Nullable Action<? super T> configurationAction) {
        project.getLogger().debug("Registering Nyx task with name: {}", name);

        TaskProvider<T> taskProvider = project.getTasks().register(name, type, configurationAction);

        project.getLogger().debug("Nyx task registered with name: {}", name);

        return taskProvider;
    }

    /**
     * Configures the task by setting the group name.
     * 
     * Child classes should invoke this method during the configuration phase.
     * 
     * @param task the task to configure
     */
    protected static void configure(AbstractTask task) {
        task.setGroup(GROUP);
    }
}