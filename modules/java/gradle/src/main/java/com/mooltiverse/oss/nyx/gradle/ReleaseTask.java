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

import org.gradle.api.Project;
import org.gradle.api.tasks.TaskProvider;

/**
 * The Release lifecycle task.
 */
public class ReleaseTask extends LifecycleTask {
    /**
     * The decription of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Releases the current project";

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
     * Configures the task.
     * 
     * @param task the task to configure
     */
    protected static void configure(ReleaseTask task) {
        AbstractTask.configure(task);
        task.setDescription(DESCRIPTION);
        task.dependsOn(PublishTask.NAME);
    }

    /**
     * Registers the task into the given project if no task with the same name has already been registered.
     * The task is lazily registered, for deferred creation.
     * 
     * @param project the project to define the task for
     * 
     * @return the task provider used for the task definition or <code>null</code> if a task with the same
     * name was already registered
     * 
     * @see #define(Project, String, Class, Action)
     */
    public static TaskProvider<ReleaseTask> conditionallyDefine(Project project) {
        return Objects.isNull(findTask(project, NAME)) ? define(project, NAME, ReleaseTask.class, task -> configure(task)) : null;
    }
}
