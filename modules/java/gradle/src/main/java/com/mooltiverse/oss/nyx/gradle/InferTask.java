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

import org.gradle.api.Project;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.TaskProvider;

/**
 * The task running the Infer command.
 */
public class InferTask extends CoreTask {
    /**
     * The decription of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Collects informations from the Git repository and generates the release data set";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "nyxInfer";

    /**
     * Default constructor
     */
    @Inject
    public InferTask() {
        super();
    }

    @TaskAction
    public void infer() {
        getLogger().info("Running InferTask: {}", NAME); // TODO: replace this with business logic
    }

    /**
     * Configures the task.
     * 
     * @param task the task to configure
     */
    protected static void configure(InferTask task) {
        AbstractTask.configure(task);
        task.setDescription(DESCRIPTION);
        task.dependsOn(AmendTask.NAME);
    }

    /**
     * Registers the task into the given project. The task is lazily registered, for deferred creation.
     * 
     * @param project the project to define the task for
     * 
     * @return the task provider used for the task definition
     * 
     * @see #define(Project, String, Class, Action)
     */
    public static TaskProvider<InferTask> define(Project project) {
        return define(project, NAME, InferTask.class, task -> configure(task));
    }
}
