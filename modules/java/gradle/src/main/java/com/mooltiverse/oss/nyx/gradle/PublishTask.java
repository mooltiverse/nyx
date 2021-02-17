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
 * The task running the Publish command.
 */
public class PublishTask extends CoreTask {
    /**
     * The decription of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Publishes the current release to the configured services";

    /**
     * The name of the task. This is the name of the task to use inside Gradle scripts.
     */
    public static final String NAME = "nyxPublish";

    /**
     * Default constructor
     */
    @Inject
    public PublishTask() {
        super();
    }

    @TaskAction
    public void publish() {
        getLogger().info("Running PublishTask: {}", NAME); // TODO: replace this with business logic
    }

    /**
     * Configures the task.
     * 
     * @param task the task to configure
     */
    protected static void configure(PublishTask task) {
        AbstractTask.configure(task);
        task.setDescription(DESCRIPTION);
        task.dependsOn(MakeTask.NAME);
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
    public static TaskProvider<PublishTask> define(Project project) {
        return define(project, NAME, PublishTask.class, task -> configure(task));
    }
}
