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
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.TaskProvider;

/**
 * The task running the Publish command.
 */
public abstract class PublishTask extends CoreTask {
    /**
     * The decription of the task. This also appears in Gradle help.
     */
    public static final String DESCRIPTION = "Publishes the new release to remote services and emits notifications";

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

    /**
     * @return see {@link NyxExtension#getBump()}
     * 
     * @see NyxExtension#getBump()
     */
    @Input
    public abstract Property<String> getBump();

    /**
     * @return see {@link NyxExtension#getDirectory()}
     * 
     * @see NyxExtension#getDirectory()
     */
    @Input
    public abstract DirectoryProperty getDirectory();

    /**
     * @return see {@link NyxExtension#getDryRun()}
     * 
     * @see NyxExtension#getDryRun()
     */
    @Input
    public abstract Property<Boolean> getDryRun();

    /**
     * @return see {@link NyxExtension#getReleasePrefix()}
     * 
     * @see NyxExtension#getReleasePrefix()
     */
    @Input
    public abstract Property<String> getReleasePrefix();

    /**
     * @return see {@link NyxExtension#getReleasePrefixLenient()}
     * 
     * @see NyxExtension#getReleasePrefixLenient()
     */
    @Input
    public abstract Property<Boolean> getReleasePrefixLenient();

    /**
     * @return see {@link NyxExtension#getScheme()}
     * 
     * @see NyxExtension#getScheme()
     */
    @Input
    public abstract Property<String> getScheme();

    /**
     * @return see {@link NyxExtension#getVerbosity()}
     * 
     * @see NyxExtension#getVerbosity()
     */
    @Input
    public abstract Property<LogLevel> getVerbosity();

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

    /**
     * Configures the task (group, description, dependencies, properties).
     * 
     * This method is invoked upon configuration as it's passed in the register(...) phase (see {@link #define(Project)}).
     * 
     * @param task the task to configure
     */
    protected static void configure(PublishTask task) {
        CoreTask.configure(task);
        task.setDescription(DESCRIPTION);

        // Configure dependencies
        task.dependsOn(MakeTask.NAME);

        // Configure task properties so that they're bridged to the properties from the extension.
        // Here we retrieve the properties from the extension and pass them as providers to this tasks' properties.
        NyxExtension extension = task.getProject().getExtensions().getByType(NyxExtension.class);
        task.getBump().set(extension.getBump());
        task.getDirectory().set(extension.getDirectory());
        task.getDryRun().set(extension.getDryRun());
        task.getReleasePrefix().set(extension.getReleasePrefix());
        task.getReleasePrefixLenient().set(extension.getReleasePrefixLenient());
        task.getScheme().set(extension.getScheme());
        task.getVerbosity().set(extension.getVerbosity());
    }

    /**
     * The actual business method for this task.
     * 
     * Gradle knows this is the method to run upon task execution thanks to the {@link TaskAction} annotation.
     */
    @TaskAction
    public void publish() {
        // TODO: replace this method body with actual business logic, invoking the Nyx backing class
        getLogger().info("Running PublishTask: {}", NAME);
        getLogger().info("Running PublishTask properties:");
        getLogger().info("      bump:                    {}", getBump().get());
        getLogger().info("      directory:               {}", getDirectory().get());
        getLogger().info("      dryRun:                  {}", getDryRun().get());
        getLogger().info("      getReleasePrefix:        {}", getReleasePrefix().get());
        getLogger().info("      getReleasePrefixLenient: {}", getReleasePrefixLenient().get());
        getLogger().info("      getScheme:               {}", getScheme().get());
        getLogger().info("      getVerbosity:            {}", getVerbosity().get());

        NyxExtension extension = getProject().getExtensions().getByType(NyxExtension.class);
        getLogger().info("      getServices:             #{}", extension.getServices().size());
        for (NyxExtension.Service service: extension.getServices()) {
            getLogger().info("      - {}:                   ", service.getName());
            getLogger().info("        provider:              {}", service.getProvider());
        }
        
    }
}
