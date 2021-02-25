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

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;

/**
 * The main plugin class. This is the entry point used by Gradle to define the contents of the plugin: extension, tasks and other
 * objects. When configuring the project, Gradle invokes the {@link #apply(Project)} method, which is in turn responsible for registering
 * all the plugin resources.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/custom_plugins.html">Developing Custom Gradle Plugins</a> for an introduction
 * on custom Gradle plugin development.
 */
public class NyxPlugin implements Plugin<Project> {
    /**
     * The plugin ID. Value is {@value}.
     */
    public static final String ID = "com.mooltiverse.oss.nyx";

    /**
     * Default constructor.
     */
    public NyxPlugin() {
      super();
    }

    /**
     * Creates and registers all the plugin resources for the given project.
     * 
     * @param project the project the plugin is applied to and resources created into
     * 
     * @see Plugin#apply(Object)
     */
    @Override
    public void apply(Project project) {
      project.getLogger().info("Applying Nyx plugin");

      // Create the project extension. This must be done first as tasks
      // need it to be already available when they are defined
      createExtensions(project);

      // Define the tasks
      defineTasks(project);

      project.getLogger().debug("Nyx plugin applied");
    }

    /**
     * Creates the plugin extensions for the project.
     * 
     * @param project the project to create the extension in
     */
    protected void createExtensions(Project project) {
      project.getLogger().debug("Creating Nyx extension with name: {}", NyxExtension.NAME);

      NyxExtension.create(project);

      project.getLogger().debug("Nyx extension created with name: {}", NyxExtension.NAME);
    }

    /**
     * Sets up the tasks and dependencies to the project.
     * 
     * @param project the project to define the tasks in
     */
    protected void defineTasks(Project project) {
      project.getLogger().debug("Defining Nyx tasks");

      // Define lifecycle tasks. These tasks are defined conditionally, only if they haven't been defined
      // elsewhere, i.e. by some other (core) plugin.
      // These tasks are define first to allow core tasks to set dependencies on lifecycle tasks, if they need so.
      ReleaseTask.conditionallyDefine(project);

      // Define core tasks
      AmendTask.define(project);
      CleanTask.define(project);
      InferTask.define(project);
      MakeTask.define(project);
      MarkTask.define(project);
      PublishTask.define(project);

      // Define lifecycle tasks dependencies
      // These dependencies can't be defined inside each task's configure() method as it may lead
      // to unexpected behavior (see https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:task_configuration_avoidance_general, bullet #2).
      // The dependencies below make no difference whether the tasks are defined by this plugin or elsewhere.

      // Make the 'release' task dependent on the Publish task. There must be one as we define it
      // within the plugin if not already defined elsewhere.
      Task releaseLifecycleTask = project.getTasks().findByName(ReleaseTask.NAME);
      if (!Objects.isNull(releaseLifecycleTask)) {
          releaseLifecycleTask.dependsOn(PublishTask.NAME);
      }

      // If there is an 'assemble' task defined make it dependent on the MakeTask
      Task assembleLifecycleTask = project.getTasks().findByName("assemble");
      if (!Objects.isNull(assembleLifecycleTask)) {
          assembleLifecycleTask.dependsOn(MakeTask.NAME);
      }

      // If there is a 'build' task defined make it dependent on the MakeTask
      Task buildLifecycleTask = project.getTasks().findByName("build");
      if (!Objects.isNull(buildLifecycleTask)) {
          buildLifecycleTask.dependsOn(MakeTask.NAME);
      }

      // If there is a 'clean' task defined make it dependent on the CleanTask
      Task cleanLifecycleTask = project.getTasks().findByName("clean");
      if (!Objects.isNull(cleanLifecycleTask)) {
          cleanLifecycleTask.dependsOn(CleanTask.NAME);
      }

      project.getLogger().debug("Nyx tasks defined");
    }
}
