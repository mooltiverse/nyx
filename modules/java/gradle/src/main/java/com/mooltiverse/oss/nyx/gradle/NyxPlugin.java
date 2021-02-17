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
import org.gradle.api.tasks.TaskProvider;

/**
 * The main plugin class.
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
     * Applies the plugin to the given project.
     * 
     * @param project the project the plugin is applied to
     * 
     * @see org.gradle.api.Plugin#apply(Object)
     */
    @Override
    public void apply(Project project) {
      // Create the project extension
      project.getLogger().info("Applying Nyx plugin");

      createExtensions(project);
      defineTasks(project);

      project.getLogger().debug("Nyx plugin applied");
    }

    /**
     * Creates the plugin extensions for the project.
     * 
     * @param project the project to create the extension in
     */
    protected void createExtensions(Project project) {
      project.getLogger().debug("Creating Nyx extension with name: %s", Extension.NAME);

      project.getExtensions().create(Extension.NAME, Extension.class);

      project.getLogger().debug("Nyx extension created with name: %s", Extension.NAME);
    } 

    /**
     * Sets up the tasks to the project.
     * 
     * @param project the project to define the tasks in
     */
    protected void defineTasks(Project project) {
      // Create the tasks.
      project.getLogger().info("Defining Nyx tasks");

      // Define core tasks
      AmendTask.define(project);
      CleanTask.define(project);
      InferTask.define(project);
      MakeTask.define(project);
      PublishTask.define(project);

      // Define lifecycle tasks, if there is no task with the same name already registered
      TaskProvider<ReleaseTask> releaseTaskProvider = ReleaseTask.conditionallyDefine(project);

      // Lifecycle tasks dependencies
      // These dependencies can't be defined inside each task's configure() method as it may lead
      // to unexpected behavior (see https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:task_configuration_avoidance_general, bullet #2)

      // if there is a 'release' task defined (outside this plugin) make it dependent on the Publish task
      if (Objects.isNull(releaseTaskProvider)) {
        Task releaseLifecycleTask = project.getTasks().findByName("release");
        if (!Objects.isNull(releaseLifecycleTask)) {
            releaseLifecycleTask.dependsOn(PublishTask.NAME);
        }
      }

      // make the 'assemble' task depend on this one, if any
      Task assembleLifecycleTask = project.getTasks().findByName("assemble");
      if (!Objects.isNull(assembleLifecycleTask)) {
          assembleLifecycleTask.dependsOn(MakeTask.NAME);
      }

      // make the 'build' task depend on this one, if any
      Task buildLifecycleTask = project.getTasks().findByName("build");
      if (!Objects.isNull(buildLifecycleTask)) {
          buildLifecycleTask.dependsOn(MakeTask.NAME);
      }

      // make the 'clean' task depend on this one, if any
      Task cleanLifecycleTask = project.getTasks().findByName("clean");
      if (!Objects.isNull(cleanLifecycleTask)) {
          cleanLifecycleTask.dependsOn(CleanTask.NAME);
      }

      project.getLogger().debug("Nyx tasks definition complete");
    }
}
