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

import org.gradle.api.Action;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.initialization.Settings;
import org.gradle.api.invocation.Gradle;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;

/**
 * The main plugin class. This plugin can be applied to {@link Project}, {@link Settings} or {@link Gradle} objects,
 * which means the {@link #apply(Object)} method can receive any of these object types. The actual type of the object
 * passed to the {@link #apply(Object)} method is defined by Gradle based on how the plugin is used, or better,
 * where the plugin is <a href="https://docs.gradle.org/current/userguide/plugins.html">applied</a>.
 * <br>
 * If the plugin is applied the common way, using the plugin DSL in the {@code build.gradle} file, then a {@link Project}
 * object is passed. If the plugin is used as a settings plugin using the plugin DSL in the {@code settings.gradle}
 * then a {@link Settings} object is passed. If the plugin is used as an initialization plugin using an 
 * <a href="https://docs.gradle.org/current/userguide/init_scripts.html">Initialization Scripts</a> like {@code init.gradle}
 * then a {@link Gradle} object is passed.
 * See <a href="https://docs.gradle.org/current/userguide/custom_plugins.html#sec:writing_a_simple_plugin">Writing a simple plugin</a>
 * for more.
 * <br>
 * The main difference between the different options is that when the plugin is used as a regular project plugin
 * (applied in the {@code build.gradle}, so using a {@link Project} object) Nyx inference may happen too late as some
 * properties like the project {@code version} are needed since the project evaluation. For example, Maven publications
 * are statically defined and evaluated before the Nyx inference has run, so they would likely get the {@code undefined}
 * default value when reading the {@code project.version} property. When the plugin is applied to the {@code settings.gradle}
 * instead, the inference happens before the project is evaluated and the {@code version} property, along with other values
 * coming from the inference, are available earlier. Last, applying the plugin to an {@code init.gradle} script has the
 * same effect as {@code settings.gradle}, but the use case may be different depending on users' needs.
 * <br>
 * Please note that when applying the plugin in the {@code settings.gradle} or {@code init.gradle} file, the plugin is
 * also applied at the project level so that users don't need to apply it twice.
 * <br>
 * See <a href="https://docs.gradle.org/current/userguide/custom_plugins.html">Developing Custom Gradle Plugins</a> for an introduction
 * on custom Gradle plugin development.
 * 
 * @param <T> the type of object the plugin is applied to. The actual type is defined by Gradle and can be
 * {@link Project}, {@link Settings} or {@link Gradle} depending on how and where the plugin is applied.
 */
public class NyxPlugin <T> implements Plugin<T> {
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
     * Creates and registers all the plugin resources for the given target.
     * 
     * @param target the target the plugin is applied to and resources created into. More in the class level documentation on this.
     * 
     * @see Plugin#apply(Object)
     */
    @Override
    public void apply(T target) {
        Logger logger = Logging.getLogger(NyxPlugin.class);

        if (Project.class.isAssignableFrom(target.getClass())) {
            logger.debug("Applying the Nyx plugin to the project");

            applyToProject(Project.class.cast(target));

            logger.debug("The Nyx plugin has been applied to the project");
        }
        else if (Settings.class.isAssignableFrom(target.getClass())) {
            logger.debug("Applying the Nyx plugin to the settings");

            // apply the plugin to the project as soon as it's available
            Settings.class.cast(target).getGradle().rootProject(project -> applyToProject(project));

            logger.debug("The Nyx plugin has been applied to the settings");
        }
        else if (Gradle.class.isAssignableFrom(target.getClass())) {
            logger.debug("Applying the Nyx plugin to the initialization");

            // apply the plugin to the project as soon as it's available
            Gradle.class.cast(target).rootProject(project -> applyToProject(project));

            logger.debug("The Nyx plugin has been applied to the initialization");
        }
        else {
            logger.error("The Nyx plugin can't be applied to objects of type {}", target.getClass().getName());
        }
    }

    /**
     * Applies the plugin to the given project.
     * 
     * @param project the project to apply the plugin to.
     */
    protected static void applyToProject(Project project) {
        if (!Objects.isNull(project.getParent()))
            project.getLogger().warn("Nyx plugin should be applied to the root project only!");

        // Create the project extension. This must be done first as tasks
        // need it to be already available when they are defined
        createExtensions(project);

        // Define the tasks
        defineTasks(project);

        // Trigger the ingerence to happen early. This may be turned on/off by a conditional if needed
        triggerInference(project);
    }

    /**
     * Creates the plugin extensions for the project.
     * 
     * @param project the project to create the extension in
     */
    protected static void createExtensions(Project project) {
        project.getLogger().debug("Creating Nyx extension with name: {}", NyxExtension.NAME);

        NyxExtension.create(project);

        project.getLogger().debug("Nyx extension created with name: {}", NyxExtension.NAME);
    }

    /**
     * Sets up the tasks and dependencies to the project.
     * 
     * @param project the project to define the tasks in
     */
    protected static void defineTasks(Project project) {
        project.getLogger().debug("Defining Nyx tasks");

        // Define lifecycle tasks. These tasks are defined conditionally, only if they haven't been defined
        // elsewhere, i.e. by some other (core) plugin.
        // These tasks are define first to allow core tasks to set dependencies on lifecycle tasks, if they need so.
        ReleaseTask.conditionallyDefine(project);

        // Define core tasks
        ArrangeTask.define(project);
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

    /**
     * Forces the execution of the {@link InferTask} as soon as the plugin has been applied so that the outcomes
     * of the inference (like the {@code project.property}) are available as soon as possible.
     * 
     * @param project the project to trigger the inference for
     */
    protected static void triggerInference(Project project) {
        try {
            Task inferTask = project.getTasks().getByName(InferTask.NAME);
            for (Action<? super Task> action: inferTask.getActions()) {
                action.execute(inferTask);
            }
        }
        catch (Exception e) {
            project.getLogger().error("Nyx failed to infer in the early project stage and some property like the project version will not be available until you succesfully run the nyxInfer task. Failure is due to: {}", e.getMessage());
            project.getLogger().debug("Nyx failed to infer due to", e);
        }
    }
}
