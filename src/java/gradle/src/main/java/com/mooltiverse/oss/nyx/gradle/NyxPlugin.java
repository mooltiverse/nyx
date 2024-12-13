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

import static com.mooltiverse.oss.nyx.log.Markers.MAIN;

import java.util.Objects;

import org.gradle.api.Action;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.initialization.Settings;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;

/**
 * The main plugin class. This plugin can be applied to {@link Project} or {@link Settings} objects,
 * which means the {@link #apply(Object)} method can receive any of these object types. The actual type of the object
 * passed to the {@link #apply(Object)} method is defined by Gradle based on how the plugin is used, or better,
 * where the plugin is <a href="https://docs.gradle.org/current/userguide/plugins.html">applied</a>.
 * <br>
 * If the plugin is applied the common way, using the plugin DSL in the {@code build.gradle} file, then a {@link Project}
 * object is passed. If the plugin is used as a settings plugin using the plugin DSL in the {@code settings.gradle}
 * then a {@link Settings} object is passed. See
 * <a href="https://docs.gradle.org/current/userguide/custom_plugins.html#sec:writing_a_simple_plugin">Writing a simple plugin</a>
 * for more.
 * <br>
 * The main difference between the different options is that when the plugin is used as a regular project plugin
 * (applied in the {@code build.gradle}, so using a {@link Project} object) Nyx inference may happen too late as some
 * properties like the project {@code version} are needed since the project evaluation. For example, Maven publications
 * are statically defined and evaluated before the Nyx inference has run, so they would likely get the {@code undefined}
 * default value when reading the {@code project.version} property. When the plugin is applied to the {@code settings.gradle}
 * instead, the inference happens before the project is evaluated and the {@code version} property, along with other values
 * coming from the inference, are available earlier.
 * <br>
 * Please note that when applying the plugin in the {@code settings.gradle} the plugin is
 * also applied at the project level so that users don't need to apply it twice.
 * <br>
 * See <a href="https://docs.gradle.org/current/userguide/custom_plugins.html">Developing Custom Gradle Plugins</a> for an introduction
 * on custom Gradle plugin development.
 * 
 * @param <T> the type of object the plugin is applied to. The actual type is defined by Gradle and can be
 * {@link Project} or {@link Settings} depending on how and where the plugin is applied.
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
        if (Project.class.isAssignableFrom(target.getClass())) {
            Project project = Project.class.cast(target);
            project.getLogger().debug(MAIN, "Applying the Nyx plugin to the project...");

            if (!Objects.isNull(project.getParent()))
                project.getLogger().warn(MAIN, "Nyx plugin should be applied to the root project only!");

            // Create the project extension
            NyxExtension nyxExtension = NyxExtension.create(project);

            // Define the tasks
            defineTasks(project, nyxExtension);

            // if we want to trigger the inference as soon as the project is evaluated this can be uncommented (based on some conditional)
            // the outcome isn't like applying the plugin at the settings level though as 'afterEvaluate' happens later
            //project.afterEvaluate(p -> triggerInference(p));

            project.getLogger().debug(MAIN, "The Nyx plugin has been applied to the project");
        }
        else if (Settings.class.isAssignableFrom(target.getClass())) {
            Logger logger = Logging.getLogger(NyxPlugin.class);
            Settings settings = Settings.class.cast(target);
            logger.debug(MAIN, "Applying the Nyx plugin to the settings...");

            // Create the settings extension
            NyxExtension nyxExtension = NyxExtension.create(settings);

            // Define the tasks
            settings.getGradle().rootProject(p -> defineTasks(p, nyxExtension));
            // trigger the inference as soon as the project is evaluated
            settings.getGradle().rootProject(p -> triggerInference(p));

            logger.debug(MAIN, "The Nyx plugin has been applied to the settings");
        }
        else {
            Logging.getLogger(NyxPlugin.class).error(MAIN, "The Nyx plugin can't be applied to objects of type {}", target.getClass().getName());
        }
    }

    /**
     * Sets up the tasks and dependencies to the project.
     * 
     * @param project the project to define the tasks in
     * @param extension the extension to be passed to tasks
     */
    protected static void defineTasks(Project project, NyxExtension extension) {
        project.getLogger().debug(MAIN, "Registering Nyx core tasks for the project...");

        // Define core tasks
        project.getTasks().register(CleanTask.NAME,   CleanTask.class, extension);
        project.getTasks().register(InferTask.NAME,   InferTask.class, extension);
        project.getTasks().register(MakeTask.NAME,    MakeTask.class, extension);
        project.getTasks().register(MarkTask.NAME,    MarkTask.class, extension);
        project.getTasks().register(PublishTask.NAME, PublishTask.class, extension);

        // Define lifecycle tasks dependencies
        // These dependencies can't be defined inside each task's configure() method as it may lead
        // to unexpected behavior (see https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:task_configuration_avoidance_general, bullet #2).
        // The dependencies below make no difference whether the tasks are defined by this plugin or elsewhere.

        project.getLogger().debug(MAIN, "Registering Nyx lifecycle tasks for the project...");

        // Make the 'release' task dependent on the Publish task. There must be one as we define it
        // within the plugin if not already defined elsewhere.
        Task releaseLifecycleTask = project.getTasks().findByName(ReleaseTask.NAME);
        if (Objects.isNull(releaseLifecycleTask)) {
            releaseLifecycleTask = project.getTasks().create(ReleaseTask.NAME, ReleaseTask.class);
        }
        releaseLifecycleTask.dependsOn(PublishTask.NAME);

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
        project.getLogger().debug(MAIN, "Nyx tasks registered for the project");
    }

    /**
     * Runs the {@link InferTask} on the given project so that the outcomes of the inference
     * (like the {@code project.property}) are available as soon as possible.
     * 
     * @param project the project to run the inference for
     */
    protected static void triggerInference(Project project) {
        project.getLogger().debug(MAIN, "Triggering Nyx inference for the project...");

        Task inferTask = project.getTasks().getByName(InferTask.NAME);
        for (Action<? super Task> action: inferTask.getActions()) {
            action.execute(inferTask);
        }

        project.getLogger().debug(MAIN, "Nyx inference complete");
    }
}
