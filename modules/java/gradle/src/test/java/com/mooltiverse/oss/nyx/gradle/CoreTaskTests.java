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

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import org.gradle.api.Action;
import org.gradle.api.Project;
import org.gradle.api.Task;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.git.script.JGitScript;

/**
 * Tests the core Gradle task.<br>
 * 
 * This test class tests the {@link CoreTask} class and also provides generic methods used by subclasses.
 */
@DisplayName("CoreTask")
public class CoreTaskTests extends AbstractTaskTests {
    /**
     * Performs checks on the task status after it's been configured.
     */
    @Nested
    @DisplayName("Definition")
    static class DefinitionTests {
        /**
         * Tests the task using eager methods.
         * 
         * Eager methods are those that return the Task and its related values regardless
         * of whether the Task is using Configuration Avoidance (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)
         * or not.
         * 
         * Eager methods create an instance of the Task even if it hasn't been used yet.
         */
        @ParameterizedTest(name = "NyxPlugin.apply() to call {2}.define() -> test eager task creation")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#coreTasksArguments")
        void defineViaNyxPluginApplyEagerTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName) 
            throws Exception {
            Project project = newTestProject(null, false);

            // pre-flight sanity checks
            testForTaskUnavailability(project, taskName, taskClass);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // run the eager tests using the superclass method
            testForTaskAvailabilityEagerly(project, taskName, taskClass);
        }

        /**
         * Tests the task using lazy methods.
         * 
         * Lazy methods are those that comply with
         * Configuration Avoidance (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)
         * and only return the task when actually needed or, instead of the actual task, return a Provider
         * object, which is in charge of managing its deferred creation.
         */
        @ParameterizedTest(name = "NyxPlugin.apply() to call {2}.define() -> test deferred task creation")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#coreTasksArguments")
        void defineViaNyxPluginApplyLazyTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, false);

            // pre-flight sanity checks
            testForTaskUnavailability(project, taskName, taskClass);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // run the lazy tests using the superclass method
            testForTaskAvailabilityLazily(project, taskName, taskClass);

            // now also run eager tests, which would invalidate the lazy tests if executed after this
            testForTaskAvailabilityEagerly(project, taskName, taskClass);
        }
    }

    /**
     * Performs checks on the working directory where the task runs.
     */
    @Nested
    @DisplayName("Working directory")
    static class WorkingDirectoryTests {
        @ParameterizedTest(name = "{2}.getActions().execute() throws Exception when running in a directory with no Git repository and without a 'directory' configuration option")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#coreTasksArguments")
        void exceptionOnActionsExecuteWithEmptyGitProjectDirectory(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            // the test project is created in a new empty directory
            Project project = newTestProject(null, false);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // Retrieve the dependent task
            Task task = project.getTasks().getByName(taskName);

            for (Action<? super Task> action: task.getActions()) {
                // Gradle wraps these exception so let's not make assumptions on the type
                assertThrows(Exception.class, () -> { action.execute(task); });
            }
        }

        @ParameterizedTest(name = "{2}.getActions().execute() runs without exceptions when running in a directory with no Git repository but with a 'directory' configuration option")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#coreTasksArguments")
        void testActionsExecuteWithoutExceptionsInEmptyGitProjectDirectoryAndValidDirectoryConfigurationOption(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
        throws Exception {
            // the test project is created in a new empty directory
            Project project = newTestProject(null, false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // a Git repository is created in a different temporary directory
            JGitScript gitScript = JGitScript.fromScratch(true);
    
            //make sure the Gradle working directory and the Git repository directory are not the same
            assumeFalse(project.getBuildDir().equals(gitScript.getWorkingDirectory()));
            assumeFalse(project.getBuildDir().getAbsolutePath().equals(gitScript.getWorkingDirectory().getAbsolutePath()));
    
            // the valid Git directory, different than the current working directory, is passed as the 'directory' configuration option through the extension
            project.getExtensions().getByType(NyxExtension.class).getDirectory().set(gitScript.getWorkingDirectory());
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(taskName);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);
            }
        }

        @ParameterizedTest(name = "{2}.getActions().execute() runs without exceptions when running in a directory with a valid Git repository")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#coreTasksArguments")
        void testActionsExecuteWithoutExceptionsInValidGitProjectDirectoryAndNoDirectoryConfigurationOption(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            JGitScript gitScript = JGitScript.fromScratch(true);
            Project project = newTestProject(gitScript.getWorkingDirectory(), false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(taskName);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);
            }
        }
    }
}
