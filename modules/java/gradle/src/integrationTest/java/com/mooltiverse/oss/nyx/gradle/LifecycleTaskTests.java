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

import org.gradle.api.Project;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests the lifecycle Gradle task.<br>
 * 
 * This test class tests the {@link LifecycleTask} class and also provides generic methods used by subclasses.
 */
@DisplayName("LifecycleTask")
public class LifecycleTaskTests extends AbstractTaskTests {
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
        @ParameterizedTest(name = "NyxPlugin.apply() ==> {2}.conditionallyDefine() ==> {2} is eagerly available")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#lifecycleTasksArguments")
        void taskEagerlyAvailableAfterNyxPluginApplyTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName) 
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
        @ParameterizedTest(name = "NyxPlugin.apply() ==> {2}.conditionallyDefine() ==> {2} is lazily available")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#lifecycleTasksArguments")
        void taskLazilyAvailableAfterNyxPluginApplyTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, false);

            // pre-flight sanity checks
            testForTaskUnavailability(project, taskName, taskClass);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // run the lazy tests using the superclass method
            testForTaskAvailabilityLazily(project, taskName, taskClass);
        }

        /**
         * Tests the task is not created when another task with the same name already exists
         */
        @ParameterizedTest(name = "NyxPlugin.apply() ==> {2}.conditionallyDefine() does not override task with the same name")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#lifecycleTasksArguments")
        void conditionallyDefineViaNyxPluginApplyWhenTaskAlreadyExistsTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, false);

            // pre-flight sanity checks
            testForTaskUnavailability(project, taskName, taskClass);

            // now create a 'release' task by just its name
            project.task(taskName);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // make sure the project has no task with the ReleaseTask type
            // even if it has a task with the same name
            assertNotNull(project.getTasks().named(taskName));
            assertTrue(project.getTasks().withType(taskClass).isEmpty());
        }

        /**
         * Tests the task is not created when another task with the same name already exists
         */
        @ParameterizedTest(name = "{2}.conditionallyDefine() does not override task with the same name")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#lifecycleTasksArguments")
        void conditionallyDefineWhenTaskAlreadyExistsTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, false);

            // pre-flight sanity checks
            testForTaskUnavailability(project, taskName, taskClass);

            // now create a 'release' task by just its name
            project.task(taskName);

            // make sure the project has no task with the ReleaseTask type
            // even if it has a task with the same name
            assertNotNull(project.getTasks().named(taskName));
            assertTrue(project.getTasks().withType(taskClass).isEmpty());
        }
    }
}
