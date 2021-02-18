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
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.Objects;
import java.util.stream.Stream;

import org.gradle.api.Project;
import org.gradle.api.Task;

import org.gradle.testfixtures.ProjectBuilder;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("InferTask")
public class InferTaskTests extends CoreTaskTests {
    /**
     * A {@link MethodSource} method that returns valid dependencies for this task.
     * Each returned argument has the fields:<br>
     * - taskName: the name of the task that represent the dependency<br>
     * - taskClass: the class of the task that represent the dependency, available for tasks provided by this plugin only,
     *   <code>null</code> for tasks defined elsewhere (i.e. lifecycle tasks provided by other plugins)<br>
     *
     * @return a stream of arguments representing dependencies
     */
    static Stream<Arguments> wellKnownTaskEfferentDependencies() {
        return Stream.of(
            arguments(AmendTask.NAME, AmendTask.class)
        );
    }

    /**
     * A {@link MethodSource} method that returns the tasks that have dependencies on this one.
     * Each returned argument has the fields:<br>
     * - taskName: the name of the dependent task<br>
     * - taskClass: the class of the dependent task, available for tasks provided by this plugin only,
     *   <code>null</code> for tasks defined elsewhere (i.e. lifecycle tasks provided by other plugins)<br>
     *
     * @return a stream of arguments representing dependencies
     */
    static Stream<Arguments> wellKnownTaskAfferentDependencies() {
        return Stream.of(
            arguments(MakeTask.NAME, MakeTask.class)
        );
    }

    /**
     * Performs checks on the task at the time it is defined.
     * 
     * Tests are available for both cases: when the task is defined via NyxPlugin.apply(...) and when it's defined alone via define(Project).
     */
    @Nested
    @DisplayName("PublishTask.define")
    class DefineTests {
        /**
         * Tests the task using eager methods.
         * 
         * Eager methods are those that return the Task and its related values regardless
         * of whether the Task is using Configuration Avoidance (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)
         * or not.
         * 
         * Eager methods create an instance of the Task even if it hasn't been used yet.
         */
        @Test
        @DisplayName("NyxPlugin.apply() to call InferTask.define() -> test eager task creation")
        void defineViaNyxPluginApplyEagerTest() {
            Project project = ProjectBuilder.builder().build();

            // pre-flight sanity checks
            testForTaskUnavailability(project, InferTask.NAME, InferTask.class);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // run the eager tests using the superclass method
            testForTaskAvailabilityEagerly(project, InferTask.NAME, InferTask.class);
        }

        /**
         * Tests the task using lazy methods.
         * 
         * Lazy methods are those that comply with
         * Configuration Avoidance (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)
         * and only return the task when actually needed or, instead of the actual task, return a Provider
         * object, which is in charge of managing its deferred creation.
         */
        @Test
        @DisplayName("NyxPlugin.apply() to call InferTask.define() -> test deferred task creation")
        void defineViaNyxPluginApplyLazyTest() {
            Project project = ProjectBuilder.builder().build();

            // pre-flight sanity checks
            testForTaskUnavailability(project, InferTask.NAME, InferTask.class);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // run the lazy tests using the superclass method
            testForTaskAvailabilityLazily(project, InferTask.NAME, InferTask.class);

            // now also run eager tests, which would invalidate the lazy tests if executed after this
            testForTaskAvailabilityEagerly(project, InferTask.NAME, InferTask.class);
        }
    }

    /**
     * Performs checks on the task configuration.
     */
    @Nested
    @DisplayName("InferTask.configure")
    class ConfigureTests {
        @Test
        @DisplayName("InferTask.getDescription()")
        void descriptionTest() {
            Project project = ProjectBuilder.builder().build();
            project.getPluginManager().apply(NyxPlugin.ID);

            assertEquals(InferTask.DESCRIPTION, project.getTasks().getByName(InferTask.NAME).getDescription());
        }

        @Test
        @DisplayName("InferTask.getGroup()")
        void groupTest() {
            Project project = ProjectBuilder.builder().build();
            project.getPluginManager().apply(NyxPlugin.ID);

            assertEquals(InferTask.GROUP, project.getTasks().getByName(InferTask.NAME).getGroup());
        }

        @Test
        @DisplayName("InferTask.getDependencies().size() >= known dependencies")
        void getDependencyCount() {
            Project project = ProjectBuilder.builder().build();
            project.getPluginManager().apply(NyxPlugin.ID);

            Task task = project.getTasks().getByName(InferTask.NAME);

            assertTrue(task.getTaskDependencies().getDependencies(task).size() >= wellKnownTaskEfferentDependencies().count());
            
        }

        @ParameterizedTest(name = "InferTask.getDependencies() contains ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.InferTaskTests#wellKnownTaskEfferentDependencies")
        void getDependency(String name, Class<?> clazz) {
            Project project = ProjectBuilder.builder().build();
            project.getPluginManager().apply(NyxPlugin.ID);
            Task task = project.getTasks().getByName(InferTask.NAME);

            boolean dependencyFound = false;
            for (Task dependency: task.getTaskDependencies().getDependencies(task)) {
                if (name.equals(dependency.getName()))
                    dependencyFound = true;
            }
            assertTrue(dependencyFound);
        }

        @ParameterizedTest(name = "Project.getTasks().findByName(''{0}'').getDependencies() contains InferTask")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.InferTaskTests#wellKnownTaskAfferentDependencies")
        void testAfferentDependency(String name, Class<?> clazz) {
            Project project = ProjectBuilder.builder().build();

            // if it's an external plugin (clazz == null) and it's not yet available, just create it before applying the plugin
            if (Objects.isNull(clazz) && Objects.isNull(project.getTasks().findByName(name)))
                project.task(name);

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // Retrieve the dependent task
            Task task = project.getTasks().getByName(name);
            assumeFalse(Objects.isNull(task));
            boolean dependencyFound = false;
            for (Task dependency: task.getTaskDependencies().getDependencies(task)) {
                if (InferTask.NAME.equals(dependency.getName()))
                    dependencyFound = true;
            }
            assertTrue(dependencyFound);
        }
    }
}
