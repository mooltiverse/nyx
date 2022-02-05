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

import java.util.List;
import java.util.Objects;

import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.tasks.TaskContainer;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests the Gradle task.<br>
 * 
 * This test class tests the {@link AbstractTask} class and also provides generic methods used by subclasses.
 */
@DisplayName("AbstractTask")
public abstract class AbstractTaskTests extends AbstractTests {
    /**
     * Makes sure the task with the given name or class is not available using both eager and lazy methods.
     * 
     * This method uses assumptions instead of assertions to make a sanity check before tests are executed
     * on a task that is yet to be created or configured.
     * 
     * @param <T> the task class
     * @param project the project to test against
     * @param name the task name
     * @param type the task class
     */
    protected static <T extends AbstractTask> void testForTaskUnavailability(Project project, String name, Class<T> type) {
        // Make sure the tasks isn't there before the plugin is applied or the task created or registered
        // Only safe methods are used

        // ... with Project.getTasksByName(...)
        assumeTrue(project.getTasksByName(name, true).isEmpty());

        // ... with TaskContainer (Project.getTasks())...
        assumeTrue(Objects.isNull(project.getTasks().findByName(name)));
        assumeTrue(project.getTasks().withType(type).isEmpty());
    }

    /**
     * Tests that the task has been defined lazily, which means it is available but its actual creation is
     * deferred until it's acually needed, according to <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html">Configuration Avoidance</a>.
     * 
     * The methods used by this test are lazy so they do not trigger the task creation but, instead, they just
     * check the task is available to be created when needed.
     * 
     * For more on the eager and lazy methods see <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:old_vs_new_configuration_api_overview">this table</a>.
     * 
     * Subclasses need to invoke this method right after the task has been defined lazily (using the {@link TaskContainer#register(String, Class)} method)
     * and before any other method triggering the actual task creation.
     * 
     * @param <T> the task class
     * @param project the project to test against
     * @param name the name the task is registered with
     * @param type the task class
     */
    protected static <T extends AbstractTask> void testForTaskAvailabilityLazily(Project project, String name, Class<T> type) {
        // I couldn't find any method to tell if the object has been configured and not yet created.
        // Comments below on single method invocation.

        // named() always returns a Provider, which may or not return the task depending on its status
        assertNotNull(project.getTasks().named(name)); 

        // I'd expect isPresent() to return true only if the object has been actually created, false otherwise
        // Instead it always returns true, even if the task has only been configured
        //assertFalse(project.getTasks().named(name).isPresent());
        assertTrue(project.getTasks().named(name).isPresent());

        // Likewise, I'd expect getOrNull() to return null until the object has been actually created
        // but instead it returns the task anyway.
        //assertNull(project.getTasks().named(name).getOrNull());
        assertNotNull(project.getTasks().named(name).getOrNull());
    }

    /**
     * Tests that the task has been defined and tries to use it using eager methods (those that trigger the task creation
     * regardless of whether they have been lazily or eagerly defined).
     * The methods used by this test are eager as they do not make differences about
     * <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html">Configuration Avoidance</a>.
     * 
     * For more on the eager and lazy methods see <a href="https://docs.gradle.org/current/userguide/task_configuration_avoidance.html#sec:old_vs_new_configuration_api_overview">this table</a>.
     * 
     * Subclasses need to invoke this method right after the task has been defined.
     * 
     * @param <T> the task class
     * @param project the project to test against
     * @param name the name the task is registered with
     * @param type the task class
     */
    protected static <T extends AbstractTask> void testForTaskAvailabilityEagerly(Project project, String name, Class<T> type) {
        // ... with TaskContainer (Project.getTasks())...
        assertNotNull(project.getTasks().findByName(name));
        assertTrue(type.isInstance(project.getTasks().findByName(name)));
        assertNotNull(project.getTasks().getAt(name));
        assertTrue(type.isInstance(project.getTasks().getAt(name)));
        assertNotNull(project.getTasks().getByName(name));
        assertTrue(type.isInstance(project.getTasks().getByName(name)));
        assertEquals(1, project.getTasks().withType(type).size());
    }

    /**
     * Performs checks on the task status after it's been configured.
     */
    @Nested
    @DisplayName("Configuration")
    static class ConfigurationTests {
        @ParameterizedTest(name = "{2}.getDescription() == {2}.DESCRIPTION")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#allTasksArguments")
        void getDescriptionTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, true);
            
            assertEquals(taskClass.getDeclaredField("DESCRIPTION").get(null), project.getTasks().getByName(taskName).getDescription());
        }

        @ParameterizedTest(name = "{2}.getGroup() == {2}.FROUP")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#allTasksArguments")
        void getGroupTest(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, true);

            assertEquals(AbstractTask.GROUP, project.getTasks().getByName(taskName).getGroup());
        }

        @ParameterizedTest(name = "{2}.getDependencies().size() == <# of known direct dependencies>")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#allTasksArguments")
        void getDependencyCount(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, true);
            Task task = project.getTasks().getByName(taskName);

            assertEquals(task.getTaskDependencies().getDependencies(task).size(), TestData.allTaskEfferentDirectDependencies.get(taskName).size(), String.format("Task '%s' is expected to have '%d' dependencies but has '%d'", taskName, TestData.allTaskEfferentDirectDependencies.get(taskName).size(), task.getTaskDependencies().getDependencies(task).size()));
        }

        @ParameterizedTest(name = "{2}.getDependencies().contains(<all known direct efferent dependencies>)")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#allTasksArguments")
        void getDependency(String taskName, Class<? extends CoreTask> taskClass, String taskClassSimpleName)
            throws Exception {
            Project project = newTestProject(null, true);
            Task task = project.getTasks().getByName(taskName);
            
            for (String dependency: TestData.allTaskEfferentDirectDependencies.get(taskName)) {
                boolean dependencyFound = false;
                for (Task taskDependency: task.getTaskDependencies().getDependencies(task)) {
                    if (dependency.equals(taskDependency.getName()))
                        dependencyFound = true;
                }
                assertTrue(dependencyFound);
            }
        }

        @ParameterizedTest(name = "Task(''{0}'').getDependencies().contains(<all known direct efferent dependencies on core tasks>)")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.TestData#standardLifecycleTasksDependencies")
        void getStandardLifecycleTaskDependency(String taskName, List<String> taskDependencies)
            throws Exception {
            Project project = newTestProject(null, false);

            // the task is created on the fly to simulate it was there from other plugins
            Task task = project.getTasks().create(taskName);
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
            
            for (String dependency: taskDependencies) {
                boolean dependencyFound = false;
                for (Task taskDependency: task.getTaskDependencies().getDependencies(task)) {
                    if (dependency.equals(taskDependency.getName()))
                        dependencyFound = true;
                }
                assertTrue(dependencyFound);
            }
        }
    }
}
