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

import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Stream;

import org.junit.jupiter.params.provider.Arguments;

/**
 * Test data to be shared among tests.
 */
public class TestData {
    /**
     * Default constructor is private on purpose.
     */
    private TestData() {
        super();
    }

    /**
     * A map of the available core tasks, where keys are task names and values are their classes.
     */
    static Map<String,Class<? extends CoreTask>> coreTasks = new HashMap<String,Class<? extends CoreTask>>(){
        private static final long serialVersionUID = 1L;
        {
            put(CleanTask.NAME,   CleanTask.class);
            put(InferTask.NAME,   InferTask.class);
            put(MakeTask.NAME,    MakeTask.class);
            put(MarkTask.NAME,    MarkTask.class);
            put(PublishTask.NAME, PublishTask.class);
        }
    };

    /**
     * A map of the available lifecycle tasks, where keys are task names and values are their classes.
     */
    static Map<String,Class<? extends LifecycleTask>> lifecycleTasks = new HashMap<String,Class<? extends LifecycleTask>>(){
        private static final long serialVersionUID = 1L;
        {
            put(ReleaseTask.NAME,   ReleaseTask.class);
        }
    };

    /**
     * A map of the all available tasks, where keys are task names and values are their classes.
     */
    static Map<String,Class<? extends AbstractTask>> allTasks = new HashMap<String,Class<? extends AbstractTask>>(){
        private static final long serialVersionUID = 1L;
        {
            putAll(coreTasks);
            putAll(lifecycleTasks);
        }
    };

    /**
     * A map that models core task direct dependencies, where keys are task names, and values are collections of the names of the tasks the key depends on.
     */
    static Map<String,Collection<String>> coreTaskEfferentDirectDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            put(CleanTask.NAME,   List.<String>of());
            put(InferTask.NAME,   List.<String>of());
            put(MakeTask.NAME,    List.<String>of());
            put(MarkTask.NAME,    List.<String>of());
            put(PublishTask.NAME, List.<String>of());
        }
    };

    /**
     * A map that models lifecycle task direct dependencies, where keys are task names, and values are collections of the names of the tasks the key depends on.
     */
    static Map<String,Collection<String>> lifecycleTaskEfferentDirectDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            put(ReleaseTask.NAME, List.<String>of(PublishTask.NAME));
        }
    };

    /**
     * A map that models task direct dependencies, where keys are task names, and values are collections of the names of the tasks the key depends on.
     */
    static Map<String,Collection<String>> allTaskEfferentDirectDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            putAll(coreTaskEfferentDirectDependencies);
            putAll(lifecycleTaskEfferentDirectDependencies);
        }
    };

    /**
     * A map that models core task dependencies, where keys are task names, and values are collections of the names of the tasks depending on the task.
     */
    /*static Map<String,Collection<String>> coreTaskAfferentDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            put(CleanTask.NAME,   List.<String>of());
            put(InferTask.NAME,   List.<String>of(ReleaseTask.NAME, PublishTask.NAME, MarkTask.NAME, MakeTask.NAME));
            put(MakeTask.NAME,    List.<String>of(ReleaseTask.NAME, PublishTask.NAME, MarkTask.NAME));
            put(MarkTask.NAME,    List.<String>of(ReleaseTask.NAME, PublishTask.NAME));
            put(PublishTask.NAME, List.<String>of(ReleaseTask.NAME));
        }
    };*/

    /**
     * A map that models lifecycle task dependencies, where keys are task names, and values are collections of the names of the tasks depending on the task.
     */
    /*static Map<String,Collection<String>> lifecycleTaskAfferentDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            put(ReleaseTask.NAME, List.<String>of());
        }
    };*/

    /**
     * A map that models task dependencies, where keys are task names, and values are collections of the names of the tasks depending on the task.
     */
    /*static Map<String,Collection<String>> allTaskAfferentDependencies = new HashMap<String,Collection<String>>(){
        private static final long serialVersionUID = 1L;
        {
            putAll(coreTaskAfferentDependencies);
            putAll(lifecycleTaskAfferentDependencies);
        }
    };*/

    /**
     * A stream that can be used in {@link MethodSource} that returns the core tasks.
     * Each returned argument has the fields:<br>
     * - taskName: the name of the task<br>
     * - taskClass: the class of the task<br>
     * - taskClassName: the simple class name of the task<br>
     *
     * @return a stream of arguments representing tasks
     * 
     * @see #coreTasks
     */
    static Stream<Arguments> coreTasksArguments() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Class<? extends CoreTask>> task: coreTasks.entrySet()) {
            arguments.add(Arguments.of(task.getKey(), task.getValue(), task.getValue().getSimpleName()));
        }
        return arguments.stream();
    }

    /**
     * A stream that can be used in {@link MethodSource} that returns the lifecycle tasks.
     * Each returned argument has the fields:<br>
     * - taskName: the name of the task<br>
     * - taskClass: the class of the task<br>
     * - taskClassName: the simple class name of the task<br>
     *
     * @return a stream of arguments representing tasks
     * 
     * @see #coreTasks
     */
    static Stream<Arguments> lifecycleTasksArguments() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Class<? extends LifecycleTask>> task: lifecycleTasks.entrySet()) {
            arguments.add(Arguments.of(task.getKey(), task.getValue(), task.getValue().getSimpleName()));
        }
        return arguments.stream();
    }

    /**
     * A stream that can be used in {@link MethodSource} that returns all tasks.
     * Each returned argument has the fields:<br>
     * - taskName: the name of the task<br>
     * - taskClass: the class of the task<br>
     * - taskClassName: the simple class name of the task<br>
     *
     * @return a stream of arguments representing tasks
     * 
     * @see #coreTasks
     */
    static Stream<Arguments> allTasksArguments() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Class<? extends AbstractTask>> task: allTasks.entrySet()) {
            arguments.add(Arguments.of(task.getKey(), task.getValue(), task.getValue().getSimpleName()));
        }
        return arguments.stream();
    }

    /**
     * A stream that can be used in {@link MethodSource} that returns the standard lifecycle tasks
     * and their dependencies on core tasks. This is used to test the behavior of our plugin and
     * its tasks with respect to externally defined standard lifecycle tasks.
     * 
     * These standard lifecycle tasks are not created by our plugin but may be defined by other
     * external plugins (i.e. the Gradle Base plugin).
     * 
     * Each returned argument has the fields:<br>
     * - taskName: the name of the standard lifecycle task<br>
     * - taskDependencies: a list of core task names the lifecycle task depends on<br>
     *
     * @return a stream of arguments representing standard lifecycle tasks and their dependencies
     * 
     * @see #coreTasks
     */
    static Stream<Arguments> standardLifecycleTasksDependencies() {
        return Stream.of(
            arguments("release", List.<String>of(PublishTask.NAME)),
            arguments("clean", List.<String>of(CleanTask.NAME)),
            arguments("assemble", List.<String>of(MakeTask.NAME))
        );
    }
}
