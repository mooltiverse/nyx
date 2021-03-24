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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.BuildTask;
import org.gradle.testkit.runner.GradleRunner;
import org.gradle.testkit.runner.TaskOutcome;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.git.Scenario;

/**
 * Functional tests for the Gradle plugin.<br>
 * 
 * This class uses TestKit and focuses on functional tests, while {@link NyxPluginTests} focuses on unit tests.
 * 
 * See <a href="https://docs.gradle.org/current/userguide/test_kit.html"Testing Build Logic with TestKit</a> for more.
 */
@DisplayName("NyxPlugin.Functional")
public class NyxPluginFunctionalTests {
    /**
     * An array of Gradle versions to succesfully test against.
     * 
     * The list is taken from https://gradle.org/releases/.
     */
    static String[] wellKnownWorkingGradleVersionsArray = new String[] {
        // Versions that are known to work
        // - version "6.5" has a bug (https://github.com/gradle/gradle/issues/13367) that prevents us to test, fixed in "6.5.1"
        "6.8.2", "6.8.1", "6.8", "6.7.1", "6.7", "6.6.1", "6.6", "6.5.1", /*"6.5",*/ "6.4.1", "6.4", "6.3", "6.2.2", "6.2.1", "6.2", "6.1.1", "6.1", "6.0.1", "6.0",

        /* Gradle versions prior than 6.0 fails to test with an exception like:
                > Could not find method services() for arguments [build_4o3mdmvy94ykemibox706yopu$_run_closure1$_closure2@18c3fdb5] on object of type com.mooltiverse.oss.nyx.gradle.NyxExtension.

           This means it has a different method for setting nested blocks into the extension object.
           If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
        //"5.6.4", "5.6.3", "5.6.2", "5.6.1", "5.6", "5.5.1", "5.5"
        
        /* Gradle versions prior than 5.5 do not support ObjectFactory.domainObjectContainer​(Class<T> elementType), indeed introduced in version 5.5,
           which is used for example in NyxExtension.
           If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
        //"5.4.1", "5.4", "5.3.1", "5.3", "5.2.1", "5.2"
        
        /* Gradle versions from 4.9 to 5.1.1 fail to test with an exception like:
                > Could not create an instance of type com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated.
                    > Could not find any public constructor for class com.mooltiverse.oss.nyx.gradle.NyxExtension_Decorated which accepts parameters [].

           This has to deal with the injection of the ObjectFactory in constructors (i.e. in the NyxExtension) and is solved by adding another
           constructor with no parameters, which in turn implies another workaround to get an ObjectFactory.
           If support for these versions is strongly needed we may find a workaround but it's worthless so far. */
        //"5.1.1", "5.1", "5.0", "4.10.3", "4.10.2", "4.10.1", "4.10", "4.9"
        
        /* Gradle versions prior than 4.9 do not support Conviguration Avoidance API (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)*/
        //"4.8.1", "4.8"

        /* Gradle version 4.7 fails to test with an exception like:
                Could not create service of type ScriptPluginFactory using BuildScopeServices.createScriptPluginFactory().
                > Could not create service of type PluginResolutionStrategyInternal using BuildScopeServices.createPluginResolutionStrategy().

           We're not investigating any further unless some user needs support for that version, which is quite outdated*/
        //"4.7",
        
        /* Gradle versions prior between 2.6 and 4.6 fail to test with an exception like:
                org.gradle.api.GradleException: Unable to start the daemon process.
                ...
                Could not create service of type DaemonContext using DaemonServices.createDaemonContext().

           We're not investigating any further unless some user needs support for those versions, which are quite outdated*/
        //"4.7", "4.6", "4.5.1", "4.5", "4.4.1", "4.4", "4.3.1", "4.3", "4.2.1", "4.2", "4.1", "4.0.2", "4.0.1", "4.0", 
        //"3.5.1", "3.5", "3.4.1", "3.4", "3.3", "3.2.1", "3.2", "3.1", "3.0",
        //"2.14.1", "2.14", "2.13", "2.12", "2.11", "2.10", "2.9", "2.8", "2.7", "2.6"
        
        /* Gradle versions prior than 2.6 are not supported by Testkit
           See: https://docs.gradle.org/current/userguide/test_kit.html#sub:test-kit-compatibility*/
        //"2.5", "2.4", "2.3", "2.2.1", "2.2", "2.1", "2.0",
        //"1.12", "1.11", "1.10", "1.9", "1.8", "1.7", "1.6", "1.5", "1.4", "1.3", "1.2", "1.1", "1.0",
        //"0.9.2", "0.9.1", "0.9", "0.8", "0.7",
    };

    /**
     * A {@link MethodSource} method that returns Gradle versions to test against.
     * Each returned argument is a version of Gradle to test the plugin with.
     *
     * @return a stream of arguments representing Gradle versions to test with
     * 
     * @see #wellKnownWorkingGradleVersionsArray()
     */
    static Stream<String> wellKnownWorkingGradleVersions() {
        return Stream.of(wellKnownWorkingGradleVersionsArray);
    }

    /**
     * A list of maps, where each list item is a map that represent a known combination of plugins. For each combination
     * of plugins (so for each map), every entry represents one plugin, where the key is the plugin ID and the value is
     * the (optonal) plugin version. Core plugins have an empty string in place of the version.
     * 
     * These plugins are tested to make sure they don't change Nyx's or the plugin's behavior.
     */
    static List<Map<String,String>> wellKnownWorkingPluginCombinations = new ArrayList<Map<String,String>>(){
        private static final long serialVersionUID = 1L;
        {
            // The list of core plugins is taken from https://docs.gradle.org/current/userguide/plugin_reference.html
            // Some are commented out just to narrow the test time. In the end we don't expect much conflicts with external
            // plugins unless some users give us notice of some in particular

            // note that we never had any issue due to plugins so most of them are commented out to not spend hourse in useless test repetitions

            // ----- no plugins
            add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
            }});

            // ----- single plugins
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("java", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("java-platform", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("groovy", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("scala", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("antlr", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("base", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("signing", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("java-gradle-plugin", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("project-report", "");
            }});*/

            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("cpp-application", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("cpp-library", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("cpp-unit-test", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("swift-application", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("swift-library", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("xctest", "");
            }});*/

            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("application", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("war", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("ear", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("maven-publish", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("ivy-publish", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("maven", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("distribution", "");   }
            });*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("java-library-distribution", "");
            }});*/

            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("checkstyle", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("pmd", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("jacoco", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("codenarc", "");
            }});*/

            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("eclipse", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("eclipse-wtp", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("idea", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("visual-studio", "");
            }});*/
            /*add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("xcode", "");
            }});*/

            // ----- plugins combinations
            add(new HashMap<String,String>(){   private static final long serialVersionUID = 1L; {
                put("base", "");
                put("java", "");
                put("antlr", "");
                put("application", "");
                put("maven", "");
            }});
        }
    };

    /**
     * A collection of tasks to run from gradle. For each task, whose name is the key in the map, there value is a nested map.
     * In the nested map, each key is a task name and the value is the expected outcome, modelled as a org.gradle.testkit.runner.TaskOutcome,
     * which is the status of the single task as the result of the execution. A null outcome means that the task should not have run.
     */
    static Map<String,Map<String,TaskOutcome>> wellKnownTasksAndOutcomes = new HashMap<String,Map<String,TaskOutcome>>(){
        private static final long serialVersionUID = 1L;
        {
            put(CleanTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, TaskOutcome.SUCCESS);
                    put(ArrangeTask.NAME, null);
                    put(InferTask.NAME, null);
                    put(MakeTask.NAME, null);
                    put(MarkTask.NAME, null);
                    put(PublishTask.NAME, null);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(ArrangeTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, null);
                    put(MakeTask.NAME, null);
                    put(MarkTask.NAME, null);
                    put(PublishTask.NAME, null);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(InferTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, TaskOutcome.SUCCESS);
                    put(MakeTask.NAME, null);
                    put(MarkTask.NAME, null);
                    put(PublishTask.NAME, null);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(MakeTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, TaskOutcome.SUCCESS);
                    put(MakeTask.NAME, TaskOutcome.SUCCESS);
                    put(MarkTask.NAME, null);
                    put(PublishTask.NAME, null);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(MarkTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, TaskOutcome.SUCCESS);
                    put(MakeTask.NAME, TaskOutcome.SUCCESS);
                    put(MarkTask.NAME, TaskOutcome.SUCCESS);
                    put(PublishTask.NAME, null);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(PublishTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, TaskOutcome.SUCCESS);
                    put(MakeTask.NAME, TaskOutcome.SUCCESS);
                    put(MarkTask.NAME, TaskOutcome.SUCCESS);
                    put(PublishTask.NAME, TaskOutcome.SUCCESS);
                    put(ReleaseTask.NAME, null);
                }
            });
            put(ReleaseTask.NAME, new HashMap<String,TaskOutcome>(){
                private static final long serialVersionUID = 1L;
                {
                    put(CleanTask.NAME, null);
                    put(ArrangeTask.NAME, TaskOutcome.SUCCESS);
                    put(InferTask.NAME, TaskOutcome.SUCCESS);
                    put(MakeTask.NAME, TaskOutcome.SUCCESS);
                    put(MarkTask.NAME, TaskOutcome.SUCCESS);
                    put(PublishTask.NAME, TaskOutcome.SUCCESS);
                    put(ReleaseTask.NAME, TaskOutcome.SUCCESS);
                }
            });
        }
    };

    /**
     * A {@link MethodSource} method that returns valid structured data to test gradle tasks.
     * This is actually a combination of other values.
     * 
     * Each returned argument has the fields:<br>
     * - task: the task to run against the script<br>
     * - outcomes: a map where names are task names and values are the expected outcome for each task, as defined by the org.gradle.testkit.runner.TaskOutcome enum<br>
     * - gradle version: a gradle version to test against<br>
     * - plugin id: the ID of a plugin to apply to the script<br>
     * - plugin version: the version of a plugin to apply to the script, may be empty for core plugins<br>
     * - positive files: a list of local path names to files that must exist after the run<br>
     * - negative files: a list of local path names to files that must NOT exist after the run<br>
     *
     * @return a stream of arguments representing test suites
     */
    static Stream<Arguments> wellKnownTestSuites() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Map<String,TaskOutcome>> taskAndOutcome: wellKnownTasksAndOutcomes.entrySet()) {
            for (String gradleversion: wellKnownWorkingGradleVersionsArray) {
                for (Map<String,String> pluginCombination: wellKnownWorkingPluginCombinations) {
                    // positive and negative files are not modelled yet so the two last parameters are empty strings
                    arguments.add(Arguments.of(taskAndOutcome.getKey(), taskAndOutcome.getValue(), gradleversion, pluginCombination, List.<String>of(), List.<String>of()));
                }
            }
        }
        return arguments.stream();
    }

    /**
     * Writes the given content to the given file.
     * 
     * @param destination the file to write to
     * @param content the content to write to the destination file
     * 
     * @throws IOException in case of any issue when writing the file
     */
    static void write(File destination, String content)
        throws IOException {
        FileWriter output = null;
        try {
            output = new FileWriter(destination);
            output.write(content);
        }
        finally {
            if (output != null) {
                output.close();
            }
        }
    }

    /**
     * Returns a string with a valid content for the gradle.settings file.
     * 
     * @param gradleVersion the Gradle version to use for the file
     * 
     * @return a string with a valid content for the gradle.settings file
     */
    static String gradleSettings(String gradleVersion) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println("rootProject.name = 'nyx-gradle-"+gradleVersion+"-plugin-test'");

        return stringWriter.toString();
    }

    /**
     * Returns a string with a valid content for the build.gradle file. The returned file only has the plugins block.
     * 
     * @param gradleVersion the Gradle version to use for the file
     * @param plugins an optional map of plugins to apply, where names are plugin IDs and values are their versions. The version may be {@code null}
     * or empty for core plugins. If the entire map is {@code null} no plugins are applied. The Nyc plugin is added by default and doesn't need to be added.
     * 
     * @return a string with a valid content for the build.gradle file
     */
    static String gradleEmptyBuild(String gradleVersion, Map<String, String> plugins) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println("plugins {");
        printWriter.println("  id 'com.mooltiverse.oss.nyx'");
        if (!Objects.isNull(plugins)) {
            for (Map.Entry<String, String> entry: plugins.entrySet()) {
                printWriter.println("  id '"+entry.getKey()+"' ");
                if (!Objects.isNull(entry.getValue()) && !entry.getValue().isEmpty())
                    printWriter.println(" version '"+entry.getValue()+"'");
                printWriter.println();
            }
        }
        printWriter.println("}");

        return stringWriter.toString();
    }

    /**
     * Returns a string with a valid content for the build.gradle file. The returned file the plugins block plus a simple extension configuration.
     * 
     * @param gradleVersion the Gradle version to use for the file
     * @param plugins an optional map of plugins to apply, where names are plugin IDs and values are their versions. The version may be {@code null}
     * or empty for core plugins. If the entire map is {@code null} no plugins are applied. The Nyc plugin is added by default and doesn't need to be added.
     * 
     * @return a string with a valid content for the build.gradle file
     */
    static String gradleSimpleBuild(String gradleVersion, Map<String, String> plugins) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println();
        // start from the empty build, with just the plugins defined
        printWriter.println(gradleEmptyBuild(gradleVersion, plugins));
        printWriter.println();

        // then add the extension configuration
        printWriter.println("nyx {");
        printWriter.println("  bump = 'minor'");
        printWriter.println("  dryRun = true");
        printWriter.println("  services {");
        printWriter.println("     github {");
        //printWriter.println("        provider = 'guesswhat'");
        printWriter.println("     }");
        printWriter.println("  }");
        printWriter.println("}");

        return stringWriter.toString();
    }

    /**
     * Instantiates a new GradleRunner to use for tests, using the given Gradle version.
     * A new temporary directory is created for each test and used by the runner if none is given.
     * The settings.gradle is created with a standard content while the build.gradle file is created with the given content.
     * 
     * @param directory the directory to create the runner in. If {@code null} a new one is created
     * @param gradleVersion the Gradle version to test against
     * @oaram gradleSettingsFileContent the content of the settings.gradle to create in the project directory
     * @oaram gradleBuildFileContent the content of the build.gradle to create in the project directory
     * 
     * @return the runner to use for tests, already using the temporary directory
     * 
     * @throws Exception in case of any issue
     */
    GradleRunner setUp(File directory, String gradleVersion, String gradleSettingsFileContent, String gradleBuildFileContent)
        throws Exception {
        
        File tempProjectDir = Objects.isNull(directory) ? Files.createTempDirectory("nyx-gradle-"+gradleVersion+"-test").toFile() : directory;

        // let the VM delete the directories on exit
        tempProjectDir.deleteOnExit(); 
        System.out.println("Set up tests into directory: "+tempProjectDir.getAbsolutePath());

        // do a couple extra checks to avoid messing up with the project dir
        assertFalse(Objects.isNull(tempProjectDir));
        assertTrue(tempProjectDir.exists());
        assertTrue(tempProjectDir.isDirectory());

        write(new File(tempProjectDir, "settings.gradle"), gradleSettingsFileContent);

        // Create the build.gradle file
        write(new File(tempProjectDir, "build.gradle"), gradleBuildFileContent);

        // withPluginClasspath() puts the Nyx plugin into the classpath
        return GradleRunner.create().withGradleVersion(gradleVersion).withProjectDir(tempProjectDir).withPluginClasspath().forwardOutput();
    }

    /**
     * Tries to clean up resources used for tests.
     * 
     * @param runner the Gradle runner used for the tests
     * 
     * @throws Exception in case of any issue
     */
    void tearDown(GradleRunner runner)
        throws Exception {
        System.out.println("Tear down tests into directory: "+runner.getProjectDir().getAbsolutePath());

        // the directory is cleaned up on exit as it was created with the deleteOnExit() option
        // nothing to do yet.
    }

    @Nested
    @DisplayName("gradle tasks")
    class TasksTests {
        /**
         * Test running 'gradle tasks' with no exceptions using the given runner and Gradle version.
         * This is a generic method to be invoked by actual tests.
         * 
         * @param gradleRunner the runner
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        void runGradleTasks(GradleRunner gradleRunner, String gradleVersion)
            throws Exception {
            assertDoesNotThrow(() -> gradleRunner.withArguments("tasks").build());
        }

        /**
         * Test running 'gradle tasks' with an empty build script.
         * 
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle tasks [Gradle Version: {0}, Script: empty]")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownWorkingGradleVersions")
        void runGradleTasksWithEmptyScriptTest(String gradleVersion)
            throws Exception {
            // use an empty directory as for running 'tasks' there must be no need for the Git repository
            GradleRunner gradleRunner = setUp(null, gradleVersion, gradleSettings(gradleVersion), gradleEmptyBuild(gradleVersion, null));

            runGradleTasks(gradleRunner, gradleVersion);

            tearDown(gradleRunner);
        }

        /**
         * Test running 'gradle tasks' with a simple build script.
         * 
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle tasks [Gradle Version: {0}, Script: simple]")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownWorkingGradleVersions")
        void runGradleTasksWithSimpleScriptTest(String gradleVersion)
            throws Exception {
            // use an empty directory as for running 'tasks' there must be no need for the Git repository
            GradleRunner gradleRunner = setUp(null, gradleVersion, gradleSettings(gradleVersion), gradleSimpleBuild(gradleVersion, null));

            runGradleTasks(gradleRunner, gradleVersion);

            tearDown(gradleRunner);
        }
    }

    @Nested
    @DisplayName("gradle nyx*")
    class NyxTaskTests {
        /**
         * Test running the given task with no exceptions using the given runner and Gradle version.
         * This is a generic method to be invoked by actual tests.
         * 
         * @throws Exception in case of any issues
         */
        void runNyxTask(GradleRunner gradleRunner, String target, Map<String,TaskOutcome> taskOutcomes, String gradleVersion, List<String> positiveFiles, List<String> negativeFiles)
            throws Exception {
            // GradleRunner.withDebug(boolean) enables debug output
            BuildResult gradleResult = gradleRunner.withDebug(false).withArguments("--info", "--stacktrace", target).build();
            System.out.println("Executed task: "+target);System.out.flush();

            for (Map.Entry<String,TaskOutcome> taskOutcome: taskOutcomes.entrySet()) {
                boolean taskFound = false;
                System.out.println("  Testing outcome for task: "+taskOutcome.getKey());System.out.flush();
                for (BuildTask buildTask: gradleResult.getTasks()) {
                    System.out.println("    Evaluating task: "+buildTask.getPath());System.out.flush();
                    if (buildTask.getPath().endsWith(taskOutcome.getKey())) {
                        taskFound = true;
                        System.out.println("      Task "+taskOutcome.getKey()+" match found. Outcome is: "+buildTask.getOutcome()+", expected was "+taskOutcome.getValue());System.out.flush();
                        assertEquals(taskOutcome.getValue(), buildTask.getOutcome(), "When running gradle "+target+" expected outcome for task "+buildTask.getPath()+" was "+taskOutcome.getValue()+" but actual value was "+buildTask.getOutcome());
                    }
                    else {
                        System.out.println("      Skipping task "+taskOutcome.getKey());System.out.flush();
                    }
                }
                System.out.println("  Task "+taskOutcome.getKey()+"="+taskOutcome.getValue()+" found: "+taskFound);System.out.flush();
                if (Objects.isNull(taskOutcome.getValue())) {
                    assertFalse(taskFound, "Task "+taskOutcome.getKey()+" was not expected to be part of the build but it was");
                }
                else assertTrue(taskFound, "Task "+taskOutcome.getKey()+" was expected to be part of the build but it was not");
            }
        }

        /**
         * Test that an exception is thrown when running in a directory that contains no Git repository.
         * 
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {2}, Plugin {3}, Script: empty] ==> {1}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void exceptionRunningNyxTaskWithNoGitRepository(String target, Map<String,TaskOutcome> taskOutcomes, String gradleVersion, Map<String,String> pluginCombination, List<String> positiveFiles, List<String> negativeFiles)
            throws Exception {
            GradleRunner gradleRunner = setUp(null, gradleVersion, gradleSettings(gradleVersion), gradleEmptyBuild(gradleVersion, pluginCombination));

            // Gradle wraps these exception so let's not make assumptions on the type
            assertThrows(Exception.class, () -> runNyxTask(gradleRunner, target, taskOutcomes, gradleVersion, positiveFiles, negativeFiles) );

            tearDown(gradleRunner);
        }

        /**
         * Test running the given task with no exceptions using the given Gradle version on an emty script.
         * 
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {2}, Plugins {3}, Script: empty] ==> {1}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void runNyxTaskWithEmptyScriptTest(String target, Map<String,TaskOutcome> taskOutcomes, String gradleVersion, Map<String,String> pluginCombination, List<String> positiveFiles, List<String> negativeFiles)
            throws Exception {
            GradleRunner gradleRunner = setUp(Scenario.INITIAL_COMMIT.realize().getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion), gradleEmptyBuild(gradleVersion, pluginCombination));

            runNyxTask(gradleRunner, target, taskOutcomes, gradleVersion, positiveFiles, negativeFiles);

            tearDown(gradleRunner);
        }

        /**
         * Test running the given task with no exceptions using the given Gradle version on a simple script.
         * 
         * @param gradleVersion the Gradle version to use
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {2}, Plugins {3}, Script: simple] ==> {1}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void runNyxTaskWithSimpleScriptTest(String target, Map<String,TaskOutcome> taskOutcomes, String gradleVersion, Map<String,String> pluginCombination, List<String> positiveFiles, List<String> negativeFiles)
            throws Exception {
            GradleRunner gradleRunner = setUp(Scenario.INITIAL_COMMIT.realize().getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion), gradleSimpleBuild(gradleVersion, pluginCombination));

            runNyxTask(gradleRunner, target, taskOutcomes, gradleVersion, positiveFiles, negativeFiles);

            tearDown(gradleRunner);
        }
    }
}
