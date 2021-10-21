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
import java.io.FileReader;
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
import com.mooltiverse.oss.nyx.git.Script;

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
        "7.2", "7.1.1", "7.1", "7.0.2", "7.0.1", "7.0",

        // - version "6.5" has a bug (https://github.com/gradle/gradle/issues/13367) that prevents us to test, fixed in "6.5.1"
        "6.9.1", "6.9", "6.8.3", "6.8.2", "6.8.1", "6.8", "6.7.1", "6.7", "6.6.1", "6.6", "6.5.1", /*"6.5",*/ "6.4.1", "6.4", "6.3", "6.2.2", "6.2.1", "6.2", "6.1.1", "6.1", "6.0.1", "6.0",

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
                put("maven-publish", "");
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
     * - gradle version: a gradle version to test against<br>
     * - a map where names are plugin IDs and values their versions (versions are empty for core plugins)
     *
     * @return a stream of arguments representing test suites
     */
    static Stream<Arguments> wellKnownTestSuites() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (String gradleversion: wellKnownWorkingGradleVersionsArray) {
            for (Map<String,String> pluginCombination: wellKnownWorkingPluginCombinations) {
                arguments.add(Arguments.of(gradleversion, pluginCombination));
            }
        }
        return arguments.stream();
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test gradle tasks.
     * This is actually a combination of other values.
     * 
     * Each returned argument has the fields:<br>
     * - task: the task to run against the script<br>
     * - gradle version: a gradle version to test against<br>
     * - a map where names are plugin IDs and values their versions (versions are empty for core plugins)
     * - outcomes: a map where names are task names and values are the expected outcome for each task, as defined by the org.gradle.testkit.runner.TaskOutcome enum<br>
     *
     * @return a stream of arguments representing test suites
     */
    static Stream<Arguments> wellKnownTaskTestSuites() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Map<String,TaskOutcome>> taskAndOutcome: wellKnownTasksAndOutcomes.entrySet()) {
            for (String gradleversion: wellKnownWorkingGradleVersionsArray) {
                for (Map<String,String> pluginCombination: wellKnownWorkingPluginCombinations) {
                    arguments.add(Arguments.of(taskAndOutcome.getKey(), gradleversion, pluginCombination, taskAndOutcome.getValue()));
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
     * Returns the content of the given file in a string
     */
    static String fileContent(File file)
        throws Exception {
        StringWriter writer = new StringWriter();
        FileReader reader = new FileReader(file);
        reader.transferTo(writer);
        reader.close();
        return writer.toString();
    }

    /**
     * Returns a string with a valid content for plugin extension.
     * 
     * @param bump the value to set for the {@code bump} option in the extension. If {@code null} no value is set.
     * @param preset the value to set for the {@code preset} option in the extension. If {@code null} a custom configuration is created.
     * 
     * @return a string with a valid content for plugin extension
     */
    static String gradleExtension(String bump, String preset) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println("nyx {");
        if (!Objects.isNull(bump))
            printWriter.println("  bump = '"+bump+"'");
        if (Objects.isNull(preset)) {
            printWriter.println("  commitMessageConventions {");
            printWriter.println("      enabled = [ 'conventionalCommits' ]");
            printWriter.println("      items {");
            printWriter.println("        conventionalCommits {");
            printWriter.println("          expression = '(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\\\((?<scope>[a-z ]+)\\\\))?:( (?<title>.+))$(?s).*'");
            printWriter.println("          bumpExpressions {");
            printWriter.println("            patch = '(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*'");
            printWriter.println("            minor = '(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*'");
            printWriter.println("            major = '(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*'");
            printWriter.println("          }");
            printWriter.println("        }");
            printWriter.println("      }");
            printWriter.println("    }");
            //printWriter.println("  configurationFile = '.nyx.json'");
            //printWriter.println("  directory = file('project/directory')");
            printWriter.println("  dryRun = false");
            printWriter.println("  initialVersion = '0.1.0'");
            //printWriter.println("  preset = 'extended'");
            printWriter.println("  releasePrefix = ''");
            printWriter.println("  releaseLenient = true");
            printWriter.println("  releaseTypes {");
            printWriter.println("    enabled = [ 'main', 'prerelease', 'internal' ]");
            printWriter.println("    items {");
            printWriter.println("      main {");
            printWriter.println("        collapseVersions = false");
            printWriter.println("        filterTags = '^({{ configuration.releasePrefix }})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)$'");
            printWriter.println("        gitCommit = 'true'");
            printWriter.println("        gitCommitMessage = 'This commit is for {{ version }}'");
            printWriter.println("        gitPush = 'true'");
            printWriter.println("        gitTag = 'true'");
            printWriter.println("        gitTagMessage = 'Tagging release {{ version }}'");
            printWriter.println("        matchBranches = '^(master|main)$'");
            /*printWriter.println("        matchEnvironmentVariables {");
            printWriter.println("          USER = '.*'");
            printWriter.println("        }");*/
            printWriter.println("        matchWorkspaceStatus = 'CLEAN'");
            printWriter.println("        publish = 'true'");
            printWriter.println("        versionRange = ''");
            printWriter.println("        versionRangeFromBranchName = false");
            printWriter.println("      }");
            printWriter.println("      prerelease {");
            printWriter.println("        collapseVersions = true");
            printWriter.println("        collapsedVersionQualifier = '{{ branch }}'");
            printWriter.println("        filterTags = '^({{ configuration.releasePrefix }})?([0-9]\\\\d*)\\\\.([0-9]\\\\d*)\\\\.([0-9]\\\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\\\.([0-9]\\\\d*))?)?$'");
            printWriter.println("        gitCommit = 'true'");
            printWriter.println("        gitCommitMessage = 'Committing pre-release {{ version }}'");
            printWriter.println("        gitPush = 'true'");
            printWriter.println("        gitTag = 'true'");
            printWriter.println("        gitTagMessage = 'Tagging pre-release {{ version }}'");
            printWriter.println("        matchBranches = '^(?!(master|main)).*$'");
            printWriter.println("        identifiers {");
            printWriter.println("          enabled = [ 'build' ]");
            printWriter.println("          items {");
            printWriter.println("            build {");
            printWriter.println("              qualifier = 'build'");
            printWriter.println("              value = '12'");    // let's hardcode a build number here
            printWriter.println("              position = 'BUILD'");
            printWriter.println("            }");
            printWriter.println("          }");
            printWriter.println("        }");
            printWriter.println("        matchBranches = '.*'"); // match any branch
            printWriter.println("        matchEnvironmentVariables {");
            printWriter.println("          USER = '.*'");
            printWriter.println("          PATH = '.*'"); // the PATH environment variable must be defined with any value
            printWriter.println("        }");
            printWriter.println("        matchWorkspaceStatus = 'CLEAN'");
            printWriter.println("        publish = 'false'");
            printWriter.println("        versionRange = ''");
            printWriter.println("        versionRangeFromBranchName = false");
            printWriter.println("      }");
            printWriter.println("      internal {");
            printWriter.println("        collapseVersions = true");
            printWriter.println("        collapsedVersionQualifier = '{{ branch }}'");
            printWriter.println("        gitCommit = 'false'");
            printWriter.println("        matchBranches = '^(?!(master|main)).*$'");
            printWriter.println("        identifiers {");
            printWriter.println("          enabled = [ 'internal', 'user' ]");
            printWriter.println("          items {");
            printWriter.println("            internal {");
            printWriter.println("              qualifier = 'internal'");
            printWriter.println("              position = 'BUILD'");
            printWriter.println("            }");
            printWriter.println("            user {");
            printWriter.println("              qualifier = 'user'");
            printWriter.println("              value = '{{ environment.user }}'");    // take the user name from the environment variable
            printWriter.println("              position = 'BUILD'");
            printWriter.println("            }");
            printWriter.println("          }");
            printWriter.println("        }");
            printWriter.println("        publish = 'false'");
            printWriter.println("      }");
            printWriter.println("    }");
            printWriter.println("  }");
            printWriter.println("  resume = true");
            printWriter.println("  scheme = 'SEMVER'");
            //printWriter.println("  sharedConfigurationFile = 'shared.config.json'");
            //printWriter.println("  stateFile = '.nyx-state.yml'");    // do not output the file to avoid polluting the working directory
            printWriter.println("  verbosity = 'INFO'");                // this is ignored with Gradle
            //printWriter.println("  version = '1.8.12'");              // let Nyx infer the version
        }
        else printWriter.println("  preset = '"+preset+"'");
        printWriter.println("}");
        return stringWriter.toString();
    }

    /**
     * Returns a string with a valid content for the gradle.settings file.
     * 
     * @param gradleVersion the Gradle version to use for the file
     * @param applyPlugin when {@code true} the Nyx plugin is applied in the settings (as a settings plugin),
     * otherwise the settings file just contains a plain definition of the project (and the Nyx plugin will need to be applied in the build file)
     * @param bump the value to set for the {@code bump} option in the extension. If {@code null} no value is set.
     * If {@code applyPlugin} is {@code false} this parameter is ignored.
     * @param preset the value to set for the {@code preset} option in the extension. If {@code null} a custom configuration is created.
     * Ignored when {@code applyPlugin} is {@code false}.
     * 
     * @return a string with a valid content for the gradle.settings file
     */
    static String gradleSettings(String gradleVersion, boolean applyPlugin, String bump, String preset) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        if (applyPlugin) {
            // apply the settings plugin
            printWriter.println("plugins {");
            printWriter.println("  id 'com.mooltiverse.oss.nyx'");
            printWriter.println("}");
        }

        printWriter.println("rootProject.name = 'nyx-gradle-"+gradleVersion+"-plugin-test'");

        if (applyPlugin) {
            // configure the settings extension
            printWriter.println(gradleExtension(bump, preset));
        }

        return stringWriter.toString();
    }

    /**
     * Returns a string with a valid content for the build.gradle file. The returned file the plugins block plus a simple extension configuration.
     * 
     * @param gradleVersion the Gradle version to use for the file
     * @param applyPlugin when {@code true} the Nyx plugin is applied in the build (as a project plugin),
     * otherwise the settings file just contains a plain definition of the build (and the Nyx plugin will need to be applied in the settings file)
     * @param plugins an optional map of plugins to apply, where names are plugin IDs and values are their versions. The version may be {@code null}
     * or empty for core plugins. If the entire map is {@code null} no plugins are applied. The Nyc plugin is added by default and doesn't need to be added.
     * @param bump the value to set for the {@code bump} option in the extension. If {@code null} no value is set.
     * If {@code applyPlugin} is {@code false} this parameter is ignored.
     * @param preset the value to set for the {@code preset} option in the extension. If {@code null} a custom configuration is created.
     * Ignored when {@code applyPlugin} is {@code false}.
     * 
     * @return a string with a valid content for the build.gradle file
     */
    static String gradleBuild(String gradleVersion, boolean applyPlugin, Map<String, String> plugins, String bump, String preset) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println("plugins {");
        if (applyPlugin) {
            // apply the project plugin
            printWriter.println("  id 'com.mooltiverse.oss.nyx'");
        }
        if (!Objects.isNull(plugins)) {
            for (Map.Entry<String, String> entry: plugins.entrySet()) {
                printWriter.println("  id '"+entry.getKey()+"' ");
                if (!Objects.isNull(entry.getValue()) && !entry.getValue().isEmpty())
                    printWriter.println(" version '"+entry.getValue()+"'");
                printWriter.println();
            }
        }
        printWriter.println("}");
        printWriter.println();

        if (applyPlugin) {
            // configure the project extension
            printWriter.println(gradleExtension(bump, preset));
        }

        // add custom tasks used for debug and diagnostics
        printWriter.println("task writeDiagnostics() {");
        printWriter.println("    project.file('diag-early-version.txt').write project.version");
        printWriter.println("    doLast {");
        printWriter.println("        project.file('diag-late-version.txt').write project.version");
        printWriter.println("    }");
        printWriter.println("}");
        printWriter.println("task writeStateDiagnostics() {");
        printWriter.println("    dependsOn nyxInfer");
        printWriter.println("    doLast {");
        printWriter.println("        project.file('state-bump.txt').write project.nyxState.bump");
        printWriter.println("        project.file('state-scheme.txt').write project.nyxState.directory.getAbsolutePath()");
        printWriter.println("        project.file('state-scheme.txt').write project.nyxState.scheme.toString()");
        printWriter.println("        project.file('state-timestamp.txt').write Long.valueOf(project.nyxState.timestamp).toString()");
        printWriter.println("        project.file('state-version.txt').write project.nyxState.version");
        printWriter.println("    }");
        printWriter.println("}");
        printWriter.println("task dummy() {");
        printWriter.println("}");

        return stringWriter.toString();
    }

    /**
     * Returns a string with a valid content for the .gitignore file. The returned file ignores the .gradle dir to avoid errors due to file locks.
     * 
     * @return a string with a valid content for the .gitignore file
     */
    static String gitIgnore() {
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        printWriter.println(".gitignore");
        printWriter.println(".gradle");
        printWriter.println("build.gradle");
        printWriter.println("settings.gradle");
        
        // the following files are those created for internal diagnostics by custom tasks
        printWriter.println("diag-*.txt");
        printWriter.println("state-*.txt");

        printWriter.println();

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
     * @oaram gitIgnoreFileContent the content of the .gitignore to create in the project directory
     * 
     * @return the runner to use for tests, already using the temporary directory
     * 
     * @throws Exception in case of any issue
     */
    GradleRunner setUp(File directory, String gradleVersion, String gradleSettingsFileContent, String gradleBuildFileContent, String gitIgnoreFileContent)
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

        // Create the .gitignore file
        write(new File(tempProjectDir, ".gitignore"), gitIgnoreFileContent);

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

    /**
     * Test running the given task with no exceptions using the given runner and Gradle version.
     * This is a generic method to be invoked by actual tests.
     * 
     * @param gradleRunner the gradle runner
     * @param gradleVersion the gradle version
     * @param target the task to run
     * @param taskOutcomes the map of outcomes to check, if {@code null} no outcomes are checked
     * 
     * @throws Exception in case of any issues
     */
    void runTask(GradleRunner gradleRunner, String gradleVersion, String target, Map<String,TaskOutcome> taskOutcomes)
        throws Exception {
        // GradleRunner.withDebug(boolean) enables debug output
        // also run the 'writeDiagnostics' to print diagnostics to files
        BuildResult gradleResult = gradleRunner.withDebug(false).withArguments("--info", "--stacktrace", target, "writeDiagnostics").build();
        System.out.println("Executed tasks: "+target);System.out.flush();

        if (!Objects.isNull(taskOutcomes)) {
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
    }

    /**
     * Test the plugin when applied to the project
     */
    @Nested
    @DisplayName("gradle project plugin")
    class NyxProjectPluginTests {
        /**
         * Test that an exception is thrown when running in a directory that contains no Git repository.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {1}, Plugin {2}, Script: empty] ==> {3}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTaskTestSuites")
        void exceptionRunningNyxTaskWithNoGitRepository(String target, String gradleVersion, Map<String,String> pluginCombination, Map<String,TaskOutcome> taskOutcomes)
            throws Exception {
            GradleRunner gradleRunner = setUp(null, gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", "extended"), gitIgnore());

            // Gradle wraps these exception so let's not make assumptions on the type
            assertThrows(Exception.class, () -> runTask(gradleRunner, gradleVersion, target, taskOutcomes));

            tearDown(gradleRunner);
        }

        /**
         * Test running the given task with no exceptions using the given Gradle version.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {1}, Plugins {2}, Script: empty] ==> {3}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTaskTestSuites")
        void runNyxTaskWithEmptyScriptTest(String target, String gradleVersion, Map<String,String> pluginCombination, Map<String,TaskOutcome> taskOutcomes)
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", "extended"), gitIgnore());

            runTask(gradleRunner, gradleVersion, target, taskOutcomes);

            tearDown(gradleRunner);
        }

        /**
         * Test running the given task with no exceptions using the given Gradle version.
         * 
         * This test runs using a configuration preset.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "gradle {0} [Gradle Version: {1}, Plugins {2}, Script: simple] ==> {3}")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTaskTestSuites")
        void runNyxTaskWithSimpleScriptWithPresetTest(String target, String gradleVersion, Map<String,String> pluginCombination, Map<String,TaskOutcome> taskOutcomes)
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", "extended"), gitIgnore());

            runTask(gradleRunner, gradleVersion, target, taskOutcomes);

            tearDown(gradleRunner);
        }

        /**
         * Test running a dummy task to produce the expected version, considering that nyxInfer runs
         * in the project afterEvaluate phase.
         * 
         * This test runs using a configuration preset.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "project plugin early infer with preset configuration [Gradle Version: {0}, Plugins {1}]: gradle dummy")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void earlyInferRunningDummyTaskWithPresetTest(String gradleVersion, Map<String,String> pluginCombination)
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", "extended"), gitIgnore());

            // do not run any nyx Task, just the tasks that write the version to a file, which must be already available
            runTask(gradleRunner, gradleVersion, "dummy", null);
            assertEquals("unspecified", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt"))); // the beforeEvaluate version
            assertEquals("unspecified", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt"))); // the afterEvaluate version

            tearDown(gradleRunner);
        }

        /**
         * Test running a dummy task to produce the expected version, considering that nyxInfer runs
         * in the project afterEvaluate phase.
         * 
         * This test runs using a configuration preset.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "project plugin early infer with preset configuration [Gradle Version: {0}, Plugins {1}]: gradle nyxInfer")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void earlyInferRunningInferWithPresetTest(String gradleVersion, Map<String,String> pluginCombination)
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", "extended"), gitIgnore());

            // just run the nyxInfer Task, just the tasks that write the version to a file, which must be already available
            runTask(gradleRunner, gradleVersion, "nyxInfer", null);
            assertEquals("unspecified", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt"))); // the beforeEvaluate version
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt"))); // the afterEvaluate version

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            tearDown(gradleRunner);
        }

        /**
         * Test running a dummy task to produce the expected version, considering that nyxInfer runs
         * in the project afterEvaluate phase.
         * 
         * This test runs using a custom configuration.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "project plugin early infer with custom configuration [Gradle Version: {0}, Plugins {1}]: gradle nyxInfer")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void earlyInferRunningInferWithCustomConfigurationTest(String gradleVersion, Map<String,String> pluginCombination)
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, false, null, null), gradleBuild(gradleVersion, true, pluginCombination, "minor", null), gitIgnore());

            // just run the nyxInfer Task, just the tasks that write the version to a file, which must be already available
            runTask(gradleRunner, gradleVersion, "nyxInfer", null);
            assertEquals("unspecified", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt"))); // the beforeEvaluate version
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt"))); // the afterEvaluate version

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            tearDown(gradleRunner);
        }
    }

    /**
     * Test the plugin when applied to the settings
     */
    @Nested
    @DisplayName("gradle settings plugin")
    class NyxSettingsPluginTests {
        /**
         * Test that applying the plugin at the settings level runs Infer before the project evaluation
         * and the version is already available at evaluation time.
         * 
         * This test runs using a configuration preset.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "settings plugin early infer with preset configuration [Gradle Version: {0}, Plugins {1}]: gradle dummy, gradle nyxInfer")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void earlyInferApplyingSettingsPluginWithPresetTest(String gradleVersion, Map<String,String> pluginCombination)
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, true, "minor", "extended"), gradleBuild(gradleVersion, false, pluginCombination, null, null), gitIgnore());

            // do not run any nyx Task, just the tasks that write the version to a file, which must be already available
            runTask(gradleRunner, gradleVersion, "dummy", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt")));
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt")));

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            // now run nyxInfer and make sure it makes no difference
            runTask(gradleRunner, gradleVersion, "nyxInfer", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt")));
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt")));

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            tearDown(gradleRunner);
        }

        /**
         * Test that applying the plugin at the settings level runs Infer before the project evaluation
         * and the version is already available at evaluation time.
         * 
         * This test runs using a custom configuration.
         * 
         * @throws Exception in case of any issues
         */
        @ParameterizedTest(name = "settings plugin early infer with custom configuration [Gradle Version: {0}, Plugins {1}]: gradle dummy, gradle nyxInfer")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownTestSuites")
        void earlyInferApplyingSettingsPluginWithCustomConfigurationTest(String gradleVersion, Map<String,String> pluginCombination)
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            script.addRemote(Scenario.FROM_SCRATCH.realize().getGitDirectory(), "origin");
            GradleRunner gradleRunner = setUp(script.getWorkingDirectory(), gradleVersion, gradleSettings(gradleVersion, true, "minor", null), gradleBuild(gradleVersion, false, pluginCombination, null, null), gitIgnore());

            // do not run any nyx Task, just the tasks that write the version to a file, which must be already available
            runTask(gradleRunner, gradleVersion, "dummy", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt")));
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt")));

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            // now run nyxInfer and make sure it makes no difference
            runTask(gradleRunner, gradleVersion, "nyxInfer", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-early-version.txt")));
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "diag-late-version.txt")));

            runTask(gradleRunner, gradleVersion, "writeStateDiagnostics", null);
            assertEquals("0.1.0", fileContent(new File(script.getWorkingDirectory(), "state-version.txt")));

            tearDown(gradleRunner);
        }
    }
}
