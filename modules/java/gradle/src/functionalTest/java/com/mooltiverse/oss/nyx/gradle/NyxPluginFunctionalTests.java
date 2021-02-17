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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;
import java.util.stream.Stream;

import org.gradle.testkit.runner.BuildResult;
import org.gradle.testkit.runner.GradleRunner;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

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
     * An ordered list of Gradle versions to succesfully test against.
     * 
     * The list is taken from https://gradle.org/releases/.
     */
    static String[] wellKnownWorkingGradleVersionsArray = new String[] {
        // Versions that are known to work
        // - version "6.5" has a bug (https://github.com/gradle/gradle/issues/13367) that prevents us to test, fixed in "6.5.1"
        "6.8.2", "6.8.1", "6.8", "6.7.1", "6.7", "6.6.1", "6.6", "6.5.1", /*"6.5",*/ "6.4.1", "6.4", "6.3", "6.2.2", "6.2.1", "6.2", "6.1.1", "6.1", "6.0.1", "6.0",
        "5.6.4", "5.6.3", "5.6.2", "5.6.1", "5.6", "5.5.1", "5.5", "5.4.1", "5.4", "5.3.1", "5.3", "5.2.1", "5.2", "5.1.1", "5.1", "5.0",
        "4.10.3", "4.10.2", "4.10.1", "4.10", "4.9"
        
        /* Gradle versions prior than 4.9 do not support Conviguration AVoidance API (https://docs.gradle.org/current/userguide/task_configuration_avoidance.html)*/
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
     * Writes the given content to the given file.
     * 
     * @param destination the file to write to
     * @param content the content to write to the destination file
     * 
     * @throws IOException in case of any issue when writing the file
     */
    static void write(File destination, String content)
        throws IOException {
        BufferedWriter output = null;
        try {
            output = new BufferedWriter(new FileWriter(destination));
            output.write(content);
        }
        finally {
            if (output != null) {
                output.close();
            }
        }
    }

    @Nested
    @DisplayName("gradle release")
    class ReleaseTests {
        /**
         * Instantiates a new GradleRunner to use for tests, using the given Gradle version.
         * A new temporary directory is created for each test and used by the runner.
         * The settings.gradle is created with a standard content while the build.gradle file is created with the given content.
         * 
         * @param gradleVersion the Gradle version to test against
         * @oaram gradleBuildFileContent the content of the build.gradle to create in the project directory
         * 
         * @return the runner to use for tests, already using the temporary directory
         * 
         * @throws Exception in case of any issue
         */
        GradleRunner setUp(String gradleVersion, String gradleBuildFileContent)
            throws Exception {
            
            File tempProjectDir = Files.createTempDirectory("nyx-gradle-"+gradleVersion+"-test").toFile();

            // let the VM delete the directories on exit
            tempProjectDir.deleteOnExit(); 
            System.out.println("Set up tests into directory: "+tempProjectDir.getAbsolutePath());

            // do a couple extra checks to avoid messing up with the project dir
            assertFalse(Objects.isNull(tempProjectDir));
            assertTrue(tempProjectDir.exists());
            assertTrue(tempProjectDir.isDirectory());

            // Create the settings.gradle file
            StringBuilder gradleSettingsFileContent = new StringBuilder("rootProject.name = 'nyx-gradle-"+gradleVersion+"-plugin-test'").append(System.getProperty("line.separator"));
            write(new File(tempProjectDir, "settings.gradle"), gradleSettingsFileContent.toString());

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

        @ParameterizedTest(name = "Gradle Version: ''{0}'' - gradle release")
        @MethodSource("com.mooltiverse.oss.nyx.gradle.NyxPluginFunctionalTests#wellKnownWorkingGradleVersions")
        void nyxReleaseTest(String gradleVersion)
            throws Exception {
            
            // Prepare the build.gradle file
            StringBuilder gradleBuildFileContent = new StringBuilder();
            gradleBuildFileContent.append(System.getProperty("line.separator"));
            gradleBuildFileContent.append("plugins {").append(System.getProperty("line.separator"));
            gradleBuildFileContent.append("  id 'com.mooltiverse.oss.nyx'").append(System.getProperty("line.separator"));
            gradleBuildFileContent.append("}").append(System.getProperty("line.separator"));

            GradleRunner gradleRunner = setUp(gradleVersion, gradleBuildFileContent.toString());

            // withGradleVersion() needs to pull the given version from the internet so if it's not in the cache, as at the first run,
            // this will stlightly increase execution time.
            // GradleRunner.withDebug(boolean) enables debug output
            /*BuildResult gradleResult = */gradleRunner.withDebug(true).withArguments("release").build();

            System.out.println("Testing with new Gradle version "+gradleVersion+" complete");

            tearDown(gradleRunner);
        }
    }
}
