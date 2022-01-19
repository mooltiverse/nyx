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
package com.mooltiverse.oss.nyx.command;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.FileReader;
import java.io.StringWriter;
import java.nio.file.Files;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

@DisplayName("Make")
public class MakeTestTemplates {
    /**
     * Reads the contents of the given file and returns its content as a string.
     * 
     * @param file the file to read
     * 
     * @return the file content
     * 
     * @throws Exception in case of any issue
     */
    private static String readFile(File file)
        throws Exception {
        StringWriter buffer = new StringWriter();
        FileReader reader = new FileReader(file);
        reader.transferTo(buffer);
        reader.close();
        return buffer.toString();
    }

    @Nested
    @DisplayName("Make constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Make()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Make state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Make.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Make isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @TestTemplate
        @DisplayName("Make.isUpToDate()")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTest(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();

            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 

            // and running again with no changes must still be up to date
            command.run();
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 
        }

        /**
         * Check that the isUpToDate() returns {@code false} when the changelog destination file is configured but
         * the file is missing
         */
        @TestTemplate
        @DisplayName("Make.isUpToDate() == false when changelog file is configured but missing")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTestWithMissingChangelogFile(@CommandSelector(Commands.MAKE) CommandProxy command, Script script)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(command.isUpToDate());
            assertFalse(changelogFile.exists());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();

            assertTrue(changelogFile.exists());

            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate());

            // now delete the file and make sure it's no longer up to date
            changelogFile.delete();
            assertFalse(changelogFile.exists());

            assertFalse(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Make run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Make.run()")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runTest(@CommandSelector(Commands.MAKE) CommandProxy command)
            throws Exception {
            // first create the temporary directory and the abstract destination file
            File destinationDir = Files.createTempDirectory("nyx-test-make-test-").toFile();
            File changelogFile = new File(destinationDir, "CHANGELOG.md");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.getChangelog().setPath(changelogFile.getAbsolutePath());
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            assertFalse(changelogFile.exists());

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertTrue(changelogFile.exists());

                String fileContent = readFile(changelogFile);
                assertTrue(fileContent.contains("PLACEHOLDER")); // TODO: change this test with actual contents
            }

            // print the file to standard output for inspection purpose
            System.out.println("------- CHANGELOG -------");
            System.out.println("Loading from: "+changelogFile.getAbsolutePath());
            System.out.println("-----------------------------------------");
            System.out.println(readFile(changelogFile));
            System.out.println("-----------------------------------------");
            System.out.flush();
        }
    }
}