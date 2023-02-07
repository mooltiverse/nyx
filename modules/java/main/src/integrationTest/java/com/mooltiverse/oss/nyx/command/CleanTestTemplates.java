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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;

@DisplayName("Clean")
public class CleanTestTemplates {
    @Nested
    @DisplayName("Clean constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Clean()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Clean state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Clean.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Clean isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @TestTemplate
        @DisplayName("Clean.isUpToDate()")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();

            // run once, to start
            command.run();

            // always up to date unless we have generated artifacts
            assertTrue(command.isUpToDate());
        }

        /**
         * Check that the isUpToDate() returns {@code false} when there's a state file
         */
        @TestTemplate
        @DisplayName("Clean.isUpToDate() with state file")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateWithStateFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String stateFilePath = "state-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.setStateFile(stateFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();
            assertTrue(command.isUpToDate());

            File stateFile = new File(script.getWorkingDirectory().getAbsolutePath(), stateFilePath);
            stateFile.deleteOnExit();
            stateFile.createNewFile();

            // now it's not up do date anymore
            assertTrue(stateFile.exists());
            assertFalse(command.isUpToDate());

            command.run();

            // now it's up do date again
            assertFalse(stateFile.exists());
            assertTrue(command.isUpToDate());
        }

        /**
         * Check that the isUpToDate() returns {@code false} when there's a summary file
         */
        @TestTemplate
        @DisplayName("Clean.isUpToDate() with summary file")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateWithSummaryFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String summaryFilePath = "summary-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.setSummaryFile(summaryFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();
            assertTrue(command.isUpToDate());

            File summaryFile = new File(script.getWorkingDirectory().getAbsolutePath(), summaryFilePath);
            summaryFile.deleteOnExit();
            summaryFile.createNewFile();

            // now it's not up do date anymore
            assertTrue(summaryFile.exists());
            assertFalse(command.isUpToDate());

            command.run();

            // now it's up do date again
            assertFalse(summaryFile.exists());
            assertTrue(command.isUpToDate());
        }

        /**
         * Check that the isUpToDate() returns {@code false} when there's a changelog file
         */
        @TestTemplate
        @DisplayName("Clean.isUpToDate() with changelog file")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateWithChangelogFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String changelogFilePath = "changelog-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.getChangelog().setPath(changelogFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();
            assertTrue(command.isUpToDate());

            File changelogFile = new File(script.getWorkingDirectory().getAbsolutePath(), changelogFilePath);
            changelogFile.deleteOnExit();
            changelogFile.createNewFile();

            // now it's up do date anymore
            assertTrue(changelogFile.exists());
            assertFalse(command.isUpToDate());

            command.run();

            // now it's up do date again
            assertFalse(changelogFile.exists());
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Clean idempotency")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class IdempotencyTests {
        /**
         * Check that multiple runs yield to the same result
         */
        @TestTemplate
        @DisplayName("Clean idempotency")
        @Baseline(Scenario.INITIAL_COMMIT)
        void idempotency(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String stateFilePath = "state-file.txt";
            String summaryFilePath = "summary-file.txt";
            String changelogFilePath = "changelog-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.setStateFile(stateFilePath);
            configurationLayerMock.setSummaryFile(summaryFilePath);
            configurationLayerMock.getChangelog().setPath(changelogFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();
            assertTrue(command.isUpToDate());

            File stateFile = new File(script.getWorkingDirectory().getAbsolutePath(), stateFilePath);
            stateFile.deleteOnExit();
            stateFile.createNewFile();
            File summaryFile = new File(script.getWorkingDirectory().getAbsolutePath(), summaryFilePath);
            summaryFile.deleteOnExit();
            summaryFile.createNewFile();
            File changelogFile = new File(script.getWorkingDirectory().getAbsolutePath(), changelogFilePath);
            changelogFile.deleteOnExit();
            changelogFile.createNewFile();

            // now it's not up do date anymore
            assertTrue(stateFile.exists());
            assertTrue(summaryFile.exists());
            assertTrue(changelogFile.exists());
            assertFalse(command.isUpToDate());

            command.run();

            // now it's up do date again
            assertFalse(stateFile.exists());
            assertFalse(summaryFile.exists());
            assertFalse(changelogFile.exists());
            assertTrue(command.isUpToDate());

            // run again and test for idempotency
            command.run();

            // now it's not up do date again
            assertFalse(stateFile.exists());
            assertFalse(summaryFile.exists());
            assertFalse(changelogFile.exists());
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Clean run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Clean.run() deletes state file")
        @Baseline(Scenario.FROM_SCRATCH)
        void deleteStateFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String stateFilePath = "state-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.setStateFile(stateFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();

            File stateFile = new File(script.getWorkingDirectory().getAbsolutePath(), stateFilePath);
            stateFile.deleteOnExit();
            stateFile.createNewFile();
            assertTrue(stateFile.exists());

            // now running the clean must delete the file
            command.run();
            assertFalse(stateFile.exists());

            // run again and test for idempotency
            command.run();
            assertFalse(stateFile.exists());
        }

        @TestTemplate
        @DisplayName("Clean.run() deletes summary file")
        @Baseline(Scenario.FROM_SCRATCH)
        void deleteSummaryFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String summaryFilePath = "summary-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.setSummaryFile(summaryFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();

            File summaryFile = new File(script.getWorkingDirectory().getAbsolutePath(), summaryFilePath);
            summaryFile.deleteOnExit();
            summaryFile.createNewFile();
            assertTrue(summaryFile.exists());

            // now running the clean must delete the file
            command.run();
            assertFalse(summaryFile.exists());

            // run again and test for idempotency
            command.run();
            assertFalse(summaryFile.exists());
        }

        @TestTemplate
        @DisplayName("Clean.run() deletes changelog file")
        @Baseline(Scenario.FROM_SCRATCH)
        void deleteChangelogFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            String changelogFilePath = "changelog-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(script.getWorkingDirectory().getAbsolutePath());
            configurationLayerMock.getChangelog().setPath(changelogFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();

            File changelogFile = new File(script.getWorkingDirectory().getAbsolutePath(), changelogFilePath);
            changelogFile.deleteOnExit();
            changelogFile.createNewFile();
            assertTrue(changelogFile.exists());
    
            // now running the clean must delete the file
            command.run();
            assertFalse(changelogFile.exists());

            // run again and test for idempotency
            command.run();
            assertFalse(changelogFile.exists());
        }
    }
}