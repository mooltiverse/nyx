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
import com.mooltiverse.oss.nyx.git.Scenario;

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
        void constructorTest(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {
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
        void stateTest(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {
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
        void isUpToDateTest(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {

            // run once, to start
            command.run();

            // always up to date unless we have generated artifacts
            assertTrue(command.isUpToDate());
        }

        /**
         * Check that the isUpToDate() returns {@code false} when there's a state file
         */
        @TestTemplate
        @DisplayName("Clean.isUpToDate()")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateWithStateFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {
            String stateFilePath = "state-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setStateFile(stateFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();
            assertTrue(command.isUpToDate());

            File stateFile = new File(stateFilePath);
            stateFile.createNewFile();

            // now it's not up do date anymore
            assertFalse(command.isUpToDate());

            stateFile.delete();

            // now it's not up do date again
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
        void deleteStateFileTest(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {
            String stateFilePath = "state-file.txt";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setStateFile(stateFilePath);
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // run once, to start
            command.run();

            File stateFile = new File(stateFilePath);
            stateFile.createNewFile();
            assertTrue(stateFile.exists());

            // now running the clean must delete the file
            command.run();
            assertFalse(stateFile.exists());
        }
    }
}