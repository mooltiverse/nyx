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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.git.Scenario;

@DisplayName("Publish")
public class PublishTestTemplates {
    @Nested
    @DisplayName("Publish constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Publish()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.PUBLISH) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Publish state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Publish.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.PUBLISH) CommandProxy command)
            throws Exception {
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Publish isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @TestTemplate
        @DisplayName("Publish.isUpToDate()")
        @Baseline(Scenario.INITIAL_COMMIT)
        void isUpToDateTest(@CommandSelector(Commands.PUBLISH) CommandProxy command)
            throws Exception {
            // simply test that running it twice returns false at the first run and true the second
            assertFalse(command.isUpToDate());
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
    }

    @Nested
    @DisplayName("Publish idempotency")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class IdempotencyTests {
        @TestTemplate
        @DisplayName("Publish idempotency")
        @Baseline(Scenario.INITIAL_COMMIT)
        void idempotency(@CommandSelector(Commands.PUBLISH) CommandProxy command)
            throws Exception {
            // simply test that running it twice returns false at the first run and true the second
            assertFalse(command.isUpToDate());
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
    }

    @Nested
    @DisplayName("Publish run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        // See the PublishTests class
    }
}