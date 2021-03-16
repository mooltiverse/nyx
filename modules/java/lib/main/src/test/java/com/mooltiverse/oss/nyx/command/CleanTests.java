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
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("Clean")
public class CleanTests {
    @Nested
    @DisplayName("Clean constructor")
    static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @Test
        @DisplayName("Clean()")
        void constructorTest()
            throws Exception {
            assertNotNull(new Clean(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())));
        }
    }

    @Nested
    @DisplayName("Clean repository")
    static class RepositoryTests {
        /**
         * Check that the repository() method never returns a {@code null} object
         */
        @Test
        @DisplayName("Clean.repository()")
        void repositoryTest()
            throws Exception {
            assertNotNull(new Clean(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).repository());
        }
    }

    @Nested
    @DisplayName("Clean state")
    static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @Test
        @DisplayName("Clean.state()")
        void stateTest()
            throws Exception {
            assertNotNull(new Clean(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).state());
        }
    }

    @Nested
    @DisplayName("Clean isUpToDate")
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @Test
        @DisplayName("Clean.isUpToDate()")
        void isUpToDateTest()
            throws Exception {
            Clean command = new Clean(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory()));

            // simply test that running it twice returns false at the first run and true the second
            assertFalse(command.isUpToDate());
            command.run();
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Clean run")
    public static class RunTests {
    }
}