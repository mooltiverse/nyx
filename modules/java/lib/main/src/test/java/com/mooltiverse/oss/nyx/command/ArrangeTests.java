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

import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.Scenario;

@DisplayName("Arrange")
public class ArrangeTests extends AbstractCommandTests {
    @Nested
    @DisplayName("Arrange isUpToDate")
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state
         */
        @Test
        @DisplayName("Arrange.isUpToDate()")
        void isUpToDateTest()
            throws Exception {
            Arrange command = getCommandInstance(Arrange.class, Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory()));

            // simply test that running it twice returns false at the first run and true the second
            assertFalse(command.isUpToDate());
            command.run();
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Arrange run")
    public static class RunTests {
        /*@Test
        @DisplayName("Arrange.run() throws exception with a valid but empty Git repository in working directory")
        void stateTest()
            throws Exception {
            assertThrows(GitException.class, () -> getCommandInstance(Arrange.class, Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).run());
        }*/
    }
}