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
import com.mooltiverse.oss.nyx.git.local.Repository;
import com.mooltiverse.oss.nyx.git.script.JGitScript;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("Infer")
public class InferTests extends AbstractCommandTests {
    @Nested
    @DisplayName("Infer.isUpToDate")
    static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @Test
        @DisplayName("Infer.isUpToDate()")
        void isUpToDateTest()
            throws Exception {
            JGitScript script = JGitScript.fromScratch(true);
            AbstractCommand command = getCommandInstance(Infer.class, new State(new Configuration()), Repository.open(script.getWorkingDirectory()));
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, which will sill have the command to be not up to date
            command.run();
            assertFalse(command.isUpToDate());

            // add some commits to the repository
            script.addBatch("11.12.13");
            assertFalse(command.isUpToDate());

            // now after one run the task should be up to date
            command.run();
            assertTrue(command.isUpToDate());

            // and running again with no changes must still be up to date
            command.run();
            assertTrue(command.isUpToDate());
        }
    }
}