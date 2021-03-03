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
import static org.junit.jupiter.api.Assumptions.*;

import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
import com.mooltiverse.oss.nyx.git.local.Repository;
import com.mooltiverse.oss.nyx.git.script.JGitScript;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.version.SemanticVersion;

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

    @Nested
    @DisplayName("Infer.run")
    static class RunTests {
        @Test
        @DisplayName("Infer.run() with version overridden by user")
        void runWithVersionOverriddenByUserTest()
            throws Exception {
            JGitScript script = JGitScript.fromScratch(true);
            Configuration configuration = new Configuration();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            State state = new State(configuration);
            AbstractCommand command = getCommandInstance(Infer.class, state, Repository.open(script.getWorkingDirectory()));

            assumeTrue(Objects.isNull(state.getVersion()));
            
            // at a first run, with no commits and no configurations, an exception is expected
            //assertThrows(ReleaseException.class, () -> command.run()); // TODO: uncomment this and delete the next row when ready
            command.run();
            assertNull(state.getVersion()); // the state still has a null version

            // inject the configuration mock at the plugin layer, still with no version set. the resulting version must not be null and be the same defined by the mock
            configuration.withPluginConfiguration(configurationMock);
            command.run();
            assertNotNull(state.getVersion());
            assertEquals(configurationMock.version, state.getVersion());
            assertEquals(configurationMock.version.toString(), state.getVersion().toString());

            // now set a version into the mock configuration and the resulting version must be the same
            configurationMock.version = SemanticVersion.valueOf("1.2.3");
            command.run();
            assertNotNull(state.getVersion());
            assertEquals("1.2.3", state.getVersion().toString());
            assertEquals(configurationMock.version, state.getVersion());
            assertEquals(configurationMock.version.toString(), state.getVersion().toString());
        }
    }
}