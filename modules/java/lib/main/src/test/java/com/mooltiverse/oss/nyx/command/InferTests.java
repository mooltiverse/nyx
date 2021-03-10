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
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.mock.CustomConfigurationLayerMock;
import com.mooltiverse.oss.nyx.configuration.mock.EmptyConfigurationLayerMock;
import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.script.GitScenario;
import com.mooltiverse.oss.nyx.git.script.GitScript;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.version.SemanticVersion;

@DisplayName("Infer")
public class InferTests extends AbstractCommandTests {
    @Nested
    @DisplayName("Infer isUpToDate")
    static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @Test
        @DisplayName("Infer.isUpToDate()")
        void isUpToDateTest()
            throws Exception {
            GitScript script = GitScript.fromScratch();
            AbstractCommand command = getCommandInstance(Infer.class, new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.addCommitWithTag("111.122.133");
            command.run();
            assertTrue(command.isUpToDate());

            // and running again with no changes must still be up to date
            command.run();
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Infer run")
    static class RunTests {
        @Test
        @DisplayName("Infer.run() throws exception with a valid but empty Git repository in working directory")
        void exceptionOnRunWithValidButEmptyGitRepositoryTest()
            throws Exception {
            assertThrows(GitException.class, () -> getCommandInstance(Infer.class, new State(new Configuration()), Git.open(GitScript.fromScratch().getWorkingDirectory())).run());
        }

        @Test
        @DisplayName("Infer.run() with a valid but just initialized Git repository")
        void runWithJustInitializedRepositoryTest()
            throws Exception {
            GitScript script = GitScenario.InitialCommit.realize();
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            //TODO: check outputs from the State
            assertEquals(Defaults.INITIAL_VERSION, command.run().getVersion());
        }

        @Test
        @DisplayName("Infer.run() with initial version override")
        void runWithInitialVersionOverriddenByUserTest()
            throws Exception {
            GitScript script = GitScenario.InitialCommit.realize();
            Configuration configuration = new Configuration();
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();
            State state = new State(configuration);
            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));

            assumeTrue(Objects.isNull(state.getVersion()));

            // inject the configuration mock at the plugin layer, with a custom initial version,
            configurationMock.initialVersion = SemanticVersion.valueOf("12.13.14");
            configuration.withPluginConfiguration(configurationMock);
            command.run();
            assertNotNull(state.getVersion());
            assertEquals(configurationMock.initialVersion.toString(), state.getVersion().toString());
        }

        @Test
        @DisplayName("Infer.run() with version override")
        void runWithVersionOverriddenByUserTest()
            throws Exception {
            GitScript script = GitScenario.InitialCommit.realize();
            Configuration configuration = new Configuration();
            CustomConfigurationLayerMock configurationMock = new CustomConfigurationLayerMock();
            State state = new State(configuration);
            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));

            assumeTrue(Objects.isNull(state.getVersion()));

            // inject the configuration mock at the plugin layer, still with no version set. The resulting version must be the same defined by the mock
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

        @Test
        @DisplayName("Infer.run() with bump override")
        void runWithBumpOverriddenByUserTest()
            throws Exception {
            GitScript script = GitScenario.OneTaggedCommitCommit.realize(); // only one version tag: 1.2.3
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            // inject the configuration mock at the plugin layer
            configuration.withPluginConfiguration(configurationMock);

            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            configurationMock.bump = "major";
            assertEquals("2.0.0", command.run().getVersion().toString());

            configurationMock.bump = "minor";
            assertEquals("1.3.0", command.run().getVersion().toString());

            configurationMock.bump = "patch";
            assertEquals("1.2.4", command.run().getVersion().toString());

            configurationMock.bump = "alpha";
            assertEquals("1.2.3-alpha.1", command.run().getVersion().toString());
        }

        @Test
        @DisplayName("Infer.run() with lenient release")
        void runWithLenientReleaseTest()
            throws Exception {
            GitScript script = GitScenario.OneTaggedCommitCommit.realize(); // only one version tag: 1.2.3
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            // inject the configuration mock at the plugin layer
            configuration.withPluginConfiguration(configurationMock);

            configurationMock.releaseLenient = Boolean.TRUE;

            script.addCommitWithTag("release-2.2.2");

            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            // the latest release must be detected thanks to the lenient parsing
            assertEquals("2.2.2", command.run().getVersion().toString());
        }

        @Test
        @DisplayName("Infer.run() without lenient release")
        void runWithoutLenientReleaseTest()
            throws Exception {
            GitScript script = GitScenario.OneTaggedCommitCommit.realize(); // only one version tag: 1.2.3
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            // inject the configuration mock at the plugin layer
            configuration.withPluginConfiguration(configurationMock);

            configurationMock.releaseLenient = Boolean.FALSE;

            script.addCommitWithTag("release-2.2.2");

            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            // the latest release must be the one that doesn't require the lenient parsing
            assertEquals("1.2.3", command.run().getVersion().toString());
        }

        @Test
        @DisplayName("Infer.run() with a simple linear commit history but no further significant commits")
        void runWithSimpleCommitHistoryButNoFurtherSignificantCommitsTest()
            throws Exception {
            GitScript script = GitScenario.OneBranchShort.realize(); // the latest tagged release is 0.0.4
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            //TODO: check outputs from the State
            assertEquals("0.0.4", command.run().getVersion().toString());
        }

        @Test
        @DisplayName("Infer.run() with a simple linear commit history and further significant commits")
        void runWithSimpleCommitHistoryAndFurtherSignificantCommitsTest()
            throws Exception {
            // TODO: write this test once we're able to bump versions based on the commit history
            /**GitScript script = GitScenario.OneBranchShort.realize(); // the latest tagged release is 0.0.4
            Configuration configuration = new Configuration();
            State state = new State(configuration);
            Infer command = getCommandInstance(Infer.class, state, Git.open(script.getWorkingDirectory()));
            //TODO: check outputs from the State
            assertEquals("0.0.4", command.run().getVersion().toString());*/
        }
    }
}