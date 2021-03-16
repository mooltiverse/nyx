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
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.version.SemanticVersion;

@DisplayName("Infer")
public class InferTests {
    @Nested
    @DisplayName("Infer constructor")
    static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @Test
        @DisplayName("Infer()")
        void constructorTest()
            throws Exception {
            assertNotNull(new Infer(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())));
        }
    }

    @Nested
    @DisplayName("Infer repository")
    static class RepositoryTests {
        /**
         * Check that the repository() method never returns a {@code null} object
         */
        @Test
        @DisplayName("Infer.repository()")
        void repositoryTest()
            throws Exception {
            assertNotNull(new Infer(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).repository());
        }
    }

    @Nested
    @DisplayName("Infer state")
    static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @Test
        @DisplayName("Infer.state()")
        void stateTest()
            throws Exception {
            assertNotNull(new Infer(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).state());
        }
    }

    @Nested
    @DisplayName("Infer isUpToDate")
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @Test
        @DisplayName("Infer.isUpToDate()")
        void isUpToDateTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            assertTrue(command.isUpToDate());

            // and running again with no changes must still be up to date
            command.run();
            assertTrue(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Infer run")
    public static class RunTests {
        @Test
        @DisplayName("Infer.run() throws exception with a valid but empty Git repository in working directory")
        void exceptionOnRunWithValidButEmptyGitRepositoryTest()
            throws Exception {
            assertThrows(GitException.class, () -> new Infer(new State(new Configuration()), Git.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory())).run());
        }

        @Test
        @DisplayName("Infer.run() with a valid but just initialized Git repository")
        void runWithJustInitializedRepositoryTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(Defaults.INITIAL_VERSION, command.run().getVersionInternal());
            assertEquals(Defaults.RELEASE_PREFIX.concat(Defaults.INITIAL_VERSION.toString()), command.run().getVersion());
            assertEquals(script.getCommits().get(0), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() with initial version override")
        void runWithInitialVersionOverriddenByUserTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            configurationMock.initialVersion = SemanticVersion.valueOf("12.13.14");
            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.initialVersion, command.state().getVersionInternal());
            assertEquals(Defaults.RELEASE_PREFIX.concat(configurationMock.initialVersion.toString()), command.state().getVersion());
            assertEquals(script.getCommits().get(0), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() with version override")
        void runWithVersionOverriddenByUserTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            CustomConfigurationLayerMock configurationMock = new CustomConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.version, command.state().getVersionInternal());
            assertEquals(configurationMock.releasePrefix.concat(configurationMock.version.toString()), command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());

            configurationMock.version = SemanticVersion.valueOf("1.2.3");
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.releasePrefix.concat("1.2.3"), command.state().getVersion());
            assertEquals(configurationMock.version, command.state().getVersionInternal());
            assertEquals(configurationMock.releasePrefix.concat(configurationMock.version.toString()), command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() with bump override")
        void runWithBumpOverriddenByUserTest()
            throws Exception {
            Script script = Scenario.INITIAL_VERSION.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("1.0.0", command.state().getVersionInternal().toString());
            assertEquals("v1.0.0", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant()); // this is valid just as long as we don't inspect commit messages, then it's expected to change to false because there are no other commits than the one tagged in the scenario

            configurationMock.bump = "minor";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.2.0", command.state().getVersionInternal().toString());
            assertEquals("v0.2.0", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant()); // this is valid just as long as we don't inspect commit messages, then it's expected to change to false because there are no other commits than the one tagged in the scenario

            configurationMock.bump = "patch";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.1", command.state().getVersionInternal().toString());
            assertEquals("v0.1.1", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant()); // this is valid just as long as we don't inspect commit messages, then it's expected to change to false because there are no other commits than the one tagged in the scenario

            configurationMock.bump = "alpha";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0-alpha.1", command.state().getVersionInternal().toString());
            assertEquals("v0.1.0-alpha.1", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant()); // this is valid just as long as we don't inspect commit messages, then it's expected to change to false because there are no other commits than the one tagged in the scenario
        }

        @Test
        @DisplayName("Infer.run() with lenient release")
        void runWithLenientReleaseTest()
            throws Exception {
            Script script = Scenario.INITIAL_VERSION.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("2.2.2", command.state().getVersionInternal().toString());
            assertEquals("v2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("2.2.2", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() without lenient release")
        void runWithoutLenientReleaseTest()
            throws Exception {
            Script script = Scenario.INITIAL_VERSION.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersionInternal().toString());
            assertEquals("v0.1.0", command.state().getVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() without lenient release and with release prefix")
        void runWithoutLenientAndWithPrefixReleaseTest()
            throws Exception {
            Script script = Scenario.INITIAL_VERSION.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("2.2.2", command.state().getVersionInternal().toString());
            assertEquals("release-2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("2.2.2", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() without lenient release and without release prefix")
        void runWithoutLenientAndWithoutPrefixReleaseTest()
            throws Exception {
            Script script = Scenario.INITIAL_VERSION.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));
            EmptyConfigurationLayerMock configurationMock = new EmptyConfigurationLayerMock();

            command.state().getConfiguration().withPluginConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = ""; // null doesn't override the default, the empty string does
            script.andCommitWithTag("release-2.2.2");

            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersionInternal().toString());
            assertEquals("0.1.0", command.state().getVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() with a simple linear commit history but no further significant commits")
        void runWithSimpleCommitHistoryButNoFurtherSignificantCommitsTest()
            throws Exception {
            Script script = Scenario.ONE_BRANCH_SHORT.realize();
            Infer command = new Infer(new State(new Configuration()), Git.open(script.getWorkingDirectory()));

            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersionInternal().toString());
            assertEquals("v0.0.4", command.state().getVersion());
            assertEquals(script.getCommits().get(script.getCommits().size()-1), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @Test
        @DisplayName("Infer.run() with a simple linear commit history and further significant commits")
        void runWithSimpleCommitHistoryAndFurtherSignificantCommitsTest()
            throws Exception {
            // TODO: write this test once we're able to bump versions based on the commit history
            /*Infer command = new Infer(new State(new Configuration()), Git.open(Scenario.ONE_BRANCH_SHORT.realize().getWorkingDirectory()));

            command.run()
            
            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersionInternal().toString());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getCommits().get(script.getCommits().size()-1), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion().toString());
            assertEquals(script.getCommits().get(script.getCommits().size()-2), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());*/
        }
    }
}