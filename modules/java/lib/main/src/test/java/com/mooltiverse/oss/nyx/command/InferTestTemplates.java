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

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

@DisplayName("Infer")
public class InferTestTemplates {
    @Nested
    @DisplayName("Infer constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Infer()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Infer state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Infer.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Infer isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @TestTemplate
        @DisplayName("Infer.isUpToDate()")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
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

        /**
         * Check that the isUpToDate() always returns {@code false} when the repository is dirty.
         */
        @TestTemplate
        @DisplayName("Infer.isUpToDate() == false in dirty repository")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateInDirtyRepositoryTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            assertTrue(command.isUpToDate());

            // but if we add uncommitted files it must return false
            script.addRandomTextWorkbenchFiles(1);
            assertFalse(command.isUpToDate());
            command.run();
            assertFalse(command.isUpToDate());

            // still false even after staging
            script.stage();
            assertFalse(command.isUpToDate());
            command.run();
            assertFalse(command.isUpToDate());
        }
    }

    @Nested
    @DisplayName("Infer run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Infer.run() throws exception with a valid but empty Git repository in working directory")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionOnRunWithValidButEmptyGitRepositoryTest(@CommandSelector(Commands.INFER) CommandProxy command)
            throws Exception {
            assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a valid but just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithJustInitializedRepositoryTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(Defaults.INITIAL_VERSION.toString(), command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(0), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with initial version override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialVersionOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            configurationMock.initialVersion = "12.13.14";
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.initialVersion.toString(), command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(0), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("12.13.14", command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with version override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithVersionOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = "1.2.3";
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.version.toString(), command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=major override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpMajorOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("1.0.0", command.state().getVersion());
            assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=minor override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpMinorOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.2.0", command.state().getVersion());
            assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=patch override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpPatchOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.1", command.state().getVersion());
            assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=alpha override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpAlphaOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
            assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=major override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("1.0.0", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=minor override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.2.0", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=patch override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.1", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=alpha override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with lenient release in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithLenientReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release and with release prefix in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("release-2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release and without release prefix in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithoutPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = ""; // null doesn't override the default, the empty string does
            script.andCommitWithTag("release-2.2.2");

            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a simple linear commit history but no further significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithSimpleCommitHistoryButNoFurtherSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a simple linear commit history and further significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithSimpleCommitHistoryAndFurtherSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();
            
            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with initial version override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithInitialVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            configurationMock.initialVersion = "12.13.14";
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with version override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = "1.2.3";
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals(configurationMock.version.toString(), command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=major override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMajorOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("1.0.0", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=minor override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMinorOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=patch override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpPatchOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.5", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with bump=alpha override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpAlphaOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4-alpha.1", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with lenient release with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithLenientReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release and with release prefix with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("release-2.2.2", command.state().getVersion());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() without lenient release and without release prefix with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientAndWithoutPrefixReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = ""; // null doesn't override the default, the empty string does
            script.andCommitWithTag("release-2.2.2");

            command.run();

            assertEquals(configurationMock.bump, command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.0.4", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
        }

        @TestTemplate
        @DisplayName("Infer.run() with a commit message convention that accepts all commits as significant")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithAllSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*")));
            configurationMock.commitMessageConventions.enabled = List.<String>of("testConvention");
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);

            command.run();

            assertEquals("minor", command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            assertEquals("0.1.0", command.state().getVersion());
            assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant());
        }
    }
}