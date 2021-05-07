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

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
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
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, using defaults > yield to previous=initial version, no new version or release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(Defaults.INITIAL_VERSION.toString(), command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with initial version override > yield to previous=initial version, no new version or release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndInitialVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_INITIAL_VERSION = "12.13.14";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setInitialVersion(CUSTOM_INITIAL_VERSION);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(CUSTOM_INITIAL_VERSION, command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with version override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_VERSION = "1.2.3";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion(CUSTOM_VERSION);
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals(CUSTOM_VERSION, command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with bump=major override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpMajorOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpMinorOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.2.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpPatchOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with the 'Initial commit' only, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpAlphaOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with one versioned commit, with bump=major override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with one versioned commit, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.2.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with one versioned commit, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with one versioned commit, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.0-alpha.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with prefixed versioned commit, with release lenient, without prefix > yield to new version and release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithReleaseLenientInRepoWithPrefixedVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("2.2.2", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with prefixed versioned commit, without release lenient, without prefix > yield to no new version or release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutReleaseLenientInRepoWithoutPrefixedVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(1, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with prefixed versioned commit, without release lenient, with prefix > yield to no new version or release")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("release-2.2.2", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with initial version override > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsWithInitialVersionOverrideTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setInitialVersion("12.13.14");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with version override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion("1.2.3");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("1.2.3", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with bump=major override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMajorOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("1.0.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with bump=minor override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMinorOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with bump=patch override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpPatchOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.5", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with bump=alpha override > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpAlphaOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            assertEquals(CUSTOM_BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.0.4-alpha.1", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with release lenient, without prefix > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("2.2.2", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, without release lenient, without prefix > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(3, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("0.0.4", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, without release lenient, with prefix > yield to no new version or release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutReleaseLenientAndWithPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            script.andCommitWithTag("release-2.2.2");
            command.run();

            assertEquals(Defaults.BUMP, command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(0, command.state().getReleaseScope().getCommits().size());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals("release-2.2.2", command.state().getVersion());
        }

        @TestTemplate
        @DisplayName("Infer.run() on repository with simple linear commit history and non significant commits, with a commit message convention that accepts all commits as significant > yield to new version and release")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithAlwayspositiveCommitConventionInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.INFER) CommandProxy command, Script script)
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            assertEquals("minor", command.state().getBump());
            assertEquals(Defaults.SCHEME, command.state().getScheme());
            assertEquals(2, command.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
            assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
            assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
            assertTrue(command.state().getReleaseScope().getSignificantCommits().containsKey(script.getCommitIDs().get(0)));
            assertTrue(command.state().getReleaseScope().getSignificantCommits().containsKey(script.getCommitIDs().get(1)));
            assertTrue(command.state().getNewVersion());
            assertTrue(command.state().getNewRelease());
            assertEquals("0.1.0", command.state().getVersion());
        }
    }
}