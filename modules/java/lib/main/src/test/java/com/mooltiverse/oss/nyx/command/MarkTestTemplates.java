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
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.ReleaseException;
import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.configuration.presets.Extended;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.data.Identifiers;
import com.mooltiverse.oss.nyx.data.IdentifierPosition;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

@DisplayName("Mark")
public class MarkTestTemplates {
    @Nested
    @DisplayName("Mark constructor")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @TestTemplate
        @DisplayName("Mark()")
        @Baseline(Scenario.FROM_SCRATCH)
        void constructorTest(@CommandSelector(Commands.MARK) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("Mark state")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @TestTemplate
        @DisplayName("Mark.state()")
        @Baseline(Scenario.FROM_SCRATCH)
        void stateTest(@CommandSelector(Commands.MARK) CommandProxy command)
            throws Exception {
            assertNotNull(command.state());
        }
    }

    @Nested
    @DisplayName("Mark isUpToDate")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class UpToDateTests {
        /**
         * Check that the isUpToDate() returns {@code false} when the command instance is just created and {@code true} after one execution in a repository
         * with at least one commit and in a clean state.
         */
        @TestTemplate
        @DisplayName("Mark.isUpToDate()")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // running in an empty repository, with no commits, throws an exception
            assertThrows(GitException.class, () -> command.run());
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
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

        /**
         * Check that the isUpToDate() always returns {@code false} when the repository is dirty.
         */
        @TestTemplate
        @DisplayName("Mark.isUpToDate() == false in dirty repository")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateInDirtyRepositoryTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 

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
    @DisplayName("Mark run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Mark.run() throws exception with a valid but empty Git repository in working directory")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionOnRunWithValidButEmptyGitRepositoryTest(@CommandSelector(Commands.MARK) CommandProxy command)
            throws Exception {
            assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, using defaults > yield to previous=initial version, no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with initial version override > yield to previous=initial version, no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndInitialVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_INITIAL_VERSION = "12.13.14";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setInitialVersion(CUSTOM_INITIAL_VERSION);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(CUSTOM_INITIAL_VERSION, command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with version override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_VERSION = "1.2.3";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion(CUSTOM_VERSION);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertNull(command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getPrimeVersion());
                assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(CUSTOM_VERSION, command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with bump=major override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMajorOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with bump=minor override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMinorOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.2.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with bump=patch override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpPatchOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with the 'Initial commit' only, with bump=alpha override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpAlphaOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with one versioned commit, with bump=major override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with one versioned commit, with bump=minor override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.2.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with one versioned commit, with bump=patch override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with one versioned commit, with bump=alpha override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with prefixed versioned commit, with release lenient, without prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithReleaseLenientInRepoWithPrefixedVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("2.2.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with prefixed versioned commit, without release lenient, without prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientInRepoWithoutPrefixedVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with prefixed versioned commit, without release lenient, with prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with initial version override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsWithInitialVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setInitialVersion("12.13.14");
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with version override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setVersion("1.2.3");
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertNull(command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getPrimeVersion());
                assertNull(command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("1.2.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with bump=major override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpMajorOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with bump=minor override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpMinorOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with bump=patch override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpPatchOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.5", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with bump=alpha override > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithBumpAlphaOverrideInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4-alpha.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with release lenient, without prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.INITIAL_VERSION)
        void runUsingDefaultReleaseTypeWithReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.TRUE);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("2.2.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, without release lenient, without prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(3, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, without release lenient, with prefix > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithoutReleaseLenientAndWithPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);
            configurationLayerMock.setReleasePrefix("release-");
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(0, command.state().getReleaseScope().getCommits().size());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and a commit with overlapping valid tags > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_WITH_OVERLAPPING_TAGS)
        void runUsingDefaultReleaseTypeInRepoWithOverlappingTagsCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String CUSTOM_BUMP = "patch";
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            configurationLayerMock.setBump(CUSTOM_BUMP);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.6", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.6"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.7", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with a commit message convention that accepts all commits as significant > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithAlwaysPositiveCommitConventionInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("minor", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertTrue(command.state().getReleaseScope().getSignificantCommits().containsKey(script.getCommitIDs().get(0)));
                assertTrue(command.state().getReleaseScope().getSignificantCommits().containsKey(script.getCommitIDs().get(1)));
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() using default release type on repository with simple linear commit history and non significant commits, with a commit message convention that accepts no commits as significant > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runUsingDefaultReleaseTypeWithAlwaysNegativeCommitConventionInRepoWithFurtherNonSignificantPrefixedCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, command.state().getReleaseType().getMatchBranches());
                assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Defaults.ReleaseType.PUBLISH, command.state().getReleaseType().getPublish());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE, command.state().getReleaseType().getVersionRange());
                assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in master branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMasterBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("master");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in master branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMasterBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("master");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.5", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in main branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMainBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("main");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("main", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the mainline release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in main branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMainBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("main");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "mainline"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("main", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in integration branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInIntegrationBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("integration");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("integration", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6-integration.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in integration branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInIntegrationBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("integration");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("integration", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-integration.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in development branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInDevelopmentBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("development");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("development", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.1-development.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the integration release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in development branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInDevelopmentBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("development");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "integration"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("development", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.1.0", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in alpha branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInAlphaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("alpha");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("alpha", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-alpha.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-alpha.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6-alpha.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in alpha branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInAlphaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("alpha");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("alpha", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-alpha.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-alpha.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-alpha.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in beta branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInBetaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("beta");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("beta", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-beta.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-beta.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(10, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6-beta.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in beta branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInBetaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("beta");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("beta", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-beta.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-beta.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-beta.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in gamma branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInGammaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("gamma");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("gamma", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(7, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6-gamma.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maturity release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in gamma branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInGammaBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("gamma");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maturity"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("gamma", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(7, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.5", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in v0.x branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV0xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("v0.x");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maintenance"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("v0.x", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.8", command.state().getVersion());
                assertNotNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in v0.x branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInV0xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("v0.x");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "maintenance"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("v0.x", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.7", command.state().getVersion());
                assertNotNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the maintenance release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in v1.x branch > yield to version range check exception")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV1xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("v1.x");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // the generated version does not comply with the version range so it raises an exception
                if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                    assertThrows(ReleaseException.class, () -> command.run());
                else assertThrows(Exception.class, () -> command.run());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in rel/0.x branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel0xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("rel/0.x");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "release"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("rel/0.x", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-rel.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-rel.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(4, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-rel.3", command.state().getVersion());
                assertNotNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in rel/0.x branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInRel0xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("rel/0.x");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "release"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("rel/0.x", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-rel.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-rel.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-rel.2", command.state().getVersion());
                assertNotNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the release release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in rel/1.x branch > yield to version range check exception")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel1xBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("rel/1.x");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // the generated version does not comply with the version range so it raises an exception
                if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                    assertThrows(ReleaseException.class, () -> command.run());
                else assertThrows(Exception.class, () -> command.run());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in feature/SSO branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureSSOBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("feature/SSO");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("feature/SSO", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-featuresso.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-featuresso.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-featuresso.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in feature/SSO branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureSSOBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("feature/SSO");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("feature/SSO", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-featuresso.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-featuresso.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-featuresso.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in feature/IN-12345 branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureIN12345BranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("feature/IN-12345");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("feature/IN-12345", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(7, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-featurein12345.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the feature release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in feature/IN-12345 branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureIN12345BranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("feature/IN-12345");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "feature"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("feature/IN-12345", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(7, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(6), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.5", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the hotfix release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in fix-98765 branch > yield to new tag but no new commit, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFix98765BranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("fix-98765");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "hotfix"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("fix-98765", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.8-fix98765.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.8-fix98765.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(5, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.8-fix98765.3", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the hotfix release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in fix-98765 branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFix98765BranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("fix-98765");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "hotfix"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("fix-98765", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.8-fix98765.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.8-fix98765.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.7", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.7"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.8-fix98765.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in internal branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInInternalBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("internal");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("internal", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(8, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                // the version contains the timestamp which is variable so let's test the start string and the overall length
                assertTrue(command.state().getVersion().startsWith("0.0.6-internal.2+timestamp."), String.format("Version %s was expected to start with %s", command.state().getVersion(), "0.0.6-internal.2+timestamp."));
                assertEquals("0.0.6-internal.2+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in internal branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInInternalBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("internal");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("internal", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in somebranch branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomebranchBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("somebranch");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("somebranch", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(7, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                // the version contains the timestamp which is variable so let's test the start string and the overall length
                assertTrue(command.state().getVersion().startsWith("0.0.6-internal.1+timestamp."), String.format("Version %s was expected to start with %s", command.state().getVersion(), "0.0.6-internal.1+timestamp."));
                assertEquals("0.0.6-internal.1+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in somebranch branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomebranchBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("somebranch");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("somebranch", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(4, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(3), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-integration.2", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in someotherbranch branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomeotherbranchBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("someotherbranch");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("someotherbranch", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(3, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.1", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.1"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(5, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                // the version contains the timestamp which is variable so let's test the start string and the overall length
                assertTrue(command.state().getVersion().startsWith("0.0.6-internal.1+timestamp."), String.format("Version %s was expected to start with %s", command.state().getVersion(), "0.0.6-internal.1+timestamp."));
                assertEquals("0.0.6-internal.1+timestamp.".length()+14, command.state().getVersion().length()); // the timestamp is 14 characters long
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring the internal release type from the Extended preset on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in someotherbranch branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomeotherbranchBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("someotherbranch");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            final String MATCHING_RELEASE_TYPE_NAME = "internal"; // the name of the release type that must be matched in the branch
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add the 'extended' preset, which comes with standard release types
            configurationLayerMock.setPreset(Extended.NAME);
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("someotherbranch", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(3, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-integration.1", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-integration.1"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapseVersions(), command.state().getReleaseType().getCollapseVersions());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getCollapsedVersionQualifier(), command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getFilterTags(), command.state().getReleaseType().getFilterTags());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommit(), command.state().getReleaseType().getGitCommit());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitCommitMessage(), command.state().getReleaseType().getGitCommitMessage());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitPush(), command.state().getReleaseType().getGitPush());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTag(), command.state().getReleaseType().getGitTag());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getGitTagMessage(), command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers())) {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers(), command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().size(), command.state().getReleaseType().getIdentifiers().getEnabled().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getEnabled().containsAll(command.state().getReleaseType().getIdentifiers().getEnabled()));
                    assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().size(), command.state().getReleaseType().getIdentifiers().getItems().size());
                    assertTrue(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getIdentifiers().getItems().keySet().containsAll(command.state().getReleaseType().getIdentifiers().getItems().keySet()));
                }
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchBranches(), command.state().getReleaseType().getMatchBranches());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchEnvironmentVariables(), command.state().getReleaseType().getMatchEnvironmentVariables());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getMatchWorkspaceStatus(), command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getPublish(), command.state().getReleaseType().getPublish());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRange(), command.state().getReleaseType().getVersionRange());
                assertEquals(command.state().getConfiguration().getReleaseTypes().getItem(MATCHING_RELEASE_TYPE_NAME).getVersionRangeFromBranchName(), command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-integration.1", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring a custom release type on repository with several branches and commits, with a commit message convention that accepts all commits as significant, in internal branch > yield to new and commit and push, with changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingCustomReleaseTypeWithAlwaysPositiveCommitConventionInInternalBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("internal");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.getReleaseTypes().getItems().put("testReleaseType", new ReleaseType() {
                {
                    setCollapseVersions(true);
                    setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                    setGitCommit(Boolean.TRUE.toString());
                    setGitPush(Boolean.TRUE.toString());
                    setGitTag(Boolean.TRUE.toString());
                    setIdentifiers(new Identifiers() {
                        {
                            setEnabled(List.<String>of("customId"));
                            setItem("customId", new Identifier("customId", "999", IdentifierPosition.PRE_RELEASE));
                        }}
                    );
                    setPublish(Boolean.TRUE.toString());
                }}
            );
            configurationLayerMock.getReleaseTypes().setEnabled(List.<String>of("testReleaseType"));
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*"))));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            script.andAddFiles(); // add some uncommitted changes to be committed by the Mark command
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("internal", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(8, command.state().getReleaseScope().getSignificantCommits().size());
                assertTrue(command.state().getReleaseType().getCollapseVersions());
                assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
                assertNull(command.state().getReleaseType().getFilterTags());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                assertEquals(1, command.state().getReleaseType().getIdentifiers().getEnabled().size());
                assertTrue(command.state().getReleaseType().getIdentifiers().getEnabled().contains("customId"));
                assertEquals(1, command.state().getReleaseType().getIdentifiers().getItems().size());
                assertTrue(command.state().getReleaseType().getIdentifiers().getItems().containsKey("customId"));
                assertNull(command.state().getReleaseType().getMatchBranches());
                assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
                assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getPublish());
                assertNull(command.state().getReleaseType().getVersionRange());
                assertFalse(command.state().getReleaseType().getVersionRangeFromBranchName());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.6-internal.2.customId.999+timestamp.003", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() inferring a custom release type on repository with several branches and commits, with a commit message convention that accepts no commits as significant, in internal branch > yield to no new tags or commits, no changes pushed to remote")
        @Baseline(Scenario.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED)
        void runUsingCustomReleaseTypeWithAlwaysNegativeCommitConventionInInternalBranchTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.checkout("internal");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.getReleaseTypes().getItems().put("testReleaseType", new ReleaseType() {
                {
                    setCollapseVersions(true);
                    setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
                    setGitCommit(Boolean.TRUE.toString());
                    setGitPush(Boolean.TRUE.toString());
                    setGitTag(Boolean.TRUE.toString());
                    setIdentifiers(new Identifiers() {
                        {
                            setEnabled(List.<String>of("customId"));
                            setItem("customId", new Identifier("customId", "999", IdentifierPosition.PRE_RELEASE));
                        }}
                    );
                    setPublish(Boolean.TRUE.toString());
                }}
            );
            configurationLayerMock.getReleaseTypes().setEnabled(List.<String>of("testReleaseType"));
            command.state().getConfiguration().withCommandLineConfiguration(configurationLayerMock);
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of())));
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("testConvention"));

            script.andAddFiles(); // add some uncommitted changes to be committed by the Mark command
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("internal", command.state().getBranch());
                assertNull(command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(1, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.6-internal.1+timestamp.003"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals("0.0.5", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.5"), command.state().getReleaseScope().getPrimeVersionCommit());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertTrue(command.state().getReleaseType().getCollapseVersions());
                assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", command.state().getReleaseType().getCollapsedVersionQualifier());
                assertNull(command.state().getReleaseType().getFilterTags());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                assertEquals(1, command.state().getReleaseType().getIdentifiers().getEnabled().size());
                assertTrue(command.state().getReleaseType().getIdentifiers().getEnabled().contains("customId"));
                assertEquals(1, command.state().getReleaseType().getIdentifiers().getItems().size());
                assertTrue(command.state().getReleaseType().getIdentifiers().getItems().containsKey("customId"));
                assertNull(command.state().getReleaseType().getMatchBranches());
                assertNull(command.state().getReleaseType().getMatchEnvironmentVariables());
                assertNull(command.state().getReleaseType().getMatchWorkspaceStatus());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getPublish());
                assertNull(command.state().getReleaseType().getVersionRange());
                assertFalse(command.state().getReleaseType().getVersionRangeFromBranchName());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.6-internal.1+timestamp.003", command.state().getVersion());
                assertNull(command.state().getVersionRange());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }
    }
}
