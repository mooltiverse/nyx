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

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.command.template.CommandProxy;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
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
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push disabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // disable all commit message conventions so no commit yields to a bump identifier
            configurationLayerMock.setCommitMessageConventions(new CommitMessageConventions(List.<String>of(),Map.<String,CommitMessageConvention>of()));
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.FALSE.toString());
                            setGitPush(Boolean.FALSE.toString());
                            setGitTag(Boolean.FALSE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push disabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // disable all commit message conventions so no commit yields to a bump identifier
            configurationLayerMock.setCommitMessageConventions(new CommitMessageConventions(List.<String>of(),Map.<String,CommitMessageConvention>of()));
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.FALSE.toString());
                            setGitPush(Boolean.FALSE.toString());
                            setGitTag(Boolean.FALSE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);
            
            // add some uncommitted changes
            script.andAddFiles();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push enabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // disable all commit message conventions so no commit yields to a bump identifier
            configurationLayerMock.setCommitMessageConventions(new CommitMessageConventions(List.<String>of(),Map.<String,CommitMessageConvention>of()));
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push enabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // disable all commit message conventions so no commit yields to a bump identifier
            configurationLayerMock.setCommitMessageConventions(new CommitMessageConventions(List.<String>of(),Map.<String,CommitMessageConvention>of()));
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // add some uncommitted changes
            script.andAddFiles();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(0, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push disabled after Infer has generated a new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.FALSE.toString());
                            setGitPush(Boolean.FALSE.toString());
                            setGitTag(Boolean.FALSE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push disabled after Infer has generated a new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.FALSE.toString());
                            setGitPush(Boolean.FALSE.toString());
                            setGitTag(Boolean.FALSE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // add some uncommitted changes
            script.andAddFiles();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT, command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Defaults.ReleaseType.GIT_PUSH, command.state().getReleaseType().getGitPush());
                assertEquals(Defaults.ReleaseType.GIT_TAG, command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(2, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "replica");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("patch", ".*")))
                )
            );
            // add a custom release type that always enables committing, tagging and pushing
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of(),
                    List.<String>of("replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);
            
            // add some uncommitted changes
            script.andAddFiles();

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("master", command.state().getBranch());
                assertEquals("patch", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(3, command.state().getReleaseScope().getCommits().size());
                assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit().getSHA());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit().getSHA());
                assertEquals("0.0.4", command.state().getReleaseScope().getPrimeVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPrimeVersionCommit().getSHA());
                assertEquals(2, command.state().getReleaseScope().getSignificantCommits().size());
                assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, command.state().getReleaseType().getCollapseVersions());
                assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, command.state().getReleaseType().getCollapsedVersionQualifier());
                assertEquals(Defaults.ReleaseType.DESCRIPTION, command.state().getReleaseType().getDescription());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitCommit());
                assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, command.state().getReleaseType().getGitCommitMessage());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitPush());
                assertEquals(Boolean.TRUE.toString(), command.state().getReleaseType().getGitTag());
                assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, command.state().getReleaseType().getGitTagMessage());
                if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, command.state().getReleaseType().getIdentifiers());
                }
                else {
                    assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(command.state().getReleaseType().getIdentifiers()));
                    for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), command.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), command.state().getReleaseType().getIdentifiers().get(i).getValue());
                        assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), command.state().getReleaseType().getIdentifiers().get(i).getPosition());
                    }
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
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }
    }
}
