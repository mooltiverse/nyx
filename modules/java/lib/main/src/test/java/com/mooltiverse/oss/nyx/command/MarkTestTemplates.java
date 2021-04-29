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
import com.mooltiverse.oss.nyx.command.template.StandaloneCommandProxy;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
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
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, using defaults > yield to previous=initial version, no new tags or commits")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with initial version override > yield to previous=initial version, no new tags or commits")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndInitialVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_INITIAL_VERSION = "12.13.14";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            configurationMock.initialVersion = CUSTOM_INITIAL_VERSION;
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(CUSTOM_INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(CUSTOM_INITIAL_VERSION, command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with version override > yield to new tag and commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_VERSION = "1.2.3";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = CUSTOM_VERSION;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(CUSTOM_VERSION, command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with bump=major override > yield to new tag and commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpMajorOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with bump=minor override > yield to new tag and commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpMinorOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.2.0", command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with bump=patch override > yield to new tag and commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpPatchOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.1", command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with the 'Initial commit' only, with bump=alpha override > yield to new tag and commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialCommitOnlyAndBumpAlphaOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getRootCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with one versioned commit, with bump=major override > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with one versioned commit, with bump=minor override > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.2.0", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with one versioned commit, with bump=patch override > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.1", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with one versioned commit, with bump=alpha override > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with prefixed versioned commit, with release lenient, without prefix > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithLenientReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("2.2.2", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with prefixed versioned commit, without release lenient, without prefix > yield to no new tag or commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutReleaseLenientInRepoWithoutPrefixedVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag(Defaults.INITIAL_VERSION), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with prefixed versioned commit, without release lenient, with prefix > yield to no new tag or commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits > yield to no new tag or commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with initial version override > yield to no new tag or commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsWithInitialVersionOverrideTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.initialVersion = "12.13.14";
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with version override > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = "1.2.3";
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("1.2.3", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with bump=major override > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMajorOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "major";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("1.0.0", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with bump=minor override > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMinorOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "minor";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with bump=patch override > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpPatchOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "patch";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.5", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with bump=alpha override > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpAlphaOverrideInRepoWithFurtherNonSignificantCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            final String CUSTOM_BUMP = "alpha";
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = CUSTOM_BUMP;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(CUSTOM_BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.0.4-alpha.1", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with release lenient, without prefix > yield to new tag but no new commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommitID(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("2.2.2", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, without release lenient, without prefix > yield to no new tag or commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommitsTestTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(2), command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, without release lenient, with prefix > yield to no new tag or commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            script.andCommitWithTag("release-2.2.2");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals(Defaults.BUMP, command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on repository with simple linear commit history and non significant commits, with a commit message convention that accepts all commits as significant > yield to new tag but no new commit")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithAllSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommitID();
            List<String> previousCommits = script.getCommitIDs();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*")));
            configurationMock.commitMessageConventions.enabled = List.<String>of("testConvention");
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("minor", command.state().getBump());
                assertEquals(Defaults.SCHEME, command.state().getScheme());
                assertEquals(script.getCommitIDs().get(1), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getCommitIDs().get(0), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("0.1.0", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }
    }
}