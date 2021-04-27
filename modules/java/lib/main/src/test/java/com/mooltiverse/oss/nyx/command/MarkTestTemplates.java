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
            if (command.getContextName().equals("standalone"))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 

            // and running again with no changes must still be up to date
            command.run();
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals("standalone"))
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
            if (command.getContextName().equals("standalone"))
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
        @DisplayName("Mark.run() with a valid but just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithJustInitializedRepositoryTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();

            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(Defaults.INITIAL_VERSION.toString(), command.state().getVersion());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getWorkbenchCommits().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with initial version override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithInitialVersionOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            configurationMock.initialVersion = "12.13.14";
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.initialVersion, command.state().getVersion());
                assertEquals(configurationMock.initialVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getWorkbenchCommits().get(0), command.state().getReleaseScope().getInitialCommit());
                assertEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with version override in a just initialized Git repository")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithVersionOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = "1.2.3";
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
            }
            else {
                assertEquals(configurationMock.version.toString(), command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=major override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpMajorOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size(), script.getTags().size());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("1.0.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(script.getRootCommit().getName(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=minor override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpMinorOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size(), script.getTags().size());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.2.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(script.getRootCommit().getName(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=patch override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpPatchOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size(), script.getTags().size());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.1.1", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(script.getRootCommit().getName(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=alpha override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_COMMIT)
        void runWithBumpAlphaOverriddenByUserInJustInitializedRepoTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size(), script.getTags().size());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(script.getRootCommit().getName(), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=major override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("1.0.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=minor override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.2.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=patch override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.1.1", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=alpha override in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.1.0-alpha.1", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with lenient release in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithLenientReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("2.2.2", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
                assertNotNull(command.state().getReleaseScope().getFinalCommit());
                assertNotEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release and with release prefix in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release and without release prefix in a Git repository with just one initial version commit")
        @Baseline(Scenario.INITIAL_VERSION)
        void runWithoutLenientAndWithoutPrefixReleaseInRepoWithJustOneInitialVersionCommitTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = ""; // null doesn't override the default, the empty string does
            script.andCommitWithTag("release-2.2.2");

            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getVersion());
                assertEquals(Defaults.INITIAL_VERSION, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getInitialCommit());
                assertNotNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals(script.getCommitByTag("0.1.0"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with a simple linear commit history but no further significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithSimpleCommitHistoryButNoFurtherSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            command.run();
            
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertNull(command.state().getBump());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with a simple linear commit history and further significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithSimpleCommitHistoryAndFurtherSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            command.run();
            
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertNull(command.state().getBump());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with initial version override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithInitialVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            configurationMock.initialVersion = "12.13.14";
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with version override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            assumeTrue(Objects.isNull(command.state().getVersion()));

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.version = "1.2.3";
            command.run();

            assertNull(command.state().getBump());
            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
            }
            else {
                assertEquals(configurationMock.version.toString(), command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNull(command.state().getReleaseScope().getPreviousVersion());
            assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
            assertNull(command.state().getReleaseScope().getSignificant());
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size(), script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=major override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMajorOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "major";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size(), script.getTags().size());
                assertFalse(script.getTags().containsKey("v1.0.0"));
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("1.0.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getId().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size()+1, script.getTags().size());
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=minor override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpMinorOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "minor";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size(), script.getTags().size());
                assertFalse(script.getTags().containsKey("v1.0.0"));
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.1.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getId().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size()+1, script.getTags().size());
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=patch override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpPatchOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "patch";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size(), script.getTags().size());
                assertFalse(script.getTags().containsKey("v1.0.0"));
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.0.5", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getId().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size()+1, script.getTags().size());
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
        }

        @TestTemplate
        @DisplayName("Mark.run() with bump=alpha override with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithBumpAlphaOverriddenByUserInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.bump = "alpha";
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size(), script.getTags().size());
                assertFalse(script.getTags().containsKey("v1.0.0"));
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.0.4-alpha.1", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getLastCommit().getId().getName(), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
                assertEquals(previousTags.size()+1, script.getTags().size());
                //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
                //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
            }
            assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
        }

        @TestTemplate
        @DisplayName("Mark.run() with lenient release with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithLenientReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.TRUE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("2.2.2", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
            //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
            //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getInitialCommit());
                assertNotNull(command.state().getReleaseScope().getFinalCommit());
                assertNotEquals(previousLastCommit, command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
            //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
            //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release and with release prefix with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientAndWithPrefixReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = "release-";
            script.andCommitWithTag("release-2.2.2");
            
            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("release-2.2.2", command.state().getVersion());
                assertEquals("release-2.2.2", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("release-2.2.2"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNull(command.state().getReleaseScope().getInitialCommit());
            assertNull(command.state().getReleaseScope().getFinalCommit());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
            //assertFalse(remoteScript.getCommits().containsAll(script.getCommits()));
            //assertFalse(remoteScript.getTags().keySet().containsAll(script.getTags().keySet()));
        }

        @TestTemplate
        @DisplayName("Mark.run() without lenient release and without release prefix with further non significant commits")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithoutLenientAndWithoutPrefixReleaseInRepoWithFurtherNonSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();

            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);
            configurationMock.releaseLenient = Boolean.FALSE;
            configurationMock.releasePrefix = ""; // null doesn't override the default, the empty string does
            script.andCommitWithTag("release-2.2.2");

            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
            }
            else {
                assertEquals(configurationMock.bump, command.state().getBump());
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-3), command.state().getReleaseScope().getInitialCommit());
                assertNotNull(command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, command.state().getReleaseScope().getSignificant());
            }
            assertFalse(command.state().getNewVersion());
            assertFalse(command.state().getNewRelease());
            assertNotEquals(previousLastCommit, script.getLastCommit().getId().getName());
            assertEquals(previousTags.size()+1, script.getTags().size());
        }

        @TestTemplate
        @DisplayName("Mark.run() with a commit message convention that accepts all commits as significant")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runWithAllSignificantCommitsTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script, @Baseline(Scenario.FROM_SCRATCH) Script remoteScript)
            throws Exception {
            script.addRemote(remoteScript.getGitDirectory(), "origin");
            String previousLastCommit = script.getLastCommit().getId().getName();
            Map<String,String> previousTags = script.getTags();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*")));
            configurationMock.commitMessageConventions.enabled = List.<String>of("testConvention");
            command.state().getConfiguration().withCommandLineConfiguration(configurationMock);

            command.run();

            assertEquals(command.state().getConfiguration().getScheme(), command.state().getScheme());
            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (command.getContextName().equals("standalone")) {
                assertNull(command.state().getBump());
                assertNull(command.state().getVersion());
                assertFalse(command.state().getNewVersion());
                assertFalse(command.state().getNewRelease());
                assertNull(command.state().getReleaseScope().getInitialCommit());
                assertNull(command.state().getReleaseScope().getFinalCommit());
                assertNull(command.state().getReleaseScope().getPreviousVersion());
                assertNull(command.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size(), script.getTags().size());
            }
            else {
                assertEquals("minor", command.state().getBump());
                assertEquals("0.1.0", command.state().getVersion());
                assertTrue(command.state().getNewVersion());
                assertTrue(command.state().getNewRelease());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-2), command.state().getReleaseScope().getInitialCommit());
                assertEquals(script.getWorkbenchCommits().get(script.getWorkbenchCommits().size()-1), command.state().getReleaseScope().getFinalCommit());
                assertEquals("0.0.4", command.state().getReleaseScope().getPreviousVersion());
                assertEquals(script.getCommitByTag("0.0.4"), command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.TRUE, command.state().getReleaseScope().getSignificant());
                assertEquals(previousLastCommit, script.getLastCommit().getId().getName());
                assertEquals(previousTags.size()+1, script.getTags().size());
            }
        }
    }
}