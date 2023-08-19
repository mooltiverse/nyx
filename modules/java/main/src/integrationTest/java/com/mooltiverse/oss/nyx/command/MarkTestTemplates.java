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
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.version.Scheme;

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
        void constructorTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
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
        void stateTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
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
            script.getWorkingDirectory().deleteOnExit();
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
         * Check that the isUpToDate() always returns {@code true} even when the repository is dirty.
         */
        @TestTemplate
        @DisplayName("Mark.isUpToDate() == true in dirty repository")
        @Baseline(Scenario.FROM_SCRATCH)
        void isUpToDateInDirtyRepositoryTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertFalse(command.isUpToDate());

            // add some commits to the repository and after one run the task should be up to date
            script.andCommitWithTag("111.122.133");
            command.run();
            
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME))
                assertFalse(command.isUpToDate());
            else assertTrue(command.isUpToDate()); 

            // if we add uncommitted files it must return still return true
            script.addRandomTextWorkbenchFiles(1);
            // when the command is executed standalone, Infer is not executed so isUpToDate() will always return false
            if (command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertFalse(command.isUpToDate());
            }
            else {
                command.run();
                assertTrue(command.isUpToDate());

                // still true even after staging
                script.stage();
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // but returns false with a new commit as long as it doesn't run again
                script.andCommitWithTag("111.122.144");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());
            }
        }
    }

    @Nested
    @DisplayName("Mark idempotency")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class IdempotencyTests {
        /**
         * Check that multiple runs yield to the same result with a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Mark idempotency with commit message convention")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void idempotencyWithCommitMessageConvention(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the minor identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("minor", ".*")))
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // run a first time
                command.run();

                // collect its state values
                String branch = command.state().getBranch();
                String bump = command.state().getBump();
                Boolean coreVersion = command.state().getCoreVersion();
                Boolean latestVersion = command.state().getLatestVersion();
                Boolean newRelease = command.state().getNewRelease();
                Boolean newVersion = command.state().getNewVersion();
                List<Commit> commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                List<Commit> significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                String previousVersion = command.state().getReleaseScope().getPreviousVersion();
                Commit previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                String matchBranches = command.state().getReleaseType().getMatchBranches();
                Scheme scheme = command.state().getScheme();
                Long timestamp = command.state().getTimestamp();
                String version = command.state().getVersion();
                String versionRange = command.state().getVersionRange();

                // collect repository values
                List<String> branches = List.<String>copyOf(script.getBranches());
                List<String> commitIDs = List.<String>copyOf(script.getCommitIDs());
                String lastCommitID = script.getLastCommitID();
                Map<String,String> tags =  Map.<String,String>copyOf(script.getTags());

                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(matchBranches, command.state().getReleaseType().getMatchBranches());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNotEquals(commits, command.state().getReleaseScope().getCommits());
                assertNotEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertNotEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertNotEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(timestamp, command.state().getTimestamp());
                assertNotEquals(version, command.state().getVersion());
                assertNotEquals(commitIDs, script.getCommitIDs());
                assertNotEquals(lastCommitID, script.getLastCommitID());
                assertNotEquals(tags, script.getTags());

                // collect state values again
                branch = command.state().getBranch();
                bump = command.state().getBump();
                coreVersion = command.state().getCoreVersion();
                latestVersion = command.state().getLatestVersion();
                newRelease = command.state().getNewRelease();
                newVersion = command.state().getNewVersion();
                commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                previousVersion = command.state().getReleaseScope().getPreviousVersion();
                previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                matchBranches = command.state().getReleaseType().getMatchBranches();
                scheme = command.state().getScheme();
                timestamp = command.state().getTimestamp();
                version = command.state().getVersion();
                versionRange = command.state().getVersionRange();

                // collect repository values again
                branches = List.<String>copyOf(script.getBranches());
                commitIDs = List.<String>copyOf(script.getCommitIDs());
                lastCommitID = script.getLastCommitID();
                tags = Map.<String,String>copyOf(script.getTags());

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(matchBranches, command.state().getReleaseType().getMatchBranches());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(matchBranches, command.state().getReleaseType().getMatchBranches());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());
            }
        }

        /**
         * Check that multiple runs yield to the same result without a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Mark idempotency without commit message convention")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void idempotencyWithoutCommitMessageConvention(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            // run a first time
            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // collect its state values
                String branch = command.state().getBranch();
                String bump = command.state().getBump();
                Boolean coreVersion = command.state().getCoreVersion();
                Boolean latestVersion = command.state().getLatestVersion();
                Boolean newRelease = command.state().getNewRelease();
                Boolean newVersion = command.state().getNewVersion();
                List<Commit> commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                List<Commit> significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                String previousVersion = command.state().getReleaseScope().getPreviousVersion();
                Commit previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                Scheme scheme = command.state().getScheme();
                Long timestamp = command.state().getTimestamp();
                String version = command.state().getVersion();
                String versionRange = command.state().getVersionRange();

                // collect repository values
                List<String> branches = List.<String>copyOf(script.getBranches());
                List<String> commitIDs = List.<String>copyOf(script.getCommitIDs());
                String lastCommitID = script.getLastCommitID();
                Map<String,String> tags =  Map.<String,String>copyOf(script.getTags());
                
                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNotEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits()); // with no convention no commit is significant
                assertNotEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertNotEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(timestamp, command.state().getTimestamp());
                assertNotEquals(version, command.state().getVersion());
                assertNotEquals(commitIDs, script.getCommitIDs());
                assertNotEquals(lastCommitID, script.getLastCommitID());
                assertNotEquals(tags, script.getTags());
                
                // collect state values again
                branch = command.state().getBranch();
                bump = command.state().getBump();
                coreVersion = command.state().getCoreVersion();
                latestVersion = command.state().getLatestVersion();
                newRelease = command.state().getNewRelease();
                newVersion = command.state().getNewVersion();
                commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                previousVersion = command.state().getReleaseScope().getPreviousVersion();
                previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                scheme = command.state().getScheme();
                timestamp = command.state().getTimestamp();
                version = command.state().getVersion();
                versionRange = command.state().getVersionRange();

                // collect repository values again
                branches = List.<String>copyOf(script.getBranches());
                commitIDs = List.<String>copyOf(script.getCommitIDs());
                lastCommitID = script.getLastCommitID();
                tags = Map.<String,String>copyOf(script.getTags());

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());
            }
        }

        /**
         * Check that multiple runs yield to the same result without a commit message convention configured
         */
        @TestTemplate
        @DisplayName("Mark idempotency in dirty repository")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void idempotencyInDirtyRepository(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                // run a first time
                command.run();

                // collect its state values
                String branch = command.state().getBranch();
                String bump = command.state().getBump();
                Boolean coreVersion = command.state().getCoreVersion();
                Boolean latestVersion = command.state().getLatestVersion();
                Boolean newRelease = command.state().getNewRelease();
                Boolean newVersion = command.state().getNewVersion();
                List<Commit> commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                List<Commit> significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                String previousVersion = command.state().getReleaseScope().getPreviousVersion();
                Commit previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                Scheme scheme = command.state().getScheme();
                Long timestamp = command.state().getTimestamp();
                String version = command.state().getVersion();
                String versionRange = command.state().getVersionRange();

                // collect repository values
                List<String> branches = List.<String>copyOf(script.getBranches());
                List<String> commitIDs = List.<String>copyOf(script.getCommitIDs());
                String lastCommitID = script.getLastCommitID();
                Map<String,String> tags =  Map.<String,String>copyOf(script.getTags());

                // add some uncommitted changes
                script.updateAllWorkbenchFiles();

                // run again and check that all values are still the same
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // add some commits to the repository and after one run the task should be up to date
                script.andCommitWithTag("111.122.133");
                assertFalse(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                // check that some values have changed
                assertNotEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits()); // with no convention no commit is significant
                assertNotEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertNotEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(timestamp, command.state().getTimestamp());
                assertNotEquals(version, command.state().getVersion());
                assertNotEquals(commitIDs, script.getCommitIDs());
                assertNotEquals(lastCommitID, script.getLastCommitID());
                assertNotEquals(tags, script.getTags());
                
                // collect state values again
                branch = command.state().getBranch();
                bump = command.state().getBump();
                coreVersion = command.state().getCoreVersion();
                latestVersion = command.state().getLatestVersion();
                newRelease = command.state().getNewRelease();
                newVersion = command.state().getNewVersion();
                commits = List.<Commit>copyOf(command.state().getReleaseScope().getCommits());
                significantCommits = List.<Commit>copyOf(command.state().getReleaseScope().getSignificantCommits());
                previousVersion = command.state().getReleaseScope().getPreviousVersion();
                previousVersionCommit = command.state().getReleaseScope().getPreviousVersionCommit();
                scheme = command.state().getScheme();
                timestamp = command.state().getTimestamp();
                version = command.state().getVersion();
                versionRange = command.state().getVersionRange();

                // collect repository values again
                branches = List.<String>copyOf(script.getBranches());
                commitIDs = List.<String>copyOf(script.getCommitIDs());
                lastCommitID = script.getLastCommitID();
                tags = Map.<String,String>copyOf(script.getTags());

                // add some uncommitted changes
                script.updateAllWorkbenchFiles();

                // run again and make sure values didn't change
                assertTrue(command.isUpToDate());
                command.run();
                assertTrue(command.isUpToDate());

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());

                // add some uncommitted changes
                script.updateAllWorkbenchFiles();

                // once more, also considering that its still up to date
                assertTrue(command.isUpToDate());
                command.run();

                assertEquals(branch, command.state().getBranch());
                assertEquals(bump, command.state().getBump());
                assertEquals(coreVersion, command.state().getCoreVersion());
                assertEquals(latestVersion, command.state().getLatestVersion());
                assertEquals(newRelease, command.state().getNewRelease());
                assertEquals(newVersion, command.state().getNewVersion());
                assertEquals(commits, command.state().getReleaseScope().getCommits());
                assertEquals(significantCommits, command.state().getReleaseScope().getSignificantCommits());
                assertEquals(previousVersion, command.state().getReleaseScope().getPreviousVersion());
                assertEquals(previousVersionCommit, command.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(scheme, command.state().getScheme());
                assertEquals(timestamp, command.state().getTimestamp());
                assertEquals(version, command.state().getVersion());
                assertEquals(versionRange, command.state().getVersionRange());
                assertEquals(branches, script.getBranches());
                assertEquals(commitIDs, script.getCommitIDs());
                assertEquals(lastCommitID, script.getLastCommitID());
                assertEquals(tags, script.getTags());
            }
        }
    }

    @Nested
    @DisplayName("Mark run")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class RunTests {
        @TestTemplate
        @DisplayName("Mark.run() throws exception with a valid but empty Git repository in working directory")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionOnRunWithValidButEmptyGitRepositoryTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            assertThrows(Exception.class, () -> command.run());
        }

        @TestTemplate
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push disabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push disabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push enabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push enabled after Infer has generated no new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNoNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.4", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push disabled after Infer has generated a new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.5", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push disabled after Infer has generated a new Version > yield to no new commit or tag, no changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushDisabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.5", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size(), script.getTags().size());
                assertEquals(0, remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.5", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a dirty workspace using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnDirtyWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                assertEquals("0.0.5", command.state().getVersion());
                assertNotEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size()+1, script.getCommitIDs().size());
                assertEquals(previousTags.size()+1, script.getTags().size());
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
            }
        }

        @TestTemplate
        @DisplayName("Mark.run() on a clean workspace using a release type with Commit, Tag and Push enabled after Infer has generated a new Version using multiple tag names > yield to a new commit and multiple tags, with changes pushed to remote")
        @Baseline(Scenario.ONE_BRANCH_SHORT)
        void runOnCleanWorkspaceWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledUsingMultipleTagnamesTest(@CommandSelector(Commands.MARK) CommandProxy command, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            Script remoteScript = Scenario.BARE.realize(true);
            remoteScript.getGitDirectory().deleteOnExit();
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
                            // here 0.0.1 is an existing tag so we test for updating/rewriting tags
                            setGitTagNames(List.<String>of("0.0.1", "{{version}}", "{{versionMajorNumber}}", "{{versionMajorNumber}}.{{versionMinorNumber}}"));
                        }}
                    )
                )
            );
            command.state().getConfiguration().withRuntimeConfiguration(configurationLayerMock);

            command.run();

            // when the command is executed standalone, Infer is not executed so run() will just do nothing as the release scope is undefined
            if (!command.getContextName().equals(StandaloneCommandProxy.CONTEXT_NAME)) {
                assertEquals("0.0.5", command.state().getVersion());
                assertEquals(previousLastCommit, script.getLastCommitID());
                assertEquals(previousCommits.size(), script.getCommitIDs().size());
                assertEquals(previousTags.size()+3, script.getTags().size());
                assertTrue(script.getTags().containsKey(command.state().getVersion()));
                assertTrue(script.getTags().containsKey(command.state().getVersionMajorNumber()));
                assertTrue(script.getTags().containsKey(command.state().getVersionMajorNumber()+"."+command.state().getVersionMinorNumber()));
                assertEquals(script.getTags().size(), remoteScript.getTags().size());
                assertTrue(remoteScript.getTags().containsKey(command.state().getVersion()));
                assertTrue(remoteScript.getTags().containsKey(command.state().getVersionMajorNumber()));
                assertTrue(remoteScript.getTags().containsKey(command.state().getVersionMajorNumber()+"."+command.state().getVersionMinorNumber()));
            }
        }
    }
}
