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
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.github.GitHubRepository;
import com.mooltiverse.oss.nyx.services.gitlab.GitLab;
import com.mooltiverse.oss.nyx.services.gitlab.GitLabRepository;

@DisplayName("Mark")
public class MarkTests {
    @Nested
    @DisplayName("Mark run")
    public static class UpToDateTests {
        @Test
        @DisplayName("Mark.run() on a cloned workspace from GitHub with an additional remote using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to all remotes")
        void runOnGitHubClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            // when a token for user and password authentication for plain Git operations against a GitHub repository,
            // the user is the token and the password is the empty string
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, false, true);

            // if we clone too quickly next calls may fail
            Thread.sleep(4000);

            Script replicaScript = Scenario.FROM_SCRATCH.realize();
            replicaScript.getWorkingDirectory().deleteOnExit();
            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            script.getWorkingDirectory().deleteOnExit();
            script.addRemote(replicaScript.getGitDirectory(), "replica");
            
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a service configuration to pass the credentials required to push to the remote repository
            configurationLayerMock.setServices(Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB, Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")))
                )
            );
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration(System.getProperty("gitHubTestUserToken"), ""))
            );
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
                    List.<String>of("origin", "replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            
            Nyx nyx = new Nyx(script.getWorkingDirectory());
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

            nyx.mark();

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // clone the remote repo again into a a new directory and test
            Script remoteScript = Script.cloneFrom(gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            remoteScript.getWorkingDirectory().deleteOnExit();

            assertEquals("0.0.5", nyx.state().getVersion());
            assertEquals(script.getTags().size(), replicaScript.getTags().size());
            assertEquals(script.getTags().size(), remoteScript.getTags().size());

            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // test for idempotency running the command again
            nyx.mark();
            assertEquals("0.0.5", nyx.state().getVersion());
            assertEquals(script.getTags().size(), replicaScript.getTags().size());
            assertEquals(script.getTags().size(), remoteScript.getTags().size());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @Test
        @DisplayName("Mark.run() on a cloned workspace from GitLab with an additional remote using a release type with Commit, Tag and Push enabled after Infer has generated a new Version > yield to a new commit and a new tag, with changes pushed to all remotes")
        void runOnGitLabClonedWorkspaceWithAdditionalRemoteWithNewVersionOrNewReleaseWithCommitAndTagAndPushEnabledTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            // when a token for user and password authentication for plain Git operations against a GitLab repository,
            // the user is the "PRIVATE-TOKEN" string and the password is the token
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, false, true);

            // if we clone too quickly next calls may fail
            Thread.sleep(4000);

            Script replicaScript = Scenario.FROM_SCRATCH.realize();
            replicaScript.getWorkingDirectory().deleteOnExit();
            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            script.getWorkingDirectory().deleteOnExit();
            script.addRemote(replicaScript.getGitDirectory(), "replica");
            
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a service configuration to pass the credentials required to push to the remote repository
            configurationLayerMock.setServices(Map.<String,ServiceConfiguration>of(
                    "gitlab", new ServiceConfiguration(Provider.GITLAB, Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")))
                )
            );
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken")))
            );
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
                    List.<String>of("origin", "replica"),
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                        }}
                    )
                )
            );
            
            Nyx nyx = new Nyx(script.getWorkingDirectory());
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

            nyx.mark();

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // clone the remote repo again into a a new directory and test
            Script remoteScript = Script.cloneFrom(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            remoteScript.getWorkingDirectory().deleteOnExit();

            assertEquals("0.0.5", nyx.state().getVersion());
            assertEquals(script.getTags().size(), replicaScript.getTags().size());
            assertEquals(script.getTags().size(), remoteScript.getTags().size());

            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // test for idempotency running the command again
            nyx.mark();
            assertEquals("0.0.5", nyx.state().getVersion());
            assertEquals(script.getTags().size(), replicaScript.getTags().size());
            assertEquals(script.getTags().size(), remoteScript.getTags().size());

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }
}
