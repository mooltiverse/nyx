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
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.services.Provider;
import com.mooltiverse.oss.nyx.services.git.Scenario;
import com.mooltiverse.oss.nyx.services.git.Script;
import com.mooltiverse.oss.nyx.services.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.github.GitHubRepository;
import com.mooltiverse.oss.nyx.services.github.GitHubRelease;
import com.mooltiverse.oss.nyx.services.github.GitHubUser;
import com.mooltiverse.oss.nyx.services.gitlab.GitLab;
import com.mooltiverse.oss.nyx.services.gitlab.GitLabRepository;
import com.mooltiverse.oss.nyx.services.gitlab.GitLabRelease;
import com.mooltiverse.oss.nyx.services.gitlab.GitLabUser;

@DisplayName("Publish")
public class PublishTests {
    @Nested
    @DisplayName("Publish run")
    public static class RunTests {
        @Test
        @DisplayName("Publish.run() after a new release has been created on a GitHub hosted repository > publishes a new version")
        void runWithNewReleaseOnGitHubRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            GitHubUser user = gitHub.getAuthenticatedUser();
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, false, true);

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            script.push(System.getProperty("gitHubTestUserToken"), "");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the major identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", ".*")))
                )
            );
            // add the test publishing service
            configurationLayerMock.setServices(Map.<String,ServiceConfiguration>of(
                "github", new ServiceConfiguration(Provider.GITHUB, Map.<String,String>of(
                    GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken"),
                    GitHub.REPOSITORY_NAME_OPTION_NAME, gitHubRepository.getName(),
                    GitHub.REPOSITORY_OWNER_OPTION_NAME, user.getUserName()
                ))
            ));
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of("github"),
                    List.<String>of(), // default 'origin' is used if we don't specify the remotes here
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                            setPublish(Boolean.TRUE.toString());
                        }}
                    )
                )
            );

            Nyx nyx = new Nyx(script.getWorkingDirectory());
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

            assertNull(gitHub.getReleaseByTag(user.getUserName(), gitHubRepository.getName(), "1.0.0"));

            nyx.publish();

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // read the release from the hosting service
            GitHubRelease gitHubRelease = gitHub.getReleaseByTag(user.getUserName(), gitHubRepository.getName(), "1.0.0");
        
            assertEquals("main", nyx.state().getBranch()); // new GitHub repositories default to 'main' as the default branch
            assertEquals("major", nyx.state().getBump());
            assertEquals(Defaults.SCHEME, nyx.state().getScheme());
            assertEquals(2, nyx.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), nyx.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), nyx.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", nyx.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), nyx.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", nyx.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), nyx.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(2, nyx.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, nyx.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, nyx.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, nyx.state().getReleaseType().getDescription());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, nyx.state().getReleaseType().getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitPush());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, nyx.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, nyx.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(nyx.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), nyx.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), nyx.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), nyx.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, nyx.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, nyx.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, nyx.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, nyx.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, nyx.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(nyx.state().getNewVersion());
            assertTrue(nyx.state().getNewRelease());
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNull(nyx.state().getVersionRange());
            assertNotNull(gitHubRelease);
            assertEquals("1.0.0", gitHubRelease.getTag());
            assertEquals("1.0.0", gitHubRelease.getTitle());
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @Test
        @DisplayName("Publish.run() after a new release has been created on a GitLab hosted repository > publishes a new version")
        void runWithNewReleaseOnGitLabRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabUser user = gitLab.getAuthenticatedUser();
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, false, true);

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            // when a token for user and password authentication for plain Git operations against a GitLab repository,
            // the user is the "PRIVATE-TOKEN" string and the password is the token
            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            script.push("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add a mock convention that accepts all non null messages and dumps the major identifier for each
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("testConvention"),
                    Map.<String,CommitMessageConvention>of("testConvention", new CommitMessageConvention(".*", Map.<String,String>of("major", ".*")))
                )
            );
            // add the test publishing service
            configurationLayerMock.setServices(Map.<String,ServiceConfiguration>of(
                "gitlab", new ServiceConfiguration(Provider.GITLAB, Map.<String,String>of(
                    GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken"),
                    GitLab.REPOSITORY_NAME_OPTION_NAME, gitLabRepository.getName(),
                    GitLab.REPOSITORY_OWNER_OPTION_NAME, user.getUserName()
                ))
            ));
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of("gitlab"),
                    List.<String>of(), // default 'origin' is used if we don't specify the remotes here
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setGitCommit(Boolean.TRUE.toString());
                            setGitPush(Boolean.TRUE.toString());
                            setGitTag(Boolean.TRUE.toString());
                            setPublish(Boolean.TRUE.toString());
                        }}
                    )
                )
            );

            Nyx nyx = new Nyx(script.getWorkingDirectory());
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

            assertNull(gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0"));

            nyx.publish();

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // read the release from the hosting service
            GitLabRelease gitLabRelease = gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0");
        
            assertEquals("main", nyx.state().getBranch()); // new GitLab repositories default to 'main' as the default branch
            assertEquals("major", nyx.state().getBump());
            assertEquals(Defaults.SCHEME, nyx.state().getScheme());
            assertEquals(2, nyx.state().getReleaseScope().getCommits().size());
            assertEquals(script.getCommitIDs().get(1), nyx.state().getReleaseScope().getInitialCommit().getSHA());
            assertEquals(script.getCommitIDs().get(0), nyx.state().getReleaseScope().getFinalCommit().getSHA());
            assertEquals("0.0.4", nyx.state().getReleaseScope().getPreviousVersion());
            assertEquals(script.getCommitByTag("0.0.4"), nyx.state().getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals("0.0.4", nyx.state().getReleaseScope().getPrimeVersion());
            assertEquals(script.getCommitByTag("0.0.4"), nyx.state().getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(2, nyx.state().getReleaseScope().getSignificantCommits().size());
            assertEquals(Defaults.ReleaseType.COLLAPSE_VERSIONS, nyx.state().getReleaseType().getCollapseVersions());
            assertEquals(Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER, nyx.state().getReleaseType().getCollapsedVersionQualifier());
            assertEquals(Defaults.ReleaseType.DESCRIPTION, nyx.state().getReleaseType().getDescription());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitCommit());
            assertEquals(Defaults.ReleaseType.GIT_COMMIT_MESSAGE, nyx.state().getReleaseType().getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitPush());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getGitTag());
            assertEquals(Defaults.ReleaseType.GIT_TAG_MESSAGE, nyx.state().getReleaseType().getGitTagMessage());
            if (Objects.isNull(Defaults.ReleaseType.IDENTIFIERS)) {
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, nyx.state().getReleaseType().getIdentifiers());
            }
            else {
                assertTrue(Defaults.ReleaseType.IDENTIFIERS.containsAll(nyx.state().getReleaseType().getIdentifiers()));
                for (int i=0; i<=Defaults.ReleaseType.IDENTIFIERS.size(); i++) {
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getQualifier(), nyx.state().getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getValue(), nyx.state().getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.get(i).getPosition(), nyx.state().getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(Defaults.ReleaseType.MATCH_BRANCHES, nyx.state().getReleaseType().getMatchBranches());
            assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, nyx.state().getReleaseType().getMatchEnvironmentVariables());
            assertEquals(Defaults.ReleaseType.MATCH_WORKSPACE_STATUS, nyx.state().getReleaseType().getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), nyx.state().getReleaseType().getPublish());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE, nyx.state().getReleaseType().getVersionRange());
            assertEquals(Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME, nyx.state().getReleaseType().getVersionRangeFromBranchName());
            assertTrue(nyx.state().getNewVersion());
            assertTrue(nyx.state().getNewRelease());
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNull(nyx.state().getVersionRange());
            assertNotNull(gitLabRelease);
            assertEquals("1.0.0", gitLabRelease.getTag());
            assertEquals("1.0.0", gitLabRelease.getTitle());
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }
}