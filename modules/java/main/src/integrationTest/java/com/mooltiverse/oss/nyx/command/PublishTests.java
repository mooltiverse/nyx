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

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.Provider;
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
        @DisplayName("Publish.run() after a new release has been created on a GitHub hosted repository using only global assets > publishes a new version with local files published as release assets")
        void runWithNewReleaseAndGlobalAssetsOnGitHubRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            GitHubUser user = gitHub.getAuthenticatedUser();
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, false, true);

            Path assetPath1 = Files.createTempFile("nyx-test-githun-release-test-", ".txt");
            Files.write(assetPath1, "content1".getBytes());
            Path assetPath2 = Files.createTempFile("nyx-test-github-release-test-", ".bin");
            Files.write(assetPath2, "content2".getBytes());

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            script.push(System.getProperty("gitHubTestUserToken"), "");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add release assets to the configuration
            configurationLayerMock.setReleaseAssets(Map.<String,Attachment>of(
                "asset1", new Attachment("asset1.txt", "Text asset", "text/plain", assetPath1.toFile().getAbsolutePath()),
                "asset2", new Attachment("asset2", "Binary asset", "application/octet-stream", assetPath2.toFile().getAbsolutePath()),
                "nonexistentfile", new Attachment("nonexistentfile", "Non existent asset", "application/octet-stream", "nonexistentfile"), // this file does not exist and should only generate a warning
                "remote1", new Attachment("remote1", "Remote link asset", "application/octet-stream", "http://www.example.com/remote1") // this is an URL and should be skipped by GitHub
            ));

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
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration(System.getProperty("gitHubTestUserToken"), ""))
            );
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            // this release type does not filter release assets so all global ones must be published
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
            Thread.sleep(2000);

            // read the release from the hosting service
            GitHubRelease gitHubRelease = gitHub.getReleaseByTag(user.getUserName(), gitHubRepository.getName(), "1.0.0");
        
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNotNull(gitHubRelease);
            assertEquals("1.0.0", gitHubRelease.getTag());
            assertEquals("1.0.0", gitHubRelease.getTitle());
            // the release assets must contain the two existing files but not the remote URL (not supported by GitHub), nor the non existing file
            assertEquals(2, gitHubRelease.getAssets().size());
            for (Attachment asset: gitHubRelease.getAssets()) {
                assertTrue(asset.getFileName().equals("asset1") || asset.getFileName().equals("asset2"));
                assertTrue(asset.getDescription().equals("Text asset") || asset.getDescription().equals("Binary asset"));
                assertTrue(asset.getType().equals("text/plain") || asset.getType().equals("application/octet-stream"));
                assertTrue(asset.getPath().startsWith("https://api.github.com/repos/"));
            }
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @Test
        @DisplayName("Publish.run() after a new release has been created on a GitHub hosted repository using filtered assets > publishes a new version with filtered assets published as release assets")
        void runWithNewReleaseAndFilteredAssetsOnGitHubRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            GitHubUser user = gitHub.getAuthenticatedUser();
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, false, true);

            Path assetPath1 = Files.createTempFile("nyx-test-githun-release-test-", ".txt");
            Files.write(assetPath1, "content1".getBytes());
            Path assetPath2 = Files.createTempFile("nyx-test-github-release-test-", ".bin");
            Files.write(assetPath2, "content2".getBytes());

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            script.push(System.getProperty("gitHubTestUserToken"), "");

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add release assets to the configuration
            configurationLayerMock.setReleaseAssets(Map.<String,Attachment>of(
                "asset1", new Attachment("asset1.txt", "Text asset", "text/plain", assetPath1.toFile().getAbsolutePath()),
                "asset2", new Attachment("asset2", "Binary asset", "application/octet-stream", assetPath2.toFile().getAbsolutePath()),
                "nonexistentfile", new Attachment("nonexistentfile", "Non existent asset", "application/octet-stream", "nonexistentfile"), // this file does not exist and should only generate a warning
                "remote1", new Attachment("remote1", "Remote link asset", "application/octet-stream", "http://www.example.com/remote1") // this is an URL and should be skipped by GitHub
            ));

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
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration(System.getProperty("gitHubTestUserToken"), ""))
            );
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            // this release type only allows the 'asset1' to be published as release asset
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of("github"),
                    List.<String>of(), // default 'origin' is used if we don't specify the remotes here
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setAssets(List.<String>of("asset1"));
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
            Thread.sleep(2000);

            // read the release from the hosting service
            GitHubRelease gitHubRelease = gitHub.getReleaseByTag(user.getUserName(), gitHubRepository.getName(), "1.0.0");
        
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNotNull(gitHubRelease);
            assertEquals("1.0.0", gitHubRelease.getTag());
            assertEquals("1.0.0", gitHubRelease.getTitle());
            // the release assets must contain only the 'asset1' as it was filtered by the release type
            assertEquals(1, gitHubRelease.getAssets().size());
            for (Attachment asset: gitHubRelease.getAssets()) {
                assertTrue(asset.getFileName().equals("asset1"));
                assertTrue(asset.getDescription().equals("Text asset"));
                assertTrue(asset.getType().equals("text/plain"));
                assertTrue(asset.getPath().startsWith("https://api.github.com/repos/"));
            }
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @Test
        @DisplayName("Publish.run() after a new release has been created on a GitLab hosted repository using only global assets > publishes a new version with local files and URLs published as release assets")
        void runWithNewReleaseAndGlobalAssetsOnGitLabRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabUser user = gitLab.getAuthenticatedUser();
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, false, true);

            Path assetPath1 = Files.createTempFile("nyx-test-gitlab-release-test-", ".txt");
            Files.write(assetPath1, "content1".getBytes());
            Path assetPath2 = Files.createTempFile("nyx-test-gitlab-release-test-", ".bin");
            Files.write(assetPath2, "content2".getBytes());

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            // when a token for user and password authentication for plain Git operations against a GitLab repository,
            // the user is the "PRIVATE-TOKEN" string and the password is the token
            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            script.push("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add release assets to the configuration
            configurationLayerMock.setReleaseAssets(Map.<String,Attachment>of(
                "asset1", new Attachment("asset1.txt", "Text asset", "text/plain", assetPath1.toFile().getAbsolutePath()),
                "asset2", new Attachment("asset2", "Binary asset", "application/octet-stream", assetPath2.toFile().getAbsolutePath()),
                "nonexistentfile", new Attachment("nonexistentfile", "Non existent asset", "application/octet-stream", "nonexistentfile"), // this file does not exist and should only generate a warning
                "remote1", new Attachment("remote1", "Remote link asset", "application/octet-stream", "http://www.example.com/remote1") // this is an URL and should be published by GitLab
            ));

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
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken")))
            );
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            // this release type does not filter release assets so all global ones must be published
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
            Thread.sleep(2000);

            // read the release from the hosting service
            GitLabRelease gitLabRelease = gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0");
        
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNotNull(gitLabRelease);
            assertEquals("1.0.0", gitLabRelease.getTag());
            assertEquals("1.0.0", gitLabRelease.getTitle());
            // the release assets must contain the two existing files and the remote URL, but not the non existing file
            assertEquals(3, gitLabRelease.getAssets().size());
            for (Attachment asset: gitLabRelease.getAssets()) {
                assertTrue(asset.getFileName().equals("asset1") || asset.getFileName().equals("asset2") || asset.getFileName().equals("remote1"));
                //assertTrue(asset.getDescription().equals("Text asset") || asset.getDescription().equals("Binary asset") || asset.getDescription().equals("Remote link asset")); // the description is not available via this API
                //assertTrue(asset.getType().equals("text/plain") || asset.getType().equals("application/octet-stream")); // the content type is not available via this API
                //assertTrue(asset.getPath().startsWith("https://api.github.com/repos/")); // as of now these URLS are like https://storage.googleapis.com...
            }
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }

        @Test
        @DisplayName("Publish.run() after a new release has been created on a GitLab hosted repository using filtered assets > publishes a new version with filtered assets published as release assets")
        void runWithNewReleaseAndFilteredAssetsOnGitLabRepositoryTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabUser user = gitLab.getAuthenticatedUser();
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, false, true);

            Path assetPath1 = Files.createTempFile("nyx-test-gitlab-release-test-", ".txt");
            Files.write(assetPath1, "content1".getBytes());
            Path assetPath2 = Files.createTempFile("nyx-test-gitlab-release-test-", ".bin");
            Files.write(assetPath2, "content2".getBytes());

            // if we clone too quickly next calls may fail
            Thread.sleep(2000);

            // when a token for user and password authentication for plain Git operations against a GitLab repository,
            // the user is the "PRIVATE-TOKEN" string and the password is the token
            Script script = Scenario.ONE_BRANCH_SHORT.applyOnClone(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            script.push("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            // add release assets to the configuration
            configurationLayerMock.setReleaseAssets(Map.<String,Attachment>of(
                "asset1", new Attachment("asset1.txt", "Text asset", "text/plain", assetPath1.toFile().getAbsolutePath()),
                "asset2", new Attachment("asset2", "Binary asset", "application/octet-stream", assetPath2.toFile().getAbsolutePath()),
                "nonexistentfile", new Attachment("nonexistentfile", "Non existent asset", "application/octet-stream", "nonexistentfile"), // this file does not exist and should only generate a warning
                "remote1", new Attachment("remote1", "Remote link asset", "application/octet-stream", "http://www.example.com/remote1") // this is an URL and should be published by GitLab
            ));

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
            // set up the Git remote credentials
            configurationLayerMock.getGit().setRemotes(Map.<String,GitRemoteConfiguration>of(
                "origin", new GitRemoteConfiguration("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken")))
            );
            // add a custom release type that always enables committing, tagging and pushing
            // and all the publishing service enabled
            // this release type only allows the 'asset1' to be published as release asset
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("testReleaseType"),
                    List.<String>of("gitlab"),
                    List.<String>of(), // default 'origin' is used if we don't specify the remotes here
                    Map.<String,ReleaseType>of("testReleaseType", new ReleaseType() {
                        {
                            setAssets(List.<String>of("asset1"));
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
            Thread.sleep(2000);

            // read the release from the hosting service
            GitLabRelease gitLabRelease = gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0");
        
            assertEquals("1.0.0", nyx.state().getVersion());
            assertNotNull(gitLabRelease);
            assertEquals("1.0.0", gitLabRelease.getTag());
            assertEquals("1.0.0", gitLabRelease.getTitle());
            // the release assets must contain only the 'asset1' as it was filtered by the release type
            assertEquals(1, gitLabRelease.getAssets().size());
            for (Attachment asset: gitLabRelease.getAssets()) {
                assertTrue(asset.getFileName().equals("asset1"));
                //assertTrue(asset.getDescription().equals("Text asset")); // the description is not available via this API
                //assertTrue(asset.getType().equals("text/plain")); // the content type is not available via this API
                //assertTrue(asset.getPath().startsWith("https://api.github.com/repos/")); // as of now these URLS are like https://storage.googleapis.com...
            }
        
            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(2000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }
}