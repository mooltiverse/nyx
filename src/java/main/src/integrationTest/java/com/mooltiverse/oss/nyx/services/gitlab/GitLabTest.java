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
package com.mooltiverse.oss.nyx.services.gitlab;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.Release;
import com.mooltiverse.oss.nyx.services.Service;

@DisplayName("GitLab")
public class GitLabTest {
    @Nested
    @DisplayName("Instance")
    class InstanceTest {
        @Test
        public void exceptionWithNullOptions()
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitLab.instance(null));
        }

        @Test
        public void instanceWithEmptyOptions()
            throws Exception {
            GitLab service = GitLab.instance(Map.<String,String>of());
            assertNotNull(service);
        }
    }

    @Nested
    @DisplayName("Supports")
    class SupportsTest {
        @ParameterizedTest(name = "GitLab.instance().supports(''{0}'') == true")
        @EnumSource(Service.Feature.class)
        public void supportAnyFeature(Service.Feature feature)
            throws Exception {
            if (Service.Feature.GIT_HOSTING.equals(feature) || Service.Feature.RELEASES.equals(feature) || Service.Feature.RELEASE_ASSETS.equals(feature) || Service.Feature.USERS.equals(feature))
                assertTrue(GitLab.instance(Map.<String,String>of()).supports(feature));
            else assertFalse(GitLab.instance(Map.<String,String>of()).supports(feature));
        }
    }

    @Nested
    @DisplayName("User Service")
    class UserServiceTest {
        @Test
        @DisplayName("GitLab.instance().getAuthenticatedUser() throws SecurityException with no authentication token")
        public void exceptionUsingGetAuthenticatedUserWithNoAuthenticationToken()
            throws Exception {
            assertThrows(com.mooltiverse.oss.nyx.services.SecurityException.class, () -> GitLab.instance(Map.<String,String>of()).getAuthenticatedUser());
        }

        @Test
        @DisplayName("GitLab.instance().getAuthenticatedUser()")
        public void getAuthenticatedUser()
            throws Exception {
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLabUser user = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken"))).getAuthenticatedUser();
            assertNotNull(user.getID());
            assertNotNull(user.getUserName());
            assertNotNull(user.geFullName());
        }
    }

    @Nested
    @DisplayName("Hosting Service")
    class HostingServiceTest {
        @Test
        @DisplayName("GitLab.instance().createGitRepository() and GitLab.instance().deleteGitRepository()")
        public void createGitRepository()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabUser user = gitLab.getAuthenticatedUser();
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, true, false);
            
            assertEquals("main", gitLabRepository.getDefaultBranch());
            assertEquals("Test repository "+randomID, gitLabRepository.getDescription());
            assertEquals(randomID, gitLabRepository.getName());
            assertEquals(randomID, gitLabRepository.getFullName());
            assertEquals("https://gitlab.com/"+user.getUserName()+"/"+randomID+".git", gitLabRepository.getHTTPURL());
            assertEquals("git@gitlab.com:"+user.getUserName()+"/"+randomID+".git", gitLabRepository.getSSHURL());

            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }

    @Nested
    @DisplayName("Release Service")
    class ReleaseServiceTest {
        @Test
        @DisplayName("GitLab.instance().createRelease() and GitLab.instance().getRelease()")
        public void createRelease()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));
            GitLabUser user = gitLab.getAuthenticatedUser();
            GitLabRepository gitLabRepository = gitLab.createGitRepository(randomID, "Test repository "+randomID, false, true);

            assertNull(gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0-alpha.1"));

            // if we clone too quickly next calls may fail
            Thread.sleep(4000);

            // when a token for user and password authentication for plain Git operations against a GitLab repository,
            // the user is the "PRIVATE-TOKEN" string and the password is the token
            Script script = Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED.applyOnClone(gitLabRepository.getHTTPURL(), "PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));
            script.getWorkingDirectory().deleteOnExit();
            script.push("PRIVATE-TOKEN", System.getProperty("gitLabTestUserToken"));

            // publish the release
            Release release = gitLab.publishRelease(user.getUserName(), gitLabRepository.getName(), "Release 1.0.0-alpha.1", "1.0.0-alpha.1", "A test description for the release\non multiple lines\nlike these", null);
            assertEquals("Release 1.0.0-alpha.1", release.getTitle());
            assertEquals("1.0.0-alpha.1", release.getTag());

            // so far the release has no assets attached
            assertTrue(Objects.isNull(release.getAssets()) || release.getAssets().isEmpty());

            // also upload some assets
            // if we uploade too quickly before the release is published next calls may fail
            Thread.sleep(2000);

            Path assetPath1 = Files.createTempFile("nyx-test-gitlab-release-test-", ".txt");
            assetPath1.toFile().deleteOnExit();
            Files.write(assetPath1, "content1".getBytes());
            Path assetPath2 = Files.createTempFile("nyx-test-gitlab-release-test-", ".bin");
            assetPath2.toFile().deleteOnExit();
            Files.write(assetPath2, "content2".getBytes());
            Set<Attachment> assetsToUpload = new HashSet<Attachment>();
            assetsToUpload.add(new Attachment("asset1", "Text asset", "text/plain", assetPath1.toFile().getAbsolutePath()));
            assetsToUpload.add(new Attachment("asset2", "Binary asset", "application/octet-stream", assetPath2.toFile().getAbsolutePath()));
            assetsToUpload.add(new Attachment("nonexistentfile", "Non existent asset", "application/octet-stream", "nonexistentfile")); // this file does not exist and should only generate a warning
            assetsToUpload.add(new Attachment("remote1", "Remote link asset", "application/octet-stream", "http://www.example.com/remote1")); // this is an URL and should be linked to the release
            Release releaseWithAssets = gitLab.publishReleaseAssets(user.getUserName(), gitLabRepository.getName(), release, assetsToUpload);
            assertEquals(3, releaseWithAssets.getAssets().size());
            for (Attachment asset: releaseWithAssets.getAssets()) {
                assertTrue(asset.getFileName().equals("asset1") || asset.getFileName().equals("asset2") || asset.getFileName().equals("remote1"));
                assertTrue(asset.getDescription().equals("Text asset") || asset.getDescription().equals("Binary asset") || asset.getDescription().equals("Remote link asset"));
                assertTrue(asset.getType().equals("text/plain") || asset.getType().equals("application/octet-stream"));
                //assertTrue(asset.getPath().startsWith("https://api.github.com/repos/")); // as of now these URLS are like https://storage.googleapis.com...
            }

            // now test again the getReleaseByTag and also make sure it has the assets
            release = gitLab.getReleaseByTag(user.getUserName(), gitLabRepository.getName(), "1.0.0-alpha.1");
            assertEquals("Release 1.0.0-alpha.1", release.getTitle());
            assertEquals("1.0.0-alpha.1", release.getTag());
            assertEquals(3, release.getAssets().size());
            for (Attachment asset: release.getAssets()) {
                assertTrue(asset.getFileName().equals("Text asset") || asset.getFileName().equals("Binary asset") || asset.getFileName().equals("Remote link asset"));
                //assertTrue(asset.getDescription().equals("Text asset") || asset.getDescription().equals("Binary asset") || asset.getDescription().equals("Remote link asset")); // the description is not available via this API
                //assertTrue(asset.getType().equals("text/plain") || asset.getType().equals("application/octet-stream")); // the content type is not available via this API
                //assertTrue(asset.getPath().startsWith("https://api.github.com/repos/")); // as of now these URLS are like https://storage.googleapis.com...
            }

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }
}
