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

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.mooltiverse.oss.nyx.services.Release;
import com.mooltiverse.oss.nyx.services.Service;
import com.mooltiverse.oss.nyx.services.git.Scenario;
import com.mooltiverse.oss.nyx.services.git.Script;
import com.mooltiverse.oss.nyx.services.git.util.RandomUtil;

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
            if (Service.Feature.GIT_HOSTING.equals(feature) || Service.Feature.GIT_REMOTE.equals(feature) || Service.Feature.RELEASES.equals(feature) || Service.Feature.USERS.equals(feature))
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
            assertNotNull(user.getAttributes());
            assertFalse(user.getAttributes().isEmpty());
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

            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }

    @Nested
    @DisplayName("Remote Service")
    class RemoteServiceTest {
        @Test
        @DisplayName("GitLab.instance().getSupportedRemoteNames()")
        public void getSupportedRemoteNames()
            throws Exception {
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.REMOTES_OPTION_NAME, "one,two"));

            assertNotNull(gitLab.getSupportedRemoteNames());
            assertEquals(2, gitLab.getSupportedRemoteNames().size());
            assertTrue(gitLab.getSupportedRemoteNames().containsAll(List.<String>of("one", "two")));
        }

        @Test
        @DisplayName("GitLab.instance().getSupportedRemoteNames() with empty list")
        public void getSupportedRemoteNamesWithEmptyList()
            throws Exception {
            GitLab gitLab = GitLab.instance(Map.<String,String>of());

            assertNull(gitLab.getSupportedRemoteNames());
        }

        @Test
        @DisplayName("GitLab.instance().getUserForRemote() and GitLab.instance().getPasswordForRemote()")
        public void getUserForRemote()
            throws Exception {
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitLabTestUserToken")));

            assertEquals("PRIVATE-TOKEN", gitLab.getUser());
            assertEquals(System.getProperty("gitLabTestUserToken"), gitLab.getPassword());
        }

        @Test
        @DisplayName("GitLab.instance().getUserForRemote() and GitLab.instance().getPasswordForRemote() with no credentials")
        public void getUserForRemoteWithNoCredentials()
            throws Exception {
            GitLab gitLab = GitLab.instance(Map.<String,String>of());

            assertNull(gitLab.getUser());
            assertNull(gitLab.getPassword());
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
            Thread.sleep(2000);

            Script script = Scenario.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED.applyOnClone(gitLabRepository.getHTTPURL(), gitLab.getUser(), gitLab.getPassword());
            script.push(gitLab.getUser(), gitLab.getPassword());

            // publish the release
            Release release = gitLab.publishRelease(user.getUserName(), gitLabRepository.getName(), "Release 1.0.0-alpha.1", "1.0.0-alpha.1", "A test description for the release\non multiple lines\nlike these");
            assertEquals("Release 1.0.0-alpha.1", release.getTitle());
            assertEquals("1.0.0-alpha.1", release.getTag());

            // if we delete too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(1000);

            // now delete it
            gitLab.deleteGitRepository(gitLabRepository.getID());
        }
    }
}
