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
package com.mooltiverse.oss.nyx.services.git;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.mooltiverse.oss.nyx.services.Service;

@DisplayName("Git")
public class GitTest {
    @Nested
    @DisplayName("Instance")
    class InstanceTest {
        @Test
        public void exceptionWithNullOptions()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance(null));
        }

        @Test
        public void instanceWithEmptyOptions()
            throws Exception {
            Git service = Git.instance(Map.<String,String>of());
            assertNotNull(service);
        }
    }

    @Nested
    @DisplayName("Supports")
    class SupportsTest {
        @ParameterizedTest(name = "Git.instance().supports(''{0}'') == true")
        @EnumSource(Service.Feature.class)
        public void supportAnyFeature(Service.Feature feature)
            throws Exception {
            if (Service.Feature.GIT_REMOTE.equals(feature))
                assertTrue(Git.instance(Map.<String,String>of()).supports(feature));
            else assertFalse(Git.instance(Map.<String,String>of()).supports(feature));
        }
    }

    @Nested
    @DisplayName("Remote Service")
    class RemoteServiceTest {
        @Test
        @DisplayName("Git.instance().getSupportedRemoteNames()")
        public void getSupportedRemoteNames()
            throws Exception {
            Git git = Git.instance(Map.<String,String>of(Git.REMOTES_OPTION_NAME, "one,two"));

            assertNotNull(git.getSupportedRemoteNames());
            assertEquals(2, git.getSupportedRemoteNames().size());
            assertTrue(git.getSupportedRemoteNames().containsAll(List.<String>of("one", "two")));
        }

        @Test
        @DisplayName("Git.instance().getSupportedRemoteNames() with empty list")
        public void getSupportedRemoteNamesWithEmptyList()
            throws Exception {
            Git git = Git.instance(Map.<String,String>of());

            assertNull(git.getSupportedRemoteNames());
        }

        @Test
        @DisplayName("Git.instance().getUserForRemote() and Git.instance().getPasswordForRemote()")
        public void getUserForRemote()
            throws Exception {
            Git gitHub = Git.instance(Map.<String,String>of(Git.USER_OPTION_NAME, "jdoe", Git.PASSWORD_OPTION_NAME, "pwd"));

            assertEquals("jdoe", gitHub.getUserForRemote());
            assertEquals("pwd", gitHub.getPasswordForRemote());
        }

        @Test
        @DisplayName("Git.instance().getUserForRemote() and Git.instance().getPasswordForRemote() with no credentials")
        public void getUserForRemoteWithNoCredentials()
            throws Exception {
            Git git = Git.instance(Map.<String,String>of());

            assertNull(git.getUserForRemote());
            assertNull(git.getPasswordForRemote());
        }
    }
}
