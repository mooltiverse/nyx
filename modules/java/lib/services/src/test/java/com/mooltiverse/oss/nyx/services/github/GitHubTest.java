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
package com.mooltiverse.oss.nyx.services.github;

import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import com.mooltiverse.oss.nyx.services.GitServiceFeature;

@DisplayName("GitHub")
public class GitHubTest {
    @Nested
    @DisplayName("Instance")
    class InstanceTest {
        @Test
        public void exceptionWithNullBaseURI()
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitHub.instance((URI)null));
        }

        @Test
        public void exceptionWithNullBaseURIAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> GitHub.instance((String)null));
        }

        @Test
        public void exceptionWithEmptyURIAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> GitHub.instance(""));
            assertThrows(IllegalArgumentException.class, () -> GitHub.instance("  "));
        }

        @Test
        public void instanceWithDefaultBaseURI()
            throws Exception {
            assertEquals(new URI(GitHub.API_BASE_URL), GitHub.instance().getBaseURI());
        }
    }

    @Disabled("Ping sends unauthenticated requests that may fail due to rate limits")
    @Nested
    @DisplayName("Ping")
    class PingTest {
        @Test
        public void pingWithDefaultBaseURI()
            throws Exception {
            assertTrue(GitHub.instance().ping());
        }
    }

    @Disabled("Ping sends unauthenticated requests that may fail due to rate limits")
    @Nested
    @DisplayName("Supports")
    class SupportsTest {
        @ParameterizedTest(name = "GitHub.instance().supports(''{0}'') == true")
        @EnumSource(GitServiceFeature.class)
        public void supportAnyFeature(GitServiceFeature feature)
            throws Exception {
            assertTrue(GitHub.instance().supports(feature));
        }
    }
}

/*
@DisplayName("GitHubRepository")
@TestInstance(Lifecycle.PER_CLASS)
public class GitHubRepositoryTest extends RemoteRepositoryTest {
    private String repositoryName = null;

    //GitHubAccount account = null;

    @BeforeAll
    public void setup()
        throws IOException, InterruptedException {

        //repositoryName = randomAlphabeticString(8);
        //account = new GitHubAccount();

        //account.createRepository(repositoryName, "Nyx test temporary repository created at "+LocalDateTime.now().toString());
    }

    @AfterAll
    public void teardown()
        throws IOException, InterruptedException {
        //account.deleteRepository(repositoryName);
    }
}
*/