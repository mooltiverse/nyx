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

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.git.tools.Script;

@DisplayName("GitHubRepositoryPool")
public class GitHubRepositoryPoolTests {
    @Nested
    @DisplayName("Naming")
    class NamingTests {
        @Test
        @DisplayName("GitHubRepositoryPool.repositoryName() is scoped by environment and slot")
        public void repositoryNameIsScopedByEnvironmentAndSlot()
            throws Exception {
            GitHubRepositoryPool pool = new GitHubRepositoryPool("token", "env");
            assertEquals("nyx-test-pool-java-env-0", pool.repositoryName(0));
            assertEquals("nyx-test-pool-java-env-3", pool.repositoryName(3));
            assertNotEquals(pool.repositoryName(0), new GitHubRepositoryPool("token", "other").repositoryName(0));
            assertEquals("nyx-test-pool-java-env-private-0", pool.repositoryName(0, true));
            assertEquals(pool.repositoryName(0), pool.repositoryName(0, false));
        }

        @Test
        @DisplayName("GitHubRepositoryPool.instance(String, String) appends the scope to the default environment")
        public void instanceWithScope()
            throws Exception {
            GitHubRepositoryPool pool = GitHubRepositoryPool.instance("token", "Functional");
            assertEquals(GitHubRepositoryPool.defaultEnvironment().concat("-functional"), pool.getEnvironment());
            assertNotEquals(GitHubRepositoryPool.instance("token").repositoryName(0), pool.repositoryName(0));
            assertThrows(NullPointerException.class, () -> GitHubRepositoryPool.instance("token", null));
        }

        @Test
        @DisplayName("GitHubRepositoryPool.repositoryName() throws IllegalArgumentException with a negative slot")
        public void repositoryNameWithNegativeSlot()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> new GitHubRepositoryPool("token", "env").repositoryName(-1));
        }

        @Test
        @DisplayName("GitHubRepositoryPool.sanitize() only retains characters allowed in repository names")
        public void sanitize()
            throws Exception {
            assertEquals("local-john-doe", GitHubRepositoryPool.sanitize("local-John Doe"));
            assertEquals("ci-windows-17", GitHubRepositoryPool.sanitize("ci-Windows-17"));
            assertEquals("a-b", GitHubRepositoryPool.sanitize("--a_.b--"));
            assertEquals("local-domain-user", new GitHubRepositoryPool("token", "local-DOMAIN\\user").getEnvironment());
        }

        @Test
        @DisplayName("GitHubRepositoryPool.defaultEnvironment() is a valid environment name")
        public void defaultEnvironment()
            throws Exception {
            String environment = GitHubRepositoryPool.defaultEnvironment();
            assertFalse(environment.isEmpty());
            assertEquals(GitHubRepositoryPool.sanitize(environment), environment);
            assertTrue(environment.startsWith("ci-") || environment.startsWith("local-"));
        }

        @Test
        @DisplayName("new GitHubRepositoryPool() throws exceptions with invalid arguments")
        public void constructorWithInvalidArguments()
            throws Exception {
            assertThrows(NullPointerException.class, () -> new GitHubRepositoryPool(null, "env"));
            assertThrows(NullPointerException.class, () -> new GitHubRepositoryPool("token", null));
            assertThrows(IllegalArgumentException.class, () -> new GitHubRepositoryPool(" ", "env"));
            assertThrows(IllegalArgumentException.class, () -> new GitHubRepositoryPool("token", "__"));
        }
    }

    @Nested
    @DisplayName("Reference commit")
    class ReferenceCommitTests {
        @Test
        @DisplayName("GitHubRepositoryPool.referenceCommitID() is deterministic")
        public void referenceCommitIDIsDeterministic()
            throws Exception {
            assertEquals(GitHubRepositoryPool.referenceCommitID(), GitHubRepositoryPool.referenceCommitID());
        }
    }

    @Nested
    @DisplayName("Acquire")
    class AcquireTests {
        @Test
        @DisplayName("GitHubRepositoryPool.acquire() returns a clean repository")
        public void acquireReturnsCleanRepository()
            throws Exception {
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHubRepositoryPool pool = GitHubRepositoryPool.instance(System.getProperty("gitHubTestUserToken"));
            GitHubRepository repository = pool.acquire(0);

            assertEquals(pool.repositoryName(0), repository.getName());
            assertEquals(GitHubRepositoryPool.DEFAULT_BRANCH, repository.getDefaultBranch());
            assertEquals(List.<String>of(), pool.inspect(0));

            // a clone has the reference commit only
            Script script = Script.cloneFrom(repository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), "");
            script.getWorkingDirectory().deleteOnExit();
            assertEquals(List.<String>of(GitHubRepositoryPool.referenceCommitID().getName()), script.getCommitIDs());
            assertEquals(GitHubRepositoryPool.DEFAULT_BRANCH, script.getCurrentBranch());
            assertTrue(script.getTags().isEmpty());
        }

        @Test
        @DisplayName("GitHubRepositoryPool.acquire() cleans a repository left dirty by a previous test")
        public void acquireCleansDirtyRepository()
            throws Exception {
            String token = System.getProperty("gitHubTestUserToken");
            GitHubRepositoryPool pool = GitHubRepositoryPool.instance(token);
            GitHubRepository repository = pool.acquire(0);

            // leave the repository dirty in all the ways a test can: new commits on the default branch,
            // other branches, tags and releases
            Script script = Script.cloneFrom(repository.getHTTPURL(), token, "");
            script.getWorkingDirectory().deleteOnExit();
            script.andAddFiles().andCommit("Dirty commit on the default branch").andTag("9.9.9", null);
            script.push(token, "");
            script.inBranch("dirty").andAddFiles().andCommit("Dirty commit on another branch").andTag("9.9.10", "Annotated tag");
            script.push(token, "");
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, token));
            gitHub.publishRelease(gitHub.getAuthenticatedUser().getUserName(), repository.getName(), "9.9.9", "9.9.9", "A release left by a previous test", null);
            assertFalse(pool.inspect(0).isEmpty());

            repository = pool.acquire(0);

            assertEquals(List.<String>of(), pool.inspect(0));
            assertNull(gitHub.getReleaseByTag(gitHub.getAuthenticatedUser().getUserName(), repository.getName(), "9.9.9"));
            Script clone = Script.cloneFrom(repository.getHTTPURL(), token, "");
            clone.getWorkingDirectory().deleteOnExit();
            assertEquals(List.<String>of(GitHubRepositoryPool.referenceCommitID().getName()), clone.getCommitIDs());
            assertTrue(clone.getTags().isEmpty());
        }

        @Test
        @DisplayName("GitHubRepositoryPool.acquire() returns a clean private repository that requires credentials")
        public void acquireReturnsCleanPrivateRepository()
            throws Exception {
            String token = System.getProperty("gitHubTestUserToken");
            GitHubRepositoryPool pool = GitHubRepositoryPool.instance(token);
            GitHubRepository repository = pool.acquire(0, true);

            assertEquals(pool.repositoryName(0, true), repository.getName());
            assertEquals(List.<String>of(), pool.inspect(0, true));
            // the private repository can't be cloned without credentials
            assertThrows(Exception.class, () -> Script.cloneFrom(repository.getHTTPURL(), (String)null, (String)null));
            Script script = Script.cloneFrom(repository.getHTTPURL(), token, "");
            script.getWorkingDirectory().deleteOnExit();
            assertEquals(List.<String>of(GitHubRepositoryPool.referenceCommitID().getName()), script.getCommitIDs());
        }
    }
}
