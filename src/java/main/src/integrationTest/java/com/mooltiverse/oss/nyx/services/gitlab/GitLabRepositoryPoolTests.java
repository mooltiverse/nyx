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

import com.mooltiverse.oss.nyx.git.tools.Script;

@DisplayName("GitLabRepositoryPool")
public class GitLabRepositoryPoolTests {
    @Nested
    @DisplayName("Naming")
    class NamingTests {
        @Test
        @DisplayName("GitLabRepositoryPool.repositoryName() is scoped by environment, visibility and slot")
        public void repositoryNameIsScopedByEnvironmentVisibilityAndSlot()
            throws Exception {
            GitLabRepositoryPool pool = new GitLabRepositoryPool("token", "env");
            assertEquals("nyx-test-pool-java-env-0", pool.repositoryName(0));
            assertEquals("nyx-test-pool-java-env-private-1", pool.repositoryName(1, true));
        }

        @Test
        @DisplayName("new GitLabRepositoryPool() throws exceptions with invalid arguments")
        public void constructorWithInvalidArguments()
            throws Exception {
            assertThrows(NullPointerException.class, () -> new GitLabRepositoryPool(null, "env"));
            assertThrows(NullPointerException.class, () -> new GitLabRepositoryPool("token", null));
            assertThrows(IllegalArgumentException.class, () -> new GitLabRepositoryPool(" ", "env"));
            assertThrows(IllegalArgumentException.class, () -> new GitLabRepositoryPool("token", "__"));
        }
    }

    @Nested
    @DisplayName("Acquire")
    class AcquireTests {
        @Test
        @DisplayName("GitLabRepositoryPool.acquire() returns a clean repository")
        public void acquireReturnsCleanRepository()
            throws Exception {
            // the 'gitLabTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            String token = System.getProperty("gitLabTestUserToken");
            GitLabRepositoryPool pool = GitLabRepositoryPool.instance(token);
            GitLabRepository repository = pool.acquire(0);

            assertEquals(pool.repositoryName(0), repository.getName());
            assertEquals(GitLabRepositoryPool.DEFAULT_BRANCH, repository.getDefaultBranch());
            assertEquals(List.<String>of(), pool.inspect(0));

            // a clone has the reference commit only
            Script script = Script.cloneFrom(repository.getHTTPURL(), "PRIVATE-TOKEN", token);
            script.getWorkingDirectory().deleteOnExit();
            assertEquals(List.<String>of(GitLabRepositoryPool.referenceCommitID().getName()), script.getCommitIDs());
            assertEquals(GitLabRepositoryPool.DEFAULT_BRANCH, script.getCurrentBranch());
            assertTrue(script.getTags().isEmpty());
        }

        @Test
        @DisplayName("GitLabRepositoryPool.acquire() cleans a repository left dirty by a previous test")
        public void acquireCleansDirtyRepository()
            throws Exception {
            String token = System.getProperty("gitLabTestUserToken");
            GitLabRepositoryPool pool = GitLabRepositoryPool.instance(token);
            GitLabRepository repository = pool.acquire(0);

            // leave the repository dirty in all the ways a test can: new commits on the default branch,
            // other branches, tags and releases
            Script script = Script.cloneFrom(repository.getHTTPURL(), "PRIVATE-TOKEN", token);
            script.getWorkingDirectory().deleteOnExit();
            script.andAddFiles().andCommit("Dirty commit on the default branch").andTag("9.9.9", null);
            script.push("PRIVATE-TOKEN", token);
            script.inBranch("dirty").andAddFiles().andCommit("Dirty commit on another branch").andTag("9.9.10", "Annotated tag");
            script.push("PRIVATE-TOKEN", token);
            GitLab gitLab = GitLab.instance(Map.<String,String>of(GitLab.AUTHENTICATION_TOKEN_OPTION_NAME, token));
            String owner = gitLab.getAuthenticatedUser().getUserName();
            gitLab.publishRelease(owner, repository.getName(), "9.9.9", "9.9.9", "A release left by a previous test", null);
            assertFalse(pool.inspect(0).isEmpty());

            repository = pool.acquire(0);

            assertEquals(List.<String>of(), pool.inspect(0));
            assertNull(gitLab.getReleaseByTag(owner, repository.getName(), "9.9.9"));
            Script clone = Script.cloneFrom(repository.getHTTPURL(), "PRIVATE-TOKEN", token);
            clone.getWorkingDirectory().deleteOnExit();
            assertEquals(List.<String>of(GitLabRepositoryPool.referenceCommitID().getName()), clone.getCommitIDs());
            assertTrue(clone.getTags().isEmpty());
        }
    }
}
