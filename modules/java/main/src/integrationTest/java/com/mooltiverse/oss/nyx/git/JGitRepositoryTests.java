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
package com.mooltiverse.oss.nyx.git;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.eclipse.jgit.revwalk.RevCommit;

import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.git.util.FileSystemUtil;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.github.GitHubRepository;

@DisplayName("JGitRepository")
public class JGitRepositoryTests {
    /**
     * Use this own project repo as the repository to clone for tests
     */
    public static final String REMOTE_TEST_REPOSITORY_HTTP_URL = "https://github.com/mooltiverse/nyx.git";

    /**
     * Use this own project repo as the repository to clone for tests
     */
    public static final String REMOTE_TEST_REPOSITORY_SSH_URL = "git@github.com:mooltiverse/nyx.git";

    @Nested
    @DisplayName("JGitRepository.clone")
    class CloneTests {
        @DisplayName("JGitRepository.clone(null, String) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> JGitRepository.clone((File)null, REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("JGitRepository.clone('', String) throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.clone("", REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.clone("  ", REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("JGitRepository.clone(String, String) throws GitException with non empty directory")
        @Test
        public void exceptionWithNonEmptyDirectoryAsString()
            throws Exception {
            assertThrows(GitException.class, () -> JGitRepository.clone(Scenario.FROM_SCRATCH.realize().getWorkingDirectory().getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("JGitRepository.clone(File, String) throws GitException with non empty directory")
        @Test
        public void exceptionWithNonEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(GitException.class, () -> JGitRepository.clone(Scenario.FROM_SCRATCH.realize().getWorkingDirectory(), REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("JGitRepository.clone(File, null) throws NullPointerException")
        @Test
        public void exceptionWithNullURIAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile(), null));
        }

        @DisplayName("JGitRepository.clone(String, null) throws NullPointerException")
        @Test
        public void exceptionWithNullURIAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), null));
        }

        @DisplayName("JGitRepository.clone(String, '') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyURIAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), ""));
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), " "));
        }

        @DisplayName("JGitRepository.clone(String, String) throws GitException with non existent URI")
        @Test
        public void exceptionWithNonExistingURIAsString()
            throws Exception {
            assertThrows(GitException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), "https://adomainwiththisnamesuredoesnotexists.com/"));
        }

        @DisplayName("JGitRepository.clone(File, String) throws GitException with non existent URI")
        @Test
        public void exceptionWithNonExistingURI()
            throws Exception {
            assertThrows(GitException.class, () -> JGitRepository.clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile(), "https://adomainwiththisnamesuredoesnotexists.com/"));
        }

        @DisplayName("JGitRepository.clone(File, String)")
        @Test
        public void cloneFileTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory, REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(String, String)")
        @Test
        public void cloneStringTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(File, String, String, String) with non required user and password credentials")
        @Test
        public void cloneFileWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(JGitRepository.clone(directory, REMOTE_TEST_REPOSITORY_HTTP_URL, System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(File, String, String, byte[]) with non required SSH credentials")
        @Test
        public void cloneFileWithNonRequiredSSHCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(JGitRepository.clone(directory, REMOTE_TEST_REPOSITORY_SSH_URL, System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(String, String, String, String) with non required user and password credentials")
        @Test
        public void cloneStringWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL, System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(String, String, String, byte[]) with non required SSH credentials")
        @Test
        public void cloneStringWithNonRequiredSSHCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_SSH_URL, System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("JGitRepository.clone(File, String, null, null) with required user and password credentials")
        @Test
        public void cloneFileWithRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.clone(File, String, null, byte[]) with required SSH unprotected credentials")
        @Test
        public void cloneFileWithRequiredSSHUnprotectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.clone(File, String, null, byte[]) with required SSH protected credentials")
        @Test
        public void cloneFileWithRequiredSSHProtectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.clone(String, String, null, null) with required user and password credentials")
        @Test
        public void cloneStringWithRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.clone(String, String, null, byte[]) with required SSH unprotected credentials")
        @Test
        public void cloneStringWithRequiredSSHUnprotectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.clone(String, String, null, byte[]) with required SSH protected credentials")
        @Test
        public void cloneStringWithRequiredSSHProtectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(JGitRepository.clone(directory.getAbsolutePath(), gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }
    }

    @Nested
    @DisplayName("JGitRepository.open")
    class OpenTests {
        @DisplayName("JGitRepository.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> JGitRepository.open((File)null));
        }

        @DisplayName("JGitRepository.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> JGitRepository.open((String)null));
        }

        @DisplayName("JGitRepository.open('') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.open(""));
            assertThrows(IllegalArgumentException.class, () -> JGitRepository.open("  "));
        }

        @DisplayName("JGitRepository.open(String) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open("adirectorywiththisnamesuredoesnotexists"));
        }

        @DisplayName("JGitRepository.open(File) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(new File("adirectorywiththisnamesuredoesnotexists")));
        }

        @DisplayName("JGitRepository.open(String) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(Files.createTempDirectory(null).toAbsolutePath().toString()));
        }

        @DisplayName("JGitRepository.open(File) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(Files.createTempDirectory(null).toFile()));
        }

        @DisplayName("JGitRepository.open(File)")
        @Test
        public void openFileTest()
            throws Exception {
            assertNotNull(JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory()));
        }

        @DisplayName("JGitRepository.open(String)")
        @Test
        public void openStringTest()
            throws Exception {
            assertNotNull(JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory().getAbsolutePath()));
        }
    }

    @Nested
    @DisplayName("JGitRepository.add")
    class AddTests {
        @DisplayName("JGitRepository.add() throws GitException with empty paths")
        @Test
        public void exceptionWithEmptyPaths()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.add(new ArrayList<String>()));
        }

        @DisplayName("JGitRepository.add() throws NullPointerException with null paths")
        @Test
        public void exceptionWithNullPaths()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(NullPointerException.class, () -> repository.add(null));
        }

        @DisplayName("JGitRepository.add()")
        @Test
        public void addTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remember the cache count may increas of more than 1 for each added file
            int cacheCount = script.getIndexEntryCount();
            script.addRandomTextWorkbenchFiles(1);
            repository.add(List.<String>of("."));
            assertTrue(cacheCount+1 <= script.getIndexEntryCount());

            cacheCount = script.getIndexEntryCount();
            script.addRandomTextWorkbenchFiles(2);
            repository.add(List.<String>of("."));
            assertTrue(cacheCount+2 <= script.getIndexEntryCount());
        }
    }

    @Nested
    @DisplayName("JGitRepository.commit")
    class CommitTests {
        @DisplayName("JGitRepository.commit(String) throws GitException with null message")
        @Test
        public void exceptionWithNullMessageOn1Params()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(null));
        }

        @DisplayName("JGitRepository.commit(String)")
        @Test
        public void commit1Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            RevCommit prevLastCommit = script.getLastCommit();

            script.addRandomTextWorkbenchFiles(1);
            script.stage();

            Commit commit = repository.commit("A message");

            assertNotEquals(prevLastCommit.getId().getName(), script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), commit.getSHA());
            assertEquals(script.getLastCommit().getFullMessage(), commit.getMessage().getFullMessage());
            assertEquals("A message", commit.getMessage().getFullMessage());
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String) throws GitException with empty paths")
        @Test
        public void exceptionWithEmptyPathsOn2Params()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.commit(new ArrayList<String>(), "A message"));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String) throws NullPointerException with null paths")
        @Test
        public void exceptionWithNullPathsOn2Params()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());

            assertThrows(NullPointerException.class, () -> repository.commit(null, "A message"));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String) throws GitException with null message")
        @Test
        public void exceptionWithNullMessageOn2Params()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(List.<String>of("."), null));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String)")
        @Test
        public void commit2Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            RevCommit prevLastCommit = script.getLastCommit();

            script.addRandomTextWorkbenchFiles(1);

            Commit commit = repository.commit(List.<String>of("."), "A message");

            assertNotEquals(prevLastCommit.getId().getName(), script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), commit.getSHA());
            assertEquals(script.getLastCommit().getFullMessage(), commit.getMessage().getFullMessage());
            assertEquals("A message", commit.getMessage().getFullMessage());
        }

        @DisplayName("JGitRepository.commit(String, Identity, Identity) throws GitException with null message")
        @Test
        public void exceptionWithNullMessageOn3Params()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(null, new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(String, Identity, Identity)")
        @Test
        public void commit3Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            RevCommit prevLastCommit = script.getLastCommit();

            script.addRandomTextWorkbenchFiles(1);
            script.stage();

            Commit commit = repository.commit("A message", new Identity("John Doe", "jdoe@example.com"), new Identity("Sean Moe", "smoe@example.com"));

            assertNotEquals(prevLastCommit.getId().getName(), script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), commit.getSHA());
            assertEquals(script.getLastCommit().getFullMessage(), commit.getMessage().getFullMessage());
            assertEquals("A message", commit.getMessage().getFullMessage());

            assertEquals(script.getLastCommit().getAuthorIdent().getName(), commit.getAuthorAction().getIdentity().getName());
            assertEquals("John Doe", commit.getAuthorAction().getIdentity().getName());
            assertEquals(script.getLastCommit().getAuthorIdent().getEmailAddress(), commit.getAuthorAction().getIdentity().getEmail());
            assertEquals("jdoe@example.com", commit.getAuthorAction().getIdentity().getEmail());

            assertEquals(script.getLastCommit().getCommitterIdent().getName(), commit.getCommitAction().getIdentity().getName());
            assertEquals("Sean Moe", commit.getCommitAction().getIdentity().getName());
            assertEquals(script.getLastCommit().getCommitterIdent().getEmailAddress(), commit.getCommitAction().getIdentity().getEmail());
            assertEquals("smoe@example.com", commit.getCommitAction().getIdentity().getEmail());
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String, Identity, Identity) throws GitException with empty paths")
        @Test
        public void exceptionWithEmptyPathsOn4Params()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.commit(new ArrayList<String>(), "A message", new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String, Identity, Identity) throws NullPointerException with null paths")
        @Test
        public void exceptionWithNullPathsOn4Params()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(NullPointerException.class, () -> repository.commit(null, "A message", new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String, Identity, Identity) throws GitException with null message")
        @Test
        public void exceptionWithNullMessageOn4Params()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(List.<String>of("."), null, new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String, Identity, Identity)")
        @Test
        public void commit4Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            RevCommit prevLastCommit = script.getLastCommit();

            script.addRandomTextWorkbenchFiles(1);

            Commit commit = repository.commit(List.<String>of("."), "A message", new Identity("John Doe", "jdoe@example.com"), new Identity("Sean Moe", "smoe@example.com"));

            assertNotEquals(prevLastCommit.getId().getName(), script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), commit.getSHA());
            assertEquals(script.getLastCommit().getFullMessage(), commit.getMessage().getFullMessage());
            assertEquals("A message", commit.getMessage().getFullMessage());
            
            assertEquals(script.getLastCommit().getAuthorIdent().getName(), commit.getAuthorAction().getIdentity().getName());
            assertEquals("John Doe", commit.getAuthorAction().getIdentity().getName());
            assertEquals(script.getLastCommit().getAuthorIdent().getEmailAddress(), commit.getAuthorAction().getIdentity().getEmail());
            assertEquals("jdoe@example.com", commit.getAuthorAction().getIdentity().getEmail());

            assertEquals(script.getLastCommit().getCommitterIdent().getName(), commit.getCommitAction().getIdentity().getName());
            assertEquals("Sean Moe", commit.getCommitAction().getIdentity().getName());
            assertEquals(script.getLastCommit().getCommitterIdent().getEmailAddress(), commit.getCommitAction().getIdentity().getEmail());
            assertEquals("smoe@example.com", commit.getCommitAction().getIdentity().getEmail());
        }
    }

    @Nested
    @DisplayName("JGitRepository.push")
    class PushTests {
        @DisplayName("JGitRepository.push(String, String) with non required user and password credentials")
        @Test
        public void pushWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            String pushedRemote = repository.push(System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now the default remote 'origin' has the first commit
            assertEquals("origin", pushedRemote);
            assertDoesNotThrow(() -> remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNotEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNull(remote2script.getLastCommit());

            // now push (to the default 'origin') and see the changes reflected
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemote = repository.push(System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // changes are reflected to 'origin' only
            assertEquals("origin", pushedRemote);
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNull(remote2script.getLastCommit());
        }

        @DisplayName("JGitRepository.push(String, String) with non required SSH credentials")
        @Test
        public void pushWithNonRequiredSSHCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            String pushedRemote = repository.push(System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now the default remote 'origin' has the first commit
            assertEquals("origin", pushedRemote);
            assertDoesNotThrow(() -> remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNotEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNull(remote2script.getLastCommit());

            // now push (to the default 'origin') and see the changes reflected
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemote = repository.push(System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // changes are reflected to 'origin' only
            assertEquals("origin", pushedRemote);
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNull(remote2script.getLastCommit());
        }

        @DisplayName("JGitRepository.push(String, String) without required user and password credentials")
        @Test
        public void pushWithoutRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push(null, (String)null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String) without required SSH credentials")
        @Test
        public void pushWithoutRequiredSSHCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push(null, (byte[])null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String) with required user and password credentials")
        @Test
        public void pushWithRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String) with required SSH unprotected credentials")
        @Test
        public void pushWithRequiredSSHUnprotectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String) with required SSH protected credentials")
        @Test
        public void pushWithRequiredSSHProtectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserPrivateKeyWithPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String, String) with non required user and password credentials")
        @Test
        public void pushToRemoteWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            String pushedRemote = repository.push("custom", System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now the non-default remote 'custom' has the first commit
            assertEquals("custom", pushedRemote);
            assertNull(remote1script.getLastCommit());
            assertDoesNotThrow(() -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNull(remote1script.getLastCommit());
            assertNotEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());

            // now push and see the changes reflected
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemote = repository.push("custom", System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // changes are reflected to 'custom' only
            assertEquals("custom", pushedRemote);
            assertNull(remote1script.getLastCommit());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }

        @DisplayName("JGitRepository.push(String, String, String) with non required SSH credentials")
        @Test
        public void pushToRemoteWithNonRequiredSSHCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            String pushedRemote = repository.push("custom", System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now the non-default remote 'custom' has the first commit
            assertEquals("custom", pushedRemote);
            assertNull(remote1script.getLastCommit());
            assertDoesNotThrow(() -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNull(remote1script.getLastCommit());
            assertNotEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());

            // now push and see the changes reflected
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemote = repository.push("custom", System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // changes are reflected to 'custom' only
            assertEquals("custom", pushedRemote);
            assertNull(remote1script.getLastCommit());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }

        @DisplayName("JGitRepository.push(String, String, String) without required user and password credentials")
        @Test
        public void pushToRemoteWithoutRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push("origin", null, (String)null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String, String) without required SSH credentials")
        @Test
        public void pushToRemoteWithoutRequiredSSHCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push("origin", null, (byte[])null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String, String) with required user and password credentials")
        @Test
        public void pushToRemoteWithRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            repository.push("origin", System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String, String) with required SSH unprotected credentials")
        @Test
        public void pushToRemoteWithRequiredSSHUnprotectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            repository.push("origin", System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(String, String, String) with required SSH protected credentials")
        @Test
        public void pushToRemoteWithRequiredSSHProtectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserPrivateKeyWithPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            repository.push("origin", System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) with non required user and password credentials")
        @Test
        public void pushToRemotesWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            Set<String> pushedRemotes = repository.push(List.<String>of("origin", "custom"), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now the remotes have the first commit
            assertTrue(pushedRemotes.contains("origin"));
            assertTrue(pushedRemotes.contains("custom"));
            assertEquals(2, pushedRemotes.size());
            assertDoesNotThrow(() -> remote1script.getLastCommit());
            assertDoesNotThrow(() -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNotEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNotEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());

            // now push and see the changes reflected
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemotes = repository.push(List.<String>of("origin", "custom"), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // changes are reflected to both remotes
            assertTrue(pushedRemotes.contains("origin"));
            assertTrue(pushedRemotes.contains("custom"));
            assertEquals(2, pushedRemotes.size());
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) with non required SSH credentials")
        @Test
        public void pushToRemotesWithNonRequiredSSHCredentialsTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.BARE.realize(true);
            remote1script.getGitDirectory().deleteOnExit();
            Script remote2script = Scenario.BARE.realize(true);
            remote2script.getGitDirectory().deleteOnExit();
            script.addRemote(remote1script.getGitDirectory(), "origin"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories
            script.addRemote(remote2script.getGitDirectory(), "custom"); // use the GitDirectory as the WorkingDirectory is not available for bare repositories

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits
            assertNull(remote1script.getLastCommit());
            assertNull(remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            Set<String> pushedRemotes = repository.push(List.<String>of("origin", "custom"), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now the remotes have the first commit
            assertTrue(pushedRemotes.contains("origin"));
            assertTrue(pushedRemotes.contains("custom"));
            assertEquals(2, pushedRemotes.size());
            assertDoesNotThrow(() -> remote1script.getLastCommit());
            assertDoesNotThrow(() -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNotEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertNotEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());

            // now push and see the changes reflected
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            pushedRemotes = repository.push(List.<String>of("origin", "custom"), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // changes are reflected to both remotes
            assertTrue(pushedRemotes.contains("origin"));
            assertTrue(pushedRemotes.contains("custom"));
            assertEquals(2, pushedRemotes.size());
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) without required user and password credentials")
        @Test
        public void pushToRemotesWithoutRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push(List.<String>of("origin"), null, (String)null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) without required SSH credentials")
        @Test
        public void pushToRemotesWithoutRequiredSSHCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            assertThrows(GitException.class, () -> repository.push(List.<String>of("origin"), null, (byte[])null));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) with required user and password credentials")
        @Test
        public void pushToRemotesWithRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(List.<String>of("origin"), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken"));

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) with required SSH unprotected credentials")
        @Test
        public void pushToRemotesWithRequiredSSHUnprotectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(List.<String>of("origin"), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null);

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("JGitRepository.push(Collection<String>, String, String) with required SSH protected credentials")
        @Test
        public void pushToRemotesWithRequiredSSHProtectedCredentialsTest()
            throws Exception {
            String randomID = RandomUtil.randomAlphabeticString(5);
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            GitHub gitHub = GitHub.instance(Map.<String,String>of(GitHub.AUTHENTICATION_TOKEN_OPTION_NAME, System.getProperty("gitHubTestUserToken")));
            // create a brand new test repository for this purpose
            GitHubRepository gitHubRepository = gitHub.createGitRepository(randomID, "Test repository "+randomID, true, true);

            // if we read too quickly we often get a 404 from the server so let's wait a short while
            Thread.sleep(4000);

            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            Scenario.ONE_BRANCH_SHORT.apply(directory);

            Repository repository = JGitRepository.open(directory);
            
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            repository.push(List.<String>of("origin"), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }
    }

    @Nested
    @DisplayName("JGitRepository.tag")
    class TagTests {
        @DisplayName("JGitRepository.tag(String, String)")
        @Test
        public void tagTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // make sure an exception is thrown when the tag name is null
            assertThrows(GitException.class, () -> repository.tag(null));
            
            assertEquals(0, script.getTags().size());

            Tag lTag = repository.tag("ltag");
            assertEquals(repository.getLatestCommit(), lTag.getTarget());
            assertEquals("ltag", lTag.getName());
            assertEquals(1, script.getTags().size());
            assertTrue(script.getTags().containsKey("ltag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("ltag"));
            assertEquals(script.getCommitByTag("ltag"), repository.getLatestCommit());

            // make sure an exception is thrown when the tag name is duplicated
            assertThrows(GitException.class, () -> repository.tag("ltag"));
        }

        @DisplayName("JGitRepository.tag(String, String)")
        @Test
        public void tagWithMessageTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // make sure an exception is thrown when the tag name is null
            assertThrows(GitException.class, () -> repository.tag(null, null));
            assertThrows(GitException.class, () -> repository.tag(null, "The tag message"));
            
            assertEquals(0, script.getTags().size());

            Tag lTag = repository.tag("ltag", null);
            assertEquals(repository.getLatestCommit(), lTag.getTarget());
            assertEquals("ltag", lTag.getName());
            assertEquals(1, script.getTags().size());
            assertTrue(script.getTags().containsKey("ltag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("ltag"));
            assertEquals(script.getCommitByTag("ltag"), repository.getLatestCommit());

            Tag aTag = repository.tag("atag", "The tag message");
            assertEquals(repository.getLatestCommit(), aTag.getTarget());
            assertEquals("atag", aTag.getName());
            assertEquals(2, script.getTags().size());
            assertTrue(script.getTags().containsKey("atag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("atag"));
            assertEquals(script.getCommitByTag("atag"), repository.getLatestCommit());

            // make sure an exception is thrown when the tag name is duplicated
            assertThrows(GitException.class, () -> repository.tag("atag", null));
            assertThrows(GitException.class, () -> repository.tag("atag", "The tag message"));
        }

        @DisplayName("JGitRepository.tag(String, String, Identity)")
        @Test
        public void tagWithMessageAndIdentityTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // make sure an exception is thrown when the tag name is null
            assertThrows(GitException.class, () -> repository.tag(null, null, null));
            assertThrows(GitException.class, () -> repository.tag(null, "The tag message", null));
            
            assertEquals(0, script.getTags().size());

            Tag lTag = repository.tag("ltag", null, new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), lTag.getTarget());
            assertEquals("ltag", lTag.getName());
            assertEquals(1, script.getTags().size());
            assertTrue(script.getTags().containsKey("ltag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("ltag"));
            assertEquals(script.getCommitByTag("ltag"), repository.getLatestCommit());

            Tag aTag = repository.tag("atag", "The tag message", new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), aTag.getTarget());
            assertEquals("atag", aTag.getName());
            assertEquals(2, script.getTags().size());
            assertTrue(script.getTags().containsKey("atag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("atag"));
            assertEquals(script.getCommitByTag("atag"), repository.getLatestCommit());

            // make sure an exception is thrown when the tag name is duplicated
            assertThrows(GitException.class, () -> repository.tag("ltag", null, null));
            assertThrows(GitException.class, () -> repository.tag("atag", "The tag message", null));
        }

        @DisplayName("JGitRepository.tag(String, String, String, Identity)")
        @Test
        public void tagCommitWithMessageAndIdentityTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // make sure an exception is thrown when the tag name is null
            assertThrows(GitException.class, () -> repository.tag(repository.getLatestCommit(), null, null, null));
            assertThrows(GitException.class, () -> repository.tag(repository.getLatestCommit(), null, "The tag message", null));
            
            assertEquals(0, script.getTags().size());

            Tag lTag = repository.tag(repository.getLatestCommit(), "ltag", null, new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), lTag.getTarget());
            assertEquals("ltag", lTag.getName());
            assertEquals(1, script.getTags().size());
            assertTrue(script.getTags().containsKey("ltag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("ltag"));
            assertEquals(script.getCommitByTag("ltag"), repository.getLatestCommit());

            lTag = repository.tag(null, "ltag2", null, new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), lTag.getTarget());
            assertEquals("ltag2", lTag.getName());
            assertEquals(2, script.getTags().size());
            assertTrue(script.getTags().containsKey("ltag2"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("ltag2"));
            assertEquals(script.getCommitByTag("ltag2"), repository.getLatestCommit());

            Tag aTag = repository.tag(repository.getLatestCommit(), "atag", "The tag message", new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), aTag.getTarget());
            assertEquals("atag", aTag.getName());
            assertEquals(3, script.getTags().size());
            assertTrue(script.getTags().containsKey("atag"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("atag"));
            assertEquals(script.getCommitByTag("atag"), repository.getLatestCommit());

            aTag = repository.tag(null, "atag2", "The tag message", new Identity("John Doe", "jdoe@example.com"));
            assertEquals(repository.getLatestCommit(), aTag.getTarget());
            assertEquals("atag2", aTag.getName());
            assertEquals(4, script.getTags().size());
            assertTrue(script.getTags().containsKey("atag2"));
            assertEquals(repository.getLatestCommit(), script.getTags().get("atag2"));
            assertEquals(script.getCommitByTag("atag2"), repository.getLatestCommit());

            // make sure an exception is thrown when the tag name is duplicated
            assertThrows(GitException.class, () -> repository.tag(repository.getLatestCommit(), "ltag", null, null));
            assertThrows(GitException.class, () -> repository.tag(repository.getLatestCommit(), "atag", "The tag message", null));
        }
    }

    @Nested
    @DisplayName("JGitRepository.getCurrentBranch")
    class GetCurrentBranchTests {
        @DisplayName("JGitRepository.getCurrentBranch()")
        @Test
        public void getCurrentBranchTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            assertEquals("master", repository.getCurrentBranch());
            
            // add and stage some files
            script.andAddFiles().andStage().commit("A commit");

            assertEquals("master", repository.getCurrentBranch());

            script.inBranch("testbranch");
            assertEquals("testbranch", repository.getCurrentBranch());
        }
    }

    @Nested
    @DisplayName("JGitRepository.getLatestCommit")
    class GetLatestCommitTests {
        @DisplayName("JGitRepository.getLatestCommit() throws GitException without commits")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            assertThrows(GitException.class, () -> repository.getLatestCommit());
            
            // add some new files and test
            script.andAddFiles();
            assertThrows(GitException.class, () -> repository.getLatestCommit());

            // stage the files without committing
            script.andStage();
            assertThrows(GitException.class, () -> repository.getLatestCommit());
        }

        @DisplayName("JGitRepository.getLatestCommit()")
        @Test
        public void getLatestCommitTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            // add and stage some files
            script.andAddFiles().andStage();

            // commit the files and get the commit SHA
            String commitSHA1 = script.commit("Test commit").getId().getName();
            assumeFalse(Objects.isNull(commitSHA1));
            assertNotNull(repository.getLatestCommit());
            assertEquals(commitSHA1, repository.getLatestCommit());

            // make sure the root and latest commits are the same by now
            assertEquals(repository.getRootCommit(), repository.getLatestCommit());

            // repeat the above with new changes
            script.andAddFiles(2);
            String commitSHA2 = script.commit("Test another commit").getId().getName();
            assertNotEquals(commitSHA1, commitSHA2);
            assumeFalse(Objects.isNull(commitSHA2));
            assertNotNull(repository.getLatestCommit());
            assertEquals(commitSHA2, repository.getLatestCommit());

            // make sure the root and latest commits are not the same anymore
            assertNotEquals(repository.getRootCommit(), repository.getLatestCommit());
        }
    }

    @Nested
    @DisplayName("JGitRepository.getRootCommit")
    class GetRootCommitTests {
        @DisplayName("JGitRepository.getRootCommit() throws GitException without commits")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            assertThrows(GitException.class, () -> repository.getRootCommit());
            
            // add some new files and test
            script.andAddFiles();
            assertThrows(GitException.class, () -> repository.getRootCommit());

            // stage the files without committing
            script.andStage();
            assertThrows(GitException.class, () -> repository.getRootCommit());
        }

        @DisplayName("JGitRepository.getRootCommit()")
        @Test
        public void getRootCommitTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            // add and stage some files
            script.andAddFiles().andStage();

            // commit the files and get the commit SHA
            String commitSHA1 = script.commit("Test commit").getId().getName();
            assumeFalse(Objects.isNull(commitSHA1));
            assertNotNull(repository.getRootCommit());
            assertEquals(commitSHA1, repository.getRootCommit());

            // make sure the root and latest commits are the same by now
            assertEquals(repository.getRootCommit(), repository.getLatestCommit());

            // repeat the above with new changes
            script.andAddFiles(2);
            String commitSHA2 = script.commit("Test another commit").getId().getName();
            assertNotEquals(commitSHA1, commitSHA2);
            assumeFalse(Objects.isNull(commitSHA2));
            assertNotNull(repository.getRootCommit());
            assertNotEquals(commitSHA2, repository.getRootCommit());

            // make sure the root and latest commits are not the same anymore
            assertNotEquals(repository.getRootCommit(), repository.getLatestCommit());
        }
    }

    @Nested
    @DisplayName("JGitRepository.isClean")
    class IsCleanTests {
        @DisplayName("JGitRepository.isClean()")
        @Test
        public void isCleanTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            assertTrue(repository.isClean());
            
            // add some new files and test
            script.andAddFiles();
            assertFalse(repository.isClean());

            // stage the files without committing
            script.andStage();
            assertFalse(repository.isClean());

            // commit the files, now we're clean again
            script.andCommit();
            assertTrue(repository.isClean());
        }

        @DisplayName("JGitRepository.isClean() with a test file containing linefeeds using the embedded library")
        @Test
        public void isCleanWithTextFileContainingLineFeedsUsingEmbeddedLibraryTest()
            throws Exception {
            // This test reproduces bugs:
            // - https://github.com/mooltiverse/nyx/issues/130
            // - https://github.com/mooltiverse/nyx/issues/129
            // The use case for the bug is when a commit with a simple change that adds a LF or CRLF in some text file
            // makes isClean() return false (even after the change has been commited) when it's supposed to return true.
            // The bug actually does not affect the Java version, but is here to match with the tests for the Go version
            // The difference between this test and the below test is that here we use the internal library both to check the
            // repository status and also to execute the Git commands to build the test repository.
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            assertTrue(JGitRepository.open(script.getWorkingDirectory()).isClean());
            
            // add one file with a LF and a CRLF and test
            File f = new File(script.getWorkingDirectory(), "README.txt");
            FileWriter fw = new FileWriter(f);
            fw.write("one");
            fw.write("\n");
            fw.write("two");
            fw.write("\r\n");
            fw.flush();
            fw.close();
            assertFalse(JGitRepository.open(script.getWorkingDirectory()).isClean());

            // stage the files without committing
            script.andStage();
            assertFalse(JGitRepository.open(script.getWorkingDirectory()).isClean());

            // commit the files, now we're supposed to be clean again but when the bug is present we're not
            script.andCommit();
            // when the bug is present, this call to IsClean() returns false even if it's supposed to return true
            assertTrue(JGitRepository.open(script.getWorkingDirectory()).isClean());
        }

        @DisplayName("JGitRepository.isClean() with a test file containing linefeeds using the git command")
        @Test
        public void isCleanWithTextFileContainingLineFeedsUsingGitCommandTest()
            throws Exception {
            // This test reproduces bugs:
            // - https://github.com/mooltiverse/nyx/issues/130
            // - https://github.com/mooltiverse/nyx/issues/129
            // The use case for the bug is when a commit with a simple change that adds a LF or CRLF in some text file
            // makes isClean() return false (even after the change has been commited) when it's supposed to return true.
            // The bug actually does not affect the Java version, but is here to match with the tests for the Go version
            // The difference between this test and the above test is that here we use the internal library just to check the
            // repository status, while Git commands to build the test repository are executed using the external executable Git command.
            File testDirectory = FileSystemUtil.newTempDirectory(null, "nyx-test-script-");
            testDirectory.deleteOnExit();
            File repoDirectory = new File(testDirectory, "testrepo");
            Process p = new ProcessBuilder(new String[]{"git", "init", "testrepo"}).directory(testDirectory).redirectErrorStream(true).start();
            p.waitFor();
            System.out.println("Output from 'git init testrepo' is:");
            System.out.write(p.getInputStream().readAllBytes());
            System.out.println();
            System.out.flush();
            p.destroy();
            assertTrue(JGitRepository.open(repoDirectory).isClean());

            // Give the local repository an identity or some further steps may fail
            p = new ProcessBuilder(new String[]{"git", "config", "user.email", "\"jdoe@example.com\""}).directory(repoDirectory).redirectErrorStream(true).start();
            p.waitFor();
            System.out.println("Output from 'git config user.email \"jdoe@example.com\"' is:");
            System.out.write(p.getInputStream().readAllBytes());
            System.out.println();
            System.out.flush();
            p.destroy();
            p = new ProcessBuilder(new String[]{"git", "config", "user.name", "\"John Doe\""}).directory(repoDirectory).redirectErrorStream(true).start();
            p.waitFor();
            System.out.println("Output from 'git config user.name \"John Doe\"' is:");
            System.out.write(p.getInputStream().readAllBytes());
            System.out.println();
            System.out.flush();
            p.destroy();
            
            // add one file with a LF and a CRLF and test
            File f = new File(repoDirectory, "README.txt");
            FileWriter fw = new FileWriter(f);
            fw.write("one");
            fw.write("\n");
            fw.write("two");
            fw.write("\r\n");
            fw.flush();
            fw.close();
            assertFalse(JGitRepository.open(repoDirectory).isClean());

            // stage the files without committing
            p = new ProcessBuilder(new String[]{"git", "add", "."}).directory(repoDirectory).redirectErrorStream(true).start();
            p.waitFor();
            System.out.println("Output from 'git add .' is:");
            System.out.write(p.getInputStream().readAllBytes());
            System.out.println();
            System.out.flush();
            p.destroy();
            assertFalse(JGitRepository.open(repoDirectory).isClean());

            // commit the files, now we're supposed to be clean again but when the bug is present we're not
            p = new ProcessBuilder(new String[]{"git", "commit", "-m", "\"commit\""}).directory(repoDirectory).redirectErrorStream(true).start();
            p.waitFor();
            System.out.println("Output from 'git commit -m \"commit\"' is:");
            System.out.write(p.getInputStream().readAllBytes());
            System.out.println();
            System.out.flush();
            p.destroy();
            // when the bug is present, this call to IsClean() returns false even if it's supposed to return true
            assertTrue(JGitRepository.open(repoDirectory).isClean());
        }
    }

    @Nested
    @DisplayName("JGitRepository.getCommitTags")
    class GetCommitTagsTests {
        @DisplayName("JGitRepository.getCommitTags(null) returns empty without commits")
        @Test
        public void getCommitTagsReturnsEmptyResultWithNullTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertEquals(0, repository.getCommitTags(null).size());
        }

        @DisplayName("JGitRepository.getCommitTags() returns empty without commits")
        @Test
        public void getCommitTagsReturnsEmptyResultWithRepositoryWithNoCommitsTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertEquals(0, repository.getCommitTags("").size());
        }

        @DisplayName("JGitRepository.getCommitTags()")
        @Test
        public void getCommitTagsTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // add a commit
            script.andAddFiles().andStage();
            RevCommit commit = script.commit("A message");
            
            // test with no tags
            assertEquals(0, repository.getCommitTags(commit.getId().getName()).size());

            // test with one lightweight tag
            script.tag("l1", null);
            assertEquals(1, repository.getCommitTags(commit.getId().getName()).size());
            assertEquals("l1", repository.getCommitTags(commit.getId().getName()).iterator().next().getName());
            assertFalse(repository.getCommitTags(commit.getId().getName()).iterator().next().isAnnotated());

            // test with one more tag
            script.tag("a1", "Tag message");
            assertEquals(2, repository.getCommitTags(commit.getId().getName()).size());
        }
    }

    @Nested
    @DisplayName("JGitRepository.getRemoteNames")
    class GetRemotesTests {
        @DisplayName("JGitRepository.getRemoteNames() with no remotes")
        @Test
        public void getRemoteNamesWithNoRemotesTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            assertTrue(repository.getRemoteNames().isEmpty());
        }

        @DisplayName("JGitRepository.getRemoteNames() after clone")
        @Test
        public void getRemoteNamesAfterCloneTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-remote-names-test-").toFile();
            directory.deleteOnExit();

            Repository repository = Git.instance().clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL/*, System.getProperty("gitHubTestUserToken"), ""*/);

            assertFalse(repository.getRemoteNames().isEmpty());
            assertEquals(1, repository.getRemoteNames().size());
            assertTrue(repository.getRemoteNames().contains("origin"));
        }

        @DisplayName("JGitRepository.getRemoteNames() after adding a local repository")
        @Test
        public void getRemoteNamesAfterAddingLocalRepositoryTest()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
            script.getWorkingDirectory().deleteOnExit();
            Script localRepositoryScript = Scenario.FROM_SCRATCH.realize();
            localRepositoryScript.getWorkingDirectory().deleteOnExit();
            script.addRemote(localRepositoryScript.getGitDirectory(), "local");
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            assertFalse(repository.getRemoteNames().isEmpty());
            assertEquals(1, repository.getRemoteNames().size());
            assertTrue(repository.getRemoteNames().contains("local"));
        }

        @DisplayName("JGitRepository.getRemoteNames() after clone and adding a local repository")
        @Test
        public void getRemoteNamesAfterCloneAndAddingLocalRepositoryTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-remote-names-test-").toFile();
            directory.deleteOnExit();

            Repository repository = Git.instance().clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL/*, System.getProperty("gitHubTestUserToken"), ""*/);
            Script script = new Script(directory);
            script.getWorkingDirectory().deleteOnExit();

            Script localRepositoryScript = Scenario.FROM_SCRATCH.realize();
            localRepositoryScript.getWorkingDirectory().deleteOnExit();
            script.addRemote(localRepositoryScript.getGitDirectory(), "local");

            assertFalse(repository.getRemoteNames().isEmpty());
            assertEquals(2, repository.getRemoteNames().size());
            assertTrue(repository.getRemoteNames().contains("origin"));
            assertTrue(repository.getRemoteNames().contains("local"));
        }
    }

    @Nested
    @DisplayName("JGitRepository.walkHistory")
    class WalkHistoryTests {
        @DisplayName("JGitRepository.walkHistory(null, null, CommitVisitor)")
        @Test
        public void walkHistoryWithNoBoundariesTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return true;
            });

            assertEquals(10, visitedCommits.size());
            // make sure the last one is the root commit
            assertTrue(visitedCommits.get(visitedCommits.size()-1).getParents().isEmpty());
            assertEquals(repository.getRootCommit(), visitedCommits.get(visitedCommits.size()-1).getSHA());
        }

        @DisplayName("JGitRepository.walkHistory(null, null, CommitVisitor) throws GitException without commits")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.walkHistory(null, null, c -> {
                return true;
            }));
        }

        @DisplayName("JGitRepository.walkHistory(null, null, CommitVisitor) stopped by CommitVisitor")
        @Test
        public void walkHistoryWithVisitorStoppingBrowsingTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            // make the visitor stop after 2 commits
            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return visitedCommits.size() < 2;
            });

            assertEquals(2, visitedCommits.size());
        }

        @DisplayName("JGitRepository.walkHistory(start, null, CommitVisitor)")
        @Test
        public void walkHistoryWithStartBoundaryTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return true;
            });

            assertEquals(10, visitedCommits.size());
            
            // now browse again with a start boundary (starting at the 3rd commit)
            List<Commit> boundaryVisitedCommits = new ArrayList<Commit>();
            repository.walkHistory(visitedCommits.get(2).getSHA(), null, c -> {
                boundaryVisitedCommits.add(c);
                return true;
            });

            assertEquals(visitedCommits.size()-2, boundaryVisitedCommits.size());
            assertEquals(visitedCommits.get(2).getSHA(), boundaryVisitedCommits.get(0).getSHA()); // test the first visited commit
            assertEquals(visitedCommits.get(visitedCommits.size()-1).getSHA(), boundaryVisitedCommits.get(boundaryVisitedCommits.size()-1).getSHA()); // test the last visited commit
        }

        @DisplayName("JGitRepository.walkHistory(null, end, CommitVisitor)")
        @Test
        public void walkHistoryWithEndBoundaryTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return true;
            });

            assertEquals(10, visitedCommits.size());
            
            // now browse again with an end boundary (ending at the Nth-2 commit)
            List<Commit> boundaryVisitedCommits = new ArrayList<Commit>();
            repository.walkHistory(null, visitedCommits.get(visitedCommits.size()-3).getSHA(), c -> {
                boundaryVisitedCommits.add(c);
                return true;
            });

            assertEquals(visitedCommits.size()-2, boundaryVisitedCommits.size());
            assertEquals(visitedCommits.get(0).getSHA(), boundaryVisitedCommits.get(0).getSHA()); // test the first visited commit
            assertEquals(visitedCommits.get(visitedCommits.size()-3).getSHA(), boundaryVisitedCommits.get(boundaryVisitedCommits.size()-1).getSHA()); // test the last visited commit
        }

        @DisplayName("JGitRepository.walkHistory(start, end, CommitVisitor)")
        @Test
        public void walkHistoryWithBothBoundariesTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return true;
            });

            assertEquals(10, visitedCommits.size());
            
            // now browse again with an end boundary (starting at the 3rd commit and ending at the Nth-2 commit)
            List<Commit> boundaryVisitedCommits = new ArrayList<Commit>();
            repository.walkHistory(visitedCommits.get(2).getSHA(), visitedCommits.get(visitedCommits.size()-3).getSHA(), c -> {
                boundaryVisitedCommits.add(c);
                return true;
            });

            assertEquals(visitedCommits.size()-4, boundaryVisitedCommits.size());
            assertEquals(visitedCommits.get(2).getSHA(), boundaryVisitedCommits.get(0).getSHA()); // test the first visited commit
            assertEquals(visitedCommits.get(visitedCommits.size()-3).getSHA(), boundaryVisitedCommits.get(boundaryVisitedCommits.size()-1).getSHA()); // test the last visited commit
        }

        @DisplayName("JGitRepository.walkHistory(start, null, CommitVisitor) throws GitException with unresolved boundary")
        @Test
        public void walkHistoryWithStartBoundaryUnresolvedTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            // this SHA is unknown to the repository, so it should throw an exception
            assertThrows(GitException.class, () -> repository.walkHistory("d0a19fc5776dc0c0b1a8d869c1117dac71065870", null, c -> {
                visitedCommits.add(c);
                return true;
            }));

            assertEquals(0, visitedCommits.size());
        }

        @DisplayName("JGitRepository.walkHistory(null, end, CommitVisitor) throws GitException with unresolved boundary")
        @Test
        public void walkHistoryWithEndBoundaryUnresolvedTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            // this SHA is unknown to the repository, so it should throw an exception
            assertThrows(GitException.class, () -> repository.walkHistory(null, "31cab6562ed66dfc71a4fcf65292a97fb81e0e75", c -> {
                visitedCommits.add(c);
                return true;
            }));

            assertEquals(0, visitedCommits.size());
        }

        @DisplayName("JGitRepository.walkHistory(start, end, CommitVisitor) throws GitException with unresolved boundaries")
        @Test
        public void walkHistoryWithBothBoundariesUnresolvedTest()
            throws Exception {
            Repository repository = JGitRepository.open(Scenario.TWO_BRANCH_SHORT_MERGED.realize().getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            // these two SHAs are unknown to the repository, so they should throw an exception
            assertThrows(GitException.class, () -> repository.walkHistory("d0a19fc5776dc0c0b1a8d869c1117dac71065870", "31cab6562ed66dfc71a4fcf65292a97fb81e0e75", c -> {
                visitedCommits.add(c);
                return true;
            }));

            assertEquals(0, visitedCommits.size());
        }

        @DisplayName("JGitRepository.walkHistory(start, end, CommitVisitor) throws GitException with out-of-scope end boundary")
        @Test
        public void walkHistoryWithEndBoundaryOutOfScopeTest()
            throws Exception {
            Script script = Scenario.TWO_BRANCH_SHORT_UNMERGED.realize();
            script.getWorkingDirectory().deleteOnExit();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            // Keep track of the visited commits
            List<Commit> visitedCommitsWithoutBoundaries = new ArrayList<Commit>();
            List<Commit> visitedCommitsWithBoundaries = new ArrayList<Commit>();

            // find out the SHA-1 of the alpha branch HEAD, so it's out of scope
            script.checkout("alpha");
            String alphaHead = script.getLastCommit().getId().getName();

            // and switch back to the master branch
            script.checkout("master");

            // do a first walk with no boundaries
            repository.walkHistory(null, null, c -> {
                visitedCommitsWithoutBoundaries.add(c);
                return true;
            });

            // now do the same walk with boundaries
            // this boundary is out of the branch we're working in to the repository, so it should not affect the outcome
            repository.walkHistory(null, alphaHead, c -> {
                visitedCommitsWithBoundaries.add(c);
                return true;
            });

            assertEquals(visitedCommitsWithoutBoundaries.size(), visitedCommitsWithBoundaries.size());
        }
    }
}