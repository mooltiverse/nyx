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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.util.RandomUtil;
import com.mooltiverse.oss.nyx.services.github.GitHub;
import com.mooltiverse.oss.nyx.services.github.GitHubRepository;

@DisplayName("Git")
public class GitTests {
    /**
     * Use this own project repo as the repository to clone for tests
     */
    public static final String REMOTE_TEST_REPOSITORY_HTTP_URL = "https://github.com/mooltiverse/nyx.git";

    /**
     * Use this own project repo as the repository to clone for tests
     */
    public static final String REMOTE_TEST_REPOSITORY_SSH_URL = "git@github.com:mooltiverse/nyx.git";

    @Nested
    @DisplayName("Instance")
    class InstanceTest {
        @Test
        public void instanceWithEmptyOptions()
            throws Exception {
            Git service = Git.instance();
            assertNotNull(service);
        }
    }

    @Nested
    @DisplayName("Git.clone")
    class CloneTests {
        @DisplayName("Git.instance().clone(null, String) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance().clone((File)null, REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("Git.instance().clone('', String) throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> Git.instance().clone("", REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertThrows(IllegalArgumentException.class, () -> Git.instance().clone("  ", REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("Git.instance().clone(String, String) throws GitException with non empty directory")
        @Test
        public void exceptionWithNonEmptyDirectoryAsString()
            throws Exception {
            assertThrows(GitException.class, () -> Git.instance().clone(Scenario.FROM_SCRATCH.realize().getWorkingDirectory().getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("Git.instance().clone(File, String) throws GitException with non empty directory")
        @Test
        public void exceptionWithNonEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(GitException.class, () -> Git.instance().clone(Scenario.FROM_SCRATCH.realize().getWorkingDirectory(), REMOTE_TEST_REPOSITORY_HTTP_URL));
        }

        @DisplayName("Git.instance().clone(File, null) throws NullPointerException")
        @Test
        public void exceptionWithNullURIAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile(), null));
        }

        @DisplayName("Git.instance().clone(String, null) throws NullPointerException")
        @Test
        public void exceptionWithNullURIAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), null));
        }

        @DisplayName("Git.instance().clone(String, '') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyURIAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), ""));
            assertThrows(IllegalArgumentException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), " "));
        }

        @DisplayName("Git.instance().clone(String, String) throws GitException with non existent URI")
        @Test
        public void exceptionWithNonExistingURIAsString()
            throws Exception {
            assertThrows(GitException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile().getAbsolutePath(), "https://adomainwiththisnamesuredoesnotexists.com/"));
        }

        @DisplayName("Git.instance().clone(File, String) throws GitException with non existent URI")
        @Test
        public void exceptionWithNonExistingURI()
            throws Exception {
            assertThrows(GitException.class, () -> Git.instance().clone(Files.createTempDirectory("nyx-test-git-clone-test-").toFile(), "https://adomainwiththisnamesuredoesnotexists.com/"));
        }

        @DisplayName("Git.instance().clone(File, String)")
        @Test
        public void cloneFileTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory, REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(String, String)")
        @Test
        public void cloneStringTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(File, String, String, String) with non required user and password credentials")
        @Test
        public void cloneFileWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(Git.instance().clone(directory, REMOTE_TEST_REPOSITORY_HTTP_URL, System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(File, String, String, byte[]) with non required SSH credentials")
        @Test
        public void cloneFileWithNonRequiredSSHCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(Git.instance().clone(directory, REMOTE_TEST_REPOSITORY_SSH_URL, System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(String, String, String, String) with non required user and password credentials")
        @Test
        public void cloneStringWithNonRequiredUserAndPasswordCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserToken' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_HTTP_URL, System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(String, String, String, byte[]) with non required SSH credentials")
        @Test
        public void cloneStringWithNonRequiredSSHCredentialsTest()
            throws Exception {
            File directory = Files.createTempDirectory("nyx-test-git-clone-test-").toFile();
            directory.deleteOnExit();
            assertFalse(new File(directory, "README.md").exists());
            // the 'gitHubTestUserPrivateKeyWithoutPassphrase' system property is set by the build script, which in turn reads it from an environment variable
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), REMOTE_TEST_REPOSITORY_SSH_URL, System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());
        }

        @DisplayName("Git.instance().clone(File, String, null, null) with required user and password credentials")
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
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("Git.instance().clone(File, String, null, byte[]) with required SSH unprotected credentials")
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
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("Git.instance().clone(File, String, null, byte[]) with required SSH protected credentials")
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
            assertNotNull(Git.instance().clone(directory, gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("Git.instance().clone(String, String, null, null) with required user and password credentials")
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
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), gitHubRepository.getHTTPURL(), System.getProperty("gitHubTestUserToken"), System.getProperty("gitHubTestUserToken")));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("Git.instance().clone(String, String, null, byte[]) with required SSH unprotected credentials")
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
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithoutPassphrase"), (byte[])null));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }

        @DisplayName("Git.instance().clone(String, String, null, byte[]) with required SSH protected credentials")
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
            assertNotNull(Git.instance().clone(directory.getAbsolutePath(), gitHubRepository.getSSHURL(), System.getProperty("gitHubTestUserPrivateKeyWithPassphrase"), System.getProperty("gitHubTestUserPrivateKeyPassphrase").getBytes()));
            assertTrue(new File(directory, "README.md").exists());

            // now delete it
            gitHub.deleteGitRepository(randomID);
        }
    }

    @Nested
    @DisplayName("Git.open")
    class OpenTests {
        @DisplayName("Git.instance().open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance().open((File)null));
        }

        @DisplayName("Git.instance().open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.instance().open((String)null));
        }

        @DisplayName("Git.instance().open('') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> Git.instance().open(""));
            assertThrows(IllegalArgumentException.class, () -> Git.instance().open("  "));
        }

        @DisplayName("Git.instance().open(String) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> Git.instance().open("adirectorywiththisnamesuredoesnotexists"));
        }

        @DisplayName("Git.instance().open(File) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> Git.instance().open(new File("adirectorywiththisnamesuredoesnotexists")));
        }

        @DisplayName("Git.instance().open(String) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> Git.instance().open(Files.createTempDirectory(null).toAbsolutePath().toString()));
        }

        @DisplayName("Git.instance().open(File) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> Git.instance().open(Files.createTempDirectory(null).toFile()));
        }

        @DisplayName("Git.instance().open(File)")
        @Test
        public void openFileTest()
            throws Exception {
            assertNotNull(Git.instance().open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory()));
        }

        @DisplayName("Git.instance().open(String)")
        @Test
        public void openStringTest()
            throws Exception {
            assertNotNull(Git.instance().open(Scenario.FROM_SCRATCH.realize().getWorkingDirectory().getAbsolutePath()));
        }
    }
}