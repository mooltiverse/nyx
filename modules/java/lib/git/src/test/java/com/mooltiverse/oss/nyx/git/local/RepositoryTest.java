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
package com.mooltiverse.oss.nyx.git.local;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.io.File;
import java.io.IOException;

import java.nio.file.Files;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.git.script.JGitScript;

@DisplayName("Repository")
public class RepositoryTest {
    @Nested
    @DisplayName("Repository.open")
    class OpenTest {
        @DisplayName("Repository.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Repository.open((File)null));
        }

        @DisplayName("Repository.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Repository.open((String)null));
        }

        @DisplayName("Repository.open('') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> Repository.open(""));
            assertThrows(IllegalArgumentException.class, () -> Repository.open("  "));
        }

        @DisplayName("Repository.open(<RUBBISHDIRECTORYNAME>) throws IOException")
        @Test
        public void exceptionWithNonExistingDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> Repository.open("adirectorywiththisnamesuredoesnotexists"));
        }

        @DisplayName("Repository.open(<RUBBISHDIRECTORYNAME>) throws IOException")
        @Test
        public void exceptionWithNonExistingDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> Repository.open(new File("adirectorywiththisnamesuredoesnotexists")));
        }

        @DisplayName("Repository.open(<EMPTYDIRECTORY>) throws IOException")
        @Test
        public void exceptionWithNewEmptyDirectoryAsString(/*@TempDir File tempEmptyDir*/)
            throws Exception {

            // JUnit returns the same TempDir for multiple tests, even from the inherited class, so this makes the test fail.
            // As a workaround we just create a new temp dir the old way.
            // See: https://github.com/junit-team/junit5/issues/1967
            
            // the directory exists but is empty
            //assumeTrue(tempEmptyDir.exists());
            //assumeTrue(tempEmptyDir.isDirectory());
            //assertThrows(IOException.class, () -> Repository.open(tempEmptyDir.getAbsolutePath()));
            assertThrows(IOException.class, () -> Repository.open(Files.createTempDirectory(null).toAbsolutePath().toString()));
        }

        @DisplayName("Repository.open(<EMPTYDIRECTORY>) throws IOException")
        @Test
        public void exceptionWithNewEmptyDirectoryAsFile(/*@TempDir File tempEmptyDir*/)
            throws Exception {

            // JUnit returns the same TempDir for multiple tests, even from the inherited class, so this makes the test fail.
            // As a workaround we just create a new temp dir the old way.
            // See: https://github.com/junit-team/junit5/issues/1967

            // the directory exists but is empty
            //assumeTrue(tempEmptyDir.exists());
            //assumeTrue(tempEmptyDir.isDirectory());
            //assertThrows(IOException.class, () -> Repository.open(tempEmptyDir));
            assertThrows(IOException.class, () -> Repository.open(Files.createTempDirectory(null).toFile()));
        }
    }

    @Nested
    @DisplayName("Repository.getLatestCommit")
    class GetLatestCommitTest {
        @DisplayName("Repository.getLatestCommit()")
        @Test
        public void getLatestCommitTest()
            throws Exception {
            // start with a new repository, just initialized
            JGitScript script = JGitScript.fromScratch(true);
            Repository repository = Repository.open(script.getWorkingDirectory());
            assertNull(repository.getLatestCommit());
            
            // add some new files and test
            script.withFiles();
            assertNull(repository.getLatestCommit());

            // stage the files without committing
            script.stage();
            assertNull(repository.getLatestCommit());

            // commit the files and get the commit SHA
            String commitSHA1 = script.commitFiles("Test commit").getId().getName();
            assumeFalse(Objects.isNull(commitSHA1));
            assertNotNull(repository.getLatestCommit());
            assertEquals(commitSHA1, repository.getLatestCommit());

            // repeat the above with new changes
            script.withFiles(2);
            String commitSHA2 = script.commitFiles("Test another commit").getId().getName();
            assertNotEquals(commitSHA1, commitSHA2);
            assumeFalse(Objects.isNull(commitSHA2));
            assertNotNull(repository.getLatestCommit());
            assertEquals(commitSHA2, repository.getLatestCommit());
        }
    }

    @Nested
    @DisplayName("Repository.isClean")
    class IsCleanTest {
        @DisplayName("Repository.isClean()")
        @Test
        public void isCleanTest()
            throws Exception {
            // start with a new repository, just initialized
            JGitScript script = JGitScript.fromScratch(true);
            Repository repository = Repository.open(script.getWorkingDirectory());
            assertTrue(repository.isClean());
            
            // add some new files and test
            script.withFiles();
            assertFalse(repository.isClean());

            // stage the files without committing
            script.stage();
            assertFalse(repository.isClean());

            // commit the files, now we're clean again
            script.commit();
            assertTrue(repository.isClean());
        }
    }
}