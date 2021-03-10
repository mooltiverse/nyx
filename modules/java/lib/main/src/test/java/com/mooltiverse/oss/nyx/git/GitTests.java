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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.git.script.GitScript;

@DisplayName("Git")
public class GitTests {
    @Nested
    @DisplayName("Git.open")
    class OpenTests {
        @DisplayName("Git.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsFile()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.open((File)null));
        }

        @DisplayName("Git.open(null) throws NullPointerException")
        @Test
        public void exceptionWithNullDirectoryAsString()
            throws Exception {
            assertThrows(NullPointerException.class, () -> Git.open((String)null));
        }

        @DisplayName("Git.open('') throws IllegalArgumentException")
        @Test
        public void exceptionWithEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IllegalArgumentException.class, () -> Git.open(""));
            assertThrows(IllegalArgumentException.class, () -> Git.open("  "));
        }

        @DisplayName("Git.open(String) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> Git.open("adirectorywiththisnamesuredoesnotexists"));
        }

        @DisplayName("Git.open(File) throws IOException with non existent directory")
        @Test
        public void exceptionWithNonExistingDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> Git.open(new File("adirectorywiththisnamesuredoesnotexists")));
        }

        @DisplayName("Git.open(String) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> Git.open(Files.createTempDirectory(null).toAbsolutePath().toString()));
        }

        @DisplayName("Git.open(File) throws IOException with empty directory")
        @Test
        public void exceptionWithNewEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> Git.open(Files.createTempDirectory(null).toFile()));
        }

        @DisplayName("Git.open(File)")
        @Test
        public void openFileTest()
            throws Exception {
            assertNotNull(Git.open(GitScript.fromScratch().getWorkingDirectory()));
        }

        @DisplayName("Git.open(String)")
        @Test
        public void openStringTest()
            throws Exception {
            assertNotNull(Git.open(GitScript.fromScratch().getWorkingDirectory().getAbsolutePath()));
        }
    }
}