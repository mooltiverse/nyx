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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import java.nio.file.Files;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.eclipse.jgit.revwalk.RevCommit;

import com.mooltiverse.oss.nyx.data.Commit;
import com.mooltiverse.oss.nyx.git.script.GitScript;
import com.mooltiverse.oss.nyx.git.script.GitScenario;

@DisplayName("JGitRepository")
public class JGitRepositoryTest {
    @Nested
    @DisplayName("JGitRepository.open")
    class OpenTest {
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

        @DisplayName("JGitRepository.open(<RUBBISHDIRECTORYNAME>) throws IOException")
        @Test
        public void exceptionWithNonExistingDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open("adirectorywiththisnamesuredoesnotexists"));
        }

        @DisplayName("JGitRepository.open(<RUBBISHDIRECTORYNAME>) throws IOException")
        @Test
        public void exceptionWithNonExistingDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(new File("adirectorywiththisnamesuredoesnotexists")));
        }

        @DisplayName("JGitRepository.open(<EMPTYDIRECTORY>) throws IOException")
        @Test
        public void exceptionWithNewEmptyDirectoryAsString()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(Files.createTempDirectory(null).toAbsolutePath().toString()));
        }

        @DisplayName("JGitRepository.open(<EMPTYDIRECTORY>) throws IOException")
        @Test
        public void exceptionWithNewEmptyDirectoryAsFile()
            throws Exception {
            assertThrows(IOException.class, () -> JGitRepository.open(Files.createTempDirectory(null).toFile()));
        }

        @DisplayName("JGitRepository.open(File)")
        @Test
        public void openFileTest()
            throws Exception {
            assertNotNull(JGitRepository.open(GitScript.fromScratch().getWorkingDirectory()));
        }

        @DisplayName("JGitRepository.open(String)")
        @Test
        public void openStringTest()
            throws Exception {
            assertNotNull(JGitRepository.open(GitScript.fromScratch().getWorkingDirectory().getAbsolutePath()));
        }
    }

    @Nested
    @DisplayName("JGitRepository.getLatestCommit")
    class GetLatestCommitTest {
        @DisplayName("JGitRepository.getLatestCommit() throws GitException")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            assertThrows(GitException.class, () -> repository.getLatestCommit());
            
            // add some new files and test
            script.withFiles();
            assertThrows(GitException.class, () -> repository.getLatestCommit());

            // stage the files without committing
            script.andStage();
            assertThrows(GitException.class, () -> repository.getLatestCommit());
        }

        @DisplayName("JGitRepository.getLatestCommit()")
        @Test
        public void getLatestCommitTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.getLatestCommit());
            
            // add and stage some files
            script.withFiles().andStage();

            // commit the files and get the commit SHA
            String commitSHA1 = script.commit("Test commit").getId().getName();
            assumeFalse(Objects.isNull(commitSHA1));
            assertNotNull(repository.getLatestCommit());
            assertEquals(commitSHA1, repository.getLatestCommit());

            // make sure the root and latest commits are the same by now
            assertEquals(repository.getRootCommit(), repository.getLatestCommit());

            // repeat the above with new changes
            script.withFiles(2);
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
    class GetRootCommitTest {
        @DisplayName("JGitRepository.getRootCommit() throws GitException")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            assertThrows(GitException.class, () -> repository.getRootCommit());
            
            // add some new files and test
            script.withFiles();
            assertThrows(GitException.class, () -> repository.getRootCommit());

            // stage the files without committing
            script.andStage();
            assertThrows(GitException.class, () -> repository.getRootCommit());
        }

        @DisplayName("JGitRepository.getRootCommit()")
        @Test
        public void getRootCommitTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            assertThrows(GitException.class, () -> repository.getRootCommit());
            
            // add and stage some files
            script.withFiles().andStage();

            // commit the files and get the commit SHA
            String commitSHA1 = script.commit("Test commit").getId().getName();
            assumeFalse(Objects.isNull(commitSHA1));
            assertNotNull(repository.getRootCommit());
            assertEquals(commitSHA1, repository.getRootCommit());

            // make sure the root and latest commits are the same by now
            assertEquals(repository.getRootCommit(), repository.getLatestCommit());

            // repeat the above with new changes
            script.withFiles(2);
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
    class IsCleanTest {
        @DisplayName("JGitRepository.isClean()")
        @Test
        public void isCleanTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            assertTrue(repository.isClean());
            
            // add some new files and test
            script.withFiles();
            assertFalse(repository.isClean());

            // stage the files without committing
            script.andStage();
            assertFalse(repository.isClean());

            // commit the files, now we're clean again
            script.andCommit();
            assertTrue(repository.isClean());
        }
    }

    @Nested
    @DisplayName("JGitRepository.getCommitTags")
    class GetCommitTagsTest {
        @DisplayName("JGitRepository.getCommitTags()")
        @Test
        public void getCommitTagsTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScript.fromScratch();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // add a commit
            script.withFiles().andStage();
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
    @DisplayName("JGitRepository.walkHistory")
    class WalkhistoryTest {
        @DisplayName("JGitRepository.walkHistory(CommitVisitor) with CommitVisitor stopping browsing")
        @Test
        public void walkHistoryWithVisitorStoppingBrowsingTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScenario.TwoBranchesShort.realize();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // Keep track of the visited commits
            List<Commit> visitedCommits = new ArrayList<Commit>();

            // make the visitor stop after 2 commits
            repository.walkHistory(null, null, c -> {
                visitedCommits.add(c);
                return visitedCommits.size() < 2;
            });

            assertEquals(2, visitedCommits.size());
        }

        @DisplayName("JGitRepository.walkHistory(CommitVisitor) with no boundaries")
        @Test
        public void walkHistoryWithNoBoundariesTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScenario.TwoBranchesShort.realize();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

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

        @DisplayName("JGitRepository.walkHistory(CommitVisitor) with start boundary")
        @Test
        public void walkHistoryWithStartBoundaryTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScenario.TwoBranchesShort.realize();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
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

        @DisplayName("JGitRepository.walkHistory(CommitVisitor) with end boundary")
        @Test
        public void walkHistoryWithEndBoundaryTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScenario.TwoBranchesShort.realize();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
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

        @DisplayName("JGitRepository.walkHistory(CommitVisitor) with both boundaries")
        @Test
        public void walkHistoryWithBothBoundariesTest()
            throws Exception {
            // start with a new repository, just initialized
            GitScript script = GitScenario.TwoBranchesShort.realize();
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
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
    }
}