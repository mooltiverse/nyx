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
import java.util.Set;

import java.nio.file.Files;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import org.eclipse.jgit.revwalk.RevCommit;

import com.mooltiverse.oss.nyx.data.Commit;
import com.mooltiverse.oss.nyx.data.Identity;
import com.mooltiverse.oss.nyx.data.Tag;

@DisplayName("JGitRepository")
public class JGitRepositoryTests {
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
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(null));
        }

        @DisplayName("JGitRepository.commit(String)")
        @Test
        public void commit1Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(List.<String>of("."), null));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String)")
        @Test
        public void commit2Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(null, new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(String, Identity, Identity)")
        @Test
        public void commit3Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
            Repository repository = JGitRepository.open(script.getWorkingDirectory());
            
            script.addRandomTextWorkbenchFiles(1);
            assertThrows(GitException.class, () -> repository.commit(List.<String>of("."), null, new Identity("John Doe", "jdoe@example.com"), new Identity("John Doe", "jdoe@example.com")));
        }

        @DisplayName("JGitRepository.commit(Collection<String>, String, Identity, Identity)")
        @Test
        public void commit4Params()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
        @DisplayName("JGitRepository.push()")
        @Test
        public void pushToRemoteTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.FROM_SCRATCH.realize();
            Script remote2script = Scenario.FROM_SCRATCH.realize();
            script.addRemote(remote1script.getGitDirectory(), "origin");
            script.addRemote(remote2script.getGitDirectory(), "custom");

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits, so this must throw an exception
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            String pushedRemote = repository.push();

            // now the default remote 'origin' has the first commit
            assertEquals("origin", pushedRemote);
            assertDoesNotThrow(() -> remote1script.getLastCommit());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertNotEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());

            // now push (to the default 'origin') and see the changes reflected
            pushedRemote = repository.push();

            // changes are reflected to 'origin' only
            assertEquals("origin", pushedRemote);
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());
        }
        
        @DisplayName("JGitRepository.push(String)")
        @Test
        public void pushTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.FROM_SCRATCH.realize();
            Script remote2script = Scenario.FROM_SCRATCH.realize();
            script.addRemote(remote1script.getGitDirectory(), "origin");
            script.addRemote(remote2script.getGitDirectory(), "custom");

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits, so this must throw an exception
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            String pushedRemote = repository.push("custom");

            // now the non-default remote 'custom' has the first commit
            assertEquals("custom", pushedRemote);
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertDoesNotThrow(() -> remote2script.getLastCommit());

            // add a commit into the local repo and make sure it's not into the others
            script.andCommit("A commit message");
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertNotEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());

            // now push and see the changes reflected
            pushedRemote = repository.push("custom");

            // changes are reflected to 'custom' only
            assertEquals("custom", pushedRemote);
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }

        @DisplayName("JGitRepository.push(Collection<String>)")
        @Test
        public void pushToRemotesTest()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();

            // also create two new empty repositories to use as remotes
            Script remote1script = Scenario.FROM_SCRATCH.realize();
            Script remote2script = Scenario.FROM_SCRATCH.realize();
            script.addRemote(remote1script.getGitDirectory(), "origin");
            script.addRemote(remote2script.getGitDirectory(), "custom");

            Repository repository = JGitRepository.open(script.getWorkingDirectory());

            // remotes still have no commits, so this must throw an exception
            assertThrows(Exception.class, () -> remote1script.getLastCommit());
            assertThrows(Exception.class, () -> remote2script.getLastCommit());

            // make a first sync, just to have a starting commit in remotes as well
            Set<String> pushedRemotes = repository.push(List.<String>of("origin", "custom"));

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

            // now push (to the default 'origin') and see the changes reflected
            pushedRemotes = repository.push(List.<String>of("origin", "custom"));

            // changes are reflected to both remotes
            assertTrue(pushedRemotes.contains("origin"));
            assertTrue(pushedRemotes.contains("custom"));
            assertEquals(2, pushedRemotes.size());
            assertEquals(script.getLastCommit().getId().getName(), remote1script.getLastCommit().getId().getName());
            assertEquals(script.getLastCommit().getId().getName(), remote2script.getLastCommit().getId().getName());
        }
    }

    @Nested
    @DisplayName("JGitRepository.tag")
    class TagTests {
        @DisplayName("JGitRepository.tag(String, String)")
        @Test
        public void tag1Test()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
        public void tag2Test()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
            assertThrows(GitException.class, () -> repository.tag("ltag", null));
            assertThrows(GitException.class, () -> repository.tag("atag", "The tag message"));
        }

        @DisplayName("JGitRepository.tag(String, String, Identity)")
        @Test
        public void tag3Test()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
        public void tag4Test()
            throws Exception {
            Script script = Scenario.INITIAL_COMMIT.realize();
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
    @DisplayName("JGitRepository.getLatestCommit")
    class GetLatestCommitTests {
        @DisplayName("JGitRepository.getLatestCommit() throws GitException without commits")
        @Test
        public void exceptionWithRepositoryWithNoCommits()
            throws Exception {
            Script script = Scenario.FROM_SCRATCH.realize();
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
    @DisplayName("JGitRepository.walkHistory")
    class WalkhistoryTests {
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