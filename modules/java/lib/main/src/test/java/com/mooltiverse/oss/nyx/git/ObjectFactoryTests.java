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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Set;

import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.revwalk.RevCommit;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.data.Action;
import com.mooltiverse.oss.nyx.data.Commit;
import com.mooltiverse.oss.nyx.data.Identity;
import com.mooltiverse.oss.nyx.data.Message;
import com.mooltiverse.oss.nyx.data.Tag;
import com.mooltiverse.oss.nyx.data.TimeStamp;
import com.mooltiverse.oss.nyx.git.script.GitScript;

@DisplayName("ObjectFactory")
public class ObjectFactoryTests {
    @Nested
    @DisplayName("ObjectFactory")
    class OpenTest {
        @DisplayName("ObjectFactory.actionFrom(PersonIdent)")
        @Test
        public void actionFrom()
            throws Exception {
            GitScript script = GitScript.fromScratch().withFiles();
            RevCommit commit = script.commit("A message");

            Action action = ObjectFactory.actionFrom(commit.getAuthorIdent());

            assertEquals(commit.getAuthorIdent().getName(), action.getIdentity().getName());
            assertEquals(commit.getAuthorIdent().getEmailAddress(), action.getIdentity().getEmail());

            assertEquals(commit.getAuthorIdent().getWhen(), action.getTimeStamp().getTimeStamp());
            assertEquals(commit.getAuthorIdent().getTimeZone(), action.getTimeStamp().getTimeZone());
        }

        @DisplayName("ObjectFactory.timeStampFrom(PersonIdent)")
        @Test
        public void timeStampFrom()
            throws Exception {
            GitScript script = GitScript.fromScratch().withFiles();
            RevCommit commit = script.commit("A message");

            TimeStamp timeStamp = ObjectFactory.timeStampFrom(commit.getAuthorIdent());

            assertEquals(commit.getAuthorIdent().getWhen(), timeStamp.getTimeStamp());
            assertEquals(commit.getAuthorIdent().getTimeZone(), timeStamp.getTimeZone());
        }

        @DisplayName("ObjectFactory.identityFrom(PersonIdent)")
        @Test
        public void identityFrom()
            throws Exception {
            GitScript script = GitScript.fromScratch().withFiles();
            RevCommit commit = script.commit("A message");

            Identity identity = ObjectFactory.identityFrom(commit.getAuthorIdent());

            assertEquals(commit.getAuthorIdent().getName(), identity.getName());
            assertEquals(commit.getAuthorIdent().getEmailAddress(), identity.getEmail());
        }

        @DisplayName("ObjectFactory.messageFrom(RevCommit)")
        @Test
        public void messageFrom()
            throws Exception {

            String messageHeader = "Commit message header";

            StringWriter messageBodyStringWriter = new StringWriter();
            PrintWriter messageBodyPrintWriter = new PrintWriter(messageBodyStringWriter);
            messageBodyPrintWriter.println("Body row 1");
            messageBodyPrintWriter.println("Body row 2");
            
            messageBodyPrintWriter.println();
            messageBodyPrintWriter.println("Body row 3");
            messageBodyPrintWriter.println("Body row 4");
            String paragraphMessageBody = messageBodyStringWriter.toString();
            

            String footer1Key = "Reviewed-By";
            String footer1Value = "John Doe";
            String footer2Key = "Issue";
            String footer2Value = "98765";
            StringWriter messageFooterStringWriter = new StringWriter();
            PrintWriter messageFooterPrintWriter = new PrintWriter(messageFooterStringWriter);
            // watch out here, the footer lines must not end with an LF
            messageFooterPrintWriter.print(footer1Key+": "+footer1Value);messageFooterPrintWriter.print("\n");
            messageFooterPrintWriter.print(footer2Key+": "+footer2Value);messageFooterPrintWriter.print("\n");
            String messageFooters = messageFooterStringWriter.toString();

            StringWriter fullMessageStringWriter = new StringWriter();
            PrintWriter fullMessageBodyPrintWriter = new PrintWriter(fullMessageStringWriter);
            fullMessageBodyPrintWriter.println(messageHeader);
            fullMessageBodyPrintWriter.println();
            fullMessageBodyPrintWriter.println(paragraphMessageBody);
            fullMessageBodyPrintWriter.println();
            fullMessageBodyPrintWriter.println(messageFooters);
            String fullCommitMessage = fullMessageStringWriter.toString();

            GitScript script = GitScript.fromScratch().withFiles();

            // test with a message with just the simple message
            RevCommit commit = script.commit(messageHeader);
            Message message = ObjectFactory.messageFrom(commit);

            assertEquals(messageHeader, message.getFullMessage());
            assertEquals(messageHeader, message.getShortMessage());
            assertEquals(0, message.getFooters().size());

            // test with a message with all elements
            commit = script.commit(fullCommitMessage);
            message = ObjectFactory.messageFrom(commit);

            assertEquals(fullCommitMessage, message.getFullMessage());
            assertEquals(messageHeader, message.getShortMessage());
            assertEquals(2, message.getFooters().size());
            assertEquals(footer1Value, message.getFooters().get(footer1Key));
            assertEquals(footer2Value, message.getFooters().get(footer2Key));
        }

        @DisplayName("ObjectFactory.tagFrom(RevCommit)")
        @Test
        public void tagFrom()
            throws Exception {
            GitScript script = GitScript.fromScratch().withFiles();

            script.commit("Commit 1");

            // test a lightweight tag
            Ref refTag1 = script.tag("t1", null);
            Tag tag1 = ObjectFactory.tagFrom(refTag1);
            assertEquals("t1", tag1.getName());
            assertFalse(tag1.isAnnotated());

            // test an annotated tag
            Ref refTag2 = script.tag("t2", "Tag message");

            Tag tag2 = ObjectFactory.tagFrom(script.peel(refTag2)); // the ref must be peeled first!
            assertEquals("t2", tag2.getName());
            assertTrue(tag2.isAnnotated());
        }

        @DisplayName("ObjectFactory.commitFrom(RevCommit)")
        @Test
        public void commitFrom()
            throws Exception {
            GitScript script = GitScript.fromScratch().withFiles();

            // test with a message with just the simple message
            RevCommit revCommit1 = script.commit("Commit 1");
            Ref tag1 = script.tag("t1", null);
            Commit commit1 = ObjectFactory.commitFrom(revCommit1, Set.<Tag>of(ObjectFactory.tagFrom(tag1)));

            assertEquals(revCommit1.getId().getName(), commit1.getSHA());
            assertEquals(revCommit1.getCommitTime(), commit1.getDate());
            assertEquals(0, commit1.getParents().size());
            assertEquals(revCommit1.getAuthorIdent().getName(), commit1.getAuthorAction().getIdentity().getName());
            assertEquals(revCommit1.getAuthorIdent().getEmailAddress(), commit1.getAuthorAction().getIdentity().getEmail());
            assertEquals(revCommit1.getCommitterIdent().getName(), commit1.getCommitAction().getIdentity().getName());
            assertEquals(revCommit1.getCommitterIdent().getEmailAddress(), commit1.getCommitAction().getIdentity().getEmail());
            assertEquals(revCommit1.getShortMessage(), commit1.getMessage().getShortMessage());
            assertEquals(revCommit1.getFullMessage(), commit1.getMessage().getFullMessage());
            assertEquals(1, commit1.getTags().size());
            assertEquals("t1", commit1.getTags().iterator().next().getName());

            RevCommit revCommit2 = script.commit("Commit 2");
            Ref tag2 = script.tag("t2", "Tag message");
            Commit commit2 = ObjectFactory.commitFrom(revCommit2, Set.<Tag>of(ObjectFactory.tagFrom(tag2)));

            assertEquals(revCommit2.getId().getName(), commit2.getSHA());
            assertEquals(revCommit2.getCommitTime(), commit2.getDate());
            assertEquals(1, commit2.getParents().size());
            assertEquals(commit1.getSHA(), commit2.getParents().get(0));
            assertEquals(revCommit2.getAuthorIdent().getName(), commit2.getAuthorAction().getIdentity().getName());
            assertEquals(revCommit2.getAuthorIdent().getEmailAddress(), commit2.getAuthorAction().getIdentity().getEmail());
            assertEquals(revCommit2.getCommitterIdent().getName(), commit2.getCommitAction().getIdentity().getName());
            assertEquals(revCommit2.getCommitterIdent().getEmailAddress(), commit2.getCommitAction().getIdentity().getEmail());
            assertEquals(revCommit2.getShortMessage(), commit2.getMessage().getShortMessage());
            assertEquals(revCommit2.getFullMessage(), commit2.getMessage().getFullMessage());
            assertEquals(1, commit2.getTags().size());
            assertEquals("t2", commit2.getTags().iterator().next().getName());
        }
    }
}