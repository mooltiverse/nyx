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
package com.mooltiverse.oss.nyx.data;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Commit")
public class CommitTests {
    @Test
    @DisplayName("Commit()")
    void constructorTest()
        throws Exception {
        Tag lightweightTag = new Tag("t1", "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", false);
        Tag annotatedTag = new Tag("t2", "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", true);
        Set<Tag> tags = Set.<Tag>of(lightweightTag, annotatedTag);
        List<String> parents = List.<String>of("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        TimeStamp timeStamp = new TimeStamp(new Date(), null);

        Identity authorIdentity = new Identity("author", null);
        Identity committerIdentity = new Identity("committer", null);
        Action authorAction = new Action(authorIdentity, timeStamp);
        Action commitAction = new Action(committerIdentity, timeStamp);

        Map<String,String> footers = Map.<String,String>of("k1", "v1", "k2", "v2");
        Message message = new Message("full", "short", footers);

        Commit commit = new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 999999, parents, authorAction, commitAction, message, tags);

        assertEquals("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", commit.getSHA());
        assertEquals(999999, commit.getDate());
        assertEquals(1, commit.getParents().size());
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", commit.getParents().get(0));
        assertEquals("author", commit.getAuthorAction().getIdentity().getName());
        assertEquals("committer", commit.getCommitAction().getIdentity().getName());
        assertEquals("full", commit.getMessage().getFullMessage());
        assertEquals(2, commit.getTags().size());

        // test with null values
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, null, null, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, null, null, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, parents, null, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, null, authorAction, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, null, null, commitAction, null, null));
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, null, null, null, message, null));
        assertThrows(NullPointerException.class, () -> new Commit(null, 0, null, null, null, null, tags));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, null, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, null, authorAction, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, null, null, commitAction, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, null, null, null, message, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, null, null, null, null, tags));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, null, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, null, commitAction, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, null, null, message, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, null, null, null, tags));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, commitAction, null, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, null, message, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, null, null, tags));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, commitAction, message, null));
        assertThrows(NullPointerException.class, () -> new Commit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, parents, authorAction, commitAction, null, tags));
    }
}