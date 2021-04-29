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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.version.SemanticVersion;

@DisplayName("ReleaseScope")
public class ReleaseScopeTests {
    @Test
    @DisplayName("ReleaseScope()")
    void constructorTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersion());
        assertNull(releaseScope.getPreviousVersionCommit());
        assertFalse(releaseScope.hasInitialCommit());
        assertNull(releaseScope.getInitialCommit());
        assertFalse(releaseScope.hasFinalCommit());
        assertNull(releaseScope.getFinalCommit());
        assertFalse(releaseScope.getSignificant());
        assertTrue(releaseScope.getCommits().isEmpty());
    }

    @Test
    @DisplayName("ReleaseScope.getCommits()")
    void getCommitsTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertTrue(releaseScope.getCommits().isEmpty());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertEquals(1, releaseScope.getCommits().size());
        releaseScope.getCommits().add("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertEquals(2, releaseScope.getCommits().size());
        releaseScope.getCommits().add("d0a19fc5776dc0c0b1a8d869c1117dac71065870");
        assertEquals(3, releaseScope.getCommits().size());
    }

    @Test
    @DisplayName("ReleaseScope.hasPreviousVersion()")
    void hasPreviousVersionTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertFalse(releaseScope.hasPreviousVersion());
        releaseScope.setPreviousVersion(SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).toString());
        assertTrue(releaseScope.hasPreviousVersion());
    }

    @Test
    @DisplayName("ReleaseScope.getPreviousVersion()")
    void getPreviousVersionTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersion());
        releaseScope.setPreviousVersion(SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).toString());
        assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, releaseScope.getPreviousVersion());
    }

    @Test
    @DisplayName("ReleaseScope.hasPreviousVersionCommit()")
    void hasPreviousVersionCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertFalse(releaseScope.hasPreviousVersionCommit());
        releaseScope.setPreviousVersionCommit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertTrue(releaseScope.hasPreviousVersionCommit());
    }

    @Test
    @DisplayName("ReleaseScope.getPreviousVersionCommit()")
    void getPreviousVersionCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersionCommit());
        releaseScope.setPreviousVersionCommit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertEquals("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", releaseScope.getPreviousVersionCommit());
    }

    @Test
    @DisplayName("ReleaseScope.hasInitialCommit()")
    void hasInitialCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertFalse(releaseScope.hasInitialCommit());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertTrue(releaseScope.hasInitialCommit());
    }

    @Test
    @DisplayName("ReleaseScope.getInitialCommit()")
    void getInitialCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getInitialCommit());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getInitialCommit());
        releaseScope.getCommits().add("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertEquals("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", releaseScope.getInitialCommit());
        releaseScope.getCommits().add("d0a19fc5776dc0c0b1a8d869c1117dac71065870");
        assertEquals("d0a19fc5776dc0c0b1a8d869c1117dac71065870", releaseScope.getInitialCommit());
    }

    @Test
    @DisplayName("ReleaseScope.hasFinalCommit()")
    void hasFinalCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertFalse(releaseScope.hasFinalCommit());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertTrue(releaseScope.hasFinalCommit());
    }

    @Test
    @DisplayName("ReleaseScope.getFinalCommit()")
    void getFinalCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getFinalCommit());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getFinalCommit());
        releaseScope.getCommits().add("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getFinalCommit());
        releaseScope.getCommits().add("d0a19fc5776dc0c0b1a8d869c1117dac71065870");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getFinalCommit());
    }

    @Test
    @DisplayName("ReleaseScope.getSignificant()")
    void getSignificantTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertFalse(releaseScope.getSignificant());
        releaseScope.getCommits().add("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertTrue(releaseScope.getSignificant());
    }
}