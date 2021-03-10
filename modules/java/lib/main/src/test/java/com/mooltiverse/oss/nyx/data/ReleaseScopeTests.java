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
    @DisplayName("ReleaseScope constructor")
    void constructorTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersion());
        assertNull(releaseScope.getPreviousVersionCommit());
        assertNull(releaseScope.getInitialCommit());
        assertNull(releaseScope.getFinalCommit());
        
        releaseScope = new ReleaseScope(SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION), "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", "e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", "d0a19fc5776dc0c0b1a8d869c1117dac71065870");
        assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, releaseScope.getPreviousVersion().toString());
        assertEquals("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", releaseScope.getPreviousVersionCommit());
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getInitialCommit());
        assertEquals("d0a19fc5776dc0c0b1a8d869c1117dac71065870", releaseScope.getFinalCommit());
    }

    @Test
    @DisplayName("ReleaseScope previous version")
    void previousVersionTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersion());
        releaseScope.setPreviousVersion(SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION));
        assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, releaseScope.getPreviousVersion().toString());
    }

    @Test
    @DisplayName("ReleaseScope previous version commit")
    void previousVersionCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getPreviousVersionCommit());
        releaseScope.setPreviousVersionCommit("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44");
        assertEquals("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", releaseScope.getPreviousVersionCommit());
    }

    @Test
    @DisplayName("ReleaseScope initial commit")
    void initialCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getInitialCommit());
        releaseScope.setInitialCommit("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getInitialCommit());
    }

    @Test
    @DisplayName("ReleaseScope final commit")
    void finalCommitTest()
        throws Exception {

        ReleaseScope releaseScope = new ReleaseScope();
        assertNull(releaseScope.getFinalCommit());
        releaseScope.setFinalCommit("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6");
        assertEquals("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", releaseScope.getFinalCommit());
    }
}