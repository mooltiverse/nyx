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
package com.mooltiverse.oss.nyx.configuration;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.io.File;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.version.SemanticVersion;

@DisplayName("Defaults")
public class DefaultsTests {
    @Test
    @DisplayName("Defaults.DIRECTORY == System.getProperty('user.dir')")
    void directoryTest()
        throws Exception {
        assertEquals(new File(System.getProperty("user.dir")), Defaults.DIRECTORY);
    }

    @Test
    @DisplayName("Defaults.SCHEME = Scheme.SEMVER")
    void schemeTest()
        throws Exception {
        assumeTrue(Scheme.SEMVER.equals(Defaults.SCHEME));
    }

    @Test
    @DisplayName("Defaults.INITIAL_VERSION = SemanticVersion.DEFAULT_INITIAL_VERSION")
    void initialVersionTest()
        throws Exception {
        assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, Defaults.INITIAL_VERSION.toString());
    }
}