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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Scheme")
public class SchemeTests {
    @Test
    @DisplayName("Scheme.getScheme")
    void getSchemeTest()
        throws Exception {

        assertEquals(com.mooltiverse.oss.nyx.version.Scheme.SEMVER, Scheme.SEMVER.getScheme());
    }

    @Test
    @DisplayName("Scheme.getValue")
    void getValueTest()
        throws Exception {

        assertEquals("semver", Scheme.SEMVER.getValue());
    }

    @Test
    @DisplayName("Scheme.from(Scheme)")
    void fromSchemeTest()
        throws Exception {

        assertEquals(Scheme.SEMVER, Scheme.from(com.mooltiverse.oss.nyx.version.Scheme.SEMVER));

        assertThrows(NullPointerException.class, () -> { Scheme.from((com.mooltiverse.oss.nyx.version.Scheme)null); });
    }

    @Test
    @DisplayName("Scheme.from(String)")
    void fromStringTest()
        throws Exception {

        assertEquals(Scheme.SEMVER, Scheme.from("semver"));

        assertThrows(NullPointerException.class, () -> { Scheme.from((String)null); });
        assertThrows(IllegalArgumentException.class, () -> { Scheme.from(""); });
        assertThrows(IllegalArgumentException.class, () -> { Scheme.from("someotherstring"); });
    }
}