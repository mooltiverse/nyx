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
package com.mooltiverse.oss.nyx.entities;

import static org.junit.jupiter.api.Assertions.*;

import org.slf4j.event.Level;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Verbosity")
public class VerbosityTests {
    @Test
    @DisplayName("Verbosity.VALUE.getLevel()")
    void getLevelTest()
        throws Exception {

        assertEquals(Level.TRACE, Verbosity.TRACE.getLevel());
        assertEquals(Level.DEBUG, Verbosity.DEBUG.getLevel());
        assertEquals(Level.INFO, Verbosity.INFO.getLevel());
        assertEquals(Level.WARN, Verbosity.WARNING.getLevel());
        assertEquals(Level.ERROR, Verbosity.ERROR.getLevel());
        assertEquals(Level.ERROR, Verbosity.FATAL.getLevel()); // FATAL is not available in SLF4J
    }

    @Test
    @DisplayName("Verbosity.fromLevel(Level)")
    void fromLevelTest()
        throws Exception {

        assertEquals(Verbosity.TRACE, Verbosity.fromLevel(Level.TRACE));
        assertEquals(Verbosity.DEBUG, Verbosity.fromLevel(Level.DEBUG));
        assertEquals(Verbosity.INFO, Verbosity.fromLevel(Level.INFO));
        assertEquals(Verbosity.WARNING, Verbosity.fromLevel(Level.WARN));
        assertEquals(Verbosity.ERROR, Verbosity.fromLevel(Level.ERROR));
        // FATAL is not available in SLF4J

        assertThrows(NullPointerException.class, () -> { Verbosity.fromLevel((Level)null); });
    }
}