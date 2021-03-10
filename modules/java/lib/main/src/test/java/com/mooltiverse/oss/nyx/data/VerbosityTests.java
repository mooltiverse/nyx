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
    @DisplayName("Verbosity.VALUE.getValue()")
    void getValueTest()
        throws Exception {

        assertEquals("trace", Verbosity.TRACE.getValue());
        assertEquals("debug", Verbosity.DEBUG.getValue());
        assertEquals("info", Verbosity.INFO.getValue());
        assertEquals("warning", Verbosity.WARNING.getValue());
        assertEquals("error", Verbosity.ERROR.getValue());
        assertEquals("fatal", Verbosity.FATAL.getValue()); // FATAL is not available in SLF4J
    }

    @Test
    @DisplayName("Verbosity.from(Level)")
    void fromLevelTest()
        throws Exception {

        assertEquals(Verbosity.TRACE, Verbosity.from(Level.TRACE));
        assertEquals(Verbosity.DEBUG, Verbosity.from(Level.DEBUG));
        assertEquals(Verbosity.INFO, Verbosity.from(Level.INFO));
        assertEquals(Verbosity.WARNING, Verbosity.from(Level.WARN));
        assertEquals(Verbosity.ERROR, Verbosity.from(Level.ERROR));
        // FATAL is not available in SLF4J

        assertThrows(NullPointerException.class, () -> { Verbosity.from((Level)null); });
    }

    @Test
    @DisplayName("Verbosity.from(String)")
    void fromStringTest()
        throws Exception {

        assertEquals(Verbosity.TRACE, Verbosity.from("trace"));
        assertEquals(Verbosity.DEBUG, Verbosity.from("debug"));
        assertEquals(Verbosity.INFO, Verbosity.from("info"));
        assertEquals(Verbosity.WARNING, Verbosity.from("warning"));
        assertEquals(Verbosity.ERROR, Verbosity.from("error"));
        assertEquals(Verbosity.FATAL, Verbosity.from("fatal"));

        assertThrows(NullPointerException.class, () -> { Verbosity.from((String)null); });
        assertThrows(IllegalArgumentException.class, () -> { Verbosity.from(""); });
        assertThrows(IllegalArgumentException.class, () -> { Verbosity.from("someotherstring"); });
    }
}