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

import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Message")
public class MessageTests {
    @Test
    @DisplayName("Message()")
    void constructorTest()
        throws Exception {
        Map<String,String> footers = Map.<String,String>of("k1", "v1", "k2", "v2");

        Message message = new Message("full", "short", footers);
        assertEquals("full", message.getFullMessage());
        assertEquals("short", message.getShortMessage());
        assertEquals(2, message.getFooters().size());
        assertEquals("v1", message.getFooters().get("k1"));
        assertEquals("v2", message.getFooters().get("k2"));

        // test with null values
        assertThrows(NullPointerException.class, () -> new Message(null, null, null));
        assertThrows(NullPointerException.class, () -> new Message(null, null, footers));
        assertThrows(NullPointerException.class, () -> new Message(null, "short", footers));
        assertThrows(NullPointerException.class, () -> new Message(null, "short", null));
        assertThrows(NullPointerException.class, () -> new Message("full", "short", null));
        assertThrows(NullPointerException.class, () -> new Message("full", null, null));
    }
}