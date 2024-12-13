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
package com.mooltiverse.oss.nyx.entities.git;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Tag")
public class TagTests {
    @Test
    @DisplayName("Tag()")
    void constructorTest()
        throws Exception {
        Tag lightweightTag = new Tag("ltag", "target", false);
        Tag annotatedTag = new Tag("atag", "target", true);

        assertEquals("ltag", lightweightTag.getName());
        assertEquals("atag", annotatedTag.getName());

        assertEquals("target", lightweightTag.getTarget());
        assertEquals("target", annotatedTag.getTarget());

        assertFalse(lightweightTag.isAnnotated());
        assertTrue(annotatedTag.isAnnotated());

        // test with null values
        assertThrows(NullPointerException.class, () -> new Tag(null, null, true));
        assertThrows(NullPointerException.class, () -> new Tag("ltag", null, true));
        assertThrows(NullPointerException.class, () -> new Tag(null, "target", true));

        assertThrows(NullPointerException.class, () -> new Tag(null, null, false));
        assertThrows(NullPointerException.class, () -> new Tag("atag", null, false));
        assertThrows(NullPointerException.class, () -> new Tag(null, "target", false));
    }
}