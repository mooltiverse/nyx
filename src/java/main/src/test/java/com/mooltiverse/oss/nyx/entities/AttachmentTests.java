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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Attachment")
public class AttachmentTests {
    @Test
    @DisplayName("Attachment()")
    void constructorTest()
        throws Exception {
        assertEquals("n1", new Attachment("n1", "d1", "t1", "p1").getFileName());
        assertEquals("d1", new Attachment("n1", "d1", "t1", "p1").getDescription());
        assertEquals("t1", new Attachment("n1", "d1", "t1", "p1").getType());
        assertEquals("p1", new Attachment("n1", "d1", "t1", "p1").getPath());
    }
}