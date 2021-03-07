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

@DisplayName("Identity")
public class IdentityTest {
    @Test
    @DisplayName("Identity()")
    void constructorTest()
        throws Exception {
        Identity identity = new Identity("John", "jdoe@example.com");
        assertEquals("John", identity.getName());
        assertEquals("jdoe@example.com", identity.getEmail());

        // test with null values
        assertThrows(NullPointerException.class, () -> new Identity(null, null));
        assertThrows(NullPointerException.class, () -> new Identity(null, "jdoe@example.com"));
        assertDoesNotThrow(() -> new Identity("John", null));
    }
}