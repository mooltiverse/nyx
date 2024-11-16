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

import java.util.Date;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TimeStamp")
public class TimeStampTests {
    @Test
    @DisplayName("TimeStamp()")
    void constructorTest()
        throws Exception {
        Date date = new Date();
        Integer offset = Integer.valueOf(0);
        
        TimeStamp timeStamp = new TimeStamp(date, offset);
        assertEquals(date, timeStamp.getTimeStamp());
        assertEquals(offset, timeStamp.getOffset());

        // test with null values
        assertThrows(NullPointerException.class, () -> new TimeStamp(null, null));
        assertThrows(NullPointerException.class, () -> new TimeStamp(null, offset));
        assertDoesNotThrow(() -> new TimeStamp(date, null));
    }
}