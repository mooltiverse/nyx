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

import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TimeStamp")
public class TimeStampTest {
    @Test
    @DisplayName("TimeStamp()")
    void constructorTest()
        throws Exception {
        Date date = new Date();
        TimeZone zone = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC");
        
        TimeStamp timeStamp = new TimeStamp(date, zone);
        assertEquals(date, timeStamp.getTimeStamp());
        assertEquals(zone, timeStamp.getTimeZone());

        // test with null values
        assertThrows(NullPointerException.class, () -> new TimeStamp(null, null));
        assertThrows(NullPointerException.class, () -> new TimeStamp(null, zone));
        assertDoesNotThrow(() -> new TimeStamp(date, null));
    }
}