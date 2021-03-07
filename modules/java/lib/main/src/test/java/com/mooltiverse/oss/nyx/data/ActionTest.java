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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Action")
public class ActionTest {
    @Test
    @DisplayName("Action()")
    void constructorTest()
        throws Exception {
        Identity identity = new Identity("commit", null);
        TimeStamp timeStamp = new TimeStamp(new Date(), null);

        Action action = new Action(identity, timeStamp);
        assertEquals(identity.getName(), action.getIdentity().getName());
        assertEquals(timeStamp.getTimeStamp(), action.getTimeStamp().getTimeStamp());

        // test with null values
        assertThrows(NullPointerException.class, () -> new Action(null, null));
        assertThrows(NullPointerException.class, () -> new Action(null, timeStamp));
        assertDoesNotThrow(() -> new Action(identity, null));
    }
}