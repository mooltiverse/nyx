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
package com.mooltiverse.oss.nyx.io;

import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import java.net.URL;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("FileMapper")
public class FileMapperTests {
    @Nested
    @DisplayName("Load from URL")
    public static class LoadFromURL {
        /**
         * A dummy class used to test the output from the <a href="https://www.jsontest.com/">JSON Test</a> service.
         */
        public static class JSONTestIPOutput {

            public String ip = null;

            public JSONTestIPOutput() {
                super();
            }
        }

        /**
         * In this test we just try to load a JSON file from the internet.
         * For this we use the <a href="https://www.jsontest.com/">JSON Test</a> service, which just returns a value like:
         * <pre>
         *      {"ip": "8.8.8.8"}
         * </pre>
         * 
         * @throws Exception
         */
        @Test
        @DisplayName("FileMapper.load(URL, ?)")
        void loadFromURLTest()
            throws Exception {
            URL url = new URI("http://ip.jsontest.com/").toURL();

            try {
                JSONTestIPOutput testOutput = FileMapper.load(url, JSONTestIPOutput.class);
                assertNotNull(testOutput.ip);
            }
            catch (DataAccessException dae) {
                // do nothing, just skip the test as sometimes http://ip.jsontest.com/ is unavailable and the call yields to a 500 server error
            }
        }
    }
}