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
package com.mooltiverse.oss.nyx.state;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;

@DisplayName("State")
public class StateTests {
    @Nested
    @DisplayName("State")
    class ConstructorTests {
        @Test
        @DisplayName("State()")
        void constructorTest()
            throws Exception {
            // passing the null argument must throw an exception
            assertThrows(NullPointerException.class, () -> { new State(null); });
            assertDoesNotThrow(() -> { new State(new Configuration()); });
        }

        @Test
        @DisplayName("State.getBump()")
        void getBumpTest()
            throws Exception {
            // make sure the bump is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getBump());
        }

        @Test
        @DisplayName("State.setBump(String)")
        void setBumpTest()
            throws Exception {
            State state = new State(new Configuration());

            String bump = "alpha";
            state.setBump(bump);
            assertEquals(bump, state.getBump());
        }

        @Test
        @DisplayName("State.getConfiguration()")
        void getConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            assertEquals(configuration, new State(configuration).getConfiguration());
        }

        @Test
        @DisplayName("State.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration configuration = new Configuration();
            // make sure the state directory is the same from the configuration
            assertEquals(configuration.getDirectory(), new State(configuration).getDirectory());
        }

        @Test
        @DisplayName("State.getInternals()")
        void getInternalsTest()
            throws Exception {
            State state = new State(new Configuration());

            // make sure the initial internals is never null and empty
            assertNotNull(state.getInternals());
            assertTrue(state.getInternals().isEmpty());
        }

        @Test
        @DisplayName("State.getReleaseScope()")
        void getReleaseScopeTest()
            throws Exception {
            // make sure the release scope is initialized
            assertNotNull(new State(new Configuration()).getReleaseScope());
        }

        @Test
        @DisplayName("State.getScheme()")
        void getSchemeTest()
            throws Exception {
            Configuration configuration = new Configuration();
            // make sure the scheme is the same from the configuration
            assertEquals(configuration.getScheme(), new State(configuration).getScheme());
        }

        @Test
        @DisplayName("State.getTimestamp()")
        void getTimestampTest()
            throws Exception {
            // make sure the current timestamp is a fresh one
            State state = new State(new Configuration());
            assertTrue(System.currentTimeMillis() >= state.getTimestamp());
        }

        @Test
        @DisplayName("State.touchTimestamp()")
        void touchTimestampTest()
            throws Exception {
            // make sure that when touching the timestamp the new value is updated
            State state = new State(new Configuration());

            long oldTimestamp = state.getTimestamp();
            do {
                // just do nothing and let at least 1 millisecond pass
                //this.wait(1); // throws an IllegalMonitorStateException
            }
            while (oldTimestamp == System.currentTimeMillis()); // exit as soon as at least 1 millisecond passed
            
            assertEquals(state.touchTimestamp(), state.getTimestamp());

            assertNotEquals(oldTimestamp, state.getTimestamp());
        }

        @Test
        @DisplayName("State.getVersion()")
        void getVersionTest()
            throws Exception {
            // make sure the version is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getVersion());
        }

        @Test
        @DisplayName("State.getVersion() with prefix")
        void getVersionWithPrefixTest()
            throws Exception {
            Configuration configuration = new Configuration();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            configuration.withCommandLineConfiguration(configurationMock);
            State state = new State(configuration);

            state.setVersion("1.2.3");
            
            configurationMock.releasePrefix = "v";
            assertEquals("v1.2.3", state.getVersion());

            configurationMock.releasePrefix = "prefix";
            assertEquals("prefix1.2.3", state.getVersion());
        }

        @Test
        @DisplayName("State.getVersion() without prefix")
        void getVersionWithoutPrefixTest()
            throws Exception {
            Configuration configuration = new Configuration();
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            configuration.withCommandLineConfiguration(configurationMock);
            State state = new State(configuration);

            state.setVersion("1.2.3");
            
            configurationMock.releasePrefix = null; // no effects, as the default is not null
            assertEquals("1.2.3", state.getVersion());

            configurationMock.releasePrefix = ""; // this has effect and uses no prefix
            assertEquals("1.2.3", state.getVersion());
        }

        @Test
        @DisplayName("State.setVersion(String)")
        void setVersionTest()
            throws Exception {
            Configuration configuration = new Configuration();
            State state = new State(configuration);

            String version = "1.2.3";
            state.setVersion(version);
            assertEquals(version, state.getVersion());
        }

        /*@Test
        @DisplayName("State.setVersion(Version) throws exception when using wrong scheme")
        void setVersionWithWrongSchemeTest()
            throws Exception {
            State state = new State(new Configuration());

            // here we should try passing a version whose scheme doesn't match the configured scheme
            // and make sure an exception is thrown
        }*/
    }
}