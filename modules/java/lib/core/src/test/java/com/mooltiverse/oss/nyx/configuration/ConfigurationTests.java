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
package com.mooltiverse.oss.nyx.configuration;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("Configuration")
public class ConfigurationTests {
    /**
     * Performs checks on the task configuration.
     */
    @Nested
    @DisplayName("Configuration defaults only")
    class DefaultsOnlyTests {
        @Test
        @DisplayName("Configuration.getBump()")
        void getBumpTest()
            throws Exception {
            assertEquals(Defaults.BUMP, Configuration.initial().getBump());
        }

        @Test
        @DisplayName("Configuration.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            assertEquals(Defaults.DIRECTORY, Configuration.initial().getDirectory());
        }

        @Test
        @DisplayName("Configuration.getDryRun()")
        void getDryRunTest()
            throws Exception {
            assertEquals(Defaults.DRY_RUN, Configuration.initial().getDryRun());
        }

        @Test
        @DisplayName("Configuration.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            assertEquals(Defaults.RELEASE_PREFIX, Configuration.initial().getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.getReleasePrefixLenient()")
        void getReleasePrefixLenientTest()
            throws Exception {
            assertEquals(Defaults.RELEASE_PREFIX_LENIENT, Configuration.initial().getReleasePrefixLenient());
        }

        @Test
        @DisplayName("Configuration.getScheme()")
        void getSchemeTest()
            throws Exception {
            assertEquals(Defaults.SCHEME, Configuration.initial().getScheme());
        }

        @Test
        @DisplayName("Configuration.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            assertEquals(Defaults.VERBOSITY, Configuration.initial().getVerbosity());
        }
    }
}