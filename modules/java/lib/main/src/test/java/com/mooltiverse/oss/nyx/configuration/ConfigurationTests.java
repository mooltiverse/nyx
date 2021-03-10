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
import static org.junit.jupiter.api.Assumptions.*;

import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.mock.CustomConfigurationLayerMock;

@DisplayName("Configuration")
public class ConfigurationTests {
    /**
     * Performs checks against default values
     */
    @Nested
    @DisplayName("Configuration defaults only")
    class DefaultsOnlyTests {
        @Test
        @DisplayName("Configuration.getBump() == Defaults.BUMP")
        void getBumpTest()
            throws Exception {
            assertEquals(Defaults.BUMP, new Configuration().getBump());
        }

        @Test
        @DisplayName("Configuration.getDirectory() == Defaults.DIRECTORY")
        void getDirectoryTest()
            throws Exception {
            assertEquals(Defaults.DIRECTORY, new Configuration().getDirectory());
        }

        @Test
        @DisplayName("Configuration.getDryRun() == Defaults.DRY_RUN")
        void getDryRunTest()
            throws Exception {
            assertEquals(Defaults.DRY_RUN, new Configuration().getDryRun());
        }

        @Test
        @DisplayName("Configuration.getReleaseLenient() == Defaults.RELEASE_LENIENT")
        void getReleaseLenientTest()
            throws Exception {
            assertEquals(Defaults.RELEASE_LENIENT, new Configuration().getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.getReleasePrefix() == Defaults.RELEASE_PREFIX")
        void getReleasePrefixTest()
            throws Exception {
            assertEquals(Defaults.RELEASE_PREFIX, new Configuration().getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.getScheme() == Defaults.SCHEME")
        void getSchemeTest()
            throws Exception {
            assertEquals(Defaults.SCHEME, new Configuration().getScheme());
        }

        @Test
        @DisplayName("Configuration.getVerbosity() == Defaults.VERBOSITY")
        void getVerbosityTest()
            throws Exception {
            assertEquals(Defaults.VERBOSITY, new Configuration().getVerbosity());
        }

        @Test
        @DisplayName("Configuration.getVersion() == Defaults.VERSION")
        void getVersionTest()
            throws Exception {
            assertEquals(Defaults.VERSION, new Configuration().getVersion());
        }
    }

    /**
     * Performs checks against the injection of a plugin configuration
     */
    @Nested
    @DisplayName("Configuration.withPluginConfiguration")
    class withPluginConfigurationTests {
        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getBump() == MOCK.getBump()")
        void getBumpTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeTrue(Objects.isNull(Defaults.BUMP) && !Objects.isNull(CustomConfigurationLayerMock.BUMP));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Objects.isNull(Defaults.BUMP) && Objects.isNull(configuration.getBump()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.BUMP, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getBump());

            // now remove the plugin configuration and test that now default values are returned again
            assertNull(configuration.withPluginConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.DIRECTORY.equals(CustomConfigurationLayerMock.DIRECTORY));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.DIRECTORY.equals(configuration.getDirectory()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.DIRECTORY, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getDirectory());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withPluginConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.DRY_RUN.equals(CustomConfigurationLayerMock.DRY_RUN));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.DRY_RUN.equals(configuration.getDryRun()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.DRY_RUN, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getDryRun());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withPluginConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.INITIAL_VERSION.equals(CustomConfigurationLayerMock.INITIAL_VERSION));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.INITIAL_VERSION.equals(configuration.getInitialVersion()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.INITIAL_VERSION, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getInitialVersion());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withPluginConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.RELEASE_PREFIX.equals(CustomConfigurationLayerMock.RELEASE_PREFIX));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.RELEASE_PREFIX.equals(configuration.getReleasePrefix()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.RELEASE_PREFIX, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getReleasePrefix());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withPluginConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.RELEASE_LENIENT.equals(CustomConfigurationLayerMock.RELEASE_LENIENT));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.RELEASE_LENIENT.equals(configuration.getReleaseLenient()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.RELEASE_LENIENT, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getReleaseLenient());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withPluginConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // since there is only one scheme available, this assumption can't be assumed
            //assumeFalse(Defaults.SCHEME.equals(ConfigurationLayerMock.SCHEME));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.SCHEME.equals(configuration.getScheme()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.SCHEME, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getScheme());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withPluginConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeFalse(Defaults.VERBOSITY.equals(CustomConfigurationLayerMock.VERBOSITY));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Defaults.VERBOSITY.equals(configuration.getVerbosity()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.VERBOSITY, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getVerbosity());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withPluginConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            Configuration configuration = new Configuration();

            // in order to make the test meaningful, make sure the default and mock values are different
            assumeTrue(Objects.isNull(Defaults.VERSION) && !Objects.isNull(CustomConfigurationLayerMock.VERSION));

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assumeTrue(Objects.isNull(Defaults.VERSION) && Objects.isNull(configuration.getVersion()));
            
            // inject the plugin configuration and test the new value is returned from that
            assertEquals(CustomConfigurationLayerMock.VERSION, configuration.withPluginConfiguration(new CustomConfigurationLayerMock()).getVersion());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.VERSION, configuration.withPluginConfiguration(null).getVersion());
        }
    }
}