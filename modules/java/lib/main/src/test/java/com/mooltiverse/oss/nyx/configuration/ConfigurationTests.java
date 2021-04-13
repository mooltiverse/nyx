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

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;

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
        @DisplayName("Configuration.getCommitMessageConventions() == Defaults.COMMIT_MESSAGE_CONVENTIONS")
        void getCommitMessageConventionsTest()
            throws Exception {
            assertEquals(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled(), new Configuration().getCommitMessageConventions().getEnabled());
            assertEquals(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems(), new Configuration().getCommitMessageConventions().getItems());
        }

        @Test
        @DisplayName("Configuration.getDirectory() == Defaults.DIRECTORY")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // make sure the default value is reset, in case previous tests left it dirty
            assertEquals(Defaults.DIRECTORY, new Configuration().getDirectory());
        }

        @Test
        @DisplayName("Configuration.setDirectory(File)")
        void setDirectoryTest()
            throws Exception {
            File directory = Files.createTempDirectory(null).toFile();
            Configuration.setDefaultDirectory(directory);
            
            assertEquals(directory, new Configuration().getDirectory());

            Configuration.setDefaultDirectory(null); // clean up
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
     * Performs checks against the injection of a command line configuration
     */
    @Nested
    @DisplayName("Configuration.withCommandLineConfiguration")
    class withCommandLineConfigurationTests {
        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getBump() == MOCK.getBump()")
        void getBumpTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.bump = "alpha";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.BUMP);
            assertNotNull(configurationMock.bump);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.BUMP);
            assertNull(configuration.getBump());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);

            assertEquals(configurationMock.bump, configuration.getBump());

            // now remove the command line configuration and test that now default values are returned again
            assertNull(configuration.withCommandLineConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.commitMessageConventions.enabled = List.<String>of("convention1");
            configurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationMock.commitMessageConventions);
            assertNotSame(configuration.getCommitMessageConventions(), configurationMock.commitMessageConventions);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled());
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems());
            assertNull(configuration.getCommitMessageConventions().getItems());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItem("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertNull(configuration.getCommitMessageConventions().getItems());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.directory = new File(System.getProperty("java.io.tmpdir"), "this directory does not exists");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DIRECTORY, configurationMock.directory);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.DIRECTORY, configuration.getDirectory());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.directory, configuration.getDirectory());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withCommandLineConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.dryRun = Boolean.TRUE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DRY_RUN, configurationMock.dryRun);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.DRY_RUN, configuration.getDryRun());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.dryRun, configuration.getDryRun());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withCommandLineConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.initialVersion = "9.9.9";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.INITIAL_VERSION, configurationMock.initialVersion);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.INITIAL_VERSION, configuration.getInitialVersion());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.initialVersion, configuration.getInitialVersion());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withCommandLineConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.releasePrefix = "testprefix";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_PREFIX, configurationMock.releasePrefix);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.RELEASE_PREFIX, configuration.getReleasePrefix());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.releasePrefix, configuration.getReleasePrefix());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withCommandLineConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.releaseLenient = Boolean.FALSE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_LENIENT, configurationMock.releaseLenient);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.RELEASE_LENIENT, configuration.getReleaseLenient());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.releaseLenient, configuration.getReleaseLenient());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withCommandLineConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.scheme = Scheme.SEMVER;

            // since there is only one scheme available, this assumption can't be assumed
            //assertNotEquals(Defaults.SCHEME, configurationMock.scheme);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.SCHEME, configuration.getScheme());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.scheme, configuration.getScheme());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withCommandLineConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.verbosity = Verbosity.TRACE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.VERBOSITY, configurationMock.verbosity);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.VERBOSITY, configuration.getVerbosity());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.verbosity, configuration.getVerbosity());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withCommandLineConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.version = "11.12.13";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.VERSION);
            assertNotNull(configurationMock.version);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.VERSION);
            assertNull(configuration.getVersion());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationMock);
            assertEquals(configurationMock.version, configuration.getVersion());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.VERSION, configuration.withCommandLineConfiguration(null).getVersion());
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
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.bump = "alpha";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.BUMP);
            assertNotNull(configurationMock.bump);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertNull(Defaults.BUMP);
            assertNull(configuration.getBump());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);

            assertEquals(configurationMock.bump, configuration.getBump());

            // now remove the plugin configuration and test that now default values are returned again
            assertNull(configuration.withPluginConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.commitMessageConventions.enabled = List.<String>of("convention1");
            configurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationMock.commitMessageConventions);
            assertNotSame(configuration.getCommitMessageConventions(), configurationMock.commitMessageConventions);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled());
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems());
            assertNull(configuration.getCommitMessageConventions().getItems());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItem("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertNull(configuration.getCommitMessageConventions().getItems());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.directory = new File(System.getProperty("java.io.tmpdir"), "this directory does not exists");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DIRECTORY, configurationMock.directory);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.DIRECTORY, configuration.getDirectory());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.directory, configuration.getDirectory());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withPluginConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.dryRun = Boolean.TRUE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DRY_RUN, configurationMock.dryRun);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.DRY_RUN, configuration.getDryRun());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.dryRun, configuration.getDryRun());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withPluginConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.initialVersion = "9.9.9";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.INITIAL_VERSION, configurationMock.initialVersion);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.INITIAL_VERSION, configuration.getInitialVersion());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.initialVersion, configuration.getInitialVersion());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withPluginConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.releasePrefix = "testprefix";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_PREFIX, configurationMock.releasePrefix);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.RELEASE_PREFIX, configuration.getReleasePrefix());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.releasePrefix, configuration.getReleasePrefix());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withPluginConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.releaseLenient = Boolean.FALSE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_LENIENT, configurationMock.releaseLenient);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.RELEASE_LENIENT, configuration.getReleaseLenient());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.releaseLenient, configuration.getReleaseLenient());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withPluginConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.scheme = Scheme.SEMVER;

            // since there is only one scheme available, this assumption can't be assumed
            //assertNotEquals(Defaults.SCHEME, configurationMock.scheme);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.SCHEME, configuration.getScheme());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.scheme, configuration.getScheme());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withPluginConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.verbosity = Verbosity.TRACE;

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.VERBOSITY, configurationMock.verbosity);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.VERBOSITY, configuration.getVerbosity());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.verbosity, configuration.getVerbosity());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withPluginConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            configurationMock.version = "11.12.13";

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.VERSION);
            assertNotNull(configurationMock.version);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertNull(Defaults.VERSION);
            assertNull(configuration.getVersion());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationMock);
            assertEquals(configurationMock.version, configuration.getVersion());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.VERSION, configuration.withPluginConfiguration(null).getVersion());
        }
    }

    /**
     * Performs checks against the injection of multiple configuration layers
     */
    @Nested
    @DisplayName("Configuration.with multiple configuration layers")
    class withMultipleConfigurationLayersTests {
        @Test
        @DisplayName("Configuration[multiple layers].getBump() == MOCK.getBump()")
        void getBumpTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.bump = "alpha";
            highPriorityconfigurationMock.bump = "beta";
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);

            assertEquals(highPriorityconfigurationMock.bump, configuration.getBump());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.commitMessageConventions.enabled = List.<String>of("convention1");
            lowPriorityconfigurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()));
            highPriorityconfigurationMock.commitMessageConventions.enabled = List.<String>of("convention2");
            highPriorityconfigurationMock.commitMessageConventions.items = Map.<String,CommitMessageConvention>of("convention2", new CommitMessageConvention("expr2", Map.<String,String>of()));
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention2"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr2", configuration.getCommitMessageConventions().getItem("convention2").getExpression());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.directory = new File(System.getProperty("java.io.tmpdir"), "this directory does not exists");
            highPriorityconfigurationMock.directory = new File(System.getProperty("java.io.tmpdir"), "another directory that does not exists");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.directory, configuration.getDirectory());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.dryRun = Boolean.TRUE;
            highPriorityconfigurationMock.dryRun = Boolean.FALSE;
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.dryRun, configuration.getDryRun());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.initialVersion = "9.9.9";
            highPriorityconfigurationMock.initialVersion = "8.8.8";
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.initialVersion, configuration.getInitialVersion());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.releasePrefix = "lpprefix";
            highPriorityconfigurationMock.releasePrefix = "hpprefix";
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.releasePrefix, configuration.getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.releaseLenient = Boolean.FALSE;
            highPriorityconfigurationMock.releaseLenient = Boolean.TRUE;
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.releaseLenient, configuration.getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.scheme = Scheme.SEMVER;
            highPriorityconfigurationMock.scheme = Scheme.SEMVER;
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.scheme, configuration.getScheme());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.verbosity = Verbosity.TRACE;
            highPriorityconfigurationMock.verbosity = Verbosity.INFO;
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.verbosity, configuration.getVerbosity());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            ConfigurationLayerMock lowPriorityconfigurationMock = new ConfigurationLayerMock();
            ConfigurationLayerMock highPriorityconfigurationMock = new ConfigurationLayerMock();
            Configuration configuration = new Configuration();
            lowPriorityconfigurationMock.version = "11.12.13";
            highPriorityconfigurationMock.version = "21.22.23";
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityconfigurationMock);
            configuration.withCommandLineConfiguration(highPriorityconfigurationMock);
            assertEquals(highPriorityconfigurationMock.version, configuration.getVersion());
        }
    }
}