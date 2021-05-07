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

import com.mooltiverse.oss.nyx.configuration.examples.SimplestConfigurationExample;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.FileMapper;
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
            assertTrue(new Configuration().getCommitMessageConventions().getItems().isEmpty());
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
            
            assertEquals(directory.getAbsolutePath(), new File(new Configuration().getDirectory()).getAbsolutePath());

            Configuration.setDefaultDirectory(null); // clean up
        }

        @Test
        @DisplayName("Configuration.getConfigurationFile() == Defaults.CONFIGURATION_FILE")
        void getConfigurationFileTest()
            throws Exception {
            assertEquals(Defaults.CONFIGURATION_FILE, new Configuration().getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.getDryRun() == Defaults.DRY_RUN")
        void getDryRunTest()
            throws Exception {
            assertEquals(Defaults.DRY_RUN, new Configuration().getDryRun());
        }

        @Test
        @DisplayName("Configuration.getPreset() == Defaults.PRESET")
        void getPresetTest()
            throws Exception {
            assertEquals(Defaults.PRESET, new Configuration().getPreset());
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
        @DisplayName("Configuration.getResume() == Defaults.RESUME")
        void getResumeTest()
            throws Exception {
            assertEquals(Defaults.RESUME, new Configuration().getResume());
        }

        @Test
        @DisplayName("Configuration.getScheme() == Defaults.SCHEME")
        void getSchemeTest()
            throws Exception {
            assertEquals(Defaults.SCHEME, new Configuration().getScheme());
        }

        @Test
        @DisplayName("Configuration.getSharedConfigurationFile() == Defaults.SHARED_CONFIGURATION_FILE")
        void getSharedConfigurationFileTest()
            throws Exception {
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, new Configuration().getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.getStateFile() == Defaults.STATE_FILE")
        void getStateFileTest()
            throws Exception {
            assertEquals(Defaults.STATE_FILE, new Configuration().getStateFile());
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
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setBump("alpha");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.BUMP);
            assertNotNull(configurationLayerMock.getBump());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.BUMP);
            assertNull(configuration.getBump());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertEquals(configurationLayerMock.getBump(), configuration.getBump());

            // now remove the command line configuration and test that now default values are returned again
            assertNull(configuration.withCommandLineConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("convention1"));
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of())));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationLayerMock.getCommitMessageConventions());
            assertNotSame(configuration.getCommitMessageConventions(), configurationLayerMock.getCommitMessageConventions());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled());
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItem("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setConfigurationFile(savedFile.getAbsolutePath());

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.CONFIGURATION_FILE, configurationLayerMock.getConfigurationFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.getConfigurationFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getConfigurationFile(), configuration.getConfigurationFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.withCommandLineConfiguration(null).getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setDirectory("some/directory");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DIRECTORY, configurationLayerMock.getDirectory());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.DIRECTORY, configuration.getDirectory());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDirectory(), configuration.getDirectory());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withCommandLineConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setDryRun(Boolean.TRUE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DRY_RUN, configurationLayerMock.getDryRun());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.DRY_RUN, configuration.getDryRun());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDryRun(), configuration.getDryRun());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withCommandLineConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setInitialVersion("9.9.9");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.INITIAL_VERSION, configurationLayerMock.getInitialVersion());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.INITIAL_VERSION, configuration.getInitialVersion());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getInitialVersion(), configuration.getInitialVersion());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withCommandLineConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getPreset() == MOCK.getPreset()")
        void getPresetTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setPreset("simple");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.PRESET, configurationLayerMock.getPreset());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.PRESET, configuration.getPreset());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getPreset(), configuration.getPreset());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.PRESET, configuration.withCommandLineConfiguration(null).getPreset());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleasePrefix("testprefix");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_PREFIX, configurationLayerMock.getReleasePrefix());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.RELEASE_PREFIX, configuration.getReleasePrefix());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleasePrefix(), configuration.getReleasePrefix());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withCommandLineConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_LENIENT, configurationLayerMock.getReleaseLenient());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.RELEASE_LENIENT, configuration.getReleaseLenient());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleaseLenient(), configuration.getReleaseLenient());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withCommandLineConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getResume() == MOCK.getResume()")
        void getResumeTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setResume(Boolean.TRUE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RESUME, configurationLayerMock.getResume());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.RESUME, configuration.getResume());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getResume(), configuration.getResume());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RESUME, configuration.withCommandLineConfiguration(null).getResume());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setScheme(Scheme.SEMVER);

            // since there is only one scheme available, this assumption can't be assumed
            //assertNotEquals(Defaults.SCHEME, configurationLayerMock.scheme);

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.SCHEME, configuration.getScheme());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getScheme(), configuration.getScheme());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withCommandLineConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setSharedConfigurationFile(savedFile.getAbsolutePath());

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.SHARED_CONFIGURATION_FILE, configurationLayerMock.getSharedConfigurationFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.getSharedConfigurationFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getSharedConfigurationFile(), configuration.getSharedConfigurationFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.withCommandLineConfiguration(null).getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getStateFile() == MOCK.getStateFile()")
        void getStateFileTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setStateFile("state-file.yml");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.STATE_FILE, configurationLayerMock.getStateFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.STATE_FILE, configuration.getStateFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getStateFile(), configuration.getStateFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.STATE_FILE, configuration.withCommandLineConfiguration(null).getStateFile());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setVerbosity(Verbosity.TRACE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.VERBOSITY, configurationLayerMock.getVerbosity());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.VERBOSITY, configuration.getVerbosity());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVerbosity(), configuration.getVerbosity());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withCommandLineConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setVersion("11.12.13");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.VERSION);
            assertNotNull(configurationLayerMock.getVersion());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.VERSION);
            assertNull(configuration.getVersion());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVersion(), configuration.getVersion());

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
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setBump("alpha");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.BUMP);
            assertNotNull(configurationLayerMock.getBump());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertNull(Defaults.BUMP);
            assertNull(configuration.getBump());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertEquals(configurationLayerMock.getBump(), configuration.getBump());

            // now remove the plugin configuration and test that now default values are returned again
            assertNull(configuration.withPluginConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("convention1"));
            configurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of())));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationLayerMock.getCommitMessageConventions());
            assertNotSame(configuration.getCommitMessageConventions(), configurationLayerMock.getCommitMessageConventions());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled());
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItem("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertNull(configuration.getCommitMessageConventions().getEnabled());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setConfigurationFile(savedFile.getAbsolutePath());

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.CONFIGURATION_FILE, configurationLayerMock.getConfigurationFile());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.getConfigurationFile());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getConfigurationFile(), configuration.getConfigurationFile());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.withPluginConfiguration(null).getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setDirectory("some/directory");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DIRECTORY, configurationLayerMock.getDirectory());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.DIRECTORY, configuration.getDirectory());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDirectory(), configuration.getDirectory());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withPluginConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setDryRun(Boolean.TRUE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.DRY_RUN, configurationLayerMock.getDryRun());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.DRY_RUN, configuration.getDryRun());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDryRun(), configuration.getDryRun());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withPluginConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setInitialVersion("9.9.9");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.INITIAL_VERSION, configurationLayerMock.getInitialVersion());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.INITIAL_VERSION, configuration.getInitialVersion());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getInitialVersion(), configuration.getInitialVersion());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withPluginConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getPreset() == MOCK.getPreset()")
        void getPresetTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setPreset("simple");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.PRESET, configurationLayerMock.getPreset());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.PRESET, configuration.getPreset());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getPreset(), configuration.getPreset());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.PRESET, configuration.withPluginConfiguration(null).getPreset());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleasePrefix("testprefix");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_PREFIX, configurationLayerMock.getReleasePrefix());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.RELEASE_PREFIX, configuration.getReleasePrefix());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleasePrefix(), configuration.getReleasePrefix());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withPluginConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleaseLenient(Boolean.FALSE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RELEASE_LENIENT, configurationLayerMock.getReleaseLenient());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.RELEASE_LENIENT, configuration.getReleaseLenient());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleaseLenient(), configuration.getReleaseLenient());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withPluginConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getResume() == MOCK.getResume()")
        void getResumeTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setResume(Boolean.TRUE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.RESUME, configurationLayerMock.getResume());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.RESUME, configuration.getResume());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getResume(), configuration.getResume());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.RESUME, configuration.withPluginConfiguration(null).getResume());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setScheme(Scheme.SEMVER);

            // since there is only one scheme available, this assumption can't be assumed
            //assertNotEquals(Defaults.SCHEME, configurationLayerMock.scheme);

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.SCHEME, configuration.getScheme());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getScheme(), configuration.getScheme());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withPluginConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setSharedConfigurationFile(savedFile.getAbsolutePath());

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.SHARED_CONFIGURATION_FILE, configurationLayerMock.getSharedConfigurationFile());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.getSharedConfigurationFile());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getSharedConfigurationFile(), configuration.getSharedConfigurationFile());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.withPluginConfiguration(null).getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getStateFile() == MOCK.getStateFile()")
        void getStateFileTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setStateFile("state-file.yml");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.STATE_FILE, configurationLayerMock.getStateFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.STATE_FILE, configuration.getStateFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getStateFile(), configuration.getStateFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.STATE_FILE, configuration.withPluginConfiguration(null).getStateFile());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setVerbosity(Verbosity.TRACE);

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.VERBOSITY, configurationLayerMock.getVerbosity());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertEquals(Defaults.VERBOSITY, configuration.getVerbosity());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVerbosity(), configuration.getVerbosity());

            // now remove the plugin configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withPluginConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setVersion("11.12.13");

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNull(Defaults.VERSION);
            assertNotNull(configurationLayerMock.getVersion());

            // make sure the initial values come from defaults, until we inject the plugin configuration
            assertNull(Defaults.VERSION);
            assertNull(configuration.getVersion());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVersion(), configuration.getVersion());

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
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setBump("alpha");
            highPriorityConfigurationLayerMock.setBump("beta");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);

            assertEquals(highPriorityConfigurationLayerMock.getBump(), configuration.getBump());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("convention1"));
            lowPriorityConfigurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of())));
            highPriorityConfigurationLayerMock.getCommitMessageConventions().setEnabled(List.<String>of("convention2"));
            highPriorityConfigurationLayerMock.getCommitMessageConventions().getItems().putAll(Map.<String,CommitMessageConvention>of("convention2", new CommitMessageConvention("expr2", Map.<String,String>of())));
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention2"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr2", configuration.getCommitMessageConventions().getItem("convention2").getExpression());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            File savedFileLow = new File(System.getProperty("java.io.tmpdir"), "extendedLow"+this.hashCode()+".json");
            savedFileLow.deleteOnExit();
            FileMapper.save(savedFileLow.getAbsolutePath(), new SimplestConfigurationExample());
            File savedFileHigh = new File(System.getProperty("java.io.tmpdir"), "extendedHigh"+this.hashCode()+".json");
            savedFileHigh.deleteOnExit();
            FileMapper.save(savedFileHigh.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setConfigurationFile(savedFileLow.getAbsolutePath());
            highPriorityConfigurationLayerMock.setConfigurationFile(savedFileHigh.getAbsolutePath());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getConfigurationFile(), configuration.getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setDirectory("some/directory");
            highPriorityConfigurationLayerMock.setDirectory("some/other/directory");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getDirectory(), configuration.getDirectory());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setDryRun(Boolean.FALSE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getDryRun(), configuration.getDryRun());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setInitialVersion("9.9.9");
            highPriorityConfigurationLayerMock.setInitialVersion("8.8.8");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getInitialVersion(), configuration.getInitialVersion());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getPreset() == MOCK.getPreset()")
        void getPresetTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setPreset("low");
            highPriorityConfigurationLayerMock.setPreset("high");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getPreset(), configuration.getPreset());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setReleasePrefix("lpprefix");
            highPriorityConfigurationLayerMock.setReleasePrefix("hpprefix");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getReleasePrefix(), configuration.getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            highPriorityConfigurationLayerMock.setReleaseLenient(Boolean.TRUE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getReleaseLenient(), configuration.getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getResume() == MOCK.getResume()")
        void getResumeTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setResume(Boolean.FALSE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getResume(), configuration.getResume());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            highPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getScheme(), configuration.getScheme());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            File savedFileLow = new File(System.getProperty("java.io.tmpdir"), "extendedLow"+this.hashCode()+".json");
            savedFileLow.deleteOnExit();
            FileMapper.save(savedFileLow.getAbsolutePath(), new SimplestConfigurationExample());
            File savedFileHigh = new File(System.getProperty("java.io.tmpdir"), "extendedHigh"+this.hashCode()+".json");
            savedFileHigh.deleteOnExit();
            FileMapper.save(savedFileHigh.getAbsolutePath(), new SimplestConfigurationExample());

            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setSharedConfigurationFile(savedFileLow.getAbsolutePath());
            highPriorityConfigurationLayerMock.setSharedConfigurationFile(savedFileHigh.getAbsolutePath());
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getSharedConfigurationFile(), configuration.getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getStateFile() == MOCK.getStateFile()")
        void getStateFileTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setStateFile("file.yaml");
            highPriorityConfigurationLayerMock.setStateFile("file.json");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getStateFile(), configuration.getStateFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setVerbosity(Verbosity.TRACE);
            highPriorityConfigurationLayerMock.setVerbosity(Verbosity.INFO);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getVerbosity(), configuration.getVerbosity());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setVersion("11.12.13");
            highPriorityConfigurationLayerMock.setVersion("21.22.23");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getVersion(), configuration.getVersion());
        }
    }

    /**
     * Performs checks against the injection of a combination of configuration layers
     */
    @Nested
    @DisplayName("Configuration.with combined configuration layers")
    class withCombinedConfigurationLayersTests {
        @Test
        @DisplayName("Configuration[combined layers with standard files]")
        void combinedConfigurationWithStandardFilesTest()
            throws Exception {
            File tempDir = Files.createTempDirectory(String.valueOf(this.hashCode())).toFile();
            Configuration.setDefaultDirectory(tempDir);

            // create 4 different config files with the bump field different for each
            SimpleConfigurationLayer standardSharedConfiguration = new SimpleConfigurationLayer();
            standardSharedConfiguration.setBump("standard-shared");
            standardSharedConfiguration.setReleasePrefix("sharedPrefix");
            SimpleConfigurationLayer standardLocalConfiguration = new SimpleConfigurationLayer();
            standardLocalConfiguration.setBump("standard-local");

            // save the standard files to standard locations
            File standardSharedConfigurationFile = new File(tempDir, ".nyx-shared.json");
            standardSharedConfigurationFile.deleteOnExit();
            FileMapper.save(standardSharedConfigurationFile.getAbsolutePath(), standardSharedConfiguration);
            File standardLocalConfigurationFile = new File(tempDir, ".nyx.json");
            standardLocalConfigurationFile.deleteOnExit();
            FileMapper.save(standardLocalConfigurationFile.getAbsolutePath(), standardLocalConfiguration);

            Configuration configuration = new Configuration();

            // first test against the standard files, which must have been loaded by default
            assertEquals("sharedPrefix", configuration.getReleasePrefix()); // this is configured only on the standard shared file
            assertEquals("standard-local", configuration.getBump()); // this is the value configured in the standard local file and has higher priority over the standard shared
        }

        @Test
        @DisplayName("Configuration[combined layers with standard and custom files]")
        void combinedConfigurationWithStandardAndCustomFilesTest()
            throws Exception {
            File tempDir = Files.createTempDirectory(String.valueOf(this.hashCode())).toFile();
            Configuration.setDefaultDirectory(tempDir);

            // create 4 different config files with the bump field different for each
            // here the standard local file adds the custom configuration file, which in turn adds the shared custom file
            // so we can test all the chain
            SimpleConfigurationLayer standardSharedConfiguration = new SimpleConfigurationLayer();
            standardSharedConfiguration.setBump("standard-shared");
            standardSharedConfiguration.setReleasePrefix("sharedPrefix");
            SimpleConfigurationLayer standardLocalConfiguration = new SimpleConfigurationLayer();
            standardLocalConfiguration.setBump("standard-local");
            standardLocalConfiguration.setConfigurationFile("custom-local.json");
            SimpleConfigurationLayer customSharedConfiguration = new SimpleConfigurationLayer();
            customSharedConfiguration.setBump("custom-shared");
            SimpleConfigurationLayer customLocalConfiguration = new SimpleConfigurationLayer();
            customLocalConfiguration.setBump("custom-local");
            customLocalConfiguration.setSharedConfigurationFile("custom-shared.json");

            // save the standard files to standard locations
            File standardSharedConfigurationFile = new File(tempDir, ".nyx-shared.json");
            standardSharedConfigurationFile.deleteOnExit();
            FileMapper.save(standardSharedConfigurationFile.getAbsolutePath(), standardSharedConfiguration);
            File standardLocalConfigurationFile = new File(tempDir, ".nyx.json");
            standardLocalConfigurationFile.deleteOnExit();
            FileMapper.save(standardLocalConfigurationFile.getAbsolutePath(), standardLocalConfiguration);

            // save custom files to custom file names in the same directory
            File customSharedConfigurationFile = new File(tempDir, "custom-shared.json");
            customSharedConfigurationFile.deleteOnExit();
            FileMapper.save(customSharedConfigurationFile.getAbsolutePath(), customSharedConfiguration);
            File customLocalConfigurationFile = new File(tempDir, "custom-local.json");
            customLocalConfigurationFile.deleteOnExit();
            FileMapper.save(customLocalConfigurationFile.getAbsolutePath(), customLocalConfiguration);

            Configuration configuration = new Configuration();

            // first test against the standard files, which must have been loaded by default
            assertEquals("sharedPrefix", configuration.getReleasePrefix()); // this is configured only on the standard shared file
            assertEquals("custom-local", configuration.getBump()); // this is the value configured in the custom local file and has higher priority over the standard shared
        }

        @Test
        @DisplayName("Configuration[combined layers with standard, custom files and command line]")
        void combinedConfigurationWithStandardCustomFilesAndCommandLineTest()
            throws Exception {
            File tempDir = Files.createTempDirectory(String.valueOf(this.hashCode())).toFile();
            Configuration.setDefaultDirectory(tempDir);

            // create 4 different config files with the bump field different for each
            // here the standard local file adds the custom configuration file, which in turn adds the shared custom file
            // so we can test all the chain
            SimpleConfigurationLayer standardSharedConfiguration = new SimpleConfigurationLayer();
            standardSharedConfiguration.setBump("standard-shared");
            standardSharedConfiguration.setReleasePrefix("sharedPrefix");
            SimpleConfigurationLayer standardLocalConfiguration = new SimpleConfigurationLayer();
            standardLocalConfiguration.setBump("standard-local");
            standardLocalConfiguration.setConfigurationFile("custom-local.json");
            SimpleConfigurationLayer customSharedConfiguration = new SimpleConfigurationLayer();
            customSharedConfiguration.setBump("custom-shared");
            SimpleConfigurationLayer customLocalConfiguration = new SimpleConfigurationLayer();
            customLocalConfiguration.setBump("custom-local");
            customLocalConfiguration.setSharedConfigurationFile("custom-shared.json");
            SimpleConfigurationLayer customCmdlineConfigurationFile = new SimpleConfigurationLayer();
            customCmdlineConfigurationFile.setReleasePrefix("cmdlinePrefix");

            // also create the command line layer, which defines a different custom local file
            SimpleConfigurationLayer commandLineConfiguration = new SimpleConfigurationLayer();
            commandLineConfiguration.setBump("cmdline");
            commandLineConfiguration.setConfigurationFile("custom-cmdline.json");

            // save the standard files to standard locations
            File standardSharedConfigurationFile = new File(tempDir, ".nyx-shared.json");
            standardSharedConfigurationFile.deleteOnExit();
            FileMapper.save(standardSharedConfigurationFile.getAbsolutePath(), standardSharedConfiguration);
            File standardLocalConfigurationFile = new File(tempDir, ".nyx.json");
            standardLocalConfigurationFile.deleteOnExit();
            FileMapper.save(standardLocalConfigurationFile.getAbsolutePath(), standardLocalConfiguration);

            // save custom files to custom file names in the same directory
            File customSharedConfigurationFile = new File(tempDir, "custom-shared.json");
            customSharedConfigurationFile.deleteOnExit();
            FileMapper.save(customSharedConfigurationFile.getAbsolutePath(), customSharedConfiguration);
            File customLocalConfigurationFile = new File(tempDir, "custom-local.json");
            customLocalConfigurationFile.deleteOnExit();
            FileMapper.save(customLocalConfigurationFile.getAbsolutePath(), customLocalConfiguration);
            File customCmdlineConfigurationFileFile = new File(tempDir, "custom-cmdline.json");
            customCmdlineConfigurationFileFile.deleteOnExit();
            FileMapper.save(customCmdlineConfigurationFileFile.getAbsolutePath(), customCmdlineConfigurationFile);

            Configuration configuration = new Configuration();
            // first test against the standard files, which must have been loaded by default
            assertEquals("sharedPrefix", configuration.getReleasePrefix()); // this is configured only on the standard shared file
            assertEquals("custom-local", configuration.getBump()); // this is the value configured in the custom local file

            // inject the plugin configuration and test the new value is returned from that
            // what happens now is that the bump is taked directly from the command line layer,
            // but the release prefix must be read from the configuration that the command line defines
            // overwriting the previous configuration file
            configuration.withCommandLineConfiguration(commandLineConfiguration);

            // first test against the standard files, which must have been loaded by default
            assertEquals("cmdlinePrefix", configuration.getReleasePrefix()); // this must come from the custom configuration file defined by the command line config
            assertEquals("cmdline", configuration.getBump()); // this is the value configured in the command line and has higher priority over all others
        }
    }
}
