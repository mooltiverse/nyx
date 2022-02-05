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

import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamples.SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;
import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamples.SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.presets.Extended;
import com.mooltiverse.oss.nyx.configuration.presets.Simple;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.services.Provider;
import com.mooltiverse.oss.nyx.io.FileMapper;
import com.mooltiverse.oss.nyx.version.Scheme;

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
        @DisplayName("Configuration.getChangelog() == Defaults.CHANGELOG")
        void getChangelogTest()
            throws Exception {
            assertEquals(Defaults.CHANGELOG.getPath(), new Configuration().getChangelog().getPath());
            assertEquals(Defaults.CHANGELOG.getSections().size(), new Configuration().getChangelog().getSections().size());
            assertTrue(new Configuration().getChangelog().getSections().isEmpty());
            assertEquals(Defaults.CHANGELOG.getSections().size(), new Configuration().getChangelog().getSubstitutions().size());
            assertTrue(new Configuration().getChangelog().getSubstitutions().isEmpty());
            assertEquals(Defaults.CHANGELOG.getTemplate(), new Configuration().getChangelog().getTemplate());
        }

        @Test
        @DisplayName("Configuration.getCommitMessageConventions() == Defaults.COMMIT_MESSAGE_CONVENTIONS")
        void getCommitMessageConventionsTest()
            throws Exception {
            assertEquals(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled(), new Configuration().getCommitMessageConventions().getEnabled());
            assertTrue(new Configuration().getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.getConfigurationFile() == Defaults.CONFIGURATION_FILE")
        void getConfigurationFileTest()
            throws Exception {
            assertEquals(Defaults.CONFIGURATION_FILE, new Configuration().getConfigurationFile());
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
        @DisplayName("Configuration.getDryRun() == Defaults.DRY_RUN")
        void getDryRunTest()
            throws Exception {
            assertEquals(Defaults.DRY_RUN, new Configuration().getDryRun());
        }

        @Test
        @DisplayName("Configuration.getGit() == Defaults.GIT")
        void getGitTest()
            throws Exception {
            assertEquals(Defaults.GIT.getRemotes().size(), new Configuration().getGit().getRemotes().size());
        }

        @Test
        @DisplayName("Configuration.getInitialVersion() == Defaults.INITIAL_VERSION")
        void getInitialVersionTest()
            throws Exception {
            assertEquals(Defaults.INITIAL_VERSION, new Configuration().getInitialVersion());
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
        @DisplayName("Configuration.getReleaseTypes() == Defaults.RELEASE_TYPES")
        void getReleaseTypesTest()
            throws Exception {
            assertEquals(Defaults.RELEASE_TYPES.getEnabled(), new Configuration().getReleaseTypes().getEnabled());
            assertEquals(1, new Configuration().getReleaseTypes().getItems().size());
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
        @DisplayName("Configuration.getServices() == Defaults.SERVICES")
        void getServicesTest()
            throws Exception {
            assertEquals(Defaults.SERVICES, new Configuration().getServices());
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
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getChangelog() == MOCK.getChangelog()")
        void getChangelogTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1"))
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.CHANGELOG);
            assertNotNull(configurationLayerMock.getChangelog());
            assertNotSame(configuration.getChangelog(), configurationLayerMock.getChangelog());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.CHANGELOG.getPath());
            assertNull(configuration.getChangelog().getPath());
            assertTrue(Defaults.CHANGELOG.getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(Defaults.CHANGELOG.getSubstitutions().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(Defaults.CHANGELOG.getTemplate());
            assertNull(configuration.getChangelog().getTemplate());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertEquals("CHANGELOG.md", configuration.getChangelog().getPath());
            assertNotNull(configuration.getChangelog().getSections());
            assertEquals(2, configuration.getChangelog().getSections().size());
            assertEquals("regex1", configuration.getChangelog().getSections().get("Section1"));
            assertEquals("regex2", configuration.getChangelog().getSections().get("Section2"));
            assertNotNull(configuration.getChangelog().getSubstitutions());
            assertEquals(1, configuration.getChangelog().getSubstitutions().size());
            assertEquals("string1", configuration.getChangelog().getSubstitutions().get("Expression1"));
            assertEquals("changelog.tpl", configuration.getChangelog().getTemplate());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertNull(configuration.getChangelog().getPath());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(configuration.getChangelog().getTemplate());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention1"),
                    Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationLayerMock.getCommitMessageConventions());
            assertNotSame(configuration.getCommitMessageConventions(), configurationLayerMock.getCommitMessageConventions());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItems().get("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

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
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getGit() == MOCK.getGit()")
        void getGitTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe", "pwd"))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.GIT);
            assertNotNull(configurationLayerMock.getGit());
            assertNotSame(configuration.getGit(), configurationLayerMock.getGit());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.GIT.getRemotes().isEmpty());
            assertTrue(configuration.getGit().getRemotes().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertNotNull(configuration.getGit().getRemotes());
            assertEquals(1, configuration.getGit().getRemotes().size());
            assertEquals("pwd", configuration.getGit().getRemotes().get("origin").getPassword());
            assertEquals("jdoe", configuration.getGit().getRemotes().get("origin").getUser());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertTrue(configuration.getGit().getRemotes().isEmpty());
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
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getReleaseTypes() == MOCK.getReleaseTypes()")
        void getReleaseTypesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(true, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", "Release description", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.RELEASE_TYPES);
            assertNotNull(configurationLayerMock.getReleaseTypes());
            assertNotSame(configuration.getReleaseTypes(), configurationLayerMock.getReleaseTypes());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNotNull(Defaults.RELEASE_TYPES.getEnabled());
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(Defaults.RELEASE_TYPES.getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(Defaults.RELEASE_TYPES.getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertEquals(1, Defaults.RELEASE_TYPES.getItems().size());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getItems());
            assertEquals(1, configuration.getReleaseTypes().getEnabled().size());
            assertTrue(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertEquals(1, configuration.getReleaseTypes().getPublicationServices().size());
            assertTrue(configuration.getReleaseTypes().getPublicationServices().contains("service1"));
            assertEquals(1, configuration.getReleaseTypes().getRemoteRepositories().size());
            assertTrue(configuration.getReleaseTypes().getRemoteRepositories().contains("remote1"));
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            assertTrue(configuration.getReleaseTypes().getItems().get("type1").getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", configuration.getReleaseTypes().getItems().get("type1").getCollapsedVersionQualifier());
            assertEquals("Release description", configuration.getReleaseTypes().getItems().get("type1").getDescription());
            assertEquals("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", configuration.getReleaseTypes().getItems().get("type1").getFilterTags());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitCommit());
            assertEquals("Committing {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitPush());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitTag());
            assertEquals("Tagging {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitTagMessage());
            assertNotNull(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers().isEmpty());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getMatchBranches());
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables().isEmpty());
            assertNull(configuration.getReleaseTypes().getItems().get("type1").getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getPublish());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getVersionRange());
            assertEquals(Boolean.FALSE, configuration.getReleaseTypes().getItems().get("type1").getVersionRangeFromBranchName());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
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
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getServices() == MOCK.getServices()")
        void getServicesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.SERVICES);
            assertNotNull(configurationLayerMock.getServices());
            assertNotSame(configuration.getServices(), configurationLayerMock.getServices());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(0, Defaults.SERVICES.size());
            assertEquals(0, configuration.getServices().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withCommandLineConfiguration(configurationLayerMock);

            assertEquals(2, configuration.getServices().size());
            assertTrue(configuration.getServices().containsKey("github"));
            assertTrue(configuration.getServices().containsKey("gitlab"));
            assertEquals(Provider.GITHUB, configuration.getServices().get("github").getType());
            assertEquals(3, configuration.getServices().get("github").getOptions().size());
            assertEquals("{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}", configuration.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo1", configuration.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner1", configuration.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(Provider.GITLAB, configuration.getServices().get("gitlab").getType());
            assertEquals(3, configuration.getServices().get("gitlab").getOptions().size());
            assertEquals("{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}", configuration.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));

            // now remove the command line configuration and test that now default values are returned again
            configuration.withCommandLineConfiguration(null);
            assertNotNull(configuration.getServices());
            assertEquals(0, configuration.getServices().size());
        }

        @Test
        @DisplayName("Configuration.withCommandLineConfiguration(MOCK).getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

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
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getChangelog() == MOCK.getChangelog()")
        void getChangelogTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1"))
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.CHANGELOG);
            assertNotNull(configurationLayerMock.getChangelog());
            assertNotSame(configuration.getChangelog(), configurationLayerMock.getChangelog());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.CHANGELOG.getPath());
            assertNull(configuration.getChangelog().getPath());
            assertTrue(Defaults.CHANGELOG.getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(Defaults.CHANGELOG.getSubstitutions().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(Defaults.CHANGELOG.getTemplate());
            assertNull(configuration.getChangelog().getTemplate());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertEquals("CHANGELOG.md", configuration.getChangelog().getPath());
            assertNotNull(configuration.getChangelog().getSections());
            assertEquals(2, configuration.getChangelog().getSections().size());
            assertEquals("regex1", configuration.getChangelog().getSections().get("Section1"));
            assertEquals("regex2", configuration.getChangelog().getSections().get("Section2"));
            assertNotNull(configuration.getChangelog().getSubstitutions());
            assertEquals(1, configuration.getChangelog().getSubstitutions().size());
            assertEquals("string1", configuration.getChangelog().getSubstitutions().get("Expression1"));
            assertEquals("changelog.tpl", configuration.getChangelog().getTemplate());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertNull(configuration.getChangelog().getPath());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(configuration.getChangelog().getTemplate());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention1"),
                    Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationLayerMock.getCommitMessageConventions());
            assertNotSame(configuration.getCommitMessageConventions(), configurationLayerMock.getCommitMessageConventions());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItems().get("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

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
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getGit() == MOCK.getGit()")
        void getGitTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe", "pwd"))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.GIT);
            assertNotNull(configurationLayerMock.getGit());
            assertNotSame(configuration.getGit(), configurationLayerMock.getGit());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.GIT.getRemotes().isEmpty());
            assertTrue(configuration.getGit().getRemotes().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertNotNull(configuration.getGit().getRemotes());
            assertEquals(1, configuration.getGit().getRemotes().size());
            assertEquals("pwd", configuration.getGit().getRemotes().get("origin").getPassword());
            assertEquals("jdoe", configuration.getGit().getRemotes().get("origin").getUser());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertTrue(configuration.getGit().getRemotes().isEmpty());
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
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getReleaseTypes() == MOCK.getReleaseTypes()")
        void getReleaseTypesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(true, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", "Release description", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.RELEASE_TYPES);
            assertNotNull(configurationLayerMock.getReleaseTypes());
            assertNotSame(configuration.getReleaseTypes(), configurationLayerMock.getReleaseTypes());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNotNull(Defaults.RELEASE_TYPES.getEnabled());
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(Defaults.RELEASE_TYPES.getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(Defaults.RELEASE_TYPES.getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertEquals(1, Defaults.RELEASE_TYPES.getItems().size());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getItems());
            assertEquals(1, configuration.getReleaseTypes().getEnabled().size());
            assertTrue(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertEquals(1, configuration.getReleaseTypes().getPublicationServices().size());
            assertTrue(configuration.getReleaseTypes().getPublicationServices().contains("service1"));
            assertEquals(1, configuration.getReleaseTypes().getRemoteRepositories().size());
            assertTrue(configuration.getReleaseTypes().getRemoteRepositories().contains("remote1"));
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            assertTrue(configuration.getReleaseTypes().getItems().get("type1").getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", configuration.getReleaseTypes().getItems().get("type1").getCollapsedVersionQualifier());
            assertEquals("Release description", configuration.getReleaseTypes().getItems().get("type1").getDescription());
            assertEquals("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", configuration.getReleaseTypes().getItems().get("type1").getFilterTags());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitCommit());
            assertEquals("Committing {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitPush());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitTag());
            assertEquals("Tagging {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitTagMessage());
            assertNotNull(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers().isEmpty());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getMatchBranches());
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables().isEmpty());
            assertNull(configuration.getReleaseTypes().getItems().get("type1").getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getPublish());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getVersionRange());
            assertEquals(Boolean.FALSE, configuration.getReleaseTypes().getItems().get("type1").getVersionRangeFromBranchName());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
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
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getServices() == MOCK.getServices()")
        void getServicesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.SERVICES);
            assertNotNull(configurationLayerMock.getServices());
            assertNotSame(configuration.getServices(), configurationLayerMock.getServices());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(0, Defaults.SERVICES.size());
            assertEquals(0, configuration.getServices().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(configurationLayerMock);

            assertEquals(2, configuration.getServices().size());
            assertTrue(configuration.getServices().containsKey("github"));
            assertTrue(configuration.getServices().containsKey("gitlab"));
            assertEquals(Provider.GITHUB, configuration.getServices().get("github").getType());
            assertEquals(3, configuration.getServices().get("github").getOptions().size());
            assertEquals("{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}", configuration.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo1", configuration.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner1", configuration.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(Provider.GITLAB, configuration.getServices().get("gitlab").getType());
            assertEquals(3, configuration.getServices().get("gitlab").getOptions().size());
            assertEquals("{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}", configuration.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));

            // now remove the command line configuration and test that now default values are returned again
            configuration.withPluginConfiguration(null);
            assertNotNull(configuration.getServices());
            assertEquals(0, configuration.getServices().size());
        }

        @Test
        @DisplayName("Configuration.withPluginConfiguration(MOCK).getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

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
     * Performs checks against the injection of a runtime configuration
     */
    @Nested
    @DisplayName("Configuration.withRuntimeConfiguration")
    class withRuntimeConfigurationTests {
        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getBump() == MOCK.getBump()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertEquals(configurationLayerMock.getBump(), configuration.getBump());

            // now remove the command line configuration and test that now default values are returned again
            assertNull(configuration.withRuntimeConfiguration(null).getBump());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getChangelog() == MOCK.getChangelog()")
        void getChangelogTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1"))
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.CHANGELOG);
            assertNotNull(configurationLayerMock.getChangelog());
            assertNotSame(configuration.getChangelog(), configurationLayerMock.getChangelog());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNull(Defaults.CHANGELOG.getPath());
            assertNull(configuration.getChangelog().getPath());
            assertTrue(Defaults.CHANGELOG.getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(Defaults.CHANGELOG.getSubstitutions().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(Defaults.CHANGELOG.getTemplate());
            assertNull(configuration.getChangelog().getTemplate());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertEquals("CHANGELOG.md", configuration.getChangelog().getPath());
            assertNotNull(configuration.getChangelog().getSections());
            assertEquals(2, configuration.getChangelog().getSections().size());
            assertEquals("regex1", configuration.getChangelog().getSections().get("Section1"));
            assertEquals("regex2", configuration.getChangelog().getSections().get("Section2"));
            assertNotNull(configuration.getChangelog().getSubstitutions());
            assertEquals(1, configuration.getChangelog().getSubstitutions().size());
            assertEquals("string1", configuration.getChangelog().getSubstitutions().get("Expression1"));
            assertEquals("changelog.tpl", configuration.getChangelog().getTemplate());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withRuntimeConfiguration(null);
            assertNull(configuration.getChangelog().getPath());
            assertTrue(configuration.getChangelog().getSections().isEmpty());
            assertTrue(configuration.getChangelog().getSubstitutions().isEmpty());
            assertNull(configuration.getChangelog().getTemplate());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention1"),
                    Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.COMMIT_MESSAGE_CONVENTIONS);
            assertNotNull(configurationLayerMock.getCommitMessageConventions());
            assertNotSame(configuration.getCommitMessageConventions(), configurationLayerMock.getCommitMessageConventions());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(Defaults.COMMIT_MESSAGE_CONVENTIONS.getItems().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention1"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr1", configuration.getCommitMessageConventions().getItems().get("convention1").getExpression());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withRuntimeConfiguration(null);
            assertTrue(configuration.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(configuration.getCommitMessageConventions().getItems().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.CONFIGURATION_FILE, configurationLayerMock.getConfigurationFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.getConfigurationFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getConfigurationFile(), configuration.getConfigurationFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.CONFIGURATION_FILE, configuration.withRuntimeConfiguration(null).getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getDirectory() == MOCK.getDirectory()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDirectory(), configuration.getDirectory());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DIRECTORY, configuration.withRuntimeConfiguration(null).getDirectory());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getDryRun() == MOCK.getDryRun()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getDryRun(), configuration.getDryRun());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.DRY_RUN, configuration.withRuntimeConfiguration(null).getDryRun());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getGit() == MOCK.getGit()")
        void getGitTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe", "pwd"))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.GIT);
            assertNotNull(configurationLayerMock.getGit());
            assertNotSame(configuration.getGit(), configurationLayerMock.getGit());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertTrue(Defaults.GIT.getRemotes().isEmpty());
            assertTrue(configuration.getGit().getRemotes().isEmpty());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertNotNull(configuration.getGit().getRemotes());
            assertEquals(1, configuration.getGit().getRemotes().size());
            assertEquals("pwd", configuration.getGit().getRemotes().get("origin").getPassword());
            assertEquals("jdoe", configuration.getGit().getRemotes().get("origin").getUser());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withRuntimeConfiguration(null);
            assertTrue(configuration.getGit().getRemotes().isEmpty());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getInitialVersion() == MOCK.getInitialVersion()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getInitialVersion(), configuration.getInitialVersion());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.INITIAL_VERSION, configuration.withRuntimeConfiguration(null).getInitialVersion());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getPreset() == MOCK.getPreset()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getPreset(), configuration.getPreset());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.PRESET, configuration.withRuntimeConfiguration(null).getPreset());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getReleasePrefix() == MOCK.getReleasePrefix()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleasePrefix(), configuration.getReleasePrefix());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_PREFIX, configuration.withRuntimeConfiguration(null).getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getReleaseLenient() == MOCK.getReleaseLenient()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getReleaseLenient(), configuration.getReleaseLenient());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RELEASE_LENIENT, configuration.withRuntimeConfiguration(null).getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getReleaseTypes() == MOCK.getReleaseTypes()")
        void getReleaseTypesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(true, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", "Release description", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.RELEASE_TYPES);
            assertNotNull(configurationLayerMock.getReleaseTypes());
            assertNotSame(configuration.getReleaseTypes(), configurationLayerMock.getReleaseTypes());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertNotNull(Defaults.RELEASE_TYPES.getEnabled());
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(Defaults.RELEASE_TYPES.getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(Defaults.RELEASE_TYPES.getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertEquals(1, Defaults.RELEASE_TYPES.getItems().size());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getItems());
            assertEquals(1, configuration.getReleaseTypes().getEnabled().size());
            assertTrue(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertEquals(1, configuration.getReleaseTypes().getPublicationServices().size());
            assertTrue(configuration.getReleaseTypes().getPublicationServices().contains("service1"));
            assertEquals(1, configuration.getReleaseTypes().getRemoteRepositories().size());
            assertTrue(configuration.getReleaseTypes().getRemoteRepositories().contains("remote1"));
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            assertTrue(configuration.getReleaseTypes().getItems().get("type1").getCollapseVersions());
            assertEquals("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", configuration.getReleaseTypes().getItems().get("type1").getCollapsedVersionQualifier());
            assertEquals("Release description", configuration.getReleaseTypes().getItems().get("type1").getDescription());
            assertEquals("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", configuration.getReleaseTypes().getItems().get("type1").getFilterTags());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitCommit());
            assertEquals("Committing {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitPush());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getGitTag());
            assertEquals("Tagging {{version}}", configuration.getReleaseTypes().getItems().get("type1").getGitTagMessage());
            assertNotNull(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getIdentifiers().isEmpty());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getMatchBranches());
            assertNotNull(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables());
            assertFalse(configuration.getReleaseTypes().getItems().get("type1").getMatchEnvironmentVariables().isEmpty());
            assertNull(configuration.getReleaseTypes().getItems().get("type1").getMatchWorkspaceStatus());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type1").getPublish());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type1").getVersionRange());
            assertEquals(Boolean.FALSE, configuration.getReleaseTypes().getItems().get("type1").getVersionRangeFromBranchName());

            // now remove the command line configuration and test that now default values are returned again
            configuration.withRuntimeConfiguration(null);
            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getResume() == MOCK.getResume()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getResume(), configuration.getResume());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.RESUME, configuration.withRuntimeConfiguration(null).getResume());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getScheme() == MOCK.getScheme()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getScheme(), configuration.getScheme());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.SCHEME, configuration.withRuntimeConfiguration(null).getScheme());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getServices() == MOCK.getServices()")
        void getServicesTest()
            throws Exception {
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotNull(Defaults.SERVICES);
            assertNotNull(configurationLayerMock.getServices());
            assertNotSame(configuration.getServices(), configurationLayerMock.getServices());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(0, Defaults.SERVICES.size());
            assertEquals(0, configuration.getServices().size());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);

            assertEquals(2, configuration.getServices().size());
            assertTrue(configuration.getServices().containsKey("github"));
            assertTrue(configuration.getServices().containsKey("gitlab"));
            assertEquals(Provider.GITHUB, configuration.getServices().get("github").getType());
            assertEquals(3, configuration.getServices().get("github").getOptions().size());
            assertEquals("{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}", configuration.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo1", configuration.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner1", configuration.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(Provider.GITLAB, configuration.getServices().get("gitlab").getType());
            assertEquals(3, configuration.getServices().get("gitlab").getOptions().size());
            assertEquals("{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}", configuration.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner2", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));

            // now remove the command line configuration and test that now default values are returned again
            configuration.withRuntimeConfiguration(null);
            assertNotNull(configuration.getServices());
            assertEquals(0, configuration.getServices().size());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            configurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            // in order to make the test meaningful, make sure the default and mock values are different
            assertNotEquals(Defaults.SHARED_CONFIGURATION_FILE, configurationLayerMock.getSharedConfigurationFile());

            // make sure the initial values come from defaults, until we inject the command line configuration
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.getSharedConfigurationFile());
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getSharedConfigurationFile(), configuration.getSharedConfigurationFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.SHARED_CONFIGURATION_FILE, configuration.withRuntimeConfiguration(null).getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getStateFile() == MOCK.getStateFile()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getStateFile(), configuration.getStateFile());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.STATE_FILE, configuration.withRuntimeConfiguration(null).getStateFile());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getVerbosity() == MOCK.getVerbosity()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVerbosity(), configuration.getVerbosity());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.VERBOSITY, configuration.withRuntimeConfiguration(null).getVerbosity());
        }

        @Test
        @DisplayName("Configuration.withRuntimeConfiguration(MOCK).getVersion() == MOCK.getVersion()")
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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            assertEquals(configurationLayerMock.getVersion(), configuration.getVersion());

            // now remove the command line configuration and test that now default values are returned again
            assertEquals(Defaults.VERSION, configuration.withRuntimeConfiguration(null).getVersion());
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
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setBump("alpha");
            mediumPriorityConfigurationLayerMock.setBump("beta");
            highPriorityConfigurationLayerMock.setBump("gamma");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertEquals(highPriorityConfigurationLayerMock.getBump(), configuration.getBump());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getChangelog() == MOCK.getChangelog()")
        void getChangelogTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG1.md", Map.<String,String>of("SectionA1", "regexA1", "SectionA2", "regexA2"), "changelog1.tpl", Map.<String,String>of("Expression1", "string1"))
            );
            mediumPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG2.md", Map.<String,String>of("SectionB1", "regexB1", "SectionB2", "regexB2"), "changelog2.tpl", Map.<String,String>of("Expression2", "string2"))
            );
            highPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG3.md", Map.<String,String>of("SectionC1", "regexC1", "SectionC2", "regexC2"), "changelog3.tpl", Map.<String,String>of("Expression3", "string3"))
            );
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertEquals("CHANGELOG3.md", configuration.getChangelog().getPath());
            assertNotNull(configuration.getChangelog().getSections());
            assertEquals(2, configuration.getChangelog().getSections().size());
            assertEquals("regexC1", configuration.getChangelog().getSections().get("SectionC1"));
            assertEquals("regexC2", configuration.getChangelog().getSections().get("SectionC2"));
            assertNotNull(configuration.getChangelog().getSubstitutions());
            assertEquals(1, configuration.getChangelog().getSubstitutions().size());
            assertEquals("string3", configuration.getChangelog().getSubstitutions().get("Expression3"));
            assertEquals("changelog3.tpl", configuration.getChangelog().getTemplate());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getCommitMessageConventions() == MOCK.getCommitMessageConventions()")
        void getCommitMessageConventionsTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention1"),
                    Map.<String,CommitMessageConvention>of("convention1", new CommitMessageConvention("expr1", Map.<String,String>of()))
                )
            );
            mediumPriorityConfigurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention2"),
                    Map.<String,CommitMessageConvention>of("convention2", new CommitMessageConvention("expr2", Map.<String,String>of()))
                )
            );
            highPriorityConfigurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("convention3"),
                    Map.<String,CommitMessageConvention>of("convention3", new CommitMessageConvention("expr3", Map.<String,String>of()))
                )
            );
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertNotNull(configuration.getCommitMessageConventions().getEnabled());
            assertNotNull(configuration.getCommitMessageConventions().getItems());
            assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
            assertTrue(configuration.getCommitMessageConventions().getEnabled().contains("convention3"));
            assertEquals(1, configuration.getCommitMessageConventions().getItems().size());
            assertEquals("expr3", configuration.getCommitMessageConventions().getItems().get("convention3").getExpression());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getConfigurationFile() == MOCK.getConfigurationFile()")
        void getConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            assertNotNull(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getConfigurationFile(), configuration.getConfigurationFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDirectory() == MOCK.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration.setDefaultDirectory(null); // clean the singleton from previous runs
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setDirectory("some/directory");
            mediumPriorityConfigurationLayerMock.setDirectory("some/other/directory");
            highPriorityConfigurationLayerMock.setDirectory("the/right/directory");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getDirectory(), configuration.getDirectory());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getDryRun() == MOCK.getDryRun()")
        void getDryRunTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setDryRun(Boolean.FALSE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getDryRun(), configuration.getDryRun());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getGit() == MOCK.getGit()")
        void getGitTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe1", "pwd1"), "replica", new GitRemoteConfiguration("stiger1", "sec1"))
                )
            );
            mediumPriorityConfigurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe2", "pwd2"), "clone", new GitRemoteConfiguration("stiger2", "sec2"))
                )
            );
            highPriorityConfigurationLayerMock.setGit(
                new GitConfiguration(
                    Map.<String,GitRemoteConfiguration>of("origin", new GitRemoteConfiguration("jdoe3", "pwd3"))
                )
            );
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertNotNull(configuration.getGit().getRemotes());
            assertEquals(3, configuration.getGit().getRemotes().size());
            assertEquals("pwd3", configuration.getGit().getRemotes().get("origin").getPassword());
            assertEquals("jdoe3", configuration.getGit().getRemotes().get("origin").getUser());
            assertEquals("sec1", configuration.getGit().getRemotes().get("replica").getPassword());
            assertEquals("stiger1", configuration.getGit().getRemotes().get("replica").getUser());
            assertEquals("sec2", configuration.getGit().getRemotes().get("clone").getPassword());
            assertEquals("stiger2", configuration.getGit().getRemotes().get("clone").getUser());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getInitialVersion() == MOCK.getInitialVersion()")
        void getInitialVersionTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setInitialVersion("9.9.9");
            mediumPriorityConfigurationLayerMock.setInitialVersion("8.8.8");
            highPriorityConfigurationLayerMock.setInitialVersion("7.7.7");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getInitialVersion(), configuration.getInitialVersion());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getPreset() == MOCK.getPreset()")
        void getPresetTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            mediumPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            highPriorityConfigurationLayerMock.setPreset(Extended.NAME);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getPreset(), configuration.getPreset());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleasePrefix() == MOCK.getReleasePrefix()")
        void getReleasePrefixTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setReleasePrefix("lpprefix");
            mediumPriorityConfigurationLayerMock.setReleasePrefix("mpprefix");
            highPriorityConfigurationLayerMock.setReleasePrefix("hpprefix");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getReleasePrefix(), configuration.getReleasePrefix());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleaseLenient() == MOCK.getReleaseLenient()")
        void getReleaseLenientTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            mediumPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            highPriorityConfigurationLayerMock.setReleaseLenient(Boolean.TRUE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getReleaseLenient(), configuration.getReleaseLenient());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getReleaseTypes() == MOCK.getReleaseTypes()")
        void getReleaseTypesTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(false, "{{branch1}}", "Release description 1", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            mediumPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type2"),
                    List.<String>of("service2"),
                    List.<String>of("remote2"),
                    Map.<String,ReleaseType>of("type2", new ReleaseType(true, "{{branch2}}", "Release description 2", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            highPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type3"),
                    List.<String>of("service3"),
                    List.<String>of("remote3"),
                    Map.<String,ReleaseType>of("type3", new ReleaseType(true, "{{branch3}}", "Release description 3", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertNotNull(configuration.getReleaseTypes().getEnabled());
            assertNotNull(configuration.getReleaseTypes().getPublicationServices());
            assertNotNull(configuration.getReleaseTypes().getRemoteRepositories());
            assertNotNull(configuration.getReleaseTypes().getItems());
            assertEquals(1, configuration.getReleaseTypes().getEnabled().size());
            assertTrue(configuration.getReleaseTypes().getEnabled().contains("type3"));
            assertEquals(1, configuration.getReleaseTypes().getPublicationServices().size());
            assertTrue(configuration.getReleaseTypes().getPublicationServices().contains("service3"));
            assertEquals(1, configuration.getReleaseTypes().getRemoteRepositories().size());
            assertTrue(configuration.getReleaseTypes().getRemoteRepositories().contains("remote3"));
            assertEquals(1, configuration.getReleaseTypes().getItems().size());
            assertTrue(configuration.getReleaseTypes().getItems().get("type3").getCollapseVersions());
            assertEquals("{{branch3}}", configuration.getReleaseTypes().getItems().get("type3").getCollapsedVersionQualifier());
            assertEquals("Release description 3", configuration.getReleaseTypes().getItems().get("type3").getDescription());
            assertEquals("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", configuration.getReleaseTypes().getItems().get("type3").getFilterTags());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type3").getGitCommit());
            assertEquals("Committing {{version}}", configuration.getReleaseTypes().getItems().get("type3").getGitCommitMessage());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type3").getGitPush());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type3").getGitTag());
            assertEquals("Tagging {{version}}", configuration.getReleaseTypes().getItems().get("type3").getGitTagMessage());
            assertFalse(configuration.getReleaseTypes().getEnabled().contains("type1"));
            assertFalse(configuration.getReleaseTypes().getEnabled().contains("type2"));
            assertTrue(configuration.getReleaseTypes().getEnabled().contains("type3"));
            assertNull(configuration.getReleaseTypes().getItems().get("type1"));
            assertNull(configuration.getReleaseTypes().getItems().get("type2"));
            assertNotNull(configuration.getReleaseTypes().getItems().get("type3").getIdentifiers());
            assertFalse(configuration.getReleaseTypes().getItems().get("type3").getIdentifiers().isEmpty());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type3").getMatchBranches());
            assertEquals(Boolean.TRUE.toString(), configuration.getReleaseTypes().getItems().get("type3").getPublish());
            assertEquals("", configuration.getReleaseTypes().getItems().get("type3").getVersionRange());
            assertEquals(Boolean.FALSE, configuration.getReleaseTypes().getItems().get("type3").getVersionRangeFromBranchName());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getResume() == MOCK.getResume()")
        void getResumeTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setResume(Boolean.FALSE);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getResume(), configuration.getResume());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getScheme() == MOCK.getScheme()")
        void getSchemeTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            mediumPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            highPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getScheme(), configuration.getScheme());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getServices() == MOCK.getServices()")
        void getServicesTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "ignoredtoken1",
                        "REPOSITORY_NAME", "ignoredrepo1",
                        "REPOSITORY_OWNER", "ignoredowner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "ignoredtoken2",
                        "REPOSITORY_NAME", "ignoredrepo2",
                        "REPOSITORY_OWNER", "ignoredowner2"
                    ))
                )
            );
            mediumPriorityConfigurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );
            highPriorityConfigurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo3",
                        "REPOSITORY_OWNER", "owner3"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}",
                        "REPOSITORY_NAME", "repo4",
                        "REPOSITORY_OWNER", "owner4"
                    ))
                )
            );
            
            // inject the command line configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);

            assertEquals(2, configuration.getServices().size());
            assertTrue(configuration.getServices().containsKey("github"));
            assertTrue(configuration.getServices().containsKey("gitlab"));
            assertEquals(Provider.GITHUB, configuration.getServices().get("github").getType());
            assertEquals(3, configuration.getServices().get("github").getOptions().size());
            assertEquals("{{#environment.variable}}GITHUB_TOKEN{{/environment.variable}}", configuration.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo3", configuration.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner3", configuration.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(Provider.GITLAB, configuration.getServices().get("gitlab").getType());
            assertEquals(3, configuration.getServices().get("gitlab").getOptions().size());
            assertEquals("{{#environment.variable}}GITLAB_TOKEN{{/environment.variable}}", configuration.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals("repo4", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals("owner4", configuration.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));
        }

        @Test
        @DisplayName("Configuration[multiple layers].getSharedConfigurationFile() == MOCK.getSharedConfigurationFile()")
        void getSharedConfigurationFileTest()
            throws Exception {
            assertNotNull(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            assertNotNull(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getSharedConfigurationFile(), configuration.getSharedConfigurationFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getStateFile() == MOCK.getStateFile()")
        void getStateFileTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setStateFile("file.yaml");
            mediumPriorityConfigurationLayerMock.setStateFile("file.yaml");
            highPriorityConfigurationLayerMock.setStateFile("file.json");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getStateFile(), configuration.getStateFile());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVerbosity() == MOCK.getVerbosity()")
        void getVerbosityTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setVerbosity(Verbosity.TRACE);
            mediumPriorityConfigurationLayerMock.setVerbosity(Verbosity.INFO);
            highPriorityConfigurationLayerMock.setVerbosity(Verbosity.DEBUG);
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            assertEquals(highPriorityConfigurationLayerMock.getVerbosity(), configuration.getVerbosity());
        }

        @Test
        @DisplayName("Configuration[multiple layers].getVersion() == MOCK.getVersion()")
        void getVersionTest()
            throws Exception {
            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();
            lowPriorityConfigurationLayerMock.setVersion("11.12.13");
            mediumPriorityConfigurationLayerMock.setVersion("21.22.23");
            highPriorityConfigurationLayerMock.setVersion("31.32.33");
            
            // inject the plugin configuration and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
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
