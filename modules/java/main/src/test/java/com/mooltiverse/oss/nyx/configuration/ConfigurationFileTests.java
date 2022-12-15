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

import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamplesTests.EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;
import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamplesTests.EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;
import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamplesTests.SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;
import static com.mooltiverse.oss.nyx.configuration.ConfigurationExamplesTests.SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY;

import java.io.File;
import java.nio.file.Files;
import java.util.Objects;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.mooltiverse.oss.nyx.configuration.presets.Extended;
import com.mooltiverse.oss.nyx.configuration.presets.Simple;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Defaults;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.io.FileMapper;
import com.mooltiverse.oss.nyx.version.Scheme;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Tests the saving and loading of configuration layers as files.
 */
@DisplayName("ConfigurationFile")
public class ConfigurationFileTests {
    /**
     * Performs checks on the serialization and deserialization using example files
     */
    @Nested
    @DisplayName("Configuration serialization and deserialization with example files")
    class ConfigurationExamplesSerializationTests {
        @Test
        @DisplayName("Save and Load JSON configuration")
        void saveAndLoadJSON()
            throws Exception {
            // load an example configuration file
            assertNotNull(System.getProperty(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer source = FileMapper.load(new File(System.getProperty(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY)), SimpleConfigurationLayer.class);

            // now save it to another file
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), source);

            // now load the second file into another object and then compare their fields
            SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

            // all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
            // so nulls are to be tested against defaults, non simple nulls
            assertEquals(Objects.isNull(source.getBump()) ? Defaults.BUMP : source.getBump(), target.getBump());
            assertEquals(Objects.isNull(source.getConfigurationFile()) ? Defaults.CONFIGURATION_FILE : source.getConfigurationFile(), target.getConfigurationFile());
            //assertEquals(Objects.isNull(source.getDirectory()) ? Defaults.DIRECTORY : source.getDirectory(), target.getDirectory()); // this depends on the current runtime
            assertEquals(Objects.isNull(source.getDryRun()) ? Defaults.DRY_RUN : source.getDryRun(), target.getDryRun());
            assertEquals(Objects.isNull(source.getInitialVersion()) ? Defaults.INITIAL_VERSION : source.getInitialVersion(), target.getInitialVersion());
            assertEquals(Objects.isNull(source.getPreset()) ? Defaults.PRESET : source.getPreset(), target.getPreset());

            assertEquals(source.getReleaseAssets().keySet(), target.getReleaseAssets().keySet());
            for (String item: source.getReleaseAssets().keySet()) {
                assertEquals(source.getReleaseAssets().get(item).getFileName(), target.getReleaseAssets().get(item).getFileName());
                assertEquals(source.getReleaseAssets().get(item).getDescription(), target.getReleaseAssets().get(item).getDescription());
                assertEquals(source.getReleaseAssets().get(item).getType(), target.getReleaseAssets().get(item).getType());
                assertEquals(source.getReleaseAssets().get(item).getPath(), target.getReleaseAssets().get(item).getPath());
            }

            assertEquals(Objects.isNull(source.getReleaseLenient()) ? Defaults.RELEASE_LENIENT : source.getReleaseLenient(), target.getReleaseLenient());
            assertEquals(Objects.isNull(source.getReleasePrefix()) ? Defaults.RELEASE_PREFIX : source.getReleasePrefix(), target.getReleasePrefix());
            assertEquals(Objects.isNull(source.getResume()) ? Defaults.RESUME : source.getResume(), target.getResume());
            assertEquals(Objects.isNull(source.getScheme()) ? Defaults.SCHEME : source.getScheme(), target.getScheme());
            assertEquals(Objects.isNull(source.getSharedConfigurationFile()) ? Defaults.SHARED_CONFIGURATION_FILE : source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
            assertEquals(Objects.isNull(source.getStateFile()) ? Defaults.STATE_FILE : source.getStateFile(), target.getStateFile());
            assertEquals(Objects.isNull(source.getVerbosity()) ? Defaults.VERBOSITY : source.getVerbosity(), target.getVerbosity());
            assertEquals(Objects.isNull(source.getVersion()) ? Defaults.VERSION : source.getVersion(), target.getVersion());

            assertEquals(source.getChangelog().getPath(), target.getChangelog().getPath());
            assertEquals(source.getChangelog().getSections().keySet(), target.getChangelog().getSections().keySet());
            for (String item: source.getChangelog().getSections().keySet()) {
                assertEquals(source.getChangelog().getSections().get(item), target.getChangelog().getSections().get(item));
            }
            assertEquals(source.getChangelog().getSubstitutions().keySet(), target.getChangelog().getSubstitutions().keySet());
            for (String item: source.getChangelog().getSubstitutions().keySet()) {
                assertEquals(source.getChangelog().getSubstitutions().get(item), target.getChangelog().getSubstitutions().get(item));
            }
            assertEquals(source.getChangelog().getTemplate(), target.getChangelog().getTemplate());

            assertEquals(source.getGit().getRemotes().keySet(), target.getGit().getRemotes().keySet());
            for (String item: source.getGit().getRemotes().keySet()) {
                assertEquals(source.getGit().getRemotes().get(item).getPassword(), target.getGit().getRemotes().get(item).getPassword());
                assertEquals(source.getGit().getRemotes().get(item).getUser(), target.getGit().getRemotes().get(item).getUser());
            }
            
            assertEquals(Objects.isNull(source.getReleaseTypes().getEnabled()) ? Defaults.RELEASE_TYPES.getEnabled() : source.getReleaseTypes().getEnabled(), target.getReleaseTypes().getEnabled());
            assertEquals(Objects.isNull(source.getReleaseTypes().getPublicationServices()) ? Defaults.RELEASE_TYPES.getPublicationServices() : source.getReleaseTypes().getPublicationServices(), target.getReleaseTypes().getPublicationServices());
            assertEquals(Objects.isNull(source.getReleaseTypes().getRemoteRepositories()) ? Defaults.RELEASE_TYPES.getRemoteRepositories() : source.getReleaseTypes().getRemoteRepositories(), target.getReleaseTypes().getRemoteRepositories());
            assertEquals(source.getReleaseTypes().getItems().keySet(), target.getReleaseTypes().getItems().keySet());
            for (String item: source.getReleaseTypes().getItems().keySet()) {
                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getAssets()))
                    assertEquals(Defaults.ReleaseType.ASSETS, target.getReleaseTypes().getItems().get(item).getAssets());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getAssets().size(), target.getReleaseTypes().getItems().get(item).getAssets().size());
                    assertTrue(target.getReleaseTypes().getItems().get(item).getAssets().containsAll(source.getReleaseTypes().getItems().get(item).getAssets()));
                }

                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapseVersions()) ? Defaults.ReleaseType.COLLAPSE_VERSIONS : source.getReleaseTypes().getItems().get(item).getCollapseVersions(), target.getReleaseTypes().getItems().get(item).getCollapseVersions());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier()) ? Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER : source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier(), target.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getDescription()) ? Defaults.ReleaseType.DESCRIPTION : source.getReleaseTypes().getItems().get(item).getDescription(), target.getReleaseTypes().getItems().get(item).getDescription());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommit()) ? Defaults.ReleaseType.GIT_COMMIT : source.getReleaseTypes().getItems().get(item).getGitCommit(), target.getReleaseTypes().getItems().get(item).getGitCommit());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommitMessage()) ? Defaults.ReleaseType.GIT_COMMIT_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitCommitMessage(), target.getReleaseTypes().getItems().get(item).getGitCommitMessage());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitPush()) ? Defaults.ReleaseType.GIT_PUSH : source.getReleaseTypes().getItems().get(item).getGitPush(), target.getReleaseTypes().getItems().get(item).getGitPush());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTag()) ? Defaults.ReleaseType.GIT_TAG : source.getReleaseTypes().getItems().get(item).getGitTag(), target.getReleaseTypes().getItems().get(item).getGitTag());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTagMessage()) ? Defaults.ReleaseType.GIT_TAG_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitTagMessage(), target.getReleaseTypes().getItems().get(item).getGitTagMessage());

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, target.getReleaseTypes().getItems().get(item).getIdentifiers());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().size());
                    for (int i=0; i<source.getReleaseTypes().getItems().get(item).getIdentifiers().size(); i++) {
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getQualifier(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getQualifier());
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getValue(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getValue());
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getPosition(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getPosition());
                    }
                }

                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchBranches()) ? Defaults.ReleaseType.MATCH_BRANCHES : source.getReleaseTypes().getItems().get(item).getMatchBranches(), target.getReleaseTypes().getItems().get(item).getMatchBranches());

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables()))
                    assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().size(), target.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().size());
                    for (Entry<String,String> entry: source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().entrySet()) {
                        assertTrue(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().containsKey(entry.getKey()));
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey())) ? Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES.get(entry.getKey()) : source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey()), target.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey()));
                    }
                }
                
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus()) ? Defaults.ReleaseType.MATCH_WORKSPACE_STATUS : source.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus(), target.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getPublish()) ? Defaults.ReleaseType.PUBLISH : source.getReleaseTypes().getItems().get(item).getPublish(), target.getReleaseTypes().getItems().get(item).getPublish());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getVersionRange()) ? Defaults.ReleaseType.VERSION_RANGE : source.getReleaseTypes().getItems().get(item).getVersionRange(), target.getReleaseTypes().getItems().get(item).getVersionRange());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName()) ? Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME : source.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName(), target.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName());
            }

            assertEquals(source.getServices().keySet(), target.getServices().keySet());
            for (String item: source.getServices().keySet()) {
                assertEquals(source.getServices().get(item).getType(), target.getServices().get(item).getType());
                if (Objects.isNull(source.getServices().get(item).getOptions()))
                    assertNull(target.getServices().get(item).getOptions());
                else {
                    assertEquals(source.getServices().get(item).getOptions().size(), target.getServices().get(item).getOptions().size());
                    for (Entry<String,String> entry: source.getServices().get(item).getOptions().entrySet()) {
                        assertTrue(source.getServices().get(item).getOptions().containsKey(entry.getKey()));
                        assertEquals(entry.getValue(), target.getServices().get(item).getOptions().get(entry.getKey()));
                    }
                }
            }
        }

        @Test
        @DisplayName("Save and Load YAML configuration")
        void saveAndLoadYAML()
            throws Exception {
            // load an example configuration file
            assertNotNull(System.getProperty(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY), "A configuration file path must be passed to this test as a system property but it was not set");
            SimpleConfigurationLayer source = FileMapper.load(new File(System.getProperty(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY)), SimpleConfigurationLayer.class);

            // now save it to another file
            File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
            savedFile.deleteOnExit();
            FileMapper.save(savedFile.getAbsolutePath(), source);

            // now load the second file into another object and then compare their fields
            SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

            // all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
            // so nulls are to be tested against defaults, non simple nulls
            assertEquals(Objects.isNull(source.getBump()) ? Defaults.BUMP : source.getBump(), target.getBump());
            assertEquals(Objects.isNull(source.getConfigurationFile()) ? Defaults.CONFIGURATION_FILE : source.getConfigurationFile(), target.getConfigurationFile());
            //assertEquals(Objects.isNull(source.getDirectory()) ? Defaults.DIRECTORY : source.getDirectory(), target.getDirectory()); // this depends on the current runtime
            assertEquals(Objects.isNull(source.getDryRun()) ? Defaults.DRY_RUN : source.getDryRun(), target.getDryRun());
            assertEquals(Objects.isNull(source.getInitialVersion()) ? Defaults.INITIAL_VERSION : source.getInitialVersion(), target.getInitialVersion());
            assertEquals(Objects.isNull(source.getPreset()) ? Defaults.PRESET : source.getPreset(), target.getPreset());

            assertEquals(source.getReleaseAssets().keySet(), target.getReleaseAssets().keySet());
            for (String item: source.getReleaseAssets().keySet()) {
                assertEquals(source.getReleaseAssets().get(item).getFileName(), target.getReleaseAssets().get(item).getFileName());
                assertEquals(source.getReleaseAssets().get(item).getDescription(), target.getReleaseAssets().get(item).getDescription());
                assertEquals(source.getReleaseAssets().get(item).getType(), target.getReleaseAssets().get(item).getType());
                assertEquals(source.getReleaseAssets().get(item).getPath(), target.getReleaseAssets().get(item).getPath());
            }
            
            assertEquals(Objects.isNull(source.getReleaseLenient()) ? Defaults.RELEASE_LENIENT : source.getReleaseLenient(), target.getReleaseLenient());
            assertEquals(Objects.isNull(source.getReleasePrefix()) ? Defaults.RELEASE_PREFIX : source.getReleasePrefix(), target.getReleasePrefix());
            assertEquals(Objects.isNull(source.getResume()) ? Defaults.RESUME : source.getResume(), target.getResume());
            assertEquals(Objects.isNull(source.getScheme()) ? Defaults.SCHEME : source.getScheme(), target.getScheme());
            assertEquals(Objects.isNull(source.getSharedConfigurationFile()) ? Defaults.SHARED_CONFIGURATION_FILE : source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
            assertEquals(Objects.isNull(source.getStateFile()) ? Defaults.STATE_FILE : source.getStateFile(), target.getStateFile());
            assertEquals(Objects.isNull(source.getVerbosity()) ? Defaults.VERBOSITY : source.getVerbosity(), target.getVerbosity());
            assertEquals(Objects.isNull(source.getVersion()) ? Defaults.VERSION : source.getVersion(), target.getVersion());

            assertEquals(source.getChangelog().getPath(), target.getChangelog().getPath());
            assertEquals(source.getChangelog().getSections().keySet(), target.getChangelog().getSections().keySet());
            for (String item: source.getChangelog().getSections().keySet()) {
                assertEquals(source.getChangelog().getSections().get(item), target.getChangelog().getSections().get(item));
            }
            assertEquals(source.getChangelog().getSubstitutions().keySet(), target.getChangelog().getSubstitutions().keySet());
            for (String item: source.getChangelog().getSubstitutions().keySet()) {
                assertEquals(source.getChangelog().getSubstitutions().get(item), target.getChangelog().getSubstitutions().get(item));
            }
            assertEquals(source.getChangelog().getTemplate(), target.getChangelog().getTemplate());

            assertEquals(Objects.isNull(source.getCommitMessageConventions().getEnabled()) ? Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled() : source.getCommitMessageConventions().getEnabled(), target.getCommitMessageConventions().getEnabled());
            assertEquals(source.getCommitMessageConventions().getItems().keySet(), target.getCommitMessageConventions().getItems().keySet());
            for (String item: source.getCommitMessageConventions().getItems().keySet()) {
                assertEquals(source.getCommitMessageConventions().getItems().get(item).getExpression(), target.getCommitMessageConventions().getItems().get(item).getExpression());
                assertEquals(source.getCommitMessageConventions().getItems().get(item).getBumpExpressions(), target.getCommitMessageConventions().getItems().get(item).getBumpExpressions());
            }

            assertEquals(source.getGit().getRemotes().keySet(), target.getGit().getRemotes().keySet());
            for (String item: source.getGit().getRemotes().keySet()) {
                assertEquals(source.getGit().getRemotes().get(item).getPassword(), target.getGit().getRemotes().get(item).getPassword());
                assertEquals(source.getGit().getRemotes().get(item).getUser(), target.getGit().getRemotes().get(item).getUser());
            }
            
            assertEquals(Objects.isNull(source.getReleaseTypes().getEnabled()) ? Defaults.RELEASE_TYPES.getEnabled() : source.getReleaseTypes().getEnabled(), target.getReleaseTypes().getEnabled());
            assertEquals(Objects.isNull(source.getReleaseTypes().getPublicationServices()) ? Defaults.RELEASE_TYPES.getPublicationServices() : source.getReleaseTypes().getPublicationServices(), target.getReleaseTypes().getPublicationServices());
            assertEquals(Objects.isNull(source.getReleaseTypes().getRemoteRepositories()) ? Defaults.RELEASE_TYPES.getRemoteRepositories() : source.getReleaseTypes().getRemoteRepositories(), target.getReleaseTypes().getRemoteRepositories());
            assertEquals(source.getReleaseTypes().getItems().keySet(), target.getReleaseTypes().getItems().keySet());
            for (String item: source.getReleaseTypes().getItems().keySet()) {
                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getAssets()))
                    assertEquals(Defaults.ReleaseType.ASSETS, target.getReleaseTypes().getItems().get(item).getAssets());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getAssets().size(), target.getReleaseTypes().getItems().get(item).getAssets().size());
                    assertTrue(target.getReleaseTypes().getItems().get(item).getAssets().containsAll(source.getReleaseTypes().getItems().get(item).getAssets()));
                }
                
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapseVersions()) ? Defaults.ReleaseType.COLLAPSE_VERSIONS : source.getReleaseTypes().getItems().get(item).getCollapseVersions(), target.getReleaseTypes().getItems().get(item).getCollapseVersions());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier()) ? Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER : source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier(), target.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getDescription()) ? Defaults.ReleaseType.DESCRIPTION : source.getReleaseTypes().getItems().get(item).getDescription(), target.getReleaseTypes().getItems().get(item).getDescription());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommit()) ? Defaults.ReleaseType.GIT_COMMIT : source.getReleaseTypes().getItems().get(item).getGitCommit(), target.getReleaseTypes().getItems().get(item).getGitCommit());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommitMessage()) ? Defaults.ReleaseType.GIT_COMMIT_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitCommitMessage(), target.getReleaseTypes().getItems().get(item).getGitCommitMessage());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitPush()) ? Defaults.ReleaseType.GIT_PUSH : source.getReleaseTypes().getItems().get(item).getGitPush(), target.getReleaseTypes().getItems().get(item).getGitPush());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTag()) ? Defaults.ReleaseType.GIT_TAG : source.getReleaseTypes().getItems().get(item).getGitTag(), target.getReleaseTypes().getItems().get(item).getGitTag());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTagMessage()) ? Defaults.ReleaseType.GIT_TAG_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitTagMessage(), target.getReleaseTypes().getItems().get(item).getGitTagMessage());

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS, target.getReleaseTypes().getItems().get(item).getIdentifiers());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().size());
                    for (int i=0; i<source.getReleaseTypes().getItems().get(item).getIdentifiers().size(); i++) {
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getQualifier(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getQualifier());
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getValue(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getValue());
                        assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getPosition(), target.getReleaseTypes().getItems().get(item).getIdentifiers().get(i).getPosition());
                    }
                }

                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchBranches()) ? Defaults.ReleaseType.MATCH_BRANCHES : source.getReleaseTypes().getItems().get(item).getMatchBranches(), target.getReleaseTypes().getItems().get(item).getMatchBranches());

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables()))
                    assertEquals(Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES, source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().size(), target.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().size());
                    for (Entry<String,String> entry: source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().entrySet()) {
                        assertTrue(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().containsKey(entry.getKey()));
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey())) ? Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES.get(entry.getKey()) : source.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey()), target.getReleaseTypes().getItems().get(item).getMatchEnvironmentVariables().get(entry.getKey()));
                    }
                }
                
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus()) ? Defaults.ReleaseType.MATCH_WORKSPACE_STATUS : source.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus(), target.getReleaseTypes().getItems().get(item).getMatchWorkspaceStatus());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getPublish()) ? Defaults.ReleaseType.PUBLISH : source.getReleaseTypes().getItems().get(item).getPublish(), target.getReleaseTypes().getItems().get(item).getPublish());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getVersionRange()) ? Defaults.ReleaseType.VERSION_RANGE : source.getReleaseTypes().getItems().get(item).getVersionRange(), target.getReleaseTypes().getItems().get(item).getVersionRange());
                assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName()) ? Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME : source.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName(), target.getReleaseTypes().getItems().get(item).getVersionRangeFromBranchName());
            }

            assertEquals(source.getServices().keySet(), target.getServices().keySet());
            for (String item: source.getServices().keySet()) {
                assertEquals(source.getServices().get(item).getType(), target.getServices().get(item).getType());
                if (Objects.isNull(source.getServices().get(item).getOptions()))
                    assertNull(target.getServices().get(item).getOptions());
                else {
                    assertEquals(source.getServices().get(item).getOptions().size(), target.getServices().get(item).getOptions().size());
                    for (Entry<String,String> entry: source.getServices().get(item).getOptions().entrySet()) {
                        assertTrue(source.getServices().get(item).getOptions().containsKey(entry.getKey()));
                        assertEquals(entry.getValue(), target.getServices().get(item).getOptions().get(entry.getKey()));
                    }
                }
            }
        }
    }

    /**
     * Performs checks on the serialization of the Configuration object, rather than just layers
     */
    @Nested
    @DisplayName("Configuration serialization with combined configuration layers")
    class ConfigurationSerializationTests {
        @Test
        @DisplayName("Configuration serialization [combined layers with standard files] to JSON")
        void configurationSerializationWithMultipleConfigurationLayersTestJSON()
            throws Exception {
            File tempDir = Files.createTempDirectory(String.valueOf(this.hashCode())).toFile();
            tempDir.deleteOnExit();
            tempDir.deleteOnExit();
            Configuration.setDefaultDirectory(tempDir);

            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();

            lowPriorityConfigurationLayerMock.setBump("alpha");
            mediumPriorityConfigurationLayerMock.setBump("beta");
            highPriorityConfigurationLayerMock.setBump("gamma");

            lowPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG1.md", Map.<String,String>of("SectionA1", "regexA1", "SectionA2", "regexA2"), "changelog1.tpl", Map.<String,String>of("Expression1", "string1"))
            );
            mediumPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG2.md", Map.<String,String>of("SectionB1", "regexB1", "SectionB2", "regexB2"), "changelog2.tpl", Map.<String,String>of("Expression2", "string2"))
            );
            highPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG3.md", Map.<String,String>of("SectionC1", "regexC1", "SectionC2", "regexC2"), "changelog3.tpl", Map.<String,String>of("Expression3", "string3"))
            );

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

            lowPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            lowPriorityConfigurationLayerMock.setDirectory("some/directory");
            mediumPriorityConfigurationLayerMock.setDirectory("some/other/directory");
            highPriorityConfigurationLayerMock.setDirectory("the/right/directory");

            lowPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setDryRun(Boolean.FALSE);

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

            lowPriorityConfigurationLayerMock.setInitialVersion("9.9.9");
            mediumPriorityConfigurationLayerMock.setInitialVersion("8.8.8");
            highPriorityConfigurationLayerMock.setInitialVersion("7.7.7");

            lowPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            mediumPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            highPriorityConfigurationLayerMock.setPreset(Extended.NAME);

            lowPriorityConfigurationLayerMock.setReleasePrefix("lpprefix");
            mediumPriorityConfigurationLayerMock.setReleasePrefix("mpprefix");
            highPriorityConfigurationLayerMock.setReleasePrefix("hpprefix");

            lowPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            mediumPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            highPriorityConfigurationLayerMock.setReleaseLenient(Boolean.TRUE);

            lowPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(List.<String>of("asset1", "asset2"), false, "{{branch1}}", "Release description 1", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            mediumPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type2"),
                    List.<String>of("service2"),
                    List.<String>of("remote2"),
                    Map.<String,ReleaseType>of("type2", new ReleaseType(List.<String>of("asset1", "asset2"), true, "{{branch2}}", "Release description 2", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            highPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type3"),
                    List.<String>of("service3"),
                    List.<String>of("remote3"),
                    Map.<String,ReleaseType>of("type3", new ReleaseType(List.<String>of("asset1", "asset2"), true, "{{branch3}}", "Release description 3", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );

            lowPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setResume(Boolean.FALSE);

            lowPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            mediumPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            highPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);

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
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );
            highPriorityConfigurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo3",
                        "REPOSITORY_OWNER", "owner3"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo4",
                        "REPOSITORY_OWNER", "owner4"
                    ))
                )
            );

            lowPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            lowPriorityConfigurationLayerMock.setStateFile("file.yaml");
            mediumPriorityConfigurationLayerMock.setStateFile("file.yaml");
            highPriorityConfigurationLayerMock.setStateFile("file.json");

            lowPriorityConfigurationLayerMock.setVerbosity(Verbosity.TRACE);
            mediumPriorityConfigurationLayerMock.setVerbosity(Verbosity.INFO);
            highPriorityConfigurationLayerMock.setVerbosity(Verbosity.DEBUG);

            lowPriorityConfigurationLayerMock.setVersion("11.12.13");
            mediumPriorityConfigurationLayerMock.setVersion("21.22.23");
            highPriorityConfigurationLayerMock.setVersion("31.32.33");
            
            // inject the layers and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            
            // serialize the overall configuration to the standard location
            File serializedonfigurationFile = new File(tempDir, ".nyx-configuration.json");
            serializedonfigurationFile.deleteOnExit();
            FileMapper.save(serializedonfigurationFile.getAbsolutePath(), configuration);
            
            // deserialize the whole configuration to a simple layer to check values were resolved and serialized correctly
            SimpleConfigurationLayer deserializedConfigurationLayer = FileMapper.load(new File(serializedonfigurationFile.getAbsolutePath()), SimpleConfigurationLayer.class);

            // now check for all values
            assertEquals(highPriorityConfigurationLayerMock.getBump(), deserializedConfigurationLayer.getBump());
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getPath(), deserializedConfigurationLayer.getChangelog().getPath());
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSections().get("SectionC1"), deserializedConfigurationLayer.getChangelog().getSections().get("SectionC1"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSections().get("SectionC2"), deserializedConfigurationLayer.getChangelog().getSections().get("SectionC2"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSubstitutions().get("Expression3"), deserializedConfigurationLayer.getChangelog().getSubstitutions().get("Expression3"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getTemplate(), deserializedConfigurationLayer.getChangelog().getTemplate());
            assertEquals(highPriorityConfigurationLayerMock.getCommitMessageConventions().getItems().get("convention3").getExpression(), deserializedConfigurationLayer.getCommitMessageConventions().getItems().get("convention3").getExpression());
            assertEquals(highPriorityConfigurationLayerMock.getVersion(), deserializedConfigurationLayer.getVersion());
            assertEquals(highPriorityConfigurationLayerMock.getConfigurationFile(), deserializedConfigurationLayer.getConfigurationFile());
            assertEquals(highPriorityConfigurationLayerMock.getDirectory(), deserializedConfigurationLayer.getDirectory());
            assertEquals(highPriorityConfigurationLayerMock.getDryRun(), deserializedConfigurationLayer.getDryRun());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getPassword());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getPassword());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getUser());
            assertEquals(lowPriorityConfigurationLayerMock.getGit().getRemotes().get("replica").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("replica").getPassword());
            assertEquals(lowPriorityConfigurationLayerMock.getGit().getRemotes().get("replica").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("replica").getUser());
            assertEquals(mediumPriorityConfigurationLayerMock.getGit().getRemotes().get("clone").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("clone").getPassword());
            assertEquals(mediumPriorityConfigurationLayerMock.getGit().getRemotes().get("clone").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("clone").getUser());
            assertEquals(highPriorityConfigurationLayerMock.getInitialVersion(), deserializedConfigurationLayer.getInitialVersion());
            assertEquals(highPriorityConfigurationLayerMock.getPreset(), deserializedConfigurationLayer.getPreset());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseAssets(), deserializedConfigurationLayer.getReleaseAssets());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseLenient(), deserializedConfigurationLayer.getReleaseLenient());
            assertEquals(highPriorityConfigurationLayerMock.getReleasePrefix(), deserializedConfigurationLayer.getReleasePrefix());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getEnabled().contains("type3"), deserializedConfigurationLayer.getReleaseTypes().getEnabled().contains("type3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getPublicationServices().contains("service3"), deserializedConfigurationLayer.getReleaseTypes().getPublicationServices().contains("service3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getRemoteRepositories().contains("remote3"), deserializedConfigurationLayer.getReleaseTypes().getRemoteRepositories().contains("remote3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getCollapseVersions(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getCollapseVersions());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getCollapsedVersionQualifier(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getCollapsedVersionQualifier());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getDescription(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getDescription());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getFilterTags(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getFilterTags());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitCommit(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitCommit());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitCommitMessage(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitCommitMessage());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitPush(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitPush());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitTag(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitTag());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitTagMessage(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitTagMessage());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getMatchBranches(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getMatchBranches());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getPublish(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getPublish());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getVersionRange(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getVersionRange());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getVersionRangeFromBranchName(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getVersionRangeFromBranchName());
            assertEquals(highPriorityConfigurationLayerMock.getResume(), deserializedConfigurationLayer.getResume());
            assertEquals(highPriorityConfigurationLayerMock.getScheme(), deserializedConfigurationLayer.getScheme());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getType(), deserializedConfigurationLayer.getServices().get("github").getType());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("REPOSITORY_NAME"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("REPOSITORY_OWNER"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getType(), deserializedConfigurationLayer.getServices().get("gitlab").getType());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(highPriorityConfigurationLayerMock.getSharedConfigurationFile(), deserializedConfigurationLayer.getSharedConfigurationFile());
            assertEquals(highPriorityConfigurationLayerMock.getStateFile(), deserializedConfigurationLayer.getStateFile());
            assertEquals(highPriorityConfigurationLayerMock.getVerbosity(), deserializedConfigurationLayer.getVerbosity());
        }

        @Test
        @DisplayName("Configuration serialization [combined layers with standard files] to YAML")
        void configurationSerializationWithMultipleConfigurationLayersTestYAML()
            throws Exception {
            File tempDir = Files.createTempDirectory(String.valueOf(this.hashCode())).toFile();
            tempDir.deleteOnExit();
            Configuration.setDefaultDirectory(tempDir);

            SimpleConfigurationLayer lowPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer mediumPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            SimpleConfigurationLayer highPriorityConfigurationLayerMock = new SimpleConfigurationLayer();
            Configuration configuration = new Configuration();

            lowPriorityConfigurationLayerMock.setBump("alpha");
            mediumPriorityConfigurationLayerMock.setBump("beta");
            highPriorityConfigurationLayerMock.setBump("gamma");

            lowPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG1.md", Map.<String,String>of("SectionA1", "regexA1", "SectionA2", "regexA2"), "changelog1.tpl", Map.<String,String>of("Expression1", "string1"))
            );
            mediumPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG2.md", Map.<String,String>of("SectionB1", "regexB1", "SectionB2", "regexB2"), "changelog2.tpl", Map.<String,String>of("Expression2", "string2"))
            );
            highPriorityConfigurationLayerMock.setChangelog(
                new ChangelogConfiguration("CHANGELOG3.md", Map.<String,String>of("SectionC1", "regexC1", "SectionC2", "regexC2"), "changelog3.tpl", Map.<String,String>of("Expression3", "string3"))
            );

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

            lowPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            lowPriorityConfigurationLayerMock.setDirectory("some/directory");
            mediumPriorityConfigurationLayerMock.setDirectory("some/other/directory");
            highPriorityConfigurationLayerMock.setDirectory("the/right/directory");

            lowPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setDryRun(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setDryRun(Boolean.FALSE);

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

            lowPriorityConfigurationLayerMock.setInitialVersion("9.9.9");
            mediumPriorityConfigurationLayerMock.setInitialVersion("8.8.8");
            highPriorityConfigurationLayerMock.setInitialVersion("7.7.7");

            lowPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            mediumPriorityConfigurationLayerMock.setPreset(Simple.NAME);
            highPriorityConfigurationLayerMock.setPreset(Extended.NAME);

            lowPriorityConfigurationLayerMock.setReleasePrefix("lpprefix");
            mediumPriorityConfigurationLayerMock.setReleasePrefix("mpprefix");
            highPriorityConfigurationLayerMock.setReleasePrefix("hpprefix");

            lowPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            mediumPriorityConfigurationLayerMock.setReleaseLenient(Boolean.FALSE);
            highPriorityConfigurationLayerMock.setReleaseLenient(Boolean.TRUE);

            lowPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type1"),
                    List.<String>of("service1"),
                    List.<String>of("remote1"),
                    Map.<String,ReleaseType>of("type1", new ReleaseType(List.<String>of("asset1", "asset2"), false, "{{branch1}}", "Release description 1", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            mediumPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type2"),
                    List.<String>of("service2"),
                    List.<String>of("remote2"),
                    Map.<String,ReleaseType>of("type2", new ReleaseType(List.<String>of("asset1", "asset2"), true, "{{branch2}}", "Release description 2", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );
            highPriorityConfigurationLayerMock.setReleaseTypes(
                new ReleaseTypes(
                    List.<String>of("type3"),
                    List.<String>of("service3"),
                    List.<String>of("remote3"),
                    Map.<String,ReleaseType>of("type3", new ReleaseType(List.<String>of("asset1", "asset2"), true, "{{branch3}}", "Release description 3", "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{version}}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{version}}", List.<Identifier>of(new Identifier("build", "12", Identifier.Position.BUILD)), "", Map.<String,String>of("PATH",".*"), null, Boolean.TRUE.toString(), "", Boolean.FALSE))
                )
            );

            lowPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            mediumPriorityConfigurationLayerMock.setResume(Boolean.TRUE);
            highPriorityConfigurationLayerMock.setResume(Boolean.FALSE);

            lowPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            mediumPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);
            highPriorityConfigurationLayerMock.setScheme(Scheme.SEMVER);

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
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo1",
                        "REPOSITORY_OWNER", "owner1"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo2",
                        "REPOSITORY_OWNER", "owner2"
                    ))
                )
            );
            highPriorityConfigurationLayerMock.setServices(
                Map.<String,ServiceConfiguration>of(
                    "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo3",
                        "REPOSITORY_OWNER", "owner3"
                        )),
                    "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                        "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
                        "REPOSITORY_NAME", "repo4",
                        "REPOSITORY_OWNER", "owner4"
                    ))
                )
            );

            lowPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            mediumPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));
            highPriorityConfigurationLayerMock.setSharedConfigurationFile(System.getProperty(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_SYSTEM_PROPERTY));

            lowPriorityConfigurationLayerMock.setStateFile("file.json");
            mediumPriorityConfigurationLayerMock.setStateFile("file.json");
            highPriorityConfigurationLayerMock.setStateFile("file.yaml");

            lowPriorityConfigurationLayerMock.setVerbosity(Verbosity.TRACE);
            mediumPriorityConfigurationLayerMock.setVerbosity(Verbosity.INFO);
            highPriorityConfigurationLayerMock.setVerbosity(Verbosity.DEBUG);

            lowPriorityConfigurationLayerMock.setVersion("11.12.13");
            mediumPriorityConfigurationLayerMock.setVersion("21.22.23");
            highPriorityConfigurationLayerMock.setVersion("31.32.33");
            
            // inject the layers and test the new value is returned from that
            configuration.withPluginConfiguration(lowPriorityConfigurationLayerMock);
            configuration.withCommandLineConfiguration(mediumPriorityConfigurationLayerMock);
            configuration.withRuntimeConfiguration(highPriorityConfigurationLayerMock);
            
            // serialize the overall configuration to the standard location
            File serializedonfigurationFile = new File(tempDir, ".nyx-configuration.yaml");
            serializedonfigurationFile.deleteOnExit();
            FileMapper.save(serializedonfigurationFile.getAbsolutePath(), configuration);
            
            // deserialize the whole configuration to a simple layer to check values were resolved and serialized correctly
            SimpleConfigurationLayer deserializedConfigurationLayer = FileMapper.load(new File(serializedonfigurationFile.getAbsolutePath()), SimpleConfigurationLayer.class);

            // now check for all values
            assertEquals(highPriorityConfigurationLayerMock.getBump(), deserializedConfigurationLayer.getBump());
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getPath(), deserializedConfigurationLayer.getChangelog().getPath());
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSections().get("SectionC1"), deserializedConfigurationLayer.getChangelog().getSections().get("SectionC1"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSections().get("SectionC2"), deserializedConfigurationLayer.getChangelog().getSections().get("SectionC2"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getSubstitutions().get("Expression3"), deserializedConfigurationLayer.getChangelog().getSubstitutions().get("Expression3"));
            assertEquals(highPriorityConfigurationLayerMock.getChangelog().getTemplate(), deserializedConfigurationLayer.getChangelog().getTemplate());
            assertEquals(highPriorityConfigurationLayerMock.getCommitMessageConventions().getItems().get("convention3").getExpression(), deserializedConfigurationLayer.getCommitMessageConventions().getItems().get("convention3").getExpression());
            assertEquals(highPriorityConfigurationLayerMock.getVersion(), deserializedConfigurationLayer.getVersion());
            assertEquals(highPriorityConfigurationLayerMock.getConfigurationFile(), deserializedConfigurationLayer.getConfigurationFile());
            assertEquals(highPriorityConfigurationLayerMock.getDirectory(), deserializedConfigurationLayer.getDirectory());
            assertEquals(highPriorityConfigurationLayerMock.getDryRun(), deserializedConfigurationLayer.getDryRun());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getPassword());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getPassword());
            assertEquals(highPriorityConfigurationLayerMock.getGit().getRemotes().get("origin").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("origin").getUser());
            assertEquals(lowPriorityConfigurationLayerMock.getGit().getRemotes().get("replica").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("replica").getPassword());
            assertEquals(lowPriorityConfigurationLayerMock.getGit().getRemotes().get("replica").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("replica").getUser());
            assertEquals(mediumPriorityConfigurationLayerMock.getGit().getRemotes().get("clone").getPassword(), deserializedConfigurationLayer.getGit().getRemotes().get("clone").getPassword());
            assertEquals(mediumPriorityConfigurationLayerMock.getGit().getRemotes().get("clone").getUser(), deserializedConfigurationLayer.getGit().getRemotes().get("clone").getUser());
            assertEquals(highPriorityConfigurationLayerMock.getInitialVersion(), deserializedConfigurationLayer.getInitialVersion());
            assertEquals(highPriorityConfigurationLayerMock.getPreset(), deserializedConfigurationLayer.getPreset());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseAssets(), deserializedConfigurationLayer.getReleaseAssets());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseLenient(), deserializedConfigurationLayer.getReleaseLenient());
            assertEquals(highPriorityConfigurationLayerMock.getReleasePrefix(), deserializedConfigurationLayer.getReleasePrefix());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getEnabled().contains("type3"), deserializedConfigurationLayer.getReleaseTypes().getEnabled().contains("type3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getPublicationServices().contains("service3"), deserializedConfigurationLayer.getReleaseTypes().getPublicationServices().contains("service3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getRemoteRepositories().contains("remote3"), deserializedConfigurationLayer.getReleaseTypes().getRemoteRepositories().contains("remote3"));
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getCollapseVersions(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getCollapseVersions());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getCollapsedVersionQualifier(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getCollapsedVersionQualifier());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getDescription(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getDescription());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getFilterTags(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getFilterTags());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitCommit(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitCommit());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitCommitMessage(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitCommitMessage());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitPush(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitPush());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitTag(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitTag());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getGitTagMessage(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getGitTagMessage());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getMatchBranches(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getMatchBranches());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getPublish(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getPublish());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getVersionRange(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getVersionRange());
            assertEquals(highPriorityConfigurationLayerMock.getReleaseTypes().getItems().get("type3").getVersionRangeFromBranchName(), deserializedConfigurationLayer.getReleaseTypes().getItems().get("type3").getVersionRangeFromBranchName());
            assertEquals(highPriorityConfigurationLayerMock.getResume(), deserializedConfigurationLayer.getResume());
            assertEquals(highPriorityConfigurationLayerMock.getScheme(), deserializedConfigurationLayer.getScheme());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getType(), deserializedConfigurationLayer.getServices().get("github").getType());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("REPOSITORY_NAME"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("github").getOptions().get("REPOSITORY_OWNER"), deserializedConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getType(), deserializedConfigurationLayer.getServices().get("gitlab").getType());
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
            assertEquals(highPriorityConfigurationLayerMock.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"), deserializedConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));
            assertEquals(highPriorityConfigurationLayerMock.getSharedConfigurationFile(), deserializedConfigurationLayer.getSharedConfigurationFile());
            assertEquals(highPriorityConfigurationLayerMock.getStateFile(), deserializedConfigurationLayer.getStateFile());
            assertEquals(highPriorityConfigurationLayerMock.getVerbosity(), deserializedConfigurationLayer.getVerbosity());
        }
    }
}