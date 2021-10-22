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
import java.util.Objects;
import java.util.Map.Entry;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.configuration.examples.ExtendedConfigurationExample;

/**
 * Tests the saving and loading of configuration layers as files.
 */
@DisplayName("ConfigurationFile")
public class ConfigurationFileTests {
    @Test
    @DisplayName("Configuration.getBump() == Defaults.BUMP")
    void getBumpTest()
        throws Exception {
        
    }

    @Test
    @DisplayName("Load YAML configuration")
    void loadYAML()
        throws Exception {
        // use an example configuration and store it as a file
        SimpleConfigurationLayer source = new ExtendedConfigurationExample(); // use the example with the most fields used
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".yaml");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), source);

        // now load the file into another object and then compare their fields
        SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

        // all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
        // so nulls are to be tested against defaults, non simple nulls
        assertEquals(Objects.isNull(source.getBump()) ? Defaults.BUMP : source.getBump(), target.getBump());
        assertEquals(Objects.isNull(source.getConfigurationFile()) ? Defaults.CONFIGURATION_FILE : source.getConfigurationFile(), target.getConfigurationFile());
        //assertEquals(Objects.isNull(source.getDirectory()) ? Defaults.DIRECTORY : source.getDirectory(), target.getDirectory()); // this depends on the current runtime
        assertEquals(Objects.isNull(source.getDryRun()) ? Defaults.DRY_RUN : source.getDryRun(), target.getDryRun());
        assertEquals(Objects.isNull(source.getInitialVersion()) ? Defaults.INITIAL_VERSION : source.getInitialVersion(), target.getInitialVersion());
        assertEquals(Objects.isNull(source.getPreset()) ? Defaults.PRESET : source.getPreset(), target.getPreset());
        assertEquals(Objects.isNull(source.getReleaseLenient()) ? Defaults.RELEASE_LENIENT : source.getReleaseLenient(), target.getReleaseLenient());
        assertEquals(Objects.isNull(source.getReleasePrefix()) ? Defaults.RELEASE_PREFIX : source.getReleasePrefix(), target.getReleasePrefix());
        assertEquals(Objects.isNull(source.getResume()) ? Defaults.RESUME : source.getResume(), target.getResume());
        assertEquals(Objects.isNull(source.getScheme()) ? Defaults.SCHEME : source.getScheme(), target.getScheme());
        assertEquals(Objects.isNull(source.getSharedConfigurationFile()) ? Defaults.SHARED_CONFIGURATION_FILE : source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
        assertEquals(Objects.isNull(source.getStateFile()) ? Defaults.STATE_FILE : source.getStateFile(), target.getStateFile());
        assertEquals(Objects.isNull(source.getVerbosity()) ? Defaults.VERBOSITY : source.getVerbosity(), target.getVerbosity());
        assertEquals(Objects.isNull(source.getVersion()) ? Defaults.VERSION : source.getVersion(), target.getVersion());

        assertEquals(Objects.isNull(source.getCommitMessageConventions().getEnabled()) ? Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled() : source.getCommitMessageConventions().getEnabled(), target.getCommitMessageConventions().getEnabled());
        assertEquals(source.getCommitMessageConventions().getItems().keySet(), target.getCommitMessageConventions().getItems().keySet());
        for (String item: source.getCommitMessageConventions().getItems().keySet()) {
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getExpression(), target.getCommitMessageConventions().getItems().get(item).getExpression());
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getBumpExpressions(), target.getCommitMessageConventions().getItems().get(item).getBumpExpressions());
        }
        
        assertEquals(Objects.isNull(source.getReleaseTypes().getEnabled()) ? Defaults.RELEASE_TYPES.getEnabled() : source.getReleaseTypes().getEnabled(), target.getReleaseTypes().getEnabled());
        assertEquals(source.getReleaseTypes().getItems().keySet(), target.getReleaseTypes().getItems().keySet());
        for (String item: source.getReleaseTypes().getItems().keySet()) {
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapseVersions()) ? Defaults.ReleaseType.COLLAPSE_VERSIONS : source.getReleaseTypes().getItems().get(item).getCollapseVersions(), target.getReleaseTypes().getItems().get(item).getCollapseVersions());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier()) ? Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER : source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier(), target.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommit()) ? Defaults.ReleaseType.GIT_COMMIT : source.getReleaseTypes().getItems().get(item).getGitCommit(), target.getReleaseTypes().getItems().get(item).getGitCommit());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommitMessage()) ? Defaults.ReleaseType.GIT_COMMIT_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitCommitMessage(), target.getReleaseTypes().getItems().get(item).getGitCommitMessage());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitPush()) ? Defaults.ReleaseType.GIT_PUSH : source.getReleaseTypes().getItems().get(item).getGitPush(), target.getReleaseTypes().getItems().get(item).getGitPush());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTag()) ? Defaults.ReleaseType.GIT_TAG : source.getReleaseTypes().getItems().get(item).getGitTag(), target.getReleaseTypes().getItems().get(item).getGitTag());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTagMessage()) ? Defaults.ReleaseType.GIT_TAG_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitTagMessage(), target.getReleaseTypes().getItems().get(item).getGitTagMessage());

            if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers()))
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, target.getReleaseTypes().getItems().get(item).getIdentifiers());
            else {
                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().size());
                    assertTrue(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().containsAll(target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled()));
                }

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().size());
                    for (Entry<String,Identifier> entry: source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().entrySet()) {
                        assertTrue(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().containsKey(entry.getKey()));
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getQualifier() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier());
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getValue() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue());
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getPosition() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition());
                    }
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
    }

    @Test
    @DisplayName("Load JSON configuration")
    void loadJSON()
        throws Exception {
        // use an example configuration and store it as a file
        SimpleConfigurationLayer source = new ExtendedConfigurationExample(); // use the example with the most fields used
        File savedFile = new File(System.getProperty("java.io.tmpdir"), "extended"+this.hashCode()+".json");
        savedFile.deleteOnExit();
        FileMapper.save(savedFile.getAbsolutePath(), source);

        // now load the file into another object and then compare their fields
        SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

        // all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
        // so nulls are to be tested against defaults, non simple nulls
        assertEquals(Objects.isNull(source.getBump()) ? Defaults.BUMP : source.getBump(), target.getBump());
        assertEquals(Objects.isNull(source.getConfigurationFile()) ? Defaults.CONFIGURATION_FILE : source.getConfigurationFile(), target.getConfigurationFile());
        //assertEquals(Objects.isNull(source.getDirectory()) ? Defaults.DIRECTORY : source.getDirectory(), target.getDirectory()); // this depends on the current runtime
        assertEquals(Objects.isNull(source.getDryRun()) ? Defaults.DRY_RUN : source.getDryRun(), target.getDryRun());
        assertEquals(Objects.isNull(source.getInitialVersion()) ? Defaults.INITIAL_VERSION : source.getInitialVersion(), target.getInitialVersion());
        assertEquals(Objects.isNull(source.getPreset()) ? Defaults.PRESET : source.getPreset(), target.getPreset());
        assertEquals(Objects.isNull(source.getReleaseLenient()) ? Defaults.RELEASE_LENIENT : source.getReleaseLenient(), target.getReleaseLenient());
        assertEquals(Objects.isNull(source.getReleasePrefix()) ? Defaults.RELEASE_PREFIX : source.getReleasePrefix(), target.getReleasePrefix());
        assertEquals(Objects.isNull(source.getResume()) ? Defaults.RESUME : source.getResume(), target.getResume());
        assertEquals(Objects.isNull(source.getScheme()) ? Defaults.SCHEME : source.getScheme(), target.getScheme());
        assertEquals(Objects.isNull(source.getSharedConfigurationFile()) ? Defaults.SHARED_CONFIGURATION_FILE : source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
        assertEquals(Objects.isNull(source.getStateFile()) ? Defaults.STATE_FILE : source.getStateFile(), target.getStateFile());
        assertEquals(Objects.isNull(source.getVerbosity()) ? Defaults.VERBOSITY : source.getVerbosity(), target.getVerbosity());
        assertEquals(Objects.isNull(source.getVersion()) ? Defaults.VERSION : source.getVersion(), target.getVersion());

        assertEquals(Objects.isNull(source.getCommitMessageConventions().getEnabled()) ? Defaults.COMMIT_MESSAGE_CONVENTIONS.getEnabled() : source.getCommitMessageConventions().getEnabled(), target.getCommitMessageConventions().getEnabled());
        assertEquals(source.getCommitMessageConventions().getItems().keySet(), target.getCommitMessageConventions().getItems().keySet());
        for (String item: source.getCommitMessageConventions().getItems().keySet()) {
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getExpression(), target.getCommitMessageConventions().getItems().get(item).getExpression());
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getBumpExpressions(), target.getCommitMessageConventions().getItems().get(item).getBumpExpressions());
        }
        
        assertEquals(Objects.isNull(source.getReleaseTypes().getEnabled()) ? Defaults.RELEASE_TYPES.getEnabled() : source.getReleaseTypes().getEnabled(), target.getReleaseTypes().getEnabled());
        assertEquals(source.getReleaseTypes().getItems().keySet(), target.getReleaseTypes().getItems().keySet());
        for (String item: source.getReleaseTypes().getItems().keySet()) {
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapseVersions()) ? Defaults.ReleaseType.COLLAPSE_VERSIONS : source.getReleaseTypes().getItems().get(item).getCollapseVersions(), target.getReleaseTypes().getItems().get(item).getCollapseVersions());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier()) ? Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER : source.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier(), target.getReleaseTypes().getItems().get(item).getCollapsedVersionQualifier());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommit()) ? Defaults.ReleaseType.GIT_COMMIT : source.getReleaseTypes().getItems().get(item).getGitCommit(), target.getReleaseTypes().getItems().get(item).getGitCommit());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitCommitMessage()) ? Defaults.ReleaseType.GIT_COMMIT_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitCommitMessage(), target.getReleaseTypes().getItems().get(item).getGitCommitMessage());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitPush()) ? Defaults.ReleaseType.GIT_PUSH : source.getReleaseTypes().getItems().get(item).getGitPush(), target.getReleaseTypes().getItems().get(item).getGitPush());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTag()) ? Defaults.ReleaseType.GIT_TAG : source.getReleaseTypes().getItems().get(item).getGitTag(), target.getReleaseTypes().getItems().get(item).getGitTag());
            assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getGitTagMessage()) ? Defaults.ReleaseType.GIT_TAG_MESSAGE : source.getReleaseTypes().getItems().get(item).getGitTagMessage(), target.getReleaseTypes().getItems().get(item).getGitTagMessage());

            if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers()))
                assertEquals(Defaults.ReleaseType.IDENTIFIERS, target.getReleaseTypes().getItems().get(item).getIdentifiers());
            else {
                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getEnabled(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().size());
                    assertTrue(source.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled().containsAll(target.getReleaseTypes().getItems().get(item).getIdentifiers().getEnabled()));
                }

                if (Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems()))
                    assertEquals(Defaults.ReleaseType.IDENTIFIERS.getItems(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems());
                else {
                    assertEquals(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().size(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().size());
                    for (Entry<String,Identifier> entry: source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().entrySet()) {
                        assertTrue(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().containsKey(entry.getKey()));
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getQualifier() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getQualifier());
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getValue() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getValue());
                        assertEquals(Objects.isNull(source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition()) ? Defaults.ReleaseType.IDENTIFIERS.getItems().get(entry.getKey()).getPosition() : source.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition(), target.getReleaseTypes().getItems().get(item).getIdentifiers().getItems().get(entry.getKey()).getPosition());
                    }
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
    }
}