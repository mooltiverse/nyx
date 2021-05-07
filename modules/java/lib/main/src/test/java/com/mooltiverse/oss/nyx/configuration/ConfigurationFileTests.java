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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.data.FileMapper;
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
        FileMapper.save(savedFile.getAbsolutePath(), new ExtendedConfigurationExample());

        // now load the file into another object and then compare their fields
        SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

        assertEquals(source.getBump(), target.getBump());
        assertEquals(source.getConfigurationFile(), target.getConfigurationFile());
        assertEquals(source.getDirectory(), target.getDirectory());
        assertEquals(source.getDryRun(), target.getDryRun());
        assertEquals(source.getInitialVersion(), target.getInitialVersion());
        assertEquals(source.getPreset(), target.getPreset());
        assertEquals(source.getReleaseLenient(), target.getReleaseLenient());
        assertEquals(source.getReleasePrefix(), target.getReleasePrefix());
        assertEquals(source.getResume(), target.getResume());
        assertEquals(source.getScheme(), target.getScheme());
        assertEquals(source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
        assertEquals(source.getStateFile(), target.getStateFile());
        assertEquals(source.getVerbosity(), target.getVerbosity());
        assertEquals(source.getVersion(), target.getVersion());

        assertEquals(source.getCommitMessageConventions().getEnabled(), target.getCommitMessageConventions().getEnabled());
        assertEquals(source.getCommitMessageConventions().getItems().keySet(), target.getCommitMessageConventions().getItems().keySet());
        for (String item: source.getCommitMessageConventions().getItems().keySet()) {
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getExpression(), target.getCommitMessageConventions().getItems().get(item).getExpression());
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getBumpExpressions(), target.getCommitMessageConventions().getItems().get(item).getBumpExpressions());
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
        FileMapper.save(savedFile.getAbsolutePath(), new ExtendedConfigurationExample());

        // now load the file into another object and then compare their fields
        SimpleConfigurationLayer target = FileMapper.load(savedFile, SimpleConfigurationLayer.class);

        assertEquals(source.getBump(), target.getBump());
        assertEquals(source.getConfigurationFile(), target.getConfigurationFile());
        assertEquals(source.getDirectory(), target.getDirectory());
        assertEquals(source.getDryRun(), target.getDryRun());
        assertEquals(source.getInitialVersion(), target.getInitialVersion());
        assertEquals(source.getPreset(), target.getPreset());
        assertEquals(source.getReleaseLenient(), target.getReleaseLenient());
        assertEquals(source.getReleasePrefix(), target.getReleasePrefix());
        assertEquals(source.getResume(), target.getResume());
        assertEquals(source.getScheme(), target.getScheme());
        assertEquals(source.getSharedConfigurationFile(), target.getSharedConfigurationFile());
        assertEquals(source.getStateFile(), target.getStateFile());
        assertEquals(source.getVerbosity(), target.getVerbosity());
        assertEquals(source.getVersion(), target.getVersion());

        assertEquals(source.getCommitMessageConventions().getEnabled(), target.getCommitMessageConventions().getEnabled());
        assertEquals(source.getCommitMessageConventions().getItems().keySet(), target.getCommitMessageConventions().getItems().keySet());
        for (String item: source.getCommitMessageConventions().getItems().keySet()) {
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getExpression(), target.getCommitMessageConventions().getItems().get(item).getExpression());
            assertEquals(source.getCommitMessageConventions().getItems().get(item).getBumpExpressions(), target.getCommitMessageConventions().getItems().get(item).getBumpExpressions());
        }
    }
}