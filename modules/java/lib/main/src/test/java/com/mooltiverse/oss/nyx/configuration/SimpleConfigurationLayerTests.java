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
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.EnabledItemsMap;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.version.Scheme;

@DisplayName("SimpleConfigurationLayer")
public class SimpleConfigurationLayerTests {
    @Test
    @DisplayName("SimpleConfigurationLayer.getBump()")
    void getBumpTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getBump());

        simpleConfigurationLayer.setBump("b");
        assertEquals("b", simpleConfigurationLayer.getBump());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getCommitMessageConventions()")
    void getCommitMessageConventionsTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getEnabled().isEmpty());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getItems().isEmpty());

        simpleConfigurationLayer.setCommitMessageConventions(
            new EnabledItemsMap<CommitMessageConvention>(
                List.<String>of("one", "two"),
                Map.<String,CommitMessageConvention>of("one", new CommitMessageConvention(), "two", new CommitMessageConvention())
            )
        );

        assertEquals(2, simpleConfigurationLayer.getCommitMessageConventions().getEnabled().size());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, simpleConfigurationLayer.getCommitMessageConventions().getItems().size());
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions().getItems().get("one"));
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions().getItems().get("two"));
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getConfigurationFile()")
    void getConfigurationFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getConfigurationFile());

        simpleConfigurationLayer.setConfigurationFile("config.yml");
        assertEquals("config.yml", simpleConfigurationLayer.getConfigurationFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getDirectory()")
    void getDirectoryTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getDirectory());

        File directory = Files.createTempDirectory(null).toFile();
        simpleConfigurationLayer.setDirectory(directory.getAbsolutePath());
        assertEquals(directory.getAbsolutePath(), new File(simpleConfigurationLayer.getDirectory()).getAbsolutePath());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getDryRun()")
    void getDryRunTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getDryRun());

        simpleConfigurationLayer.setDryRun(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getDryRun());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getInitialVersion()")
    void getInitialVersionTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getInitialVersion());

        simpleConfigurationLayer.setInitialVersion("0.3.5");
        assertEquals("0.3.5", simpleConfigurationLayer.getInitialVersion());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getPreset()")
    void getPresetTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getPreset());

        simpleConfigurationLayer.setPreset("simple");
        assertEquals("simple", simpleConfigurationLayer.getPreset());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleaseLenient()")
    void getReleaseLenientTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getReleaseLenient());

        simpleConfigurationLayer.setReleaseLenient(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getReleaseLenient());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleasePrefix()")
    void getReleasePrefixTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getReleasePrefix());

        simpleConfigurationLayer.setReleasePrefix("prefix");
        assertEquals("prefix", simpleConfigurationLayer.getReleasePrefix());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleaseTypes()")
    void getReleaseTypesTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getReleaseTypes());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getEnabled().isEmpty());
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getItems().isEmpty());

        simpleConfigurationLayer.setReleaseTypes(
            new EnabledItemsMap<ReleaseType>(
                List.<String>of("one", "two"),
                Map.<String,ReleaseType>of("one", new ReleaseType(), "two", new ReleaseType())
            )
        );

        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getEnabled().size());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getItems().size());
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("one"));
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("two"));
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getResume()")
    void getResumeTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getResume());

        simpleConfigurationLayer.setResume(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getResume());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getScheme()")
    void getSchemeTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getScheme());

        simpleConfigurationLayer.setScheme(Scheme.SEMVER);
        assertEquals(Scheme.SEMVER, simpleConfigurationLayer.getScheme());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getSharedConfigurationFile()")
    void getSharedConfigurationFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getSharedConfigurationFile());

        simpleConfigurationLayer.setSharedConfigurationFile("config.yml");
        assertEquals("config.yml", simpleConfigurationLayer.getSharedConfigurationFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getStateFile()")
    void getStateFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getStateFile());

        simpleConfigurationLayer.setStateFile("state.yml");
        assertEquals("state.yml", simpleConfigurationLayer.getStateFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getVerbosity()")
    void getVerbosityTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getVerbosity());

        simpleConfigurationLayer.setVerbosity(Verbosity.INFO);
        assertEquals(Verbosity.INFO, simpleConfigurationLayer.getVerbosity());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getVersion()")
    void getVersionTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getVersion());

        simpleConfigurationLayer.setVersion("3.5.7");
        assertEquals("3.5.7", simpleConfigurationLayer.getVersion());
    }
}