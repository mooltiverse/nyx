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
package com.mooltiverse.oss.nyx.configuration.presets;

import static org.junit.jupiter.api.Assertions.*;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.ConfigurationLayer;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.Provider;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Presets")
public class PresetsTests {
    @Test
    @DisplayName("Presets.byName(String)")
    void byNameTest()
        throws Exception {
        assertThrows(NullPointerException.class, () -> Presets.byName(null));
        assertThrows(IllegalPropertyException.class, () -> Presets.byName(""));
        assertThrows(IllegalPropertyException.class, () -> Presets.byName("notapreset"));

        assertNotNull(Presets.byName(Simple.NAME));
        assertNotNull(Presets.byName(Extended.NAME));
    }

    @Test
    @DisplayName("Load Simple Preset directly")
    void loadSimplePresetDirectlyTest()
        throws Exception {
        ConfigurationLayer configurationLayer = Presets.byName(Simple.NAME);

        assertNotNull(configurationLayer);

        assertEquals(0, configurationLayer.getChangelog().getSections().size());
        assertEquals(0, configurationLayer.getChangelog().getSubstitutions().size());

        assertEquals(1, configurationLayer.getCommitMessageConventions().getEnabled().size());
        assertEquals(1, configurationLayer.getCommitMessageConventions().getItems().size());

        assertEquals(2, configurationLayer.getReleaseTypes().getEnabled().size());
        assertEquals(2, configurationLayer.getReleaseTypes().getItems().size());

        assertEquals(0, configurationLayer.getServices().size());

        assertEquals(0, configurationLayer.getSubstitutions().getEnabled().size());
        assertEquals(0, configurationLayer.getSubstitutions().getItems().size());
    }

    @Test
    @DisplayName("Load Simple Preset from global configuration")
    void loadSimplePresetFromGlobalConfigurationTest()
        throws Exception {
        Configuration configuration = new Configuration();
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        simpleConfigurationLayer.setPreset(Simple.NAME);
        configuration.withPluginConfiguration(simpleConfigurationLayer);

        assertEquals(0, configuration.getChangelog().getSections().size());
        assertEquals(0, configuration.getChangelog().getSubstitutions().size());

        assertEquals(1, configuration.getCommitMessageConventions().getEnabled().size());
        assertEquals(1, configuration.getCommitMessageConventions().getItems().size());

        assertEquals(2, configuration.getReleaseTypes().getEnabled().size());
        assertEquals(2, configuration.getReleaseTypes().getItems().size());

        assertEquals(0, configuration.getServices().size());

        assertEquals(0, configuration.getSubstitutions().getEnabled().size());
        assertEquals(0, configuration.getSubstitutions().getItems().size());
    }

    @Test
    @DisplayName("Load Extended Preset directly")
    void loadExtendedPresetDirectlyTest()
        throws Exception {
        ConfigurationLayer configurationLayer = Presets.byName(Extended.NAME);

        assertNotNull(configurationLayer);

        assertEquals(4, configurationLayer.getChangelog().getSections().size());
        assertEquals(0, configurationLayer.getChangelog().getSubstitutions().size());

        assertEquals(2, configurationLayer.getCommitMessageConventions().getEnabled().size());
        assertEquals(3, configurationLayer.getCommitMessageConventions().getItems().size());

        assertEquals(9, configurationLayer.getReleaseTypes().getEnabled().size());
        assertEquals(9, configurationLayer.getReleaseTypes().getItems().size());

        assertEquals(2, configurationLayer.getServices().size());
        assertEquals(Provider.GITHUB, configurationLayer.getServices().get("github").getType());
        assertEquals(1, configurationLayer.getServices().get("github").getOptions().size());
        assertEquals("{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", configurationLayer.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals(Provider.GITLAB, configurationLayer.getServices().get("gitlab").getType());
        assertEquals(1, configurationLayer.getServices().get("gitlab").getOptions().size());
        assertEquals("{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", configurationLayer.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));

        assertEquals(0, configurationLayer.getSubstitutions().getEnabled().size());
        assertEquals(8, configurationLayer.getSubstitutions().getItems().size());
    }

    @Test
    @DisplayName("Load Extended Preset from global configuration")
    void loadExtendedPresetFromGlobalConfigurationTest()
        throws Exception {
        Configuration configuration = new Configuration();
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        simpleConfigurationLayer.setPreset(Extended.NAME);
        configuration.withPluginConfiguration(simpleConfigurationLayer);

        assertEquals(4, configuration.getChangelog().getSections().size());
        assertEquals(0, configuration.getChangelog().getSubstitutions().size());

        assertEquals(2, configuration.getCommitMessageConventions().getEnabled().size());
        assertEquals(2, configuration.getCommitMessageConventions().getItems().size());

        assertEquals(9, configuration.getReleaseTypes().getEnabled().size());
        assertEquals(9, configuration.getReleaseTypes().getItems().size());

        assertEquals(2, configuration.getServices().size());
        assertEquals(Provider.GITHUB, configuration.getServices().get("github").getType());
        assertEquals(1, configuration.getServices().get("github").getOptions().size());
        assertEquals("{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", configuration.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals(Provider.GITLAB, configuration.getServices().get("gitlab").getType());
        assertEquals(1, configuration.getServices().get("gitlab").getOptions().size());
        assertEquals("{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", configuration.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));

        assertEquals(0, configuration.getSubstitutions().getEnabled().size());
        assertEquals(0, configuration.getSubstitutions().getItems().size());
    }
}
