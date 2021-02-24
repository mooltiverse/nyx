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
package com.mooltiverse.oss.nyx.gradle;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Objects;

import org.gradle.api.Project;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("ConfigurationLayer")
public class ConfigurationLayerTests extends AbstractTests  {
    /**
     * Performs checks on the extension default values at the time it is created.
     */
    @Nested
    @DisplayName("ConfigurationLayer defaults")
    class DefaultsTests {
        @Test
        @DisplayName("ConfigurationLayer.getBump() default")
        void getBumpDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getBump().isPresent());
            assertNull(configurationLayer.getBump());
        }

        @Test
        @DisplayName("ConfigurationLayer.getDirectory() default")
        void getDirectoryDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            // the default must be the project directory
            assertTrue(extension.getDirectory().isPresent());
            assertEquals(project.getProjectDir().getAbsolutePath(), configurationLayer.getDirectory().getAbsolutePath());
        }

        @Test
        @DisplayName("ConfigurationLayer.getDryRun() default")
        void getDryRunDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(configurationLayer.getDryRun());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefix() default")
        void getReleasePrefixDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(configurationLayer.getReleasePrefix());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefixLenient() default")
        void getReleasePrefixLenientDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getReleasePrefixLenient().isPresent());
            assertNull(configurationLayer.getReleasePrefixLenient());
        }

        @Test
        @DisplayName("ConfigurationLayer.getScheme() default")
        void getSchemeDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getScheme().isPresent());
            assertNull(configurationLayer.getScheme());
        }

        @Test
        @DisplayName("ConfigurationLayer.getVerbosity() default")
        void getVerbosityDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension);

            assertFalse(extension.getVerbosity().isPresent());
            if (Objects.isNull(project.getLogging().getLevel()))
                assertNull(configurationLayer.getVerbosity());
            else assertEquals(project.getLogging().getLevel(), configurationLayer.getVerbosity().getLevel());
        }
    }
}
