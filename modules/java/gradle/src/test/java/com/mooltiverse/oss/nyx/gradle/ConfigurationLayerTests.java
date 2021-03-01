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

import com.mooltiverse.oss.nyx.data.IllegalPropertyException;

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
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

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
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

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
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(configurationLayer.getDryRun());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefix() default")
        void getReleasePrefixDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(configurationLayer.getReleasePrefix());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefixLenient() default")
        void getReleasePrefixLenientDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleasePrefixLenient().isPresent());
            assertNull(configurationLayer.getReleasePrefixLenient());
        }

        @Test
        @DisplayName("ConfigurationLayer.getScheme() default")
        void getSchemeDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

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
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getVerbosity().isPresent());
            if (Objects.isNull(project.getLogging().getLevel()))
                assertNull(configurationLayer.getVerbosity());
            else assertEquals(project.getLogging().getLevel(), configurationLayer.getVerbosity().getLevel());
        }

        @Test
        @DisplayName("ConfigurationLayer.getVersion() default")
        void getVersionDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            // the extension has no 'version' property as it must be taken from the standard project property with the same name
            // when there is no 'version' property specified for the project Gradle returns the 'unspecified' instead of null

            // set the property with an illegal value
            // the 'version' property is a project standard property, not defined in the extension
            // and is passed directly to the ConfigurationLayer constructor
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, "unspecified");
            assertNull(configurationLayer.getVersion());

            // but also test with null, for safety
            configurationLayer = new ConfigurationLayer(extension, null);
            assertNull(configurationLayer.getVersion());
        }
    }

    /**
     * Performs checks on the extension values and verifies that errors are raised when wrong values are provided
     */
    @Nested
    @DisplayName("ConfigurationLayer wrong values")
    class WrongValueTests {
        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getBump() with wrong values throws ConfigurationException")
        void getBumpWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getDirectory() with wrong values throws ConfigurationException")
        void getDirectoryWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getDryRun() with wrong values throws ConfigurationException")
        void getDryRunWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefix() with wrong values throws ConfigurationException")
        void getReleasePrefixWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefixLenient() with wrong values throws ConfigurationException")
        void getReleasePrefixLenientWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        @Test
        @DisplayName("ConfigurationLayer.getScheme() with wrong values throws IllegalPropertyException")
        void getSchemeWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // set the property with an illegal value
            extension.getScheme().set("illegalscheme");
            assertThrows(IllegalPropertyException.class, () -> { configurationLayer.getScheme(); });
        }

        @Test
        @DisplayName("ConfigurationLayer.getVerbosity() with wrong values throws IllegalPropertyException")
        void getVerbosityWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // set the property with an illegal value
            extension.getVerbosity().set("illegalverbosity");
            assertThrows(IllegalPropertyException.class, () -> { configurationLayer.getVerbosity(); });
        }

        @Test
        @DisplayName("ConfigurationLayer.getVersion() with wrong values throws IllegalPropertyException")
        void getVersionWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            // set the property with an illegal value
            // the 'version' property is a project standard property, not defined in the extension
            // and is passed directly to the ConfigurationLayer constructor
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, "notaversion");
            assertThrows(IllegalPropertyException.class, () -> { configurationLayer.getVersion(); });
        }
    }
}
