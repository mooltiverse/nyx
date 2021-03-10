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

import static org.gradle.api.Project.DEFAULT_VERSION;

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
    @DisplayName("ConfigurationLayer default values")
    class DefaultsTests {
        @Test
        @DisplayName("ConfigurationLayer.getBump() default value")
        void getBumpDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getBump().isPresent());
            assertNull(configurationLayer.getBump());
        }

        @Test
        @DisplayName("ConfigurationLayer.getDirectory() default value")
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
        @DisplayName("ConfigurationLayer.getDryRun() default value")
        void getDryRunDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(configurationLayer.getDryRun());
        }

        @Test
        @DisplayName("ConfigurationLayer.getInitialVersion() default value")
        void getInitialVersionTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getInitialVersion().isPresent());
            assertNull(configurationLayer.getInitialVersion());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleaseLenient() default value")
        void getReleaseLenientDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleaseLenient().isPresent());
            assertNull(configurationLayer.getReleaseLenient());
        }

        @Test
        @DisplayName("ConfigurationLayer.getReleasePrefix() default value")
        void getReleasePrefixDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(configurationLayer.getReleasePrefix());
        }

        @Test
        @DisplayName("ConfigurationLayer.getScheme() default value")
        void getSchemeDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getScheme().isPresent());
            assertNull(configurationLayer.getScheme());
        }

        @Test
        @DisplayName("ConfigurationLayer.getVerbosity() default value")
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
        @DisplayName("ConfigurationLayer.getVersion() default value")
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
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, DEFAULT_VERSION);
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
        @DisplayName("ConfigurationLayer.getBump().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetBumpWithWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getDirectory().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetDirectoryWithWrongValueTest()
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
        @DisplayName("ConfigurationLayer.getDryRun().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetDryRunWithWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /*@Test
        @DisplayName("ConfigurationLayer.getInitialVersion().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetInitialVersionWithWrongValueTest()
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
        @DisplayName("ConfigurationLayer.getReleasePrefix().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetReleasePrefixWithWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @Test
        @DisplayName("ConfigurationLayer.getReleaseLenient().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetReleaseLenientWithWrongValueTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension and the configuration layer adapter
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        @Test
        @DisplayName("ConfigurationLayer.getScheme().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetSchemeWithWrongValueTest()
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
        @DisplayName("ConfigurationLayer.getVerbosity().set() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetVerbosityWithWrongValueTest()
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
        @DisplayName("ConfigurationLayer.getVersion() throws IllegalPropertyException with illegal value")
        void exceptionUsingGetVersionWithWrongValueTest()
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
