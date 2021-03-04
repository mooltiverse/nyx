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
@DisplayName("Extension")
public class NyxExtensionTests extends AbstractTests  {
    /**
     * Performs checks on the extension default values at the time it is created.
     */
    @Nested
    @DisplayName("NyxExtension defaults")
    class DefaultsTests {
        @Test
        @DisplayName("NyxExtension.getBump() default")
        void getBumpDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getBump().isPresent());
            assertNull(extension.getBump().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getDirectory() default")
        void getDirectoryDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            // the default must be the project directory
            assertTrue(extension.getDirectory().isPresent());
            assertEquals(project.getProjectDir(), extension.getDirectory().get().getAsFile());
        }

        @Test
        @DisplayName("NyxExtension.getDryRun() default")
        void getDryRunDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(extension.getDryRun().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getInitialVersion() default")
        void getInitialVersionDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getInitialVersion().isPresent());
            assertNull(extension.getInitialVersion().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getReleasePrefix() default")
        void getReleasePrefixDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(extension.getReleasePrefix().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getReleasePrefixLenient() default")
        void getReleasePrefixLenientDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getReleasePrefixLenient().isPresent());
            assertNull(extension.getReleasePrefixLenient().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getScheme() default")
        void getSchemeDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            NyxExtension extension = newTestProject(null, true).getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getScheme().isPresent());
            assertNull(extension.getScheme().getOrNull());
        }

        @Test
        @DisplayName("NyxExtension.getVerbosity() default")
        void getVerbosityDefaultTest()
            throws Exception {
            // apply the plugin to a new project and retrieve the extension
            Project project = newTestProject(null, true);
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getVerbosity().isPresent());
            if (Objects.isNull(project.getLogging().getLevel()))
                assertNull(extension.getVerbosity().getOrNull());
            else assertEquals(project.getLogging().getLevel(), extension.getVerbosity().get());
        }
    }
}
