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
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.gradle.template.GradleCommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.git.Scenario;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("Extension")
public class NyxExtensionTestTemplates {
    /**
     * Performs checks on the extension default values at the time it is created.
     */
    @Nested
    @DisplayName("NyxExtension default values")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    class DefaultsTests {
        @TestTemplate
        @DisplayName("NyxExtension.getBump() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getBumpDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getBump().isPresent());
            assertNull(extension.getBump().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getDirectory() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getDirectoryDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            // the default must be the project directory
            assertTrue(extension.getDirectory().isPresent());
            assertEquals(project.getProjectDir(), extension.getDirectory().get().getAsFile());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getDryRun() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getDryRunDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(extension.getDryRun().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getInitialVersion() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getInitialVersionDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getInitialVersion().isPresent());
            assertNull(extension.getInitialVersion().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getReleaseLenient() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleaseLenientDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getReleaseLenient().isPresent());
            assertNull(extension.getReleaseLenient().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getReleasePrefix() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleasePrefixDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(extension.getReleasePrefix().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getScheme() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getSchemeDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getScheme().isPresent());
            assertNull(extension.getScheme().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getVerbosity() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getVerbosityDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getVerbosity().isPresent());
            if (Objects.isNull(project.getLogging().getLevel()))
                assertNull(extension.getVerbosity().getOrNull());
            else assertEquals(project.getLogging().getLevel(), extension.getVerbosity().get());
        }
    }
}