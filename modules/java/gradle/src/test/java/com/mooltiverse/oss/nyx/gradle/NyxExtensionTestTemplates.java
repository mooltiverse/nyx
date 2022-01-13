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
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.gradle.template.GradleCommandInvocationContextProvider;

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
        @DisplayName("NyxExtension.getChangelog() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getChangelogDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertNotNull(extension.getChangelog());
            assertFalse(extension.getChangelog().getCommitLink().isPresent());
            assertNull(extension.getChangelog().getCommitLink().getOrNull());
            assertFalse(extension.getChangelog().getContributorLink().isPresent());
            assertNull(extension.getChangelog().getContributorLink().getOrNull());
            assertFalse(extension.getChangelog().getIncludeUnreleased().isPresent());
            assertNull(extension.getChangelog().getIncludeUnreleased().getOrNull());
            assertFalse(extension.getChangelog().getIssueId().isPresent());
            assertNull(extension.getChangelog().getIssueId().getOrNull());
            assertFalse(extension.getChangelog().getIssueLink().isPresent());
            assertNull(extension.getChangelog().getIssueLink().getOrNull());
            assertFalse(extension.getChangelog().getPath().isPresent());
            assertNull(extension.getChangelog().getPath().getOrNull());
            //assertTrue(extension.getChangelog().geSections().isPresent());
            //assertNotNull(extension.getChangelog().geSections().get());
            assertTrue(extension.getChangelog().getSections().get().isEmpty());
            assertFalse(extension.getChangelog().getTemplate().isPresent());
            assertNull(extension.getChangelog().getTemplate().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getCommitMessageConventions() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getCommitMessageConventionsDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertNotNull(extension.getCommitMessageConventions());
            assertTrue(extension.getCommitMessageConventions().getEnabled().isPresent());
            assertNotNull(extension.getCommitMessageConventions().getEnabled().get());
            assertTrue(extension.getCommitMessageConventions().getEnabled().get().isEmpty());
            //assertTrue(extension.getCommitMessageConventions().getItems().isPresent());
            //assertNotNull(extension.getCommitMessageConventions().getItems().get());
            assertTrue(extension.getCommitMessageConventions().getItems().isEmpty());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getConfigurationFile() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getConfigurationFileDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getConfigurationFile().isPresent());
            assertNull(extension.getConfigurationFile().getOrNull());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getDirectory() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getDirectoryDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            // the default must be the project directory
            assertTrue(extension.getDirectory().isPresent());
            assertEquals(project.getProjectDir().getAbsolutePath(), extension.getDirectory().get().getAbsolutePath());
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
        @DisplayName("NyxExtension.getGit() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getGitDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertNotNull(extension.getGit());
            //assertTrue(extension.getGit().getRemotes().isPresent());
            //assertNotNull(extension.getGit().getRemotes().get());
            assertTrue(extension.getGit().getRemotes().isEmpty());
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
        @DisplayName("NyxExtension.getPreset() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getPresetDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getPreset().isPresent());
            assertNull(extension.getPreset().getOrNull());
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
        @DisplayName("NyxExtension.getReleaseTypes() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleaseTypesDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertNotNull(extension.getReleaseTypes());
            assertTrue(extension.getReleaseTypes().getEnabled().isPresent());
            assertNotNull(extension.getReleaseTypes().getEnabled().get());
            assertTrue(extension.getReleaseTypes().getEnabled().get().isEmpty());
            //assertTrue(extension.getReleaseTypes().getItems().isPresent());
            //assertNotNull(extension.getReleaseTypes().getItems().get());
            assertTrue(extension.getReleaseTypes().getItems().isEmpty());
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
        @DisplayName("NyxExtension.getServices() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getServicesDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertNotNull(extension.getServices());
            assertTrue(extension.getServices().isEmpty());
        }

        @TestTemplate
        @DisplayName("NyxExtension.getSharedConfigurationFile() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getSharedConfigurationFileDefaultTest(Project project)
            throws Exception {
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);

            assertFalse(extension.getSharedConfigurationFile().isPresent());
            assertNull(extension.getSharedConfigurationFile().getOrNull());
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
