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
import org.junit.jupiter.api.extension.ExtendWith;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.gradle.template.GradleCommandInvocationContextProvider;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("ConfigurationLayer")
public class ConfigurationLayerTestTemplates {
    /**
     * Performs checks on the extension default values at the time it is created.
     */
    @Nested
    @DisplayName("ConfigurationLayer default values")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    class DefaultsTests {
        @TestTemplate
        @DisplayName("ConfigurationLayer.getBump() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getBumpDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getBump().isPresent());
            assertNull(configurationLayer.getBump());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getChangelog() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getChangelogDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getChangelog());
            assertNotNull(configurationLayer.getChangelog());
            assertFalse(extension.getChangelog().getAppend().isPresent());
            assertNull(extension.getChangelog().getAppend().getOrNull());
            assertFalse(extension.getChangelog().getPath().isPresent());
            assertNull(extension.getChangelog().getPath().getOrNull());
            //assertTrue(extension.getChangelog().geSections().isPresent());
            assertTrue(configurationLayer.getChangelog().getSections().isEmpty());
            //assertTrue(extension.getChangelog().getSubstitutions().isPresent());
            assertTrue(configurationLayer.getChangelog().getSubstitutions().isEmpty());
            assertFalse(extension.getChangelog().getTemplate().isPresent());
            assertNull(extension.getChangelog().getTemplate().getOrNull());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getCommitMessageConventions() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getCommitMessageConventionsDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getCommitMessageConventions());
            assertNotNull(configurationLayer.getCommitMessageConventions());
            assertTrue(extension.getCommitMessageConventions().getEnabled().isPresent());
            assertTrue(configurationLayer.getCommitMessageConventions().getEnabled().isEmpty());
            assertTrue(extension.getCommitMessageConventions().getEnabled().get().isEmpty());
            //assertTrue(extension.getCommitMessageConventions().getItems().isPresent());
            assertTrue(configurationLayer.getCommitMessageConventions().getItems().isEmpty());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getConfigurationFile() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getConfigurationFileDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getConfigurationFile().isPresent());
            assertNull(configurationLayer.getConfigurationFile());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getDirectory() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getDirectoryDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // the default must be the project directory
            assertTrue(extension.getDirectory().isPresent());
            assertEquals(project.getProjectDir().getAbsolutePath(), configurationLayer.getDirectory());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getDryRun() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getDryRunDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getDryRun().isPresent());
            assertNull(configurationLayer.getDryRun());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getGit() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getGitDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getGit());
            assertNotNull(configurationLayer.getGit());
            //assertTrue(extension.getGit().getRemotes().isPresent());
            assertTrue(configurationLayer.getGit().getRemotes().isEmpty());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getInitialVersion() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getInitialVersionTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getInitialVersion().isPresent());
            assertNull(configurationLayer.getInitialVersion());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getPreset() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getPresetDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getPreset().isPresent());
            assertNull(configurationLayer.getPreset());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseAssets() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleaseAssetsDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getReleaseAssets());
            assertNotNull(configurationLayer.getReleaseAssets());
            assertTrue(extension.getReleaseAssets().isEmpty());
            assertTrue(configurationLayer.getReleaseAssets().isEmpty());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseLenient() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleaseLenientDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleaseLenient().isPresent());
            assertNull(configurationLayer.getReleaseLenient());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleasePrefix() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleasePrefixDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getReleasePrefix().isPresent());
            assertNull(configurationLayer.getReleasePrefix());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseTypes() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getReleaseTypesDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getReleaseTypes());
            assertNotNull(configurationLayer.getReleaseTypes());
            assertTrue(extension.getReleaseTypes().getEnabled().isPresent());
            assertTrue(configurationLayer.getReleaseTypes().getEnabled().isEmpty());
            assertTrue(extension.getReleaseTypes().getEnabled().get().isEmpty());
            //assertTrue(extension.getReleaseTypes().getItems().isPresent());
            assertTrue(configurationLayer.getReleaseTypes().getItems().isEmpty());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getScheme() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getSchemeDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getScheme().isPresent());
            assertNull(configurationLayer.getScheme());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getServices() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getServicesDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertNotNull(extension.getServices());
            assertNotNull(configurationLayer.getServices());
            assertTrue(extension.getServices().isEmpty());
            assertTrue(configurationLayer.getServices().isEmpty());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getSharedConfigurationFile() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getSharedConfigurationFileDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getSharedConfigurationFile().isPresent());
            assertNull(configurationLayer.getSharedConfigurationFile());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getVerbosity() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getVerbosityDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            assertFalse(extension.getVerbosity().isPresent());
            if (Objects.isNull(project.getLogging().getLevel()))
                assertNull(configurationLayer.getVerbosity());
            else assertEquals(project.getLogging().getLevel(), configurationLayer.getVerbosity().getLevel());
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getVersion() default value")
        @Baseline(Scenario.FROM_SCRATCH)
        void getVersionDefaultTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
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
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    class WrongValueTests {
        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getBump().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetBumpWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getChangelog().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetChangelogWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getCommitMessageConventions().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetCommitMessageConventionsWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getDirectory().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetDirectoryWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getDryRun().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetDryRunWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getGit().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetGitWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /*@TestTemplate
        @DisplayName("ConfigurationLayer.getInitialVersion().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetInitialVersionWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseAssets().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetReleaseAssetsWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseLenient().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetReleaseLenientWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
            // since the property is already modelled as a boolean, Gradle provides the validation for this
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleasePrefix().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetReleasePrefixWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getReleaseTypes().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetReleaseTypesWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        /* This test is commented because it has nothing to test
        @TestTemplate
        @DisplayName("ConfigurationLayer.getServices().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetServicesWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // no idea of how to test wrong values here
        }*/

        @TestTemplate
        @DisplayName("ConfigurationLayer.getScheme().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetSchemeWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // set the property with an illegal value
            extension.getScheme().set("illegalscheme");
            assertThrows(IllegalPropertyException.class, () -> { configurationLayer.getScheme(); });
        }

        @TestTemplate
        @DisplayName("ConfigurationLayer.getVerbosity().set() throws IllegalPropertyException with illegal value")
        @Baseline(Scenario.FROM_SCRATCH)
        void exceptionUsingGetVerbosityWithWrongValueTest(Project project, Script script)
            throws Exception {
            script.getWorkingDirectory().deleteOnExit();
            NyxExtension extension = project.getExtensions().getByType(NyxExtension.class);
            ConfigurationLayer configurationLayer = new ConfigurationLayer(extension, null);

            // set the property with an illegal value
            extension.getVerbosity().set("illegalverbosity");
            assertThrows(IllegalPropertyException.class, () -> { configurationLayer.getVerbosity(); });
        }
    }
}
