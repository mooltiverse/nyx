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

import org.gradle.api.Project;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.gradle.template.GradleCommandInvocationContextProvider;

/**
 * Tests the Gradle plugin.<br>
 * 
 * This class focuses on unit tests, while {@link NyxPluginFunctionalTests} focuses on unit tests.
 */
@DisplayName("NyxPlugin")
public class NyxPluginTestTemplates {
    @Nested
    @DisplayName("NyxPlugin.apply [project plugin]")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    class ApplyProjectPluginTests {
        @TestTemplate
        @DisplayName("NyxPlugin.apply()")
        @Baseline(Scenario.FROM_SCRATCH)
        void applyTest(Project project)
            throws Exception {
            // Now make sure the plugin is there.
            // ... with PluginManager (Project.getPluginManager())...
            assertTrue(project.getPluginManager().hasPlugin(NyxPlugin.ID));
            assertNotNull(project.getPluginManager().findPlugin(NyxPlugin.ID));
            assertEquals(NyxPlugin.ID, project.getPluginManager().findPlugin(NyxPlugin.ID).getId());

            // ... with PluginContainer (Project.getPlugins())...
            assertTrue(project.getPlugins().hasPlugin(NyxPlugin.ID));
            assertTrue(project.getPlugins().hasPlugin(NyxPlugin.class));

            assertNotNull(project.getPlugins().findPlugin(NyxPlugin.ID));
            assertNotNull(project.getPlugins().findPlugin(NyxPlugin.class));

            assertEquals(NyxPlugin.class, project.getPlugins().findPlugin(NyxPlugin.ID).getClass());
            assertEquals(NyxPlugin.class, project.getPlugins().findPlugin(NyxPlugin.class).getClass());

            assertNotNull(project.getPlugins().getPlugin(NyxPlugin.ID));
            assertNotNull(project.getPlugins().getPlugin(NyxPlugin.class));

            assertEquals(NyxPlugin.class, project.getPlugins().getPlugin(NyxPlugin.ID).getClass());
            assertEquals(NyxPlugin.class, project.getPlugins().getPlugin(NyxPlugin.class).getClass());

            assertNotNull(project.getPlugins().getAt(NyxPlugin.ID));
            assertNotNull(project.getPlugins().getAt(NyxPlugin.class));
            
            assertEquals(NyxPlugin.class, project.getPlugins().getAt(NyxPlugin.ID).getClass());
            assertEquals(NyxPlugin.class, project.getPlugins().getAt(NyxPlugin.class).getClass());
        }
    }

    @Nested
    @DisplayName("NyxPlugin.apply [settings plugin]")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    class ApplySettingsPluginTests {
        // Unfortunately Gradle doesn't give any means to instantiate a Settings object to use for unit tests
    }
}
