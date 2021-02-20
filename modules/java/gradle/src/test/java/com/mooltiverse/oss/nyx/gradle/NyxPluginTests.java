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
import static org.junit.jupiter.api.Assumptions.*;

import java.util.Objects;

import org.gradle.api.Project;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Tests the Gradle plugin.<br>
 * 
 * This class focuses on unit tests, while {@link NyxPluginFunctionalTests} focuses on unit tests.
 */
@DisplayName("NyxPlugin")
public class NyxPluginTests extends AbstractTests {
    @Nested
    @DisplayName("NyxPlugin.apply")
    class ApplyTests {
        @Test
        @DisplayName("NyxPlugin.apply()")
        void applyTest()
            throws Exception {
            Project project = newTestProject(null, false);

            // Make sure the plugin isn't there before apply. Only safe methods are used
            // ... with PluginManager (Project.getPluginManager())...
            assumeFalse(project.getPluginManager().hasPlugin(NyxPlugin.ID));
            assumeTrue(Objects.isNull(project.getPluginManager().findPlugin(NyxPlugin.ID)));

            // ... with PluginContainer (Project.getPlugins())...
            assumeFalse(project.getPlugins().hasPlugin(NyxPlugin.ID));
            assumeFalse(project.getPlugins().hasPlugin(NyxPlugin.class));

            assumeTrue(Objects.isNull(project.getPlugins().findPlugin(NyxPlugin.ID)));
            assumeTrue(Objects.isNull(project.getPlugins().findPlugin(NyxPlugin.class)));

            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

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
}
