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

import static org.junit.jupiter.api.Assumptions.*;

import org.gradle.api.Action;
import org.gradle.api.Project;
import org.gradle.api.Task;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.git.script.JGitScript;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("ArrangeTask")
public class ArrangeTaskTests extends CoreTaskTests {
    /**
     * Performs checks on the task business actions.
     */
    @Nested
    @DisplayName("ArrangeTask.Actions")
    static class ActionTests {
        @Test
        @DisplayName("ArrangeTask run without exceptions when running in a directory with no Git repository but with a 'directory' configuration option")
        void testActionsExecuteWithoutExceptionsInEmptyGitProjectDirectoryAndValidDirectoryConfigurationOption()
        throws Exception {
            // the test project is created in a new empty directory
            Project project = newTestProject(null, false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // a Git repository is created in a different temporary directory
            JGitScript gitScript = JGitScript.fromScratch(true);
    
            //make sure the Gradle working directory and the Git repository directory are not the same
            assumeFalse(project.getBuildDir().equals(gitScript.getWorkingDirectory()));
            assumeFalse(project.getBuildDir().getAbsolutePath().equals(gitScript.getWorkingDirectory().getAbsolutePath()));
    
            // the valid Git directory, different than the current working directory, is passed as the 'directory' configuration option through the extension
            project.getExtensions().getByType(NyxExtension.class).getDirectory().set(gitScript.getWorkingDirectory());
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(ArrangeTask.NAME);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);
            }
        }

        @Test
        @DisplayName("ArrangeTask run without exceptions when running in a directory with a valid Git repository")
        void testActionsExecuteWithoutExceptionsInValidGitProjectDirectoryAndNoDirectoryConfigurationOption()
            throws Exception {
            JGitScript gitScript = JGitScript.fromScratch(true);
            Project project = newTestProject(gitScript.getWorkingDirectory(), false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(ArrangeTask.NAME);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);
            }
        }
    }
}
