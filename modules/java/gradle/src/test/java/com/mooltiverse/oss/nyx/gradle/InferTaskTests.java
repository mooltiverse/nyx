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

import static com.mooltiverse.oss.nyx.gradle.Constants.GRADLE_VERSION_PROPERTY_NAME;

import org.gradle.api.Action;
import org.gradle.api.Project;
import org.gradle.api.Task;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.git.script.GitScript;
import com.mooltiverse.oss.nyx.git.script.GitScenario;

/**
 * Tests the Gradle task.<br>
 */
@DisplayName("InferTask")
public class InferTaskTests extends CoreTaskTests {
    /**
     * Performs checks on the task business actions.
     */
    @Nested
    @DisplayName("InferTask.Actions")
    static class ActionTests {
        @Test
        @DisplayName("InferTask.getActions().execute() throws exception with a valid but empty Git repository in working directory")
        void exceptionOnExecuteWithValidButEmptyGitRepositoryInWorkingDirectoryTest()
            throws Exception {
            Project project = newTestProject(GitScript.fromScratch().getWorkingDirectory(), false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(InferTask.NAME);
    
            for (Action<? super Task> action: task.getActions()) {
                // the task throws an exception as it has no way to infer the version
                assertThrows(Exception.class, () -> action.execute(task));
            }
        }

        @Test
        @DisplayName("InferTask.getActions().execute() with a valid but empty Git repository in custom directory ==> default initial version")
        void runWithEmptyRepositoryTest()
        throws Exception {
            // the test project is created in a new empty directory
            Project project = newTestProject(null, false);
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);
    
            // a Git repository is created in a different temporary directory
            GitScript script = GitScenario.InitialCommit.realize();
    
            //make sure the Gradle working directory and the Git repository directory are not the same
            assumeFalse(project.getBuildDir().equals(script.getWorkingDirectory()));
            assumeFalse(project.getBuildDir().getAbsolutePath().equals(script.getWorkingDirectory().getAbsolutePath()));
    
            // the valid Git directory, different than the current working directory, is passed as the 'directory' configuration option through the extension
            project.getExtensions().getByType(NyxExtension.class).getDirectory().set(script.getWorkingDirectory());
    
            // Retrieve the dependent task
            Task task = project.getTasks().getByName(InferTask.NAME);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);
                
                // test the State attributes after execution
                Nyx nyx = getNyxForTask(task);
                assertNull(nyx.state().getBump());
                assertEquals(nyx.state().getConfiguration().getScheme(), nyx.state().getScheme());
                assertEquals(nyx.state().getConfiguration().getReleasePrefix().concat(nyx.state().getConfiguration().getInitialVersion().toString()), nyx.state().getVersion());
                assertEquals(nyx.state().getConfiguration().getInitialVersion().toString(), nyx.state().getVersionInternal().toString());
                assertEquals(script.getCommits().get(0), nyx.state().getReleaseScope().getInitialCommit());
                assertNull(nyx.state().getReleaseScope().getFinalCommit());
                assertNull(nyx.state().getReleaseScope().getPreviousVersion());
                assertNull(nyx.state().getReleaseScope().getPreviousVersionCommit());
                assertEquals(Boolean.FALSE, nyx.state().getReleaseScope().getSignificant());
            }
        }

        @Test
        @DisplayName("InferTask.getActions().execute() with version override ==> custom version")
        void runWithVersionOverriddenByUserTest()
            throws Exception {
            // a Git repository is created in a different temporary directory
            GitScript script = GitScenario.InitialCommit.realize();
            Project project = newTestProject(script.getWorkingDirectory(), false);

            // set the project property
            project.setProperty(GRADLE_VERSION_PROPERTY_NAME, "1.2.3");
    
            // apply the plugin
            project.getPluginManager().apply(NyxPlugin.ID);

            // Retrieve the dependent task
            Task task = project.getTasks().getByName(InferTask.NAME);
    
            for (Action<? super Task> action: task.getActions()) {
                action.execute(task);

                // test the State attributes after execution
                // many values are not initialized because inference was skipped due to the version override
                Nyx nyx = getNyxForTask(task);
                assertNull(nyx.state().getBump());
                assertEquals(nyx.state().getConfiguration().getScheme(), nyx.state().getScheme());
                assertEquals("1.2.3", nyx.state().getConfiguration().getVersion().toString());
                assertEquals(nyx.state().getConfiguration().getReleasePrefix().concat(nyx.state().getConfiguration().getVersion().toString()), nyx.state().getVersion());
                assertEquals(nyx.state().getConfiguration().getVersion().toString(), nyx.state().getVersionInternal().toString());
                assertNull(nyx.state().getReleaseScope().getInitialCommit());
                assertNull(nyx.state().getReleaseScope().getFinalCommit());
                assertNull(nyx.state().getReleaseScope().getPreviousVersion());
                assertNull(nyx.state().getReleaseScope().getPreviousVersionCommit());
                assertNull(nyx.state().getReleaseScope().getSignificant());
            }

            // make sure the project version is now set to the value we passed (this could be true even if Nyx didn't do anything)
            // as we set the value earlier
            assertEquals("v1.2.3", project.findProperty(GRADLE_VERSION_PROPERTY_NAME));

            // make sure that the state associated to the Nyx extra property also has the same version set
            assertEquals("v1.2.3", Nyx.class.cast(project.getExtensions().getExtraProperties().get(CoreTask.NYX_INSTANCE_PROPERTY)).state().getVersion());
        }
    }
}
