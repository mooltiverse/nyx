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

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import org.gradle.api.Project;

import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.gradle.template.GradleCommandInvocationContextProvider;
import com.mooltiverse.oss.nyx.git.Scenario;
import com.mooltiverse.oss.nyx.git.Script;

/**
 * The purpose of this class is to just re-run the tests from inherited templates once more by running the Nyx commands
 * as Gradle tasks.
 * 
 * This is possible thanks to the {@link GradleCommandInvocationContextProvider} which, once applied, adds one
 * context to the inhertited tests. The new context provides {@link Command} objects to test methods that are actually wrappers
 * around Gradle tasks.
 * 
 * The only downside here is that inherited contexts will be executed once again, in addition to the new one, and that's because
 * they already have an extension that is not replaced by this one. Instead, the two sum up their contexts (by JUnit design).
 * However, that's something we can live with.
 * 
 * In addition to inherited tests some Gradle specific test templates are added.
 */
@DisplayName("Publish")
public class PublishTestTemplates {
    @Nested
    @DisplayName("Publish constructor")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class ConstructorTests extends com.mooltiverse.oss.nyx.command.PublishTestTemplates.ConstructorTests {}

    @Nested
    @DisplayName("Publish state")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class StateTests extends com.mooltiverse.oss.nyx.command.PublishTestTemplates.StateTests {}

    @Disabled("TODO: re-enable this test when working on https://github.com/mooltiverse/nyx/issues/40")
    @Nested
    @DisplayName("Publish isUpToDate")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class UpToDateTests extends com.mooltiverse.oss.nyx.command.PublishTestTemplates.UpToDateTests {}

    @Nested
    @DisplayName("Publish run")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class RunTests extends com.mooltiverse.oss.nyx.command.PublishTestTemplates.RunTests {}

    /**
     * Performs checks on the task business actions.
     */
    @Nested
    @DisplayName("PublishTask.Actions.execute")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class ActionTests {
        @TestTemplate
        @DisplayName("PublishTask.getActions().execute() doesn't throw exceptions without a valid Git repository in custom directory")
        @Baseline(Scenario.INITIAL_COMMIT)
        void noExceptionOnExecuteWithValidGitRepositoryInCustomDirectoryTest(Project project, @CommandSelector(Commands.PUBLISH) Command command, Script script)
        throws Exception {
            //make sure the Gradle working directory and the Git repository directory are not the same
            assertFalse(project.getBuildDir().equals(script.getWorkingDirectory()));
            assertFalse(project.getBuildDir().getAbsolutePath().equals(script.getWorkingDirectory().getAbsolutePath()));
    
            // the valid Git directory, different than the current working directory, is passed as the 'directory' configuration option through the extension
            project.getExtensions().getByType(NyxExtension.class).getDirectory().set(script.getWorkingDirectory());
    
            assertDoesNotThrow(() -> command.run());
        }

        @TestTemplate
        @DisplayName("PublishTask.getActions().execute() doesn't throw exceptions without a valid Git repository in working directory")
        @Baseline(Scenario.INITIAL_COMMIT)
        void noExceptionOnExecuteWithValidGitRepositoryInWorkingDirectoryTest(@CommandSelector(Commands.PUBLISH) Command command)
            throws Exception {
            assertDoesNotThrow(() -> command.run());
        }
    }
}