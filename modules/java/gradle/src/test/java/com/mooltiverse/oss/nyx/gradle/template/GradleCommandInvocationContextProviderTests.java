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
package com.mooltiverse.oss.nyx.gradle.template;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import org.gradle.api.Project;
import org.gradle.api.Task;

import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.command.template.Baseline;
import com.mooltiverse.oss.nyx.command.template.CommandSelector;
import com.mooltiverse.oss.nyx.services.git.Scenario;
import com.mooltiverse.oss.nyx.command.template.CommandInvocationContextProviderTests;

/**
 * Test the GradleCommandInvocationContextProvider and its internal classes.
 * 
 * Inherited tests are re-run to test entities like @Baseline, @Command, @CommandSelector with the new provider.
 */
@DisplayName("GradleCommandInvocationContextProvider")
public class GradleCommandInvocationContextProviderTests {
    @Nested
    @DisplayName("Baseline Script injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class BaselineScriptInjectionTests1 extends CommandInvocationContextProviderTests.BaselineScriptInjectionTests1 {}

    @Nested
    @DisplayName("Baseline Script injection with @ExtendWith at the method level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class BaselineScriptInjectionTests2 extends CommandInvocationContextProviderTests.BaselineScriptInjectionTests2 {}

    @Nested
    @DisplayName("Baseline Workbench injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class BaselineWorkbenchInjectionTests1 extends CommandInvocationContextProviderTests.BaselineWorkbenchInjectionTests1 {}

    @Nested
    @DisplayName("Baseline Workbench injection with @ExtendWith at the method level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class BaselineWorkbenchInjectionTests2 extends CommandInvocationContextProviderTests.BaselineWorkbenchInjectionTests2 {}

    @Nested
    @DisplayName("Command injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class CommandInjectionTests1 extends CommandInvocationContextProviderTests.CommandInjectionTests1 {}

    @Nested
    @DisplayName("CommandProxy injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class CommandProxyInjectionTests1 extends CommandInvocationContextProviderTests.CommandProxyInjectionTests1 {}

    @Nested
    @DisplayName("Command injection with @ExtendWith at the method level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class CommandInjectionTests2 extends CommandInvocationContextProviderTests.CommandInjectionTests2 {}

    @Nested
    @DisplayName("CommandProxy injection with @ExtendWith at the method level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class CommandProxyInjectionTests2 extends CommandInvocationContextProviderTests.CommandProxyInjectionTests2 {}

    @Nested
    @DisplayName("Task injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class TaskInjectionTests1 {
        @TestTemplate
        @DisplayName("Task injection when the parameter is annotated with @CommandFactory and @Baseline")
        void taskTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Task task)
            throws Exception {
            assertNotNull(task);
        }

        @TestTemplate
        @DisplayName("Task injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void taskTest2(@CommandSelector(Commands.CLEAN) Task task)
            throws Exception {
            assertNotNull(task);
        }
    }

    @Nested
    @DisplayName("Task injection with @ExtendWith at the method level")
    static class TaskInjectionTests2 {
        @TestTemplate
        @ExtendWith(GradleCommandInvocationContextProvider.class)
        @DisplayName("Task injection when the parameter is annotated with @CommandFactory and @Baseline")
        void taskTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Task task)
            throws Exception {
            assertNotNull(task);
        }

        @TestTemplate
        @ExtendWith(GradleCommandInvocationContextProvider.class)
        @DisplayName("Task injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void taskTest2(@CommandSelector(Commands.CLEAN) Task task)
            throws Exception {
            assertNotNull(task);
        }
    }

    @Nested
    @DisplayName("Project injection with @ExtendWith at the class level")
    @ExtendWith(GradleCommandInvocationContextProvider.class)
    static class ProjectInjectionTests1 {
        @TestTemplate
        @DisplayName("Project injection when the parameter is annotated with @CommandFactory and @Baseline")
        void projectTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Project project)
            throws Exception {
            assertNotNull(project);
        }

        @TestTemplate
        @DisplayName("Project injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void projectTest2(@CommandSelector(Commands.CLEAN) Project project)
            throws Exception {
            assertNotNull(project);
        }
    }

    @Nested
    @DisplayName("Project injection with @ExtendWith at the method level")
    static class ProjectInjectionTests2 {
        @TestTemplate
        @ExtendWith(GradleCommandInvocationContextProvider.class)
        @DisplayName("Project injection when the parameter is annotated with @CommandFactory and @Baseline")
        void projectTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Project project)
            throws Exception {
            assertNotNull(project);
        }

        @TestTemplate
        @ExtendWith(GradleCommandInvocationContextProvider.class)
        @DisplayName("Project injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void projectTest2(@CommandSelector(Commands.CLEAN) Project project)
            throws Exception {
            assertNotNull(project);
        }
    }
}
