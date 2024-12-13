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
package com.mooltiverse.oss.nyx.command.template;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.git.tools.Script;
import com.mooltiverse.oss.nyx.git.tools.Workbench;

/**
 * Test the TemplateInvocationContextProvider and its internal classes.
 */
@DisplayName("TemplateInvocationContextProvider")
public class CommandInvocationContextProviderTests {
    @Nested
    @DisplayName("Baseline Script injection with @ExtendWith at the class level")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class BaselineScriptInjectionTests1 {
        @TestTemplate
        @DisplayName("Baseline yields to different script instances when all parameters are annotated")
        void baselineTest1(@Baseline(Scenario.INITIAL_COMMIT) Script script1, @Baseline(Scenario.INITIAL_COMMIT) Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to different script instances when all parameters and the method are annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest2(@Baseline(Scenario.INITIAL_COMMIT) Script script1, @Baseline(Scenario.INITIAL_COMMIT) Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to different script instances when one parameter is annotated and the other is not")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest3(@Baseline(Scenario.INITIAL_COMMIT) Script script1, Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to same script instances when only the method is annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest4(Script script1, Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertEquals(script1.hashCode(), script2.hashCode());
        }
    }

    @Nested
    @DisplayName("Baseline Script injection with @ExtendWith at the method level")
    public static class BaselineScriptInjectionTests2 {
        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different script instances when all parameters are annotated")
        void baselineTest1(@Baseline(Scenario.INITIAL_COMMIT) Script script1, @Baseline(Scenario.INITIAL_COMMIT) Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different script instances when all parameters and the method are annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest2(@Baseline(Scenario.INITIAL_COMMIT) Script script1, @Baseline(Scenario.INITIAL_COMMIT) Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different script instances when one parameter is annotated and the other is not")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest3(@Baseline(Scenario.INITIAL_COMMIT) Script script1, Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(script1.hashCode(), script2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to same script instances when only the method is annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest4(Script script1, Script script2)
            throws Exception {
            script1.getWorkingDirectory().deleteOnExit();
            script2.getWorkingDirectory().deleteOnExit();
            assertEquals(script1.hashCode(), script2.hashCode());
        }
    }

    @Nested
    @DisplayName("Baseline Workbench injection with @ExtendWith at the class level")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class BaselineWorkbenchInjectionTests1 {
        @TestTemplate
        @DisplayName("Baseline yields to different workbench instances when all parameters are annotated")
        void baselineTest1(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, @Baseline(Scenario.INITIAL_COMMIT) Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to different workbench instances when all parameters and the method are annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest2(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, @Baseline(Scenario.INITIAL_COMMIT) Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to different workbench instances when one parameter is annotated and the other is not")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest3(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @DisplayName("Baseline yields to same workbench instances when only the method is annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest4(Workbench workbench1, Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertEquals(workbench1.hashCode(), workbench2.hashCode());
        }
    }

    @Nested
    @DisplayName("Baseline Workbench injection with @ExtendWith at the method level")
    public static class BaselineWorkbenchInjectionTests2 {
        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different workbench instances when all parameters are annotated")
        void baselineTest1(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, @Baseline(Scenario.INITIAL_COMMIT) Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different workbench instances when all parameters and the method are annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest2(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, @Baseline(Scenario.INITIAL_COMMIT) Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to different workbench instances when one parameter is annotated and the other is not")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest3(@Baseline(Scenario.INITIAL_COMMIT) Workbench workbench1, Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertNotEquals(workbench1.hashCode(), workbench2.hashCode());
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Baseline yields to same workbench instances when only the method is annotated")
        @Baseline(Scenario.INITIAL_COMMIT)
        void baselineTest4(Workbench workbench1, Workbench workbench2)
            throws Exception {
            workbench1.getWorkingDirectory().deleteOnExit();
            workbench2.getWorkingDirectory().deleteOnExit();
            assertEquals(workbench1.hashCode(), workbench2.hashCode());
        }
    }

    @Nested
    @DisplayName("Command injection with @ExtendWith at the class level")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class CommandInjectionTests1 {
        @TestTemplate
        @DisplayName("Command injection when the parameter is annotated with @CommandFactory and @Baseline")
        void commandTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Command command)
            throws Exception {
            assertNotNull(command);
        }

        @TestTemplate
        @DisplayName("Command injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void commandTest2(@CommandSelector(Commands.CLEAN) CommandProxy command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("CommandProxy injection with @ExtendWith at the class level")
    @ExtendWith(CommandInvocationContextProvider.class)
    public static class CommandProxyInjectionTests1 {
        @TestTemplate
        @DisplayName("CommandProxy injection when the parameter is annotated with @CommandFactory and @Baseline")
        void commandProxyTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) CommandProxy commandProxy)
            throws Exception {
            assertNotNull(commandProxy);
        }

        @TestTemplate
        @DisplayName("CommandProxy injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void commandProxyTest2(@CommandSelector(Commands.CLEAN) CommandProxy commandProxy)
            throws Exception {
            assertNotNull(commandProxy);
        }
    }

    @Nested
    @DisplayName("Command injection with @ExtendWith at the method level")
    public static class CommandInjectionTests2 {
        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Command injection when the parameter is annotated with @CommandFactory and @Baseline")
        void commandTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) Command command)
            throws Exception {
            assertNotNull(command);
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("Command injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void commandTest2(@CommandSelector(Commands.CLEAN) Command command)
            throws Exception {
            assertNotNull(command);
        }
    }

    @Nested
    @DisplayName("CommandProxy injection with @ExtendWith at the method level")
    public static class CommandProxyInjectionTests2 {
        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("CommandProxy injection when the parameter is annotated with @CommandFactory and @Baseline")
        void commandProxyTest1(@CommandSelector(Commands.CLEAN) @Baseline(Scenario.INITIAL_COMMIT) CommandProxy commandProxy)
            throws Exception {
            assertNotNull(commandProxy);
        }

        @TestTemplate
        @ExtendWith(CommandInvocationContextProvider.class)
        @DisplayName("CommandProxy injection when the method is annotated with @CommandFactory and @Baseline")
        @Baseline(Scenario.INITIAL_COMMIT)
        void commandProxyTest2(@CommandSelector(Commands.CLEAN) CommandProxy commandProxy)
            throws Exception {
            assertNotNull(commandProxy);
        }
    }
}
