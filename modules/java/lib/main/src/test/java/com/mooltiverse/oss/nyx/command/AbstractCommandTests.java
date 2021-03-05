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
package com.mooltiverse.oss.nyx.command;

import java.lang.reflect.Constructor;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.git.script.GitScript;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("AbstractCommand")
public class AbstractCommandTests {
    /**
     * A map of the available commands, where keys are simple command class names and values are their classes.
     */
    static Map<String,Class<? extends AbstractCommand>> commands = new HashMap<String,Class<? extends AbstractCommand>>(){
        private static final long serialVersionUID = 1L;
        {
            put(Arrange.class.getSimpleName(), Arrange.class);
            put(Clean.class.getSimpleName(),   Clean.class);
            put(Infer.class.getSimpleName(),   Infer.class);
            put(Make.class.getSimpleName(),    Make.class);
            put(Mark.class.getSimpleName(),    Mark.class);
            put(Publish.class.getSimpleName(), Publish.class);
        }
    };

    /**
     * A stream that can be used in {@link MethodSource} that returns the commands.
     * Each returned argument has the fields:<br>
     * - commandClassName: the simple name of the command class<br>
     * - commandClass: the class of the command<br>
     *
     * @return a stream of arguments representing commands
     * 
     * @see #commands
     */
    static Stream<Arguments> commandsArguments() {
        ArrayList<Arguments> arguments = new ArrayList<Arguments>();
        for (Map.Entry<String,Class<? extends AbstractCommand>> command: commands.entrySet()) {
            arguments.add(Arguments.of(command.getKey(), command.getValue()));
        }
        return arguments.stream();
    }

    /**
     * Returns an instance of of the given command class.
     * 
     * @param the class to return an instance for
     * @param state the state to pass to the constructor
     * @param repository the repository to pass to the constructor
     * 
     * @return the command instance
     * 
     * @throws Exception in case of any issue
     */
    protected static <T extends AbstractCommand> T getCommandInstance(Class<T> clazz, State state, Repository repository)
        throws Exception {
        Constructor<T> constructor = clazz.getConstructor(State.class, Repository.class);
        return constructor.newInstance(state, repository);
    }

    @Nested
    @DisplayName("AbstractCommand.constructor")
    static class ConstructorTests {
        /**
         * Test that the given class has the required 2 arguments constructor and that it doesn't fail as long as it
         * has non null parameters
         */
        @ParameterizedTest(name = "{0}(State, Repository) constructor")
        @MethodSource("com.mooltiverse.oss.nyx.command.AbstractCommandTests#commandsArguments")
        void constructorTest(String commandClassSimpleName, Class<? extends AbstractCommand> commandClass)
            throws Exception {
            assertNotNull(getCommandInstance(commandClass, new State(new Configuration()), Git.open(GitScript.fromScratch().getWorkingDirectory())));
        }
    }

    @Nested
    @DisplayName("AbstractCommand.repository")
    static class RepositoryTests {
        /**
         * Check that the repository() method never returns a {@code null} object
         */
        @ParameterizedTest(name = "{0}.repository()")
        @MethodSource("com.mooltiverse.oss.nyx.command.AbstractCommandTests#commandsArguments")
        void repositoryTest(String commandClassSimpleName, Class<? extends AbstractCommand> commandClass)
            throws Exception {
            assertNotNull(getCommandInstance(commandClass, new State(new Configuration()), Git.open(GitScript.fromScratch().getWorkingDirectory())).repository());
        }
    }

    @Nested
    @DisplayName("AbstractCommand.state")
    static class StateTests {
        /**
         * Check that the state() method never returns a {@code null} object
         */
        @ParameterizedTest(name = "{0}.state()")
        @MethodSource("com.mooltiverse.oss.nyx.command.AbstractCommandTests#commandsArguments")
        void stateTest(String commandClassSimpleName, Class<? extends AbstractCommand> commandClass)
            throws Exception {
            assertNotNull(getCommandInstance(commandClass, new State(new Configuration()), Git.open(GitScript.fromScratch().getWorkingDirectory())).state());
        }
    }
}
