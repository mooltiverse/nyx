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
package com.mooltiverse.oss.nyx;

import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * This interface models an execution context that allows running the Nyx executable from within a specific environment.
 *
 * This interface can be implemented by different contexts in order to run a certain command in a context specific way.
 */
public interface ExecutionContext {
    /**
     * Returns the command objects used to run the test.
     * 
     * @param repoDir the directory containing the Git repository
     * @param args the optional array of command line arguments to pass to Gradle. It may be {@code null}
     * @param env the optional map of environment variables to pass to Gradle. It may be {@code null}
     * 
     * @return the command instances list. Commands are required to be executed in the same order
     * they appear in the list.
     * 
     * @throws any exception that may be thrown when instantiating the command
     */
    List<? extends Command> getTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception;
    
    /**
     * Returns the command objects used to run before the test.
     * 
     * @param repoDir the directory containing the Git repository
     * @param args the optional array of command line arguments to pass to Gradle. It may be {@code null}
     * @param env the optional map of environment variables to pass to Gradle. It may be {@code null}
     * 
     * @return the command instances list, which may be empty. Commands are required to be executed
     * in the same order they appear in the list.
     * 
     * @throws any exception that may be thrown when instantiating the command
     */
    List<? extends Command> getPreTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception;

    /**
     * Returns the command object used to run after the test.
     * 
     * @param repoDir the directory containing the Git repository
     * @param args the optional array of command line arguments to pass to Gradle. It may be {@code null}
     * @param env the optional map of environment variables to pass to Gradle. It may be {@code null}
     * 
     * @return the command instances list, which may be empty. Commands are required to be executed
     * in the same order they appear in the list.
     * 
     * @throws any exception that may be thrown when instantiating the command
     */
    List<? extends Command> getPostTestCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception;

    /**
     * Returns the command object used to run the after the test, before the test exits.
     * 
     * @param repoDir the directory containing the Git repository
     * @param args the optional array of command line arguments to pass to Gradle. It may be {@code null}
     * @param env the optional map of environment variables to pass to Gradle. It may be {@code null}
     * 
     * @return the command instances list, which may be empty. Commands are required to be executed
     * in the same order they appear in the list.
     * 
     * @throws any exception that may be thrown when instantiating the command
     */
    List<? extends Command> getCleanUpCommands(File repoDir, Map<String,String> env, String[] args)
        throws Exception;
}
