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

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.configuration.mock.ConfigurationLayerMock;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.git.script.GitScript;
import com.mooltiverse.oss.nyx.state.State;

@DisplayName("Nyx")
public class NyxTests {
    /**
     * Performs checks against the configuration.
     * 
     * Note that configuration details are tested in the {@link com.mooltiverse.oss.nyx.configuration.ConfigurationTests} class.
     * Here only the Nyx class behavior pertaining the configuration handling is tested.
     */
    @Nested
    @DisplayName("Nyx.configuration")
    class ConfigurationTests {
        @Test
        @DisplayName("Nyx.configuration()")
        void configurationTest()
            throws Exception {

            Nyx nyx = new Nyx();
            Configuration configuration = nyx.configuration();
            
            assertNotNull(configuration);

            // test that multiple invocations return the same instance
            assertEquals(configuration.hashCode(), nyx.configuration().hashCode());
            assertEquals(configuration.hashCode(), nyx.configuration().hashCode());
        }
    }

    /**
     * Performs checks against the repository.
     * 
     * Note that repository details are tested in the {@link com.mooltiverse.oss.nyx.git.JGitRepositoryTest} class.
     * Here only the Nyx class behavior pertaining the repository handling is tested.
     */
    @Nested
    @DisplayName("Nyx.repository")
    class RepositoryTests {
        @Test
        @DisplayName("Nyx.repository() throws DataAccessException in empty directory")
        void repositoryThrowsExceptionInEmptyDirectoryTest()
            throws Exception {

            // the Nyx instance created this way runs in the local sources directory, with no Git repository itself, so it's supposed to throw an exception
            assertThrows(DataAccessException.class, () -> new Nyx().repository());
        }

        @Test
        @DisplayName("Nyx.repository() with valid Git directory")
        void repositoryWithValidGitDirectoryTest()
            throws Exception {

            Nyx nyx = new Nyx();

            // initialize a repository in a new directory and pass the directory to the configuration. We'll use the plugin configuration layer to pass the directory
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            configurationMock.directory = GitScript.fromScratch().getWorkingDirectory();
            nyx.configuration().withPluginConfiguration(configurationMock);

            // check that no exception is thrown when the Nyx configuration receives a valid Git repository.
            assertDoesNotThrow(() -> nyx.repository());
        }

        @Test
        @DisplayName("Nyx.repository()")
        void repositoryTest()
            throws Exception {

            Nyx nyx = new Nyx();

            // initialize a repository in a new directory and pass the directory to the configuration. We'll use the plugin configuration layer to pass the directory
            ConfigurationLayerMock configurationMock = new ConfigurationLayerMock();
            configurationMock.directory = GitScript.fromScratch().getWorkingDirectory();
            nyx.configuration().withPluginConfiguration(configurationMock);

            Repository repository = nyx.repository();

            // test that multiple invocations return the same instance
            assertEquals(repository.hashCode(), nyx.repository().hashCode());
            assertEquals(repository.hashCode(), nyx.repository().hashCode());
        }
    }

    /**
     * Performs checks against the state.
     * 
     * Note that state details are tested in the {@link com.mooltiverse.oss.nyx.state.StateTests} class.
     * Here only the Nyx class behavior pertaining the state handling is tested.
     */
    @Nested
    @DisplayName("Nyx.state")
    class StateTests {
        @Test
        @DisplayName("Nyx.state()")
        void stateTest()
            throws Exception {

            Nyx nyx = new Nyx();
            State state = nyx.state();
            
            assertNotNull(state);

            // test that multiple invocations return the same instance
            assertEquals(state.hashCode(), nyx.state().hashCode());
            assertEquals(state.hashCode(), nyx.state().hashCode());
        }
    }
}