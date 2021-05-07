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

import java.io.File;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.git.Scenario;
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
     * Note that repository details are tested in the {@link com.mooltiverse.oss.nyx.git.JGitRepositoryTests} class.
     * Here only the Nyx class behavior pertaining the repository handling is tested.
     */
    @Nested
    @DisplayName("Nyx.repository")
    class RepositoryTests {
        @Test
        @DisplayName("Nyx.repository() throws DataAccessException in empty directory")
        void exceptionInEmptyDirectoryTest()
            throws Exception {

            // the Nyx instance created this way runs in the local sources directory, with no Git repository itself, so it's supposed to throw an exception
            assertThrows(DataAccessException.class, () -> new Nyx().repository());
        }

        @Test
        @DisplayName("Nyx.repository()")
        void repositoryTest()
            throws Exception {
            Nyx nyx = new Nyx();

            // initialize a repository in a new directory and pass the directory to the configuration. We'll use the command line configuration layer to pass the directory
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setDirectory(Scenario.FROM_SCRATCH.realize().getWorkingDirectory().getAbsolutePath());
            nyx.configuration().withCommandLineConfiguration(configurationLayerMock);

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

        @Test
        @DisplayName("Nyx.state() resume")
        void stateResumeTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setResume(Boolean.TRUE);
            configurationLayerMock.setStateFile(new File(System.getProperty("java.io.tmpdir"), "state"+this.hashCode()+".json").getAbsolutePath());
            configuration.withCommandLineConfiguration(configurationLayerMock);
            State oldState = new State(configuration);

            // set a few values to use later on for comparison
            oldState.setVersion("3.5.7");
            oldState.getInternals().put("attr1", "value1");
            oldState.getReleaseScope().getCommits().add("final");
            oldState.getReleaseScope().getCommits().add("initial");
            oldState.getReleaseScope().setPreviousVersion("previous");
            oldState.getReleaseScope().setPreviousVersionCommit("previousCommit");
            oldState.getReleaseScope().getSignificantCommits().put("final", "major");
            oldState.getReleaseScope().getSignificantCommits().put("initial", "minor");

            // save the file
            FileMapper.save(configurationLayerMock.getStateFile(), oldState);
            assertTrue(new File(configurationLayerMock.getStateFile()).exists());

            // now we are ready to resume the file
            Nyx nyx = new Nyx();
            nyx.configuration().withCommandLineConfiguration(configurationLayerMock);

            State resumedState = nyx.state();
            assertEquals(oldState.getBump(), resumedState.getBump());
            assertEquals(oldState.getInternals(), resumedState.getInternals());
            assertTrue(resumedState.getInternals().containsKey("attr1"));
            assertEquals("value1", resumedState.getInternals().get("attr1"));
            assertEquals("final", resumedState.getReleaseScope().getFinalCommit());
            assertEquals(oldState.getReleaseScope().getFinalCommit(), resumedState.getReleaseScope().getFinalCommit());
            assertEquals("initial", resumedState.getReleaseScope().getInitialCommit());
            assertEquals(2, resumedState.getReleaseScope().getCommits().size());
            assertEquals(oldState.getReleaseScope().getInitialCommit(), resumedState.getReleaseScope().getInitialCommit());
            assertEquals(oldState.getReleaseScope().getPreviousVersion(), resumedState.getReleaseScope().getPreviousVersion());
            assertEquals(oldState.getReleaseScope().getPreviousVersionCommit(), resumedState.getReleaseScope().getPreviousVersionCommit());
            assertEquals(2, resumedState.getReleaseScope().getSignificantCommits().size());
            assertTrue(resumedState.getReleaseScope().getSignificantCommits().containsKey("final"));
            assertEquals("major", resumedState.getReleaseScope().getSignificantCommits().get("final"));
            assertTrue(resumedState.getReleaseScope().getSignificantCommits().containsKey("initial"));
            assertEquals("minor", resumedState.getReleaseScope().getSignificantCommits().get("initial"));

            assertEquals(oldState.getTimestamp(), resumedState.getTimestamp());
            assertEquals(oldState.getVersion(), resumedState.getVersion());
        }
    }
}