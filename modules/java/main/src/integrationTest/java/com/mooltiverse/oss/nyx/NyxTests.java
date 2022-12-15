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
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.Changelog;
import com.mooltiverse.oss.nyx.entities.git.Action;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Message;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.git.tools.Scenario;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.FileMapper;
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
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

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
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State oldState = new State(configuration);

            Commit initialCommit = new Commit("initial", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of());
            Commit finalCommit = new Commit("final", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of());

            // set a few values to use later on for comparison
            oldState.setChangelog(new Changelog());
            oldState.getChangelog().setReleases(List.<Changelog.Release>of(new Changelog.Release("MyRelease", "today")));
            oldState.getChangelog().getReleases().get(0).setSections(List.<Changelog.Release.Section>of(new Changelog.Release.Section("MySection")));
            oldState.setVersion("3.5.7");
            oldState.setVersionRange(".*");
            oldState.getInternals().put("attr1", "value1");
            oldState.getReleaseScope().getCommits().add(finalCommit);
            oldState.getReleaseScope().getCommits().add(initialCommit);
            oldState.getReleaseScope().setPreviousVersion("4.5.6");
            oldState.getReleaseScope().setPreviousVersionCommit(new Commit("05cbfd58fadbec3d96b220a0054d96875aa37011", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of(new Tag("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false))));
            oldState.getReleaseScope().setPrimeVersion("1.0.0");
            oldState.getReleaseScope().setPrimeVersionCommit(new Commit("e8fa442504d91a0187865c74093a5a4212a805f9", 0, List.<String>of(), new Action(new Identity("Jim", null), null), new Action(new Identity("Sam", null), null), new Message("full", "short", Map.<String,String>of()), Set.<Tag>of(new Tag("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false))));
            oldState.getReleaseScope().getSignificantCommits().add(finalCommit);
            oldState.getReleaseScope().getSignificantCommits().add(initialCommit);

            // save the file
            FileMapper.save(configurationLayerMock.getStateFile(), oldState);
            assertTrue(new File(configurationLayerMock.getStateFile()).exists());

            // now we are ready to resume the file
            Nyx nyx = new Nyx();
            nyx.configuration().withRuntimeConfiguration(configurationLayerMock);

            State resumedState = nyx.state();
            assertEquals(oldState.getBump(), resumedState.getBump());
            assertNotNull(resumedState.getChangelog());
            assertEquals(1, resumedState.getChangelog().getReleases().size());
            assertEquals("MyRelease", resumedState.getChangelog().getReleases().get(0).getName());
            assertEquals("today", resumedState.getChangelog().getReleases().get(0).getDate());
            assertEquals(1, resumedState.getChangelog().getReleases().get(0).getSections().size());
            assertEquals("MySection", resumedState.getChangelog().getReleases().get(0).getSections().get(0).getName());
            assertEquals(oldState.getInternals(), resumedState.getInternals());
            assertTrue(resumedState.getInternals().containsKey("attr1"));
            assertEquals("value1", resumedState.getInternals().get("attr1"));
            assertEquals("final", resumedState.getReleaseScope().getFinalCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getFinalCommit(), resumedState.getReleaseScope().getFinalCommit());
            assertEquals("initial", resumedState.getReleaseScope().getInitialCommit().getSHA());
            assertEquals(2, resumedState.getReleaseScope().getCommits().size());
            assertEquals(oldState.getReleaseScope().getInitialCommit(), resumedState.getReleaseScope().getInitialCommit());
            assertEquals(oldState.getReleaseScope().getPreviousVersion(), resumedState.getReleaseScope().getPreviousVersion());
            assertEquals(oldState.getReleaseScope().getPreviousVersionCommit(), resumedState.getReleaseScope().getPreviousVersionCommit());
            assertEquals(oldState.getReleaseScope().getPrimeVersion(), resumedState.getReleaseScope().getPrimeVersion());
            assertEquals(oldState.getReleaseScope().getPrimeVersionCommit(), resumedState.getReleaseScope().getPrimeVersionCommit());
            assertEquals(2, resumedState.getReleaseScope().getSignificantCommits().size());
            assertTrue(resumedState.getReleaseScope().getSignificantCommits().contains(finalCommit));
            assertTrue(resumedState.getReleaseScope().getSignificantCommits().contains(initialCommit));
            assertEquals(oldState.getTimestamp(), resumedState.getTimestamp());
            assertEquals(oldState.getVersion(), resumedState.getVersion());
            assertEquals(oldState.getVersionRange(), resumedState.getVersionRange());
        }
    }
}