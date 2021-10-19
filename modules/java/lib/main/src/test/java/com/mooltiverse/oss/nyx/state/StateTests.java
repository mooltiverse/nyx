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
package com.mooltiverse.oss.nyx.state;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.data.Identifiers;
import com.mooltiverse.oss.nyx.data.IdentifierPosition;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.data.WorkspaceStatus;

@DisplayName("State")
public class StateTests {
    @Nested
    @DisplayName("Constructor")
    class ConstructorTests {
        @Test
        @DisplayName("State()")
        void constructorTest()
            throws Exception {
            // passing the null argument must throw an exception
            assertThrows(NullPointerException.class, () -> { new State(null); });
            assertDoesNotThrow(() -> { new State(new Configuration()); });
        }
    }
    
    @Nested
    @DisplayName("Attributes")
    class AttributesTests {
        @Test
        @DisplayName("State.getBranch()")
        void getBranchTest()
            throws Exception {
            // make sure the branch is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getBranch());

            state.setBranch("abranch");
            assertEquals("abranch", state.getBranch());

            state.setBranch("anotherbranch");
            assertEquals("anotherbranch", state.getBranch());
        }

        @Test
        @DisplayName("State.setBranch(String)")
        void setBranchTest()
            throws Exception {
            Configuration configuration = new Configuration();
            State state = new State(configuration);

            String branch = "abranch";
            state.setBranch(branch);
            assertEquals(branch, state.getBranch());
        }

        @Test
        @DisplayName("State.getBump()")
        void getBumpTest()
            throws Exception {
            // make sure the bump is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getBump());
            state.getReleaseScope().getSignificantCommits().put("commit1", "alpha");
            assertEquals("alpha", state.getBump());
            state.getReleaseScope().getSignificantCommits().put("commit2", "major");
            assertEquals("major", state.getBump());
        }

        @Test
        @DisplayName("State.getConfiguration()")
        void getConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            assertEquals(configuration, new State(configuration).getConfiguration());
        }

        @Test
        @DisplayName("State.getDirectory()")
        void getDirectoryTest()
            throws Exception {
            Configuration configuration = new Configuration();
            // make sure the state directory is the same from the configuration
            assertEquals(new File(configuration.getDirectory()).getAbsolutePath(), new State(configuration).getDirectory().getAbsolutePath());
        }

        @Test
        @DisplayName("State.getInternals()")
        void getInternalsTest()
            throws Exception {
            State state = new State(new Configuration());

            // make sure the initial internals is never null and empty
            assertNotNull(state.getInternals());
            assertTrue(state.getInternals().isEmpty());
        }

        @Test
        @DisplayName("State.getNewRelease()")
        void getNewReleaseTest()
            throws Exception {
            State state = new State(new Configuration());
            // inject a releaseType with the 'publish' flag to TRUE
            state.setReleaseType(new ReleaseType(true, null, null, Boolean.FALSE.toString(), null, Boolean.FALSE.toString(), Boolean.FALSE.toString(), null, null, null, null, null, /*this is the 'publish' flag -> */ Boolean.TRUE.toString(), null, Boolean.FALSE));
            state.setVersion("1.2.3");
            state.getReleaseScope().setPreviousVersion("1.2.3");
            assertFalse(state.getNewVersion());
            assertFalse(state.getNewRelease());

            state.getReleaseScope().setPreviousVersion("0.1.0");
            assertTrue(state.getNewVersion());
            assertTrue(state.getNewRelease());

            // now replace the releaseType with the 'publish' flag to FALSE
            state.setReleaseType(new ReleaseType(true, null, null, Boolean.FALSE.toString(), null, Boolean.FALSE.toString(), Boolean.FALSE.toString(), null, null, null, null, null, /*this is the 'publish' flag -> */ Boolean.FALSE.toString(), null, Boolean.FALSE));

            state.getReleaseScope().setPreviousVersion("0.1.0");
            assertTrue(state.getNewVersion());
            assertFalse(state.getNewRelease());
        }

        @Test
        @DisplayName("State.getNewVersion()")
        void getNewVersionTest()
            throws Exception {
            State state = new State(new Configuration());

            state.setVersion("1.2.3");
            state.getReleaseScope().setPreviousVersion("1.2.3");
            assertFalse(state.getNewVersion());

            state.getReleaseScope().setPreviousVersion("0.1.0");
            assertTrue(state.getNewVersion());
        }

        @Test
        @DisplayName("State.getReleaseScope()")
        void getReleaseScopeTest()
            throws Exception {
            // make sure the release scope is initialized
            assertNotNull(new State(new Configuration()).getReleaseScope());
        }

        @Test
        @DisplayName("State.getReleaseType()")
        void getReleaseTypeTest()
            throws Exception {
            State state = new State(new Configuration());

            // make sure the release type is null until it has been explicitly set
            assertNull(state.getReleaseType());
            ReleaseType releaseType = new ReleaseType();
            state.setReleaseType(releaseType);
            assertSame(releaseType, state.getReleaseType());
        }

        @Test
        @DisplayName("State.getScheme()")
        void getSchemeTest()
            throws Exception {
            Configuration configuration = new Configuration();
            // make sure the scheme is the same from the configuration
            assertEquals(configuration.getScheme(), new State(configuration).getScheme());
        }

        @Test
        @DisplayName("State.getTimestamp()")
        void getTimestampTest()
            throws Exception {
            // make sure the current timestamp is a fresh one
            State state = new State(new Configuration());
            assertTrue(System.currentTimeMillis() >= state.getTimestamp());
        }

        @Test
        @DisplayName("State.touchTimestamp()")
        void touchTimestampTest()
            throws Exception {
            // make sure that when touching the timestamp the new value is updated
            State state = new State(new Configuration());

            long oldTimestamp = state.getTimestamp();
            do {
                // just do nothing and let at least 1 millisecond pass
                //this.wait(1); // throws an IllegalMonitorStateException
            }
            while (oldTimestamp == System.currentTimeMillis()); // exit as soon as at least 1 millisecond passed
            
            assertEquals(state.touchTimestamp(), state.getTimestamp());

            assertNotEquals(oldTimestamp, state.getTimestamp());
        }

        @Test
        @DisplayName("State.getVersion()")
        void getVersionTest()
            throws Exception {
            // make sure the version is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getVersion());

            state.setVersion("1.2.3");
            assertEquals("1.2.3", state.getVersion());

            state.setVersion("v1.2.3");
            assertEquals("v1.2.3", state.getVersion());
        }

        @Test
        @DisplayName("State.setVersion(String)")
        void setVersionTest()
            throws Exception {
            Configuration configuration = new Configuration();
            State state = new State(configuration);

            String version = "1.2.3";
            state.setVersion(version);
            assertEquals(version, state.getVersion());
        }

        @Test
        @DisplayName("State.getVersionRange()")
        void getVersionRangeTest()
            throws Exception {
            // make sure the version range is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getVersionRange());

            state.setVersionRange(".*");
            assertEquals(".*", state.getVersionRange());

            state.setVersionRange("v(.*)");
            assertEquals("v(.*)", state.getVersionRange());
        }

        @Test
        @DisplayName("State.setVersionRange(String)")
        void setVersionRangeTest()
            throws Exception {
            Configuration configuration = new Configuration();
            State state = new State(configuration);

            String version = ".*";
            state.setVersionRange(version);
            assertEquals(version, state.getVersionRange());
        }
    }

    @Nested
    @DisplayName("Resume")
    class ResumeTests {
        @Test
        @DisplayName("State.resume()")
        void resumeTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setResume(Boolean.TRUE);
            configurationLayerMock.setStateFile(new File(System.getProperty("java.io.tmpdir"), "state"+this.hashCode()+".json").getAbsolutePath());
            configuration.withCommandLineConfiguration(configurationLayerMock);
            State oldState = new State(configuration);

            // set a few values to use later on for comparison
            oldState.setVersion("3.5.7");
            oldState.setVersionRange(".*");
            oldState.getInternals().put("attr1", "value1");
            oldState.getReleaseScope().getCommits().add("final");
            oldState.getReleaseScope().getCommits().add("initial");
            oldState.getReleaseScope().setPreviousVersion("previous");
            oldState.getReleaseScope().setPreviousVersionCommit("previousCommit");
            oldState.getReleaseScope().setPrimeVersion("prime");
            oldState.getReleaseScope().setPrimeVersionCommit("primeCommit");
            oldState.setReleaseType(new ReleaseType());
            oldState.getReleaseType().setCollapseVersions(true);
            oldState.getReleaseType().setCollapsedVersionQualifier("rel");
            oldState.getReleaseType().setGitCommit(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitCommitMessage("Commit message");
            oldState.getReleaseType().setGitPush(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTag(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTagMessage("Tag message");
            oldState.getReleaseType().setIdentifiers(new Identifiers(List.<String>of("build"), Map.<String,Identifier>of("build", new Identifier("b", "12", IdentifierPosition.BUILD))));
            oldState.getReleaseType().setMatchBranches(".*");
            oldState.getReleaseType().setMatchEnvironmentVariables(Map.<String,String>of("USER", ".*", "PATH", ".*"));
            oldState.getReleaseType().setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            oldState.getReleaseType().setPublish(Boolean.TRUE.toString());
            oldState.getReleaseType().setVersionRange("1.x");
            oldState.getReleaseType().setVersionRangeFromBranchName(Boolean.FALSE);

            // save the file
            FileMapper.save(configurationLayerMock.getStateFile(), oldState);
            assertTrue(new File(configurationLayerMock.getStateFile()).exists());

            // now we are ready to resume the file
            State resumedState = State.resume(new File(configurationLayerMock.getStateFile()), configuration);
            assertEquals(oldState.getBump(), resumedState.getBump());
            assertEquals(oldState.getInternals(), resumedState.getInternals());
            assertTrue(resumedState.getInternals().containsKey("attr1"));
            assertEquals("value1", resumedState.getInternals().get("attr1"));
            assertEquals(oldState.getReleaseScope().getFinalCommit(), resumedState.getReleaseScope().getFinalCommit());
            assertEquals(oldState.getReleaseScope().getInitialCommit(), resumedState.getReleaseScope().getInitialCommit());
            assertEquals(oldState.getReleaseScope().getPreviousVersion(), resumedState.getReleaseScope().getPreviousVersion());
            assertEquals(oldState.getReleaseScope().getPreviousVersionCommit(), resumedState.getReleaseScope().getPreviousVersionCommit());
            assertEquals(oldState.getReleaseScope().getPrimeVersion(), resumedState.getReleaseScope().getPrimeVersion());
            assertEquals(oldState.getReleaseScope().getPrimeVersionCommit(), resumedState.getReleaseScope().getPrimeVersionCommit());
            assertEquals(oldState.getTimestamp(), resumedState.getTimestamp());
            assertEquals(oldState.getVersion(), resumedState.getVersion());
            assertEquals(oldState.getVersionRange(), resumedState.getVersionRange());
            assertEquals(oldState.getReleaseType().getCollapseVersions(), resumedState.getReleaseType().getCollapseVersions());
            assertEquals(oldState.getReleaseType().getCollapsedVersionQualifier(), resumedState.getReleaseType().getCollapsedVersionQualifier());
            assertEquals(oldState.getReleaseType().getGitCommit(), resumedState.getReleaseType().getGitCommit());
            assertEquals(oldState.getReleaseType().getGitCommitMessage(), resumedState.getReleaseType().getGitCommitMessage());
            assertEquals(oldState.getReleaseType().getGitPush(), resumedState.getReleaseType().getGitPush());
            assertEquals(oldState.getReleaseType().getGitTag(), resumedState.getReleaseType().getGitTag());
            assertEquals(oldState.getReleaseType().getGitTagMessage(), resumedState.getReleaseType().getGitTagMessage());
            if (Objects.isNull(oldState.getReleaseType().getIdentifiers())) {
                assertEquals(oldState.getReleaseType().getIdentifiers(), resumedState.getReleaseType().getIdentifiers());
            }
            else {
                assertEquals(oldState.getReleaseType().getIdentifiers().getEnabled().size(), resumedState.getReleaseType().getIdentifiers().getEnabled().size());
                assertTrue(oldState.getReleaseType().getIdentifiers().getEnabled().containsAll(resumedState.getReleaseType().getIdentifiers().getEnabled()));
                assertEquals(oldState.getReleaseType().getIdentifiers().getItems().size(), resumedState.getReleaseType().getIdentifiers().getItems().size());
                assertTrue(oldState.getReleaseType().getIdentifiers().getItems().keySet().containsAll(resumedState.getReleaseType().getIdentifiers().getItems().keySet()));
            }
            assertEquals(oldState.getReleaseType().getMatchBranches(), resumedState.getReleaseType().getMatchBranches());
            assertEquals(oldState.getReleaseType().getMatchEnvironmentVariables().size(), resumedState.getReleaseType().getMatchEnvironmentVariables().size());
            assertEquals(oldState.getReleaseType().getMatchWorkspaceStatus(), resumedState.getReleaseType().getMatchWorkspaceStatus());
            assertEquals(oldState.getReleaseType().getPublish(), resumedState.getReleaseType().getPublish());
            assertEquals(oldState.getReleaseType().getVersionRange(), resumedState.getReleaseType().getVersionRange());
            assertEquals(oldState.getReleaseType().getVersionRangeFromBranchName(), resumedState.getReleaseType().getVersionRangeFromBranchName());
        }
    }
}