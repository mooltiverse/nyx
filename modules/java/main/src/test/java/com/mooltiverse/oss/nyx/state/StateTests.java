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
import java.io.FileReader;
import java.io.StringWriter;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SimpleTimeZone;
import java.util.Objects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.Changelog;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.entities.git.Action;
import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Message;
import com.mooltiverse.oss.nyx.entities.git.Tag;
import com.mooltiverse.oss.nyx.entities.git.TimeStamp;
import com.mooltiverse.oss.nyx.io.FileMapper;

@DisplayName("State")
public class StateTests {
    /**
     * Reads the contents of the given file and returns its content as a string.
     * 
     * @param file the file to read
     * 
     * @return the file content
     * 
     * @throws Exception in case of any issue
     */
    private String readFile(File file)
        throws Exception {
        StringWriter buffer = new StringWriter();
        FileReader reader = new FileReader(file);
        reader.transferTo(buffer);
        reader.close();
        return buffer.toString();
    }

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
            state.setBump("alpha");
            assertEquals("alpha", state.getBump());
        }

        @Test
        @DisplayName("State.getBump() overridden by configuration")
        void getBumpOverrideByConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setBump("gamma");
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State state = new State(configuration);
            assertEquals("gamma", state.getBump());
        }

        @Test
        @DisplayName("State.setBump()")
        void setBumpTest()
            throws Exception {
            // make sure the bump is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getBump());
            state.setBump("alpha");
            assertEquals("alpha", state.getBump());
        }

        @Test
        @DisplayName("State.setBump() overridden by configuration")
        void setBumpOverrideByConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setBump("gamma");
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State state = new State(configuration);
            assertEquals("gamma", state.getBump());
            assertThrows(IllegalStateException.class, () -> state.setBump("any value"));
        }

        @Test
        @DisplayName("State.getChangelog()")
        void getChangelogTest()
            throws Exception {
            // make sure the changelog is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getChangelog());
            Changelog changelog = new Changelog();
            state.setChangelog(changelog);
            assertSame(changelog, state.getChangelog());
        }

        @Test
        @DisplayName("State.setChangelog()")
        void setChangelogTest()
            throws Exception {
            // make sure the changelog is null in the beginning (it's set only after the Infer task has run)
            State state = new State(new Configuration());
            assertNull(state.getChangelog());
            Changelog changelog = new Changelog();
            state.setChangelog(changelog);
            assertSame(changelog, state.getChangelog());
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
            state.setReleaseType(new ReleaseType(null, true, null, null, null, Boolean.FALSE.toString(), null, Boolean.FALSE.toString(), Boolean.FALSE.toString(), null, null, null, null, null, /*this is the 'publish' flag -> */ Boolean.TRUE.toString(), null, Boolean.FALSE));
            state.setVersion("1.2.3");
            state.getReleaseScope().setPreviousVersion("1.2.3");
            assertFalse(state.getNewVersion());
            assertFalse(state.getNewRelease());

            state.getReleaseScope().setPreviousVersion("0.1.0");
            assertTrue(state.getNewVersion());
            assertTrue(state.getNewRelease());

            // now replace the releaseType with the 'publish' flag to FALSE
            state.setReleaseType(new ReleaseType(null, true, null, null, null, Boolean.FALSE.toString(), null, Boolean.FALSE.toString(), Boolean.FALSE.toString(), null, null, null, null, null, /*this is the 'publish' flag -> */ Boolean.FALSE.toString(), null, Boolean.FALSE));

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
        @DisplayName("State.getReleaseAssets()")
        void getReleaseAssetsTest()
            throws Exception {
            State state = new State(new Configuration());

            // make sure the release type is never null but empty upon initialization
            assertNotNull(state.getReleaseAssets());
            assertTrue(state.getReleaseAssets().isEmpty());
            state.getReleaseAssets().add(new Attachment("f1", "d1", "t1", "p1"));
            state.getReleaseAssets().add(new Attachment("f2", "d2", "t2", "p2"));
            assertEquals(2, state.getReleaseAssets().size());
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
        @DisplayName("State.getVersion() overridden by configuration")
        void getVersionOverrideByConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setVersion("1.2.3");
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State state = new State(configuration);

            assertEquals("1.2.3", state.getVersion());
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
        @DisplayName("State.setVersion() overridden by configuration")
        void setVersionOverrideByConfigurationTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setVersion("1.2.3");
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State state = new State(configuration);
            assertEquals("1.2.3", state.getVersion());
            assertThrows(IllegalStateException.class, () -> state.setVersion("2.3.4"));
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

            state.setVersionRange(".*");
            assertEquals(".*", state.getVersionRange());
        }
    }

    @Nested
    @DisplayName("Save and Resume")
    class ResumeTests {
        @Test
        @DisplayName("Save and Load JSON state")
        void saveAndResumeJSONTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS)
                )
            );
            configurationLayerMock.setResume(Boolean.TRUE);
            configurationLayerMock.setStateFile(new File(System.getProperty("java.io.tmpdir"), "state"+this.hashCode()+".json").getAbsolutePath());
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State oldState = new State(configuration);

            Commit initialCommit = new Commit("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, List.<String>of(), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580515200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580515200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("initial commit", "initial commit", Map.<String,String>of()), Set.<Tag>of());
            Commit finalCommit = new Commit("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, List.<String>of("b50926577d36f403f4b3ebf51dfe34660b52eaa2"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580601600), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580601600), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("final commit", "final commit", Map.<String,String>of()), Set.<Tag>of());

            // set a few values to use later on for comparison
            oldState.setBump("minor");
            oldState.setChangelog(new Changelog());
            oldState.getChangelog().setReleases(List.<Changelog.Release>of(new Changelog.Release("MyRelease", "today")));
            oldState.getChangelog().getReleases().get(0).setSections(List.<Changelog.Release.Section>of(new Changelog.Release.Section("MySection")));
            oldState.setVersion("3.5.7");
            oldState.setVersionRange(".*");
            oldState.getInternals().put("attr1", "value1");
            oldState.getReleaseScope().getCommits().add(finalCommit);
            oldState.getReleaseScope().getCommits().add(initialCommit);
            oldState.getReleaseScope().setPreviousVersion("4.5.6");
            oldState.getReleaseScope().setPreviousVersionCommit(new Commit("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, List.<String>of("c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577833200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577833200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("fix: a commit that fixes something", "fix: a commit that fixes something", Map.<String,String>of()), Set.<Tag>of(new Tag("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false))));
            oldState.getReleaseScope().setPrimeVersion("1.0.0");
            oldState.getReleaseScope().setPrimeVersionCommit(new Commit("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, List.<String>of("2e348e90e5e1b89c678555459aecbfc34e17ef44"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577836800), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577836800), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("feat: a commit that adds a feature", "feat: a commit that adds a feature", Map.<String,String>of()), Set.<Tag>of(new Tag("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false))));
            oldState.setReleaseType(new ReleaseType());
            oldState.getReleaseType().setCollapseVersions(true);
            oldState.getReleaseType().setCollapsedVersionQualifier("rel");
            oldState.getReleaseType().setDescription("Some description");
            oldState.getReleaseType().setGitCommit(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitCommitMessage("Commit message");
            oldState.getReleaseType().setGitPush(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTag(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTagMessage("Tag message");
            oldState.getReleaseType().setIdentifiers(List.<Identifier>of(new Identifier("b", "12", Identifier.Position.BUILD)));
            oldState.getReleaseType().setMatchBranches(".*");
            oldState.getReleaseType().setMatchEnvironmentVariables(Map.<String,String>of("USER", ".*", "PATH", ".*"));
            oldState.getReleaseType().setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            oldState.getReleaseType().setPublish(Boolean.TRUE.toString());
            oldState.getReleaseType().setVersionRange("1.x");
            oldState.getReleaseType().setVersionRangeFromBranchName(Boolean.FALSE);

            // save the file
            FileMapper.save(configurationLayerMock.getStateFile(), oldState);
            File stateFile = new File(configurationLayerMock.getStateFile());
            assertTrue(stateFile.exists());

            // print the file to standard output for inspection purpose
            System.out.println("------ JSON state ------");
            System.out.println("Loading from: "+stateFile.getAbsolutePath());
            System.out.println("-----------------------------------------");
            System.out.println(readFile(stateFile));
            System.out.println("-----------------------------------------");
            System.out.flush();

            // now we are ready to resume the file
            State resumedState = State.resume(new File(configurationLayerMock.getStateFile()), configuration);
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
            assertEquals(oldState.getReleaseScope().getFinalCommit().getSHA(), resumedState.getReleaseScope().getFinalCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getInitialCommit().getSHA(), resumedState.getReleaseScope().getInitialCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getPreviousVersion(), resumedState.getReleaseScope().getPreviousVersion());
            assertEquals(oldState.getReleaseScope().getPreviousVersionCommit().getSHA(), resumedState.getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getPrimeVersion(), resumedState.getReleaseScope().getPrimeVersion());
            assertEquals(oldState.getReleaseScope().getPrimeVersionCommit().getSHA(), resumedState.getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(oldState.getTimestamp(), resumedState.getTimestamp());
            assertEquals(oldState.getVersion(), resumedState.getVersion());
            assertEquals(oldState.getVersionRange(), resumedState.getVersionRange());
            assertEquals(oldState.getReleaseType().getCollapseVersions(), resumedState.getReleaseType().getCollapseVersions());
            assertEquals(oldState.getReleaseType().getCollapsedVersionQualifier(), resumedState.getReleaseType().getCollapsedVersionQualifier());
            assertEquals(oldState.getReleaseType().getDescription(), resumedState.getReleaseType().getDescription());
            assertEquals(oldState.getReleaseType().getGitCommit(), resumedState.getReleaseType().getGitCommit());
            assertEquals(oldState.getReleaseType().getGitCommitMessage(), resumedState.getReleaseType().getGitCommitMessage());
            assertEquals(oldState.getReleaseType().getGitPush(), resumedState.getReleaseType().getGitPush());
            assertEquals(oldState.getReleaseType().getGitTag(), resumedState.getReleaseType().getGitTag());
            assertEquals(oldState.getReleaseType().getGitTagMessage(), resumedState.getReleaseType().getGitTagMessage());
            if (Objects.isNull(oldState.getReleaseType().getIdentifiers())) {
                assertEquals(oldState.getReleaseType().getIdentifiers(), resumedState.getReleaseType().getIdentifiers());
            }
            else {
                for (int i=0; i<oldState.getReleaseType().getIdentifiers().size(); i++) {
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getQualifier(), resumedState.getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getValue(), resumedState.getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getPosition(), resumedState.getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(oldState.getReleaseType().getMatchBranches(), resumedState.getReleaseType().getMatchBranches());
            assertEquals(oldState.getReleaseType().getMatchEnvironmentVariables().size(), resumedState.getReleaseType().getMatchEnvironmentVariables().size());
            assertEquals(oldState.getReleaseType().getMatchWorkspaceStatus(), resumedState.getReleaseType().getMatchWorkspaceStatus());
            assertEquals(oldState.getReleaseType().getPublish(), resumedState.getReleaseType().getPublish());
            assertEquals(oldState.getReleaseType().getVersionRange(), resumedState.getReleaseType().getVersionRange());
            assertEquals(oldState.getReleaseType().getVersionRangeFromBranchName(), resumedState.getReleaseType().getVersionRangeFromBranchName());

            // finally also test transient attributes, which should render to the same value even if they are computed on the fly
            assertEquals(oldState.getDirectory(), resumedState.getDirectory());
            assertEquals(oldState.getNewVersion(), resumedState.getNewVersion());
            assertEquals(oldState.getNewRelease(), resumedState.getNewRelease());
            assertEquals(oldState.getScheme(), resumedState.getScheme());
        }

        @Test
        @DisplayName("Save and Load YAML state")
        void saveAndResumeYAMLTest()
            throws Exception {
            Configuration configuration = new Configuration();
            SimpleConfigurationLayer configurationLayerMock = new SimpleConfigurationLayer();
            configurationLayerMock.setCommitMessageConventions(
                new CommitMessageConventions(
                    List.<String>of("conventionalCommits"),
                    Map.<String,CommitMessageConvention>of("conventionalCommits", com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS)
                )
            );
            configurationLayerMock.setResume(Boolean.TRUE);
            configurationLayerMock.setStateFile(new File(System.getProperty("java.io.tmpdir"), "state"+this.hashCode()+".yaml").getAbsolutePath());
            configuration.withRuntimeConfiguration(configurationLayerMock);
            State oldState = new State(configuration);

            Commit initialCommit = new Commit("b50926577d36f403f4b3ebf51dfe34660b52eaa2", 1580515200, List.<String>of(), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580515200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580515200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("initial commit", "initial commit", Map.<String,String>of()), Set.<Tag>of());
            Commit finalCommit = new Commit("e6b1c65eac4d81aadde22e796bb2a8e48da4c5d9", 1580515200, List.<String>of("b50926577d36f403f4b3ebf51dfe34660b52eaa2"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580601600), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1580601600), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("final commit", "final commit", Map.<String,String>of()), Set.<Tag>of());

            // set a few values to use later on for comparison
            oldState.setBump("minor");
            oldState.setChangelog(new Changelog());
            oldState.getChangelog().setReleases(List.<Changelog.Release>of(new Changelog.Release("MyRelease", "today")));
            oldState.getChangelog().getReleases().get(0).setSections(List.<Changelog.Release.Section>of(new Changelog.Release.Section("MySection")));
            oldState.setVersion("3.5.7");
            oldState.setVersionRange(".*");
            oldState.getInternals().put("attr1", "value1");
            oldState.getReleaseScope().getCommits().add(finalCommit);
            oldState.getReleaseScope().getCommits().add(initialCommit);
            oldState.getReleaseScope().setPreviousVersion("4.5.6");
            oldState.getReleaseScope().setPreviousVersionCommit(new Commit("05cbfd58fadbec3d96b220a0054d96875aa37011", 1577833200, List.<String>of("c97e4b3d0ffed8405a6b50460a1bf0177f0fde1f"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577833200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577833200), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("fix: a commit that fixes something", "fix: a commit that fixes something", Map.<String,String>of()), Set.<Tag>of(new Tag("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false))));
            oldState.getReleaseScope().setPrimeVersion("1.0.0");
            oldState.getReleaseScope().setPrimeVersionCommit(new Commit("e8fa442504d91a0187865c74093a5a4212a805f9", 1577836800, List.<String>of("2e348e90e5e1b89c678555459aecbfc34e17ef44"), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577836800), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Action(new Identity("Jim", "jim@example.com"), new TimeStamp(new Date(1577836800), new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "America/Los_Angeles"))), new Message("feat: a commit that adds a feature", "feat: a commit that adds a feature", Map.<String,String>of()), Set.<Tag>of(new Tag("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false))));
            oldState.setReleaseType(new ReleaseType());
            oldState.getReleaseType().setCollapseVersions(true);
            oldState.getReleaseType().setCollapsedVersionQualifier("rel");
            oldState.getReleaseType().setDescription("Some description");
            oldState.getReleaseType().setGitCommit(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitCommitMessage("Commit message");
            oldState.getReleaseType().setGitPush(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTag(Boolean.TRUE.toString());
            oldState.getReleaseType().setGitTagMessage("Tag message");
            oldState.getReleaseType().setIdentifiers(List.<Identifier>of(new Identifier("b", "12", Identifier.Position.BUILD)));
            oldState.getReleaseType().setMatchBranches(".*");
            oldState.getReleaseType().setMatchEnvironmentVariables(Map.<String,String>of("USER", ".*", "PATH", ".*"));
            oldState.getReleaseType().setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            oldState.getReleaseType().setPublish(Boolean.TRUE.toString());
            oldState.getReleaseType().setVersionRange("1.x");
            oldState.getReleaseType().setVersionRangeFromBranchName(Boolean.FALSE);

            // save the file
            FileMapper.save(configurationLayerMock.getStateFile(), oldState);
            File stateFile = new File(configurationLayerMock.getStateFile());
            assertTrue(stateFile.exists());

            // print the file to standard output for inspection purpose
            System.out.println("------ YAML state ------");
            System.out.println("Loading from: "+stateFile.getAbsolutePath());
            System.out.println("-----------------------------------------");
            System.out.println(readFile(stateFile));
            System.out.println("-----------------------------------------");
            System.out.flush();

            // now we are ready to resume the file
            State resumedState = State.resume(new File(configurationLayerMock.getStateFile()), configuration);
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
            assertEquals("value1", resumedState.getInternals().get("attr1"));
            assertEquals(oldState.getReleaseScope().getFinalCommit().getSHA(), resumedState.getReleaseScope().getFinalCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getInitialCommit().getSHA(), resumedState.getReleaseScope().getInitialCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getPreviousVersion(), resumedState.getReleaseScope().getPreviousVersion());
            assertEquals(oldState.getReleaseScope().getPreviousVersionCommit().getSHA(), resumedState.getReleaseScope().getPreviousVersionCommit().getSHA());
            assertEquals(oldState.getReleaseScope().getPrimeVersion(), resumedState.getReleaseScope().getPrimeVersion());
            assertEquals(oldState.getReleaseScope().getPrimeVersionCommit().getSHA(), resumedState.getReleaseScope().getPrimeVersionCommit().getSHA());
            assertEquals(oldState.getTimestamp(), resumedState.getTimestamp());
            assertEquals(oldState.getVersion(), resumedState.getVersion());
            assertEquals(oldState.getVersionRange(), resumedState.getVersionRange());
            assertEquals(oldState.getReleaseType().getCollapseVersions(), resumedState.getReleaseType().getCollapseVersions());
            assertEquals(oldState.getReleaseType().getDescription(), resumedState.getReleaseType().getDescription());
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
                for (int i=0; i<oldState.getReleaseType().getIdentifiers().size(); i++) {
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getQualifier(), resumedState.getReleaseType().getIdentifiers().get(i).getQualifier());
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getValue(), resumedState.getReleaseType().getIdentifiers().get(i).getValue());
                    assertEquals(oldState.getReleaseType().getIdentifiers().get(i).getPosition(), resumedState.getReleaseType().getIdentifiers().get(i).getPosition());
                }
            }
            assertEquals(oldState.getReleaseType().getMatchBranches(), resumedState.getReleaseType().getMatchBranches());
            assertEquals(oldState.getReleaseType().getMatchEnvironmentVariables().size(), resumedState.getReleaseType().getMatchEnvironmentVariables().size());
            assertEquals(oldState.getReleaseType().getMatchWorkspaceStatus(), resumedState.getReleaseType().getMatchWorkspaceStatus());
            assertEquals(oldState.getReleaseType().getPublish(), resumedState.getReleaseType().getPublish());
            assertEquals(oldState.getReleaseType().getVersionRange(), resumedState.getReleaseType().getVersionRange());
            assertEquals(oldState.getReleaseType().getVersionRangeFromBranchName(), resumedState.getReleaseType().getVersionRangeFromBranchName());

            // finally also test transient attributes, which should render to the same value even if they are computed on the fly
            assertEquals(oldState.getDirectory(), resumedState.getDirectory());
            assertEquals(oldState.getNewVersion(), resumedState.getNewVersion());
            assertEquals(oldState.getNewRelease(), resumedState.getNewRelease());
            assertEquals(oldState.getScheme(), resumedState.getScheme());
        }
    }
}