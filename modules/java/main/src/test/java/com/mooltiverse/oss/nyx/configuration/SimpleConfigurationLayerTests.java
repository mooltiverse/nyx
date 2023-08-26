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
package com.mooltiverse.oss.nyx.configuration;

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;

import java.nio.file.Files;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.AuthenticationMethod;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Substitution;
import com.mooltiverse.oss.nyx.entities.Substitutions;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.version.Scheme;

@DisplayName("SimpleConfigurationLayer")
public class SimpleConfigurationLayerTests {
    @Test
    @DisplayName("SimpleConfigurationLayer.getBump()")
    void getBumpTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getBump());

        simpleConfigurationLayer.setBump("b");
        assertEquals("b", simpleConfigurationLayer.getBump());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getChangelog()")
    void getChangelogTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getChangelog());
        assertNull(simpleConfigurationLayer.getChangelog().getPath());
        assertTrue(simpleConfigurationLayer.getChangelog().getSections().isEmpty());
        assertTrue(simpleConfigurationLayer.getChangelog().getSubstitutions().isEmpty());
        assertNull(simpleConfigurationLayer.getChangelog().getTemplate());

        simpleConfigurationLayer.setChangelog(
            new ChangelogConfiguration("head", "CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Map.<String,String>of("Expression1", "string1"))
        );

        assertNotNull(simpleConfigurationLayer.getChangelog());
        assertEquals("head", simpleConfigurationLayer.getChangelog().getAppend());
        assertEquals("CHANGELOG.md", simpleConfigurationLayer.getChangelog().getPath());
        assertFalse(simpleConfigurationLayer.getChangelog().getSections().isEmpty());
        assertEquals(2, simpleConfigurationLayer.getChangelog().getSections().size());
        assertTrue(simpleConfigurationLayer.getChangelog().getSections().containsKey("Section1"));
        assertEquals("regex1", simpleConfigurationLayer.getChangelog().getSections().get("Section1"));
        assertTrue(simpleConfigurationLayer.getChangelog().getSections().containsKey("Section2"));
        assertEquals("regex2", simpleConfigurationLayer.getChangelog().getSections().get("Section2"));
        assertFalse(simpleConfigurationLayer.getChangelog().getSubstitutions().isEmpty());
        assertEquals(1, simpleConfigurationLayer.getChangelog().getSubstitutions().size());
        assertTrue(simpleConfigurationLayer.getChangelog().getSubstitutions().containsKey("Expression1"));
        assertEquals("string1", simpleConfigurationLayer.getChangelog().getSubstitutions().get("Expression1"));
        assertEquals("changelog.tpl", simpleConfigurationLayer.getChangelog().getTemplate());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getCommitMessageConventions()")
    void getCommitMessageConventionsTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getEnabled().isEmpty());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getItems().isEmpty());

        simpleConfigurationLayer.setCommitMessageConventions(
            new CommitMessageConventions(
                List.<String>of("one", "two"),
                Map.<String,CommitMessageConvention>of("one", new CommitMessageConvention(), "two", new CommitMessageConvention())
            )
        );

        assertEquals(2, simpleConfigurationLayer.getCommitMessageConventions().getEnabled().size());
        assertTrue(simpleConfigurationLayer.getCommitMessageConventions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, simpleConfigurationLayer.getCommitMessageConventions().getItems().size());
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions().getItems().get("one"));
        assertNotNull(simpleConfigurationLayer.getCommitMessageConventions().getItems().get("two"));
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getConfigurationFile()")
    void getConfigurationFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getConfigurationFile());

        simpleConfigurationLayer.setConfigurationFile("config.yml");
        assertEquals("config.yml", simpleConfigurationLayer.getConfigurationFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getDirectory()")
    void getDirectoryTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getDirectory());

        File directory = Files.createTempDirectory(null).toFile();
        directory.deleteOnExit();
        simpleConfigurationLayer.setDirectory(directory.getAbsolutePath());
        assertEquals(directory.getAbsolutePath(), new File(simpleConfigurationLayer.getDirectory()).getAbsolutePath());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getDryRun()")
    void getDryRunTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getDryRun());

        simpleConfigurationLayer.setDryRun(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getDryRun());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getGit()")
    void getGitTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getGit());
        assertTrue(simpleConfigurationLayer.getGit().getRemotes().isEmpty());

        simpleConfigurationLayer.setGit(
            new GitConfiguration(
                Map.<String,GitRemoteConfiguration>of("origin1", new GitRemoteConfiguration(AuthenticationMethod.USER_PASSWORD, "jdoe1", "pwd1", "pk1", "pp1"), "origin2", new GitRemoteConfiguration(AuthenticationMethod.PUBLIC_KEY, "jdoe2", "pwd2", "pk2", "pp2"))
            )
        );

        assertEquals(2, simpleConfigurationLayer.getGit().getRemotes().size());
        assertNotNull(simpleConfigurationLayer.getGit().getRemotes().get("origin1"));
        assertEquals(AuthenticationMethod.USER_PASSWORD, simpleConfigurationLayer.getGit().getRemotes().get("origin1").getAuthenticationMethod());
        assertEquals("pwd1", simpleConfigurationLayer.getGit().getRemotes().get("origin1").getPassword());
        assertEquals("jdoe1", simpleConfigurationLayer.getGit().getRemotes().get("origin1").getUser());
        assertEquals("pk1", simpleConfigurationLayer.getGit().getRemotes().get("origin1").getPrivateKey());
        assertEquals("pp1", simpleConfigurationLayer.getGit().getRemotes().get("origin1").getPassphrase());
        assertNotNull(simpleConfigurationLayer.getGit().getRemotes().get("origin2"));
        assertEquals(AuthenticationMethod.PUBLIC_KEY, simpleConfigurationLayer.getGit().getRemotes().get("origin2").getAuthenticationMethod());
        assertEquals("pwd2", simpleConfigurationLayer.getGit().getRemotes().get("origin2").getPassword());
        assertEquals("jdoe2", simpleConfigurationLayer.getGit().getRemotes().get("origin2").getUser());
        assertEquals("pk2", simpleConfigurationLayer.getGit().getRemotes().get("origin2").getPrivateKey());
        assertEquals("pp2", simpleConfigurationLayer.getGit().getRemotes().get("origin2").getPassphrase());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getInitialVersion()")
    void getInitialVersionTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getInitialVersion());

        simpleConfigurationLayer.setInitialVersion("0.3.5");
        assertEquals("0.3.5", simpleConfigurationLayer.getInitialVersion());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getPreset()")
    void getPresetTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getPreset());

        simpleConfigurationLayer.setPreset("simple");
        assertEquals("simple", simpleConfigurationLayer.getPreset());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleaseAssets()")
    void getReleaseAssetsTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getReleaseAssets());
        assertTrue(simpleConfigurationLayer.getReleaseAssets().isEmpty());

        simpleConfigurationLayer.setReleaseAssets(
            Map.<String,Attachment>of(
                "asset1", new Attachment("asset.txt", "Text Asset", "text/plain", "asset.txt"),
                "asset2", new Attachment("asset.bin", "Binary Asset", "application/octet-stream", "asset.bin")
            )
        );

        assertEquals(2, simpleConfigurationLayer.getReleaseAssets().size());
        assertTrue(simpleConfigurationLayer.getReleaseAssets().containsKey("asset1"));
        assertTrue(simpleConfigurationLayer.getReleaseAssets().containsKey("asset2"));
        assertEquals("asset.txt", simpleConfigurationLayer.getReleaseAssets().get("asset1").getFileName());
        assertEquals("Text Asset", simpleConfigurationLayer.getReleaseAssets().get("asset1").getDescription());
        assertEquals("text/plain", simpleConfigurationLayer.getReleaseAssets().get("asset1").getType());
        assertEquals("asset.txt", simpleConfigurationLayer.getReleaseAssets().get("asset1").getPath());
        assertEquals("asset.bin", simpleConfigurationLayer.getReleaseAssets().get("asset2").getFileName());
        assertEquals("Binary Asset", simpleConfigurationLayer.getReleaseAssets().get("asset2").getDescription());
        assertEquals("application/octet-stream", simpleConfigurationLayer.getReleaseAssets().get("asset2").getType());
        assertEquals("asset.bin", simpleConfigurationLayer.getReleaseAssets().get("asset2").getPath());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleaseLenient()")
    void getReleaseLenientTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getReleaseLenient());

        simpleConfigurationLayer.setReleaseLenient(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getReleaseLenient());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleasePrefix()")
    void getReleasePrefixTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getReleasePrefix());

        simpleConfigurationLayer.setReleasePrefix("prefix");
        assertEquals("prefix", simpleConfigurationLayer.getReleasePrefix());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getReleaseTypes()")
    void getReleaseTypesTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getReleaseTypes());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getEnabled().isEmpty());
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getItems().isEmpty());

        simpleConfigurationLayer.setReleaseTypes(
            new ReleaseTypes(
                List.<String>of("one", "two"),
                List.<String>of("first", "second"),
                List.<String>of("origin", "replica"),
                Map.<String,ReleaseType>of("one", new ReleaseType(), "two", new ReleaseType())
            )
        );

        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getEnabled().size());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getPublicationServices().size());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getPublicationServices().containsAll(List.<String>of("first", "second")));
        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getRemoteRepositories().size());
        assertTrue(simpleConfigurationLayer.getReleaseTypes().getRemoteRepositories().containsAll(List.<String>of("origin", "replica")));
        assertEquals(2, simpleConfigurationLayer.getReleaseTypes().getItems().size());
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("one"));
        assertNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("one").getAssets());
        assertNotNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("two"));
        assertNull(simpleConfigurationLayer.getReleaseTypes().getItems().get("two").getAssets());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getResume()")
    void getResumeTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getResume());

        simpleConfigurationLayer.setResume(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getResume());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getScheme()")
    void getSchemeTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getScheme());

        simpleConfigurationLayer.setScheme(Scheme.SEMVER);
        assertEquals(Scheme.SEMVER, simpleConfigurationLayer.getScheme());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getServices()")
    void getServicesTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getServices());
        assertTrue(simpleConfigurationLayer.getServices().isEmpty());

        simpleConfigurationLayer.setServices(
            Map.<String,ServiceConfiguration>of(
                "github", new ServiceConfiguration(Provider.GITHUB,Map.<String,String>of(
                    "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}",
                    "REPOSITORY_NAME", "repo1",
                    "REPOSITORY_OWNER", "owner1"
                    )),
                "gitlab", new ServiceConfiguration(Provider.GITLAB,Map.<String,String>of(
                    "AUTHENTICATION_TOKEN", "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}",
                    "REPOSITORY_NAME", "repo2",
                    "REPOSITORY_OWNER", "owner2"
                ))
            )
        );

        assertEquals(2, simpleConfigurationLayer.getServices().size());
        assertTrue(simpleConfigurationLayer.getServices().containsKey("github"));
        assertTrue(simpleConfigurationLayer.getServices().containsKey("gitlab"));
        assertEquals(Provider.GITHUB, simpleConfigurationLayer.getServices().get("github").getType());
        assertEquals(3, simpleConfigurationLayer.getServices().get("github").getOptions().size());
        assertEquals("{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", simpleConfigurationLayer.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals("repo1", simpleConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
        assertEquals("owner1", simpleConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
        assertEquals(Provider.GITLAB, simpleConfigurationLayer.getServices().get("gitlab").getType());
        assertEquals(3, simpleConfigurationLayer.getServices().get("gitlab").getOptions().size());
        assertEquals("{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", simpleConfigurationLayer.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals("repo2", simpleConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
        assertEquals("owner2", simpleConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getSharedConfigurationFile()")
    void getSharedConfigurationFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getSharedConfigurationFile());

        simpleConfigurationLayer.setSharedConfigurationFile("config.yml");
        assertEquals("config.yml", simpleConfigurationLayer.getSharedConfigurationFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getStateFile()")
    void getStateFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getStateFile());

        simpleConfigurationLayer.setStateFile("state.yml");
        assertEquals("state.yml", simpleConfigurationLayer.getStateFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getSubstitutions()")
    void getSubstitutionsTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNotNull(simpleConfigurationLayer.getSubstitutions());
        assertTrue(simpleConfigurationLayer.getSubstitutions().getEnabled().isEmpty());
        assertTrue(simpleConfigurationLayer.getSubstitutions().getItems().isEmpty());

        simpleConfigurationLayer.setSubstitutions(
            new Substitutions(
                List.<String>of("one", "two"),
                Map.<String,Substitution>of("one", new Substitution(), "two", new Substitution())
            )
        );

        assertEquals(2, simpleConfigurationLayer.getSubstitutions().getEnabled().size());
        assertTrue(simpleConfigurationLayer.getSubstitutions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, simpleConfigurationLayer.getSubstitutions().getItems().size());
        assertNotNull(simpleConfigurationLayer.getSubstitutions().getItems().get("one"));
        assertNotNull(simpleConfigurationLayer.getSubstitutions().getItems().get("two"));
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getSummary()")
    void getSummaryTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getSummary());

        simpleConfigurationLayer.setSummary(Boolean.TRUE);
        assertEquals(Boolean.TRUE, simpleConfigurationLayer.getSummary());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getSummaryFile()")
    void getSummaryFileTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getSummaryFile());

        simpleConfigurationLayer.setSummaryFile("summary.txt");
        assertEquals("summary.txt", simpleConfigurationLayer.getSummaryFile());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getVerbosity()")
    void getVerbosityTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getVerbosity());

        simpleConfigurationLayer.setVerbosity(Verbosity.INFO);
        assertEquals(Verbosity.INFO, simpleConfigurationLayer.getVerbosity());
    }

    @Test
    @DisplayName("SimpleConfigurationLayer.getVersion()")
    void getVersionTest()
        throws Exception {
        SimpleConfigurationLayer simpleConfigurationLayer = new SimpleConfigurationLayer();
        assertNull(simpleConfigurationLayer.getVersion());

        simpleConfigurationLayer.setVersion("3.5.7");
        assertEquals("3.5.7", simpleConfigurationLayer.getVersion());
    }
}