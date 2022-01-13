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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.services.Provider;
import com.mooltiverse.oss.nyx.version.Scheme;

@DisplayName("EnvironmentConfigurationLayer")
public class EnvironmentConfigurationLayerTests {
    @Test
    @DisplayName("EnvironmentConfigurationLayer.getBump()")
    void getBumpTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getBump());

        environmentConfigurationLayer.environment.put("NYX_BUMP", "b");
        assertEquals("b", environmentConfigurationLayer.getBump());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getChangelog()")
    void getChangelogTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNotNull(environmentConfigurationLayer.getChangelog());
        assertNull(environmentConfigurationLayer.getChangelog().getCommitLink());
        assertNull(environmentConfigurationLayer.getChangelog().getContributorLink());
        assertNull(environmentConfigurationLayer.getChangelog().getIncludeUnreleased());
        assertNull(environmentConfigurationLayer.getChangelog().getIssueID());
        assertNull(environmentConfigurationLayer.getChangelog().getIssueLink());
        assertNull(environmentConfigurationLayer.getChangelog().getPath());
        assertTrue(environmentConfigurationLayer.getChangelog().getSections().isEmpty());
        assertNull(environmentConfigurationLayer.getChangelog().getTemplate());

        // get a new instance or a stale object is returned by getChangelog()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_PATH", "CHANGELOG.md");

        assertNull(environmentConfigurationLayer.getChangelog().getCommitLink());
        assertNull(environmentConfigurationLayer.getChangelog().getContributorLink());
        assertNull(environmentConfigurationLayer.getChangelog().getIncludeUnreleased());
        assertNull(environmentConfigurationLayer.getChangelog().getIssueID());
        assertNull(environmentConfigurationLayer.getChangelog().getIssueLink());
        assertEquals("CHANGELOG.md", environmentConfigurationLayer.getChangelog().getPath());
        assertTrue(environmentConfigurationLayer.getChangelog().getSections().isEmpty());
        assertNull(environmentConfigurationLayer.getChangelog().getTemplate());
        
        // get a new instance or a stale object is returned by getChangelog()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_COMMIT_LINK", "commitLink");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_CONTRIBUTOR_LINK", "contributorLink");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_INCLUDE_UNRELEASED", Boolean.TRUE.toString());
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_ISSUE_ID", "issueID");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_ISSUE_LINK", "issueLink");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_PATH", "CHANGELOG.md");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_SECTIONS_Section1", "regex1");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_SECTIONS_Section2", "regex2");
        environmentConfigurationLayer.environment.put("NYX_CHANGELOG_TEMPLATE", "changelog.tpl");

        assertEquals("commitLink", environmentConfigurationLayer.getChangelog().getCommitLink());
        assertEquals("contributorLink", environmentConfigurationLayer.getChangelog().getContributorLink());
        assertEquals(Boolean.TRUE, environmentConfigurationLayer.getChangelog().getIncludeUnreleased());
        assertEquals("issueID", environmentConfigurationLayer.getChangelog().getIssueID());
        assertEquals("issueLink", environmentConfigurationLayer.getChangelog().getIssueLink());
        assertEquals("CHANGELOG.md", environmentConfigurationLayer.getChangelog().getPath());
        assertEquals(2, environmentConfigurationLayer.getChangelog().getSections().size());
        assertTrue(environmentConfigurationLayer.getChangelog().getSections().containsKey("Section1"));
        assertEquals("regex1", environmentConfigurationLayer.getChangelog().getSections().get("Section1"));
        assertTrue(environmentConfigurationLayer.getChangelog().getSections().containsKey("Section2"));
        assertEquals("regex2", environmentConfigurationLayer.getChangelog().getSections().get("Section2"));
        assertEquals("changelog.tpl", environmentConfigurationLayer.getChangelog().getTemplate());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getCommitMessageConventions()")
    void getCommitMessageConventionsTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNotNull(environmentConfigurationLayer.getCommitMessageConventions());
        assertTrue(environmentConfigurationLayer.getCommitMessageConventions().getEnabled().isEmpty());
        assertTrue(environmentConfigurationLayer.getCommitMessageConventions().getItems().isEmpty());

        // get a new instance or a stale object is returned by getCommitMessageConventions()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED", "one,two");

        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getCommitMessageConventions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(0, environmentConfigurationLayer.getCommitMessageConventions().getItems().size());
        
        // get a new instance or a stale object is returned by getCommitMessageConventions()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED", "one,two");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_one_EXPRESSION", "");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_two_EXPRESSION", "");

        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getCommitMessageConventions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getItems().size());
        assertNotNull(environmentConfigurationLayer.getCommitMessageConventions().getItems().get("one"));
        assertNotNull(environmentConfigurationLayer.getCommitMessageConventions().getItems().get("two"));

        // get a new instance or a stale object is returned by getCommitMessageConventions()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED", "one,two");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_one_EXPRESSION", "expr1");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_one_BUMP_EXPRESSIONS_alpha", "alpha1");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_two_EXPRESSION", "expr2");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_two_BUMP_EXPRESSIONS_beta", "beta1");
        environmentConfigurationLayer.environment.put("NYX_COMMIT_MESSAGE_CONVENTIONS_two_BUMP_EXPRESSIONS_gamma", "gamma1");

        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getCommitMessageConventions().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getItems().size());
        assertEquals("expr1", environmentConfigurationLayer.getCommitMessageConventions().getItems().get("one").getExpression());
        assertEquals(1, environmentConfigurationLayer.getCommitMessageConventions().getItems().get("one").getBumpExpressions().size());
        assertEquals("alpha1", environmentConfigurationLayer.getCommitMessageConventions().getItems().get("one").getBumpExpressions().get("alpha"));
        assertEquals("expr2", environmentConfigurationLayer.getCommitMessageConventions().getItems().get("two").getExpression());
        assertEquals(2, environmentConfigurationLayer.getCommitMessageConventions().getItems().get("two").getBumpExpressions().size());
        assertEquals("beta1", environmentConfigurationLayer.getCommitMessageConventions().getItems().get("two").getBumpExpressions().get("beta"));
        assertEquals("gamma1", environmentConfigurationLayer.getCommitMessageConventions().getItems().get("two").getBumpExpressions().get("gamma"));
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getConfigurationFile()")
    void getConfigurationFileTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getConfigurationFile());

        environmentConfigurationLayer.environment.put("NYX_CONFIGURATION_FILE", "config.yml");
        assertEquals("config.yml", environmentConfigurationLayer.getConfigurationFile());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getDirectory()")
    void getDirectoryTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getDirectory());

        File directory = Files.createTempDirectory(null).toFile();
        environmentConfigurationLayer.environment.put("NYX_DIRECTORY", directory.getAbsolutePath());
        assertEquals(directory.getAbsolutePath(), new File(environmentConfigurationLayer.getDirectory()).getAbsolutePath());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getDryRun()")
    void getDryRunTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getDryRun());

        environmentConfigurationLayer.environment.put("NYX_DRY_RUN", "true");
        assertEquals(Boolean.TRUE, environmentConfigurationLayer.getDryRun());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getGit()")
    void getGitTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNotNull(environmentConfigurationLayer.getGit());
        assertTrue(environmentConfigurationLayer.getGit().getRemotes().isEmpty());

        // get a new instance or a stale object is returned by getGit()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_one_USER", "jdoe");
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_two_USER", "stiger");

        assertEquals(2, environmentConfigurationLayer.getGit().getRemotes().size());
        assertNotNull(environmentConfigurationLayer.getGit().getRemotes().get("one"));
        assertNotNull(environmentConfigurationLayer.getGit().getRemotes().get("two"));

        // get a new instance or a stale object is returned by getGit()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_one_USER", "jdoe");
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_one_PASSWORD", "pwd");
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_two_USER", "stiger");
        environmentConfigurationLayer.environment.put("NYX_GIT_REMOTES_two_PASSWORD", "sct");

        assertEquals(2, environmentConfigurationLayer.getGit().getRemotes().size());
        assertEquals("pwd", environmentConfigurationLayer.getGit().getRemotes().get("one").getPassword());
        assertEquals("jdoe", environmentConfigurationLayer.getGit().getRemotes().get("one").getUser());
        assertEquals("sct", environmentConfigurationLayer.getGit().getRemotes().get("two").getPassword());
        assertEquals("stiger", environmentConfigurationLayer.getGit().getRemotes().get("two").getUser());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getInitialVersion()")
    void getInitialVersionTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getInitialVersion());

        environmentConfigurationLayer.environment.put("NYX_INITIAL_VERSION", "0.3.5");
        assertEquals("0.3.5", environmentConfigurationLayer.getInitialVersion());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getPreset()")
    void getPresetTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getPreset());

        environmentConfigurationLayer.environment.put("NYX_PRESET", "simple");
        assertEquals("simple", environmentConfigurationLayer.getPreset());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getReleaseLenient()")
    void getReleaseLenientTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getReleaseLenient());

        environmentConfigurationLayer.environment.put("NYX_RELEASE_LENIENT", "true");
        assertEquals(Boolean.TRUE, environmentConfigurationLayer.getReleaseLenient());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getReleasePrefix()")
    void getReleasePrefixTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getReleasePrefix());

        environmentConfigurationLayer.environment.put("NYX_RELEASE_PREFIX", "prefix");
        assertEquals("prefix", environmentConfigurationLayer.getReleasePrefix());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getReleaseTypes()")
    void getReleaseTypesTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNotNull(environmentConfigurationLayer.getReleaseTypes());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getEnabled().isEmpty());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getPublicationServices().isEmpty());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().isEmpty());
        assertNotNull(environmentConfigurationLayer.getReleaseTypes().getItems());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getItems().isEmpty());

        // get a new instance or a stale object is returned by getReleaseTypes()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_ENABLED", "one,two");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_PUBLICATION_SERVICES", "first,second");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_REMOTE_REPOSITORIES", "origin,replica");

        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getPublicationServices().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getPublicationServices().containsAll(List.<String>of("first", "second")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().containsAll(List.<String>of("origin", "replica")));
        assertEquals(0, environmentConfigurationLayer.getReleaseTypes().getItems().size());

        // get a new instance or a stale object is returned by getReleaseTypes()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_ENABLED", "one,two");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_PUBLICATION_SERVICES", "first,second");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_REMOTE_REPOSITORIES", "origin,replica");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_COLLAPSE_VERSIONS", "");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_COLLAPSE_VERSIONS", "");

        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getPublicationServices().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getPublicationServices().containsAll(List.<String>of("first", "second")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().containsAll(List.<String>of("origin", "replica")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getItems().size());
        assertNotNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("one"));
        assertNotNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("two"));

        // get a new instance or a stale object is returned by getReleaseTypes()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_ENABLED", "one,two");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_PUBLICATION_SERVICES", "first,second");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_REMOTE_REPOSITORIES", "origin,replica");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_COLLAPSE_VERSIONS", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_COLLAPSED_VERSION_QUALIFIER", "qualifier1");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_DESCRIPTION", "description1");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_GIT_COMMIT", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_GIT_PUSH", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_GIT_TAG", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_MATCH_BRANCHES", "alpha,beta");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_MATCH_WORKSPACE_STATUS", WorkspaceStatus.DIRTY.toString());
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_PUBLISH", "false");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_one_VERSION_RANGE", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_COLLAPSE_VERSIONS", "false");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_DESCRIPTION", "description2");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_FILTER_TAGS", "filter2");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_GIT_COMMIT", "false");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_GIT_COMMIT_MESSAGE", "Commit message");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_GIT_PUSH", "false");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_GIT_TAG", "false");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_GIT_TAG_MESSAGE", "Tag message");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_0_POSITION", Identifier.Position.PRE_RELEASE.toString());
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_0_QUALIFIER", "q1");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_0_VALUE", "v1");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_1_QUALIFIER", "q2");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_1_VALUE", "v2");
        // note we use ordinal 9 here, but the list will return it at the 2nd position anyway because it sorts them regardles of gaps between ordinals
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_9_POSITION", Identifier.Position.BUILD.toString());
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_9_QUALIFIER", "q3");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_IDENTIFIERS_9_VALUE", "v3");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_MATCH_ENVIRONMENT_VARIABLES_PATH", "any path");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_MATCH_ENVIRONMENT_VARIABLES_USER", "any user");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_MATCH_WORKSPACE_STATUS", WorkspaceStatus.CLEAN.toString());
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_PUBLISH", "true");
        environmentConfigurationLayer.environment.put("NYX_RELEASE_TYPES_two_VERSION_RANGE_FROM_BRANCH_NAME", "true");
        
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getEnabled().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getEnabled().containsAll(List.<String>of("one", "two")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getPublicationServices().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getPublicationServices().containsAll(List.<String>of("first", "second")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getRemoteRepositories().containsAll(List.<String>of("origin", "replica")));
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getItems().size());
        assertTrue(environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getCollapseVersions());
        assertEquals("qualifier1", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getCollapsedVersionQualifier());
        assertEquals("description1", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getDescription());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getFilterTags());
        assertEquals("true", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getGitCommit());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getGitCommitMessage());
        assertEquals("true", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getGitTag());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getGitTagMessage());
        assertEquals("true", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getGitPush());
        assertEquals(0, environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getIdentifiers().size());
        assertEquals("alpha,beta", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getMatchBranches());
        assertEquals(0, environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getMatchEnvironmentVariables().size());
        assertEquals(WorkspaceStatus.DIRTY, environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getMatchWorkspaceStatus());
        assertEquals("false", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getPublish());
        assertEquals("true", environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getVersionRange());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("one").getVersionRangeFromBranchName());
        assertEquals(false, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getCollapseVersions());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getCollapsedVersionQualifier());
        assertEquals("description2", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getDescription());
        assertEquals("filter2", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getFilterTags());
        assertEquals("false", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getGitCommit());
        assertEquals("Commit message", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getGitCommitMessage());
        assertEquals("false", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getGitPush());
        assertEquals("false", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getGitTag());
        assertEquals("Tag message", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getGitTagMessage());
        assertEquals(3, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().size());
        assertEquals(Identifier.Position.PRE_RELEASE, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(0).getPosition());
        assertEquals("q1", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(0).getQualifier());
        assertEquals("v1", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(0).getValue());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(1).getPosition());
        assertEquals("q2", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(1).getQualifier());
        assertEquals("v2", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(1).getValue());
        assertEquals(Identifier.Position.BUILD, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(2).getPosition());
        assertEquals("q3", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(2).getQualifier());
        assertEquals("v3", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getIdentifiers().get(2).getValue());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getMatchBranches());
        assertEquals(2, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getMatchEnvironmentVariables().size());
        assertEquals("any path", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getMatchEnvironmentVariables().get("PATH"));
        assertEquals("any user", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getMatchEnvironmentVariables().get("USER"));
        assertEquals(WorkspaceStatus.CLEAN, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getMatchWorkspaceStatus());
        assertEquals("true", environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getPublish());
        assertNull(environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getVersionRange());
        assertEquals(Boolean.TRUE, environmentConfigurationLayer.getReleaseTypes().getItems().get("two").getVersionRangeFromBranchName());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getResume()")
    void getResumeTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getResume());

        environmentConfigurationLayer.environment.put("NYX_RESUME", "true");
        assertEquals(Boolean.TRUE, environmentConfigurationLayer.getResume());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getScheme()")
    void getSchemeTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getScheme());

        environmentConfigurationLayer.environment.put("NYX_SCHEME", Scheme.SEMVER.toString());
        assertEquals(Scheme.SEMVER, environmentConfigurationLayer.getScheme());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getServices()")
    void getServicesTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNotNull(environmentConfigurationLayer.getServices());
        assertTrue(environmentConfigurationLayer.getServices().isEmpty());
        
        // get a new instance or a stale object is returned by getServices()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_TYPE", "GITHUB");
        
        assertEquals(1, environmentConfigurationLayer.getServices().size());
        assertTrue(environmentConfigurationLayer.getServices().containsKey("github"));
        assertNotNull(environmentConfigurationLayer.getServices().get("github"));
        
        // get a new instance or a stale object is returned by getServices()
        environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_TYPE", "GITHUB");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_OPTIONS_BASE_URI", "https://someuri1.com/");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_OPTIONS_AUTHENTICATION_TOKEN", "1234567890");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_OPTIONS_REPOSITORY_NAME", "repo1");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_github_OPTIONS_REPOSITORY_OWNER", "owner1");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_gitlab_TYPE", "GITLAB");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_gitlab_OPTIONS_BASE_URI", "https://someuri2.com/");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_gitlab_OPTIONS_AUTHENTICATION_TOKEN", "abcdefghij");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_gitlab_OPTIONS_REPOSITORY_NAME", "repo2");
        environmentConfigurationLayer.environment.put("NYX_SERVICES_gitlab_OPTIONS_REPOSITORY_OWNER", "owner2");

        assertEquals(2, environmentConfigurationLayer.getServices().size());
        assertTrue(environmentConfigurationLayer.getServices().containsKey("github"));
        assertTrue(environmentConfigurationLayer.getServices().containsKey("gitlab"));
        assertNotNull(environmentConfigurationLayer.getServices().get("github"));
        assertEquals(Provider.GITHUB, environmentConfigurationLayer.getServices().get("github").getType());
        assertEquals(4, environmentConfigurationLayer.getServices().get("github").getOptions().size());
        assertEquals("https://someuri1.com/", environmentConfigurationLayer.getServices().get("github").getOptions().get("BASE_URI"));
        assertEquals("1234567890", environmentConfigurationLayer.getServices().get("github").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals("repo1", environmentConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_NAME"));
        assertEquals("owner1", environmentConfigurationLayer.getServices().get("github").getOptions().get("REPOSITORY_OWNER"));
        assertNotNull(environmentConfigurationLayer.getServices().get("gitlab"));
        assertEquals(Provider.GITLAB, environmentConfigurationLayer.getServices().get("gitlab").getType());
        assertEquals(4, environmentConfigurationLayer.getServices().get("gitlab").getOptions().size());
        assertEquals("https://someuri2.com/", environmentConfigurationLayer.getServices().get("gitlab").getOptions().get("BASE_URI"));
        assertEquals("abcdefghij", environmentConfigurationLayer.getServices().get("gitlab").getOptions().get("AUTHENTICATION_TOKEN"));
        assertEquals("repo2", environmentConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_NAME"));
        assertEquals("owner2", environmentConfigurationLayer.getServices().get("gitlab").getOptions().get("REPOSITORY_OWNER"));
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getSharedConfigurationFile()")
    void getSharedConfigurationFileTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getSharedConfigurationFile());

        environmentConfigurationLayer.environment.put("NYX_SHARED_CONFIGURATION_FILE", "config.yml");
        assertEquals("config.yml", environmentConfigurationLayer.getSharedConfigurationFile());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getStateFile()")
    void getStateFileTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getStateFile());

        environmentConfigurationLayer.environment.put("NYX_STATE_FILE", "state.yml");
        assertEquals("state.yml", environmentConfigurationLayer.getStateFile());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getVerbosity()")
    void getVerbosityTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getVerbosity());

        environmentConfigurationLayer.environment.put("NYX_VERBOSITY", Verbosity.INFO.toString());
        assertEquals(Verbosity.INFO, environmentConfigurationLayer.getVerbosity());
    }

    @Test
    @DisplayName("EnvironmentConfigurationLayer.getVersion()")
    void getVersionTest()
        throws Exception {
        EnvironmentConfigurationLayerMock environmentConfigurationLayer = EnvironmentConfigurationLayerMock.getInstance();
        assertNull(environmentConfigurationLayer.getVersion());

        environmentConfigurationLayer.environment.put("NYX_VERSION", "3.5.7");
        assertEquals("3.5.7", environmentConfigurationLayer.getVersion());
    }
}