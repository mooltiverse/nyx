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
package com.mooltiverse.oss.nyx.entities;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ChangelogConfiguration")
public class ChangelogConfigurationTests {
    @Test
    @DisplayName("ChangelogConfiguration()")
    void constructorTest()
        throws Exception {
        assertEquals("CHANGELOG.md", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getPath());
        assertEquals("regex1", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getSections().get("Section1"));
        assertEquals("regex2", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getSections().get("Section2"));
        assertEquals("changelog.tpl", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getTemplate());
        assertEquals(Boolean.TRUE, new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getIncludeUnreleased());
        assertEquals("commitLink", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getCommitLink());
        assertEquals("contributorLink", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getContributorLink());
        assertEquals("issueID", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getIssueID());
        assertEquals("issueLink", new ChangelogConfiguration("CHANGELOG.md", Map.<String,String>of("Section1", "regex1", "Section2", "regex2"), "changelog.tpl", Boolean.TRUE, "commitLink", "contributorLink", "issueID", "issueLink").getIssueLink());
    }
}