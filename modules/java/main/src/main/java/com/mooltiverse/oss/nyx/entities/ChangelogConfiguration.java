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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * This object models the fields used to configure the changelog generation.
 */
public class ChangelogConfiguration {
    /**
     * The format expression to use to generate working links to commits.
     */
    private String commitLink = null;

    /**
     * The format expression to use to generate working links to contributors.
     */
    private String contributorLink = null;

    /**
     * The flag telling if the unreleased section must be generated.
     */
    private Boolean includeUnreleased = null;

    /**
     * The regular expression to use to detect references to issues mentioned in commit messages.
     */
    private String issueID = null;

    /**
     * The format expression to use to generate working links to issues mentioned in commit messages.
     */
    private String issueLink = null;

    /**
     * The path to the destination file.
     */
    private String path = null;

    /**
     * The map of sections and commit types.
     */
    private Map<String,String> sections = null;

    /**
     * The path to the optional template file.
     */
    private String template = null;

    /**
     * Default constructor.
     */
    public ChangelogConfiguration() {
        super();
        this.sections = new HashMap<String,String>();
    }

    /**
     * Standard constructor.
     * 
     * @param path the path to the destination file. It may be {@code null}.
     * @param sections the map of sections and commit types.
     * @param template the path to the optional template file. It may be {@code null}.
     * @param includeUnreleased the flag telling if the unreleased section must be generated. It may be {@code null}.
     * @param commitLink the format expression to use to generate working links to commits. It may be {@code null}.
     * @param contributorLink the format expression to use to generate working links to contributors. It may be {@code null}.
     * @param issueID the regular expression to use to detect references to issues mentioned in commit messages. It may be {@code null}.
     * @param issueLink the format expression to use to generate working links to issues mentioned in commit messages. It may be {@code null}.
     * 
     * @throws NullPointerException if some mandatory argument is {@code null}
     */
    public ChangelogConfiguration(String path, Map<String,String> sections, String template, Boolean includeUnreleased, String commitLink, String contributorLink, String issueID, String issueLink) {
        super();
        Objects.requireNonNull(sections);
        this.path = path;
        this.sections = sections;
        this.template = template;
        this.includeUnreleased = includeUnreleased;
        this.commitLink = commitLink;
        this.contributorLink = contributorLink;
        this.issueID = issueID;
        this.issueLink = issueLink;
    }

    /**
     * Returns the format expression to use to generate working links to commits.
     * It may be {@code null}.
     * 
     * @return the format expression to use to generate working links to commits.
     * It may be {@code null}.
     */
    public String getCommitLink() {
        return commitLink;
    }

    /**
     * Sets the format expression to use to generate working links to commits.
     * It may be {@code null}.
     * 
     * @param commitLink the format expression to use to generate working links to commits.
     * It may be {@code null}.
     */
    public void setCommitLink(String commitLink) {
        this.commitLink = commitLink;
    }

    /**
     * Returns the format expression to use to generate working links to contributors.
     * It may be {@code null}.
     * 
     * @return the format expression to use to generate working links to contributors.
     * It may be {@code null}.
     */
    public String getContributorLink() {
        return contributorLink;
    }

    /**
     * Sets the format expression to use to generate working links to contributors.
     * It may be {@code null}.
     * 
     * @param contributorLink the format expression to use to generate working links to contributors.
     * It may be {@code null}.
     */
    public void setContributorLink(String contributorLink) {
        this.contributorLink = contributorLink;
    }

    /**
     * Returns the flag telling if the unreleased section must be generated.
     * It may be {@code null}.
     * 
     * @return the flag telling if the unreleased section must be generated.
     * It may be {@code null}.
     */
    public Boolean getIncludeUnreleased() {
        return includeUnreleased;
    }

    /**
     * Sets the flag telling if the unreleased section must be generated.
     * It may be {@code null}.
     * 
     * @param includeUnreleased the flag telling if the unreleased section must be generated.
     * It may be {@code null}.
     */
    public void setIncludeUnreleased(Boolean includeUnreleased) {
        this.includeUnreleased = includeUnreleased;
    }

    /**
     * Returns the regular expression to use to detect references to issues mentioned in commit messages.
     * It may be {@code null}.
     * 
     * @return the regular expression to use to detect references to issues mentioned in commit messages.
     * It may be {@code null}.
     */
    public String getIssueID() {
        return issueID;
    }

    /**
     * Sets the regular expression to use to detect references to issues mentioned in commit messages.
     * It may be {@code null}.
     * 
     * @param issueID the regular expression to use to detect references to issues mentioned in commit messages.
     * It may be {@code null}.
     */
    public void setIssueID(String issueID) {
        this.issueID = issueID;
    }

    /**
     * Returns the format expression to use to generate working links to issues mentioned in commit messages.
     * It may be {@code null}.
     * 
     * @return the format expression to use to generate working links to issues mentioned in commit messages.
     * It may be {@code null}.
     */
    public String getIssueLink() {
        return issueLink;
    }

    /**
     * Sets the format expression to use to generate working links to issues mentioned in commit messages.
     * It may be {@code null}.
     * 
     * @param issueLink the format expression to use to generate working links to issues mentioned in commit messages.
     * It may be {@code null}.
     */
    public void setIssueLink(String issueLink) {
        this.issueLink = issueLink;
    }

    /**
     * Returns the path to the destination file. It may be {@code null}.
     * 
     * @return the path to the destination file. It may be {@code null}.
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the path to the destination file. It may be {@code null}.
     * 
     * @param path the path to the destination file. It may be {@code null}.
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Returns the map of sections and commit types.
     * 
     * @return the map of sections and commit types.
     */
    public Map<String,String> getSections() {
        return sections;
    }

    /**
     * Sets the map of sections and commit types.
     * 
     * @param sections the map of sections and commit types.
     */
    public void setSections(Map<String,String> sections) {
        this.sections = sections;
    }

    /**
     * Returns the path to the optional template file. It may be {@code null}.
     * 
     * @return the path to the optional template file. It may be {@code null}.
     */
    public String getTemplate() {
        return template;
    }

    /**
     * Sets the path to the optional template file. It may be {@code null}.
     * 
     * @param template the path to the optional template file. It may be {@code null}.
     */
    public void setTemplate(String template) {
        this.template = template;
    }
}
