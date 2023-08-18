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

import java.util.List;
import java.util.Map;

/**
 * This object models the fields used to configure a generic release type.
 */
public class ReleaseType {
    /**
     * The list of assets to publish with this release type. When {@code null}
     * all assets configured globally (if any) are published, otherwise only the assets
     * in this list must be published for this release type. The names in this list are the map
     * keys defined in the global {@code releaseAssets}.
     */
    private List<String> assets = Defaults.ReleaseType.ASSETS;

    /**
     * The flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used.
     */
    private boolean collapseVersions = Defaults.ReleaseType.COLLAPSE_VERSIONS;

    /**
     * The optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed.
     */
    private String collapsedVersionQualifier = Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER;

    /**
     * The optional string or the template to render to use as the release description.
     */
    private String description = Defaults.ReleaseType.DESCRIPTION;

    /**
     * The optional template to render as a regular expression used to match tags from the commit history.
     */
    private String filterTags = Defaults.ReleaseType.FILTER_TAGS;

    /**
     * The optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated.
     */
    private String gitCommit = Defaults.ReleaseType.GIT_COMMIT;

    /**
     * The optional string or the template to render to use as the commit message if a commit has to be made.
     */
    private String gitCommitMessage = Defaults.ReleaseType.GIT_COMMIT_MESSAGE;

    /**
     * The optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated.
     */
    private String gitPush = Defaults.ReleaseType.GIT_PUSH;

    /**
     * The optional flag or the template to render indicating whether or not a new tag must be generated.
     */
    private String gitTag = Defaults.ReleaseType.GIT_TAG;

    /**
     * The optional string or the template to render to use as the tag message if a tag has to be made.
     */
    private String gitTagMessage = Defaults.ReleaseType.GIT_TAG_MESSAGE;

    /**
     * The list of templates to use as tag names when tagging a commit.
     */
    private List<String> gitTagNames = Defaults.ReleaseType.GIT_TAG_NAMES;

    /**
     * The identifiers configuration block.
     */
    private List<Identifier> identifiers = Defaults.ReleaseType.IDENTIFIERS;

    /**
     * The optional template to render as a regular expression used to match branch names.
     */
    private String matchBranches = Defaults.ReleaseType.MATCH_BRANCHES;

    /**
     * The map of the match environment variables items, where keys are environment variable names and values
     * are regular expressions.
     */
    private Map<String,String> matchEnvironmentVariables = Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES;

    /**
     * The identifier of a specific workspace status to be matched.
     */
    private WorkspaceStatus matchWorkspaceStatus = Defaults.ReleaseType.MATCH_WORKSPACE_STATUS;

    /**
     * The optional flag or the template to render indicating whether or not releases must be published.
     */
    private String publish = Defaults.ReleaseType.PUBLISH;

    /**
     * The optional template to render as a regular expression used to constrain versions issued by this release type.
     */
    private String versionRange = Defaults.ReleaseType.VERSION_RANGE;

    /**
     * The optional flag telling if the version range must be inferred from the branch name.
     */
    private Boolean versionRangeFromBranchName = Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME;

    /**
     * Default constructor.
     * 
     * All fields are initialized to their default values.
     * 
     * @see Defaults.ReleaseType
     */
    public ReleaseType() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param assets the list of selected asset names to publish with the release. The names
     * in this list are the map keys defined in the global {@code releaseAssets}.
     * @param collapseVersions the flag indicating whether or not the 'collapsed' versioning
     * (pre-release style) must be used.
     * @param collapsedVersionQualifier the optional qualifier or the template to render the
     * qualifier to use for the pre-release identifier when versions are collapsed.
     * @param description the optional string or the template to render to use as the release
     * description.
     * @param filterTags the optional template to render as a regular expression used to match
     * tags from the commit history.
     * @param gitCommit the optional flag or the template to render indicating whether or not
     * a new commit must be generated in case new artifacts are generated.
     * @param gitCommitMessage the optional string or the template to render to use as the
     * commit message if a commit has to be made.
     * @param gitPush the optional flag or the template to render indicating whether or not
     * a new commit must be generated and pushed in case new artifacts are generated.
     * @param gitTag the optional flag or the template to render indicating whether or not
     * a new tag must be generated.
     * @param gitTagMessage the optional identifiers configuration block.
     * @param gitTagNames the list of templates to use as tag names when tagging a commit.
     * @param identifiers the optional nested map of the custom extra identifiers to be used in a
     * release type.
     * @param matchBranches the optional template to render as a regular expression used to
     * match branch names.
     * @param matchEnvironmentVariables the map of the match environment variables items,
     * where keys are environment variable names and values are regular expressions.
     * @param matchWorkspaceStatus the identifier of a specific workspace status to be matched.
     * @param publish the optional flag or the template to render indicating whether or not
     * releases must be published.
     * @param versionRange the optional regular expression used to constrain versions issued
     * by this release type.
     * @param versionRangeFromBranchName the optional flag telling if the version range must
     * be inferred from the branch name.
     * 
     * @see Defaults.ReleaseType
     */
    public ReleaseType(List<String> assets, boolean collapseVersions, String collapsedVersionQualifier, String description, String filterTags, String gitCommit, String gitCommitMessage, String gitPush, String gitTag, String gitTagMessage, List<String> gitTagNames, List<Identifier> identifiers, String matchBranches, Map<String,String> matchEnvironmentVariables, WorkspaceStatus matchWorkspaceStatus, String publish, String versionRange, Boolean versionRangeFromBranchName) {
        super();
        this.assets = assets;
        this.collapseVersions = collapseVersions;
        this.collapsedVersionQualifier = collapsedVersionQualifier;
        this.description = description;
        this.filterTags = filterTags;
        this.gitCommit = gitCommit;
        this.gitCommitMessage = gitCommitMessage;
        this.gitPush = gitPush;
        this.gitTag = gitTag;
        this.gitTagMessage = gitTagMessage;
        this.gitTagNames = gitTagNames;
        this.identifiers = identifiers;
        this.matchBranches = matchBranches;
        this.matchEnvironmentVariables = matchEnvironmentVariables;
        this.matchWorkspaceStatus = matchWorkspaceStatus;
        this.publish = publish;
        this.versionRange = versionRange;
        this.versionRangeFromBranchName = versionRangeFromBranchName;
    }

    /**
     * Returns the list of selected asset names to publish with the release. When {@code null}
     * all assets configured globally (if any) are published, otherwise only the assets
     * in this list must be published for this release type. The names in this list are the map
     * keys defined in the global {@code releaseAssets}.
     * 
     * @return the list of selected asset names to publish with the release. When {@code null}
     * all assets configured globally (if any) are published, otherwise only the assets
     * in this list must be published for this release type. The names in this list are the map
     * keys defined in the global {@code releaseAssets}.
     */
    public List<String> getAssets() {
        return assets;
    }

    /**
     * Sets the list of selected asset names to publish with the release. When {@code null}
     * all assets configured globally (if any) are published, otherwise only the assets
     * in this list must be published for this release type. The names in this list are the map
     * keys defined in the global {@code releaseAssets}.
     * 
     * @param assets the list of selected asset names to publish with the release. When {@code null}
     * all assets configured globally (if any) are published, otherwise only the assets
     * in this list must be published for this release type. The names in this list are the map
     * keys defined in the global {@code releaseAssets}.
     */
    public void setAssets(List<String> assets) {
        this.assets = assets;
    }

    /**
     * Returns the flag indicating whether or not the 'collapsed' versioning
     * (pre-release style) must be used.
     * 
     * @return the flag indicating whether or not the 'collapsed' versioning
     * (pre-release style) must be used.
     */
    public boolean getCollapseVersions() {
        return collapseVersions;
    }

    /**
     * Sets the flag indicating whether or not the 'collapsed' versioning
     * (pre-release style) must be used.
     * 
     * @param collapseVersions the flag indicating whether or not the 'collapsed' versioning
     * (pre-release style) must be used.
     */
    public void setCollapseVersions(boolean collapseVersions) {
        this.collapseVersions = collapseVersions;
    }

    /**
     * Returns the optional qualifier or the template to render the
     * qualifier to use for the pre-release identifier when versions are collapsed.
     * 
     * @return the optional qualifier or the template to render the
     * qualifier to use for the pre-release identifier when versions are collapsed.
     */
    public String getCollapsedVersionQualifier() {
        return collapsedVersionQualifier;
    }

    /**
     * Sets the optional qualifier or the template to render the
     * qualifier to use for the pre-release identifier when versions are collapsed.
     * 
     * @param collapsedVersionQualifier the optional qualifier or the template to render the
     * qualifier to use for the pre-release identifier when versions are collapsed.
     */
    public void setCollapsedVersionQualifier(String collapsedVersionQualifier) {
        this.collapsedVersionQualifier = collapsedVersionQualifier;
    }

    /**
     * Returns the optional string or the template to render to use as the release
     * description.
     * 
     * @return the optional string or the template to render to use as the release
     * description.
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the optional string or the template to render to use as the release
     * description.
     * 
     * @param description the optional string or the template to render to use as the release
     * description.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Returns the optional template to render as a regular expression used to match tags
     * from the commit history.
     * 
     * @return the optional template to render as a regular expression used to match tags
     * from the commit history.
     */
    public String getFilterTags() {
        return filterTags;
    }

    /**
     * Sets the optional template to render as a regular expression used to match tags
     * from the commit history.
     * 
     * @param filterTags the optional template to render as a regular expression used
     * to match tags from the commit history.
     */
    public void setFilterTags(String filterTags) {
        this.filterTags = filterTags;
    }

    /**
     * Returns the optional flag or the template to render indicating whether or not
     * a new commit must be generated in case new artifacts are generated.
     * 
     * @return the optional flag or the template to render indicating whether or not
     * a new commit must be generated in case new artifacts are generated.
     */
    public String getGitCommit() {
        return gitCommit;
    }

    /**
     * Sets the optional flag or the template to render indicating whether or not
     * a new commit must be generated in case new artifacts are generated.
     * 
     * @param gitCommit the optional flag or the template to render indicating whether or not
     * a new commit must be generated in case new artifacts are generated.
     */
    public void setGitCommit(String gitCommit) {
        this.gitCommit = gitCommit;
    }

    /**
     * Returns the optional string or the template to render to use as the
     * commit message if a commit has to be made.
     * 
     * @return the optional string or the template to render to use as the
     * commit message if a commit has to be made.
     */
    public String getGitCommitMessage() {
        return gitCommitMessage;
    }

    /**
     * Sets the optional string or the template to render to use as the
     * commit message if a commit has to be made.
     * 
     * @param gitCommitMessage the optional string or the template to render to use as the
     * commit message if a commit has to be made.
     */
    public void setGitCommitMessage(String gitCommitMessage) {
        this.gitCommitMessage = gitCommitMessage;
    }

    /**
     * Returns the optional flag or the template to render indicating whether or not
     * a new commit must be generated and pushed in case new artifacts are generated.
     * 
     * @return the optional flag or the template to render indicating whether or not
     * a new commit must be generated and pushed in case new artifacts are generated.
     */
    public String getGitPush() {
        return gitPush;
    }

    /**
     * Sets the optional flag or the template to render indicating whether or not
     * a new commit must be generated and pushed in case new artifacts are generated.
     * 
     * @param gitPush the optional flag or the template to render indicating whether or not
     * a new commit must be generated and pushed in case new artifacts are generated.
     */
    public void setGitPush(String gitPush) {
        this.gitPush = gitPush;
    }

    /**
     * Returns the optional flag or the template to render indicating whether or not
     * a new tag must be generated.
     * 
     * @return the optional flag or the template to render indicating whether or not
     * a new tag must be generated.
     */
    public String getGitTag() {
        return gitTag;
    }

    /**
     * Sets the optional flag or the template to render indicating whether or not
     * a new tag must be generated.
     * 
     * @param gitTag the optional flag or the template to render indicating whether or not
     * a new tag must be generated.
     */
    public void setGitTag(String gitTag) {
        this.gitTag = gitTag;
    }

    /**
     * Returns the optional string or the template to render to use as the tag
     * message if a tag has to be made.
     * 
     * @return the optional string or the template to render to use as the tag
     * message if a tag has to be made.
     */
    public String getGitTagMessage() {
        return gitTagMessage;
    }

    /**
     * Sets the optional string or the template to render to use as the tag
     * message if a tag has to be made.
     * 
     * @param gitTagMessage the optional string or the template to render to use as the tag
     * message if a tag has to be made.
     */
    public void setGitTagMessage(String gitTagMessage) {
        this.gitTagMessage = gitTagMessage;
    }

    /**
     * Returns the list of templates to use as tag names when tagging a commit.
     * 
     * @return the list of templates to use as tag names when tagging a commit.
     */
    public List<String> getGitTagNames() {
        return gitTagNames;
    }

    /**
     * Sets the list of templates to use as tag names when tagging a commit.
     * 
     * @param gitTagNames the list of templates to use as tag names when tagging a commit.
     */
    public void setGitTagNames(List<String> gitTagNames) {
        this.gitTagNames = gitTagNames;
    }

    /**
     * Returns the identifiers configuration block.
     * 
     * @return the identifiers configuration block.
     */
    public List<Identifier> getIdentifiers() {
        return identifiers;
    }

    /**
     * Sets the identifiers configuration block.
     * 
     * @param identifiers the identifiers configuration block.
     */
    public void setIdentifiers(List<Identifier> identifiers) {
        this.identifiers = identifiers;
    }

    /**
     * Returns the optional template to render as a regular expression used to match branch names.
     * 
     * @return the optional template to render as a regular expression used to match branch names.
     */
    public String getMatchBranches() {
        return matchBranches;
    }

    /**
     * Sets the optional template to render as a regular expression used to match branch names.
     * 
     * @param matchBranches the optional template to render as a regular expression used to match branch names.
     */
    public void setMatchBranches(String matchBranches) {
        this.matchBranches = matchBranches;
    }

    /**
     * Returns the match environment variables map.
     * 
     * @return the match environment variables map.
     */
    public Map<String,String> getMatchEnvironmentVariables() {
        return matchEnvironmentVariables;
    }

    /**
     * Sets the match environment variables map.
     * 
     * @param matchEnvironmentVariables the match environment variables map.
     */
    public void setMatchEnvironmentVariables(Map<String,String> matchEnvironmentVariables) {
        this.matchEnvironmentVariables = matchEnvironmentVariables;
    }

    /**
     * Returns the identifier of a specific workspace status to be matched.
     * 
     * @return the identifier of a specific workspace status to be matched.
     */
    public WorkspaceStatus getMatchWorkspaceStatus() {
        return matchWorkspaceStatus;
    }

    /**
     * Sets the identifier of a specific workspace status to be matched.
     * 
     * @param matchWorkspaceStatus the identifier of a specific workspace status to be matched.
     */
    public void setMatchWorkspaceStatus(WorkspaceStatus matchWorkspaceStatus) {
        this.matchWorkspaceStatus = matchWorkspaceStatus;
    }

    /**
     * Returns the optional flag or the template to render indicating whether or not releases must be published.
     * 
     * @return the optional flag or the template to render indicating whether or not releases must be published.
     */
    public String getPublish() {
        return publish;
    }

    /**
     * Sets the optional flag or the template to render indicating whether or not releases must be published.
     * 
     * @param publish the optional flag or the template to render indicating whether or not releases must be published.
     */
    public void setPublish(String publish) {
        this.publish = publish;
    }

    /**
     * Returns the optional template to render as a regular expression used to constrain versions issued by this release type.
     * 
     * @return the optional template to render as a regular expression used to constrain versions issued by this release type.
     */
    public String getVersionRange() {
        return versionRange;
    }

    /**
     * Sets the optional template to render as a regular expression used to constrain versions issued by this release type.
     * 
     * @param versionRange the optional template to render as a regular expression used to constrain versions issued by this release type.
     */
    public void setVersionRange(String versionRange) {
        this.versionRange = versionRange;
    }

    /**
     * Returns the optional flag telling if the version range must be inferred from the branch name.
     * 
     * @return the optional flag telling if the version range must be inferred from the branch name.
     */
    public Boolean getVersionRangeFromBranchName() {
        return versionRangeFromBranchName;
    }

    /**
     * Sets the optional flag telling if the version range must be inferred from the branch name.
     * 
     * @param versionRangeFromBranchName the optional flag telling if the version range must be inferred from the branch name.
     */
    public void setVersionRangeFromBranchName(Boolean versionRangeFromBranchName) {
        this.versionRangeFromBranchName = versionRangeFromBranchName;
    }
}
