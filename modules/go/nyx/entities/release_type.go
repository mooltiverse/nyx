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

package entities

/*
This object models the fields used to configure a generic release type.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type ReleaseType struct {
	// The list of assets to publish with this release type. When nil
	// all assets configured globally (if any) are published, otherwise only the assets
	// in this list must be published for this release type. The names in this list are the map
	// keys defined in the global releaseAssets.
	Assets *[]*string `json:"assets,omitempty" yaml:"assets,omitempty"`

	// The flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used. A nil value means undefined.
	CollapseVersions *bool `json:"collapseVersions,omitempty" yaml:"collapseVersions,omitempty"`

	// The optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed. A nil value means undefined.
	CollapsedVersionQualifier *string `json:"collapsedVersionQualifier,omitempty" yaml:"collapsedVersionQualifier,omitempty"`

	// The optional string or the template to render to use as the release description. A nil value means undefined.
	Description *string `json:"description,omitempty" yaml:"description,omitempty"`

	// The optional template to render as a regular expression used to match tags from the commit history. A nil value means undefined.
	FilterTags *string `json:"filterTags,omitempty" yaml:"filterTags,omitempty"`

	// The optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated. A nil value means undefined.
	GitCommit *string `json:"gitCommit,omitempty" yaml:"gitCommit,omitempty"`

	// The optional string or the template to render to use as the commit message if a commit has to be made. A nil value means undefined.
	GitCommitMessage *string `json:"gitCommitMessage,omitempty" yaml:"gitCommitMessage,omitempty"`

	// The optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated. A nil value means undefined.
	GitPush *string `json:"gitPush,omitempty" yaml:"gitPush,omitempty"`

	// The optional flag or the template to render indicating whether or not a new tag must be generated. A nil value means undefined.
	GitTag *string `json:"gitTag,omitempty" yaml:"gitTag,omitempty"`

	// The optional string or the template to render to use as the tag message if a tag has to be made. A nil value means undefined.
	GitTagMessage *string `json:"gitTagMessage,omitempty" yaml:"gitTagMessage,omitempty"`

	// The identifiers configuration block. Elements of this list must be of type Identifier. A nil value means undefined.
	Identifiers *[]*Identifier `json:"identifiers,omitempty" yaml:"identifiers,omitempty"`

	// The optional template to render as a regular expression used to match branch names. A nil value means undefined.
	MatchBranches *string `json:"matchBranches,omitempty" yaml:"matchBranches,omitempty"`

	// The map of the match environment variables items, where keys are environment variable names and values are regular expressions. A nil value means undefined.
	MatchEnvironmentVariables *map[string]string `json:"matchEnvironmentVariables,omitempty" yaml:"matchEnvironmentVariables,omitempty"`

	// The identifier of a specific workspace status to be matched. A nil value means undefined.
	MatchWorkspaceStatus *WorkspaceStatus `json:"matchWorkspaceStatus,omitempty" yaml:"matchWorkspaceStatus,omitempty"`

	// The optional flag or the template to render indicating whether or not releases must be published. A nil value means undefined.
	Publish *string `json:"publish,omitempty" yaml:"publish,omitempty"`

	// The optional template to render as a regular expression used to constrain versions issued by this release type. A nil value means undefined.
	VersionRange *string `json:"versionRange,omitempty" yaml:"versionRange,omitempty"`

	// The optional flag telling if the version range must be inferred from the branch name. A nil value means undefined.
	VersionRangeFromBranchName *bool `json:"versionRangeFromBranchName,omitempty" yaml:"versionRangeFromBranchName,omitempty"`
}

/*
Default constructor
*/
func NewReleaseType() *ReleaseType {
	rt := ReleaseType{}
	rt.setDefaults()
	return &rt
}

/*
Standard constructor.

Arguments are as follows:

- assets the list of selected asset names to publish with the release. The names in this list are the map keys defined in the global releaseAssets.
- collapseVersions the flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used.
- collapsedVersionQualifier the optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed.
- description the optional string or the template to render to use as the release description.
- filterTags the optional template to render as a regular expression used to match tags from the commit history.
- gitCommit the optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated.
- gitCommitMessage the optional string or the template to render to use as the commit message if a commit has to be made.
- gitPush the optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated.
- gitTag the optional flag or the template to render indicating whether or not a new tag must be generated.
- gitTagMessage the optional identifiers configuration block.
- identifiers the optional nested map of the custom extra identifiers to be used in a release type.
- matchBranches the optional template to render as a regular expression used to match branch names.
- matchEnvironmentVariables the map of the match environment variables items, where keys are environment variable names and values are regular expressions.
- matchWorkspaceStatus the identifier of a specific workspace status to be matched.
- publish the optional flag or the template to render indicating whether or not releases must be published.
- versionRange the optional regular expression used to constrain versions issued by this release type.
- versionRangeFromBranchName the optional flag telling if the version range must be inferred from the branch name.
*/
func NewReleaseTypeWith(assets *[]*string, collapseVersions *bool, collapsedVersionQualifier *string, description *string, filterTags *string, gitCommit *string, gitCommitMessage *string, gitPush *string, gitTag *string, gitTagMessage *string, identifiers *[]*Identifier, matchBranches *string, matchEnvironmentVariables *map[string]string, matchWorkspaceStatus *WorkspaceStatus, publish *string, versionRange *string, versionRangeFromBranchName *bool) *ReleaseType {
	rt := ReleaseType{}

	rt.Assets = assets
	rt.CollapseVersions = collapseVersions
	rt.CollapsedVersionQualifier = collapsedVersionQualifier
	rt.Description = description
	rt.FilterTags = filterTags
	rt.GitCommit = gitCommit
	rt.GitCommitMessage = gitCommitMessage
	rt.GitPush = gitPush
	rt.GitTag = gitTag
	rt.GitTagMessage = gitTagMessage
	rt.Identifiers = identifiers
	rt.MatchBranches = matchBranches
	rt.MatchEnvironmentVariables = matchEnvironmentVariables
	rt.MatchWorkspaceStatus = matchWorkspaceStatus
	rt.Publish = publish
	rt.VersionRange = versionRange
	rt.VersionRangeFromBranchName = versionRangeFromBranchName

	return &rt
}

/*
Loads default values on the target instance
*/
func (rt *ReleaseType) setDefaults() {
	rt.Assets = RELEASE_TYPE_ASSETS
	rt.CollapseVersions = RELEASE_TYPE_COLLAPSE_VERSIONS
	rt.CollapsedVersionQualifier = RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER
	rt.Description = RELEASE_TYPE_DESCRIPTION
	rt.FilterTags = RELEASE_TYPE_FILTER_TAGS
	rt.GitCommit = RELEASE_TYPE_GIT_COMMIT
	rt.GitCommitMessage = RELEASE_TYPE_GIT_COMMIT_MESSAGE
	rt.GitPush = RELEASE_TYPE_GIT_PUSH
	rt.GitTag = RELEASE_TYPE_GIT_TAG
	rt.GitTagMessage = RELEASE_TYPE_GIT_TAG_MESSAGE
	rt.Identifiers = RELEASE_TYPE_IDENTIFIERS
	rt.MatchBranches = RELEASE_TYPE_MATCH_BRANCHES
	rt.MatchEnvironmentVariables = RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES
	rt.MatchWorkspaceStatus = RELEASE_TYPE_MATCH_WORKSPACE_STATUS
	rt.Publish = RELEASE_TYPE_PUBLISH
	rt.VersionRange = RELEASE_TYPE_VERSION_RANGE
	rt.VersionRangeFromBranchName = RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME
}

/*
Sets the list of selected asset names to publish with the release. When nil
all assets configured globally (if any) are published, otherwise only the assets
in this list must be published for this release type. The names in this list are the map
keys defined in the global releaseAssets.
*/
func (rt *ReleaseType) GetAssets() *[]*string {
	return rt.Assets
}

/*
Sets the assets configuration block. Elements of this list must be of type Identifier. A nil value means undefined.
*/
func (rt *ReleaseType) SetAssets(assets *[]*string) {
	rt.Assets = assets
}

/*
Returns the flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used. A nil value means undefined.
*/
func (rt *ReleaseType) GetCollapseVersions() *bool {
	return rt.CollapseVersions
}

/*
Sets the flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used. A nil value means undefined.
*/
func (rt *ReleaseType) SetCollapseVersions(collapseVersions *bool) {
	rt.CollapseVersions = collapseVersions
}

/*
Returns the optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed. A nil value means undefined.
*/
func (rt *ReleaseType) GetCollapsedVersionQualifier() *string {
	return rt.CollapsedVersionQualifier
}

/*
Sets the optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed. A nil value means undefined.
*/
func (rt *ReleaseType) SetCollapsedVersionQualifier(collapsedVersionQualifier *string) {
	rt.CollapsedVersionQualifier = collapsedVersionQualifier
}

/*
Returns the optional string or the template to render to use as the release description. A nil value means undefined.
*/
func (rt *ReleaseType) GetDescription() *string {
	return rt.Description
}

/*
Sets the optional string or the template to render to use as the release description. A nil value means undefined.
*/
func (rt *ReleaseType) SetDescription(description *string) {
	rt.Description = description
}

/*
Returns the optional template to render as a regular expression used to match tags from the commit history. A nil value means undefined.
*/
func (rt *ReleaseType) GetFilterTags() *string {
	return rt.FilterTags
}

/*
Sets the optional template to render as a regular expression used to match tags from the commit history. A nil value means undefined.
*/
func (rt *ReleaseType) SetFilterTags(filterTags *string) {
	rt.FilterTags = filterTags
}

/*
Returns the optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitCommit() *string {
	return rt.GitCommit
}

/*
Sets the optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitCommit(gitCommit *string) {
	rt.GitCommit = gitCommit
}

/*
Returns the optional string or the template to render to use as the commit message if a commit has to be made. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitCommitMessage() *string {
	return rt.GitCommitMessage
}

/*
Sets the optional string or the template to render to use as the commit message if a commit has to be made. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitCommitMessage(gitCommitMessage *string) {
	rt.GitCommitMessage = gitCommitMessage
}

/*
Returns the optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitPush() *string {
	return rt.GitPush
}

/*
Sets the optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitPush(gitPush *string) {
	rt.GitPush = gitPush
}

/*
Returns the optional flag or the template to render indicating whether or not a new tag must be generated. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitTag() *string {
	return rt.GitTag
}

/*
Sets the optional flag or the template to render indicating whether or not a new tag must be generated. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitTag(gitTag *string) {
	rt.GitTag = gitTag
}

/*
Returns the optional string or the template to render to use as the tag message if a tag has to be made. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitTagMessage() *string {
	return rt.GitTagMessage
}

/*
Sets the optional string or the template to render to use as the tag message if a tag has to be made. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitTagMessage(gitTagMessage *string) {
	rt.GitTagMessage = gitTagMessage
}

/*
Returns the identifiers configuration block. Elements of this list are of type Identifier. A nil value means undefined.
*/
func (rt *ReleaseType) GetIdentifiers() *[]*Identifier {
	return rt.Identifiers
}

/*
Sets the identifiers configuration block. Elements of this list must be of type Identifier. A nil value means undefined.
*/
func (rt *ReleaseType) SetIdentifiers(identifiers *[]*Identifier) {
	rt.Identifiers = identifiers
}

/*
Returns the optional template to render as a regular expression used to match branch names. A nil value means undefined.
*/
func (rt *ReleaseType) GetMatchBranches() *string {
	return rt.MatchBranches
}

/*
Sets the optional template to render as a regular expression used to match branch names. A nil value means undefined.
*/
func (rt *ReleaseType) SetMatchBranches(matchBranches *string) {
	rt.MatchBranches = matchBranches
}

/*
Returns the match environment variables map. A nil value means undefined.
*/
func (rt *ReleaseType) GetMatchEnvironmentVariables() *map[string]string {
	return rt.MatchEnvironmentVariables
}

/*
Sets the match environment variables map. A nil value means undefined.
*/
func (rt *ReleaseType) SetMatchEnvironmentVariables(matchEnvironmentVariables *map[string]string) {
	rt.MatchEnvironmentVariables = matchEnvironmentVariables
}

/*
Returns the identifier of a specific workspace status to be matched. A nil value means undefined.
*/
func (rt *ReleaseType) GetMatchWorkspaceStatus() *WorkspaceStatus {
	return rt.MatchWorkspaceStatus
}

/*
Sets the identifier of a specific workspace status to be matched. A nil value means undefined.
*/
func (rt *ReleaseType) SetMatchWorkspaceStatus(matchWorkspaceStatus *WorkspaceStatus) {
	rt.MatchWorkspaceStatus = matchWorkspaceStatus
}

/*
Returns the optional flag or the template to render indicating whether or not releases must be published. A nil value means undefined.
*/
func (rt *ReleaseType) GetPublish() *string {
	return rt.Publish
}

/*
Sets the optional flag or the template to render indicating whether or not releases must be published. A nil value means undefined.
*/
func (rt *ReleaseType) SetPublish(publish *string) {
	rt.Publish = publish
}

/*
Returns the optional template to render as a regular expression used to constrain versions issued by this release type. A nil value means undefined.
*/
func (rt *ReleaseType) GetVersionRange() *string {
	return rt.VersionRange
}

/*
Sets the optional template to render as a regular expression used to constrain versions issued by this release type. A nil value means undefined.
*/
func (rt *ReleaseType) SetVersionRange(versionRange *string) {
	rt.VersionRange = versionRange
}

/*
Returns the optional flag telling if the version range must be inferred from the branch name. A nil value means undefined.
*/
func (rt *ReleaseType) GetVersionRangeFromBranchName() *bool {
	return rt.VersionRangeFromBranchName
}

/*
Sets the optional flag telling if the version range must be inferred from the branch name. A nil value means undefined.
*/
func (rt *ReleaseType) SetVersionRangeFromBranchName(versionRangeFromBranchName *bool) {
	rt.VersionRangeFromBranchName = versionRangeFromBranchName
}
