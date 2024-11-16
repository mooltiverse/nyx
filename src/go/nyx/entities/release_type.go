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

	// The optional flag or the template to enable/disable the Git push operation. A nil value means undefined.
	GitPushForce *string `json:"gitPushForce,omitempty" yaml:"gitPushForce,omitempty"`

	// The optional flag or the template to render indicating whether or not a new tag must be generated. A nil value means undefined.
	GitTag *string `json:"gitTag,omitempty" yaml:"gitTag,omitempty"`

	// The optional flag or the template to enable/disable the Git tag operation. A nil value means undefined.
	GitTagForce *string `json:"gitTagForce,omitempty" yaml:"gitTagForce,omitempty"`

	// The optional string or the template to render to use as the tag message if a tag has to be made. A nil value means undefined.
	GitTagMessage *string `json:"gitTagMessage,omitempty" yaml:"gitTagMessage,omitempty"`

	// The list of templates to use as tag names when tagging a commit.
	GitTagNames *[]*string `json:"gitTagNames,omitempty" yaml:"gitTagNames,omitempty"`

	// This private field is set to true when user explicitly sets value for the GitTagNames field.
	// This is a workaround to Go not allowing default values for list/slice fields.
	// Since Go doesn't support default values for list/slice fields, when the GitTagNames is nil, this is the only way we have to know
	// if a user has explicitly set the GitTagNames to nil or not. If not, we will return the default value.
	gitTagNamesUserOverwrite bool

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

	// The optional template to set the draft flag of releases published to remote services. A nil value means undefined.
	PublishDraft *string `json:"publishDraft,omitempty" yaml:"publishDraft,omitempty"`

	// The optional template to set the pre-release flag of releases published to remote services. A nil value means undefined.
	PublishPreRelease *string `json:"publishPreRelease,omitempty" yaml:"publishPreRelease,omitempty"`

	// The optional template to set the name of releases published to remote services. A nil value means undefined.
	ReleaseName *string `json:"releaseName,omitempty" yaml:"releaseName,omitempty"`

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
- gitPushForce the optional flag or the template to enable/disable the Git tag operation.
- gitTag the optional flag or the template to render indicating whether or not a new tag must be generated.
- gitTagForce the optional flag or the template to enable/disable the Git tag operation.
- gitTagMessage the optional identifiers configuration block.
- gitTagNames the list of templates to use as tag names when tagging a commit.
- identifiers the optional nested map of the custom extra identifiers to be used in a release type.
- matchBranches the optional template to render as a regular expression used to match branch names.
- matchEnvironmentVariables the map of the match environment variables items, where keys are environment variable names and values are regular expressions.
- matchWorkspaceStatus the identifier of a specific workspace status to be matched.
- publish the optional flag or the template to render indicating whether or not releases must be published.
- publishDraft the optional template to set the draft flag of releases published to remote services.
- publishPreRelease the optional template to set the pre-release flag of releases published to remote services.
- releaseName the optional template to set the name of releases published to remote services.
- versionRange the optional regular expression used to constrain versions issued by this release type.
- versionRangeFromBranchName the optional flag telling if the version range must be inferred from the branch name.
*/
func NewReleaseTypeWith(assets *[]*string, collapseVersions *bool, collapsedVersionQualifier *string, description *string, filterTags *string, gitCommit *string, gitCommitMessage *string, gitPush *string, gitPushForce *string, gitTag *string, gitTagForce *string, gitTagMessage *string, gitTagNames *[]*string, identifiers *[]*Identifier, matchBranches *string, matchEnvironmentVariables *map[string]string, matchWorkspaceStatus *WorkspaceStatus, publish *string, publishDraft *string, publishPreRelease *string, releaseName *string, versionRange *string, versionRangeFromBranchName *bool) *ReleaseType {
	rt := ReleaseType{}

	rt.Assets = assets
	rt.CollapseVersions = collapseVersions
	rt.CollapsedVersionQualifier = collapsedVersionQualifier
	rt.Description = description
	rt.FilterTags = filterTags
	rt.GitCommit = gitCommit
	rt.GitCommitMessage = gitCommitMessage
	rt.GitPush = gitPush
	rt.GitPushForce = gitPushForce
	rt.GitTag = gitTag
	rt.GitTagForce = gitTagForce
	rt.GitTag = gitTag
	rt.GitTagMessage = gitTagMessage
	rt.GitTagNames = gitTagNames
	rt.Identifiers = identifiers
	rt.MatchBranches = matchBranches
	rt.MatchEnvironmentVariables = matchEnvironmentVariables
	rt.MatchWorkspaceStatus = matchWorkspaceStatus
	rt.Publish = publish
	rt.PublishDraft = publishDraft
	rt.PublishPreRelease = publishPreRelease
	rt.ReleaseName = releaseName
	rt.VersionRange = versionRange
	rt.VersionRangeFromBranchName = versionRangeFromBranchName

	// From now on, with this flag set, we will not return the default value for GitTagNames anymore
	// See GetGitTagNames() for more
	rt.gitTagNamesUserOverwrite = true

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
	rt.GitPushForce = RELEASE_TYPE_GIT_PUSH_FORCE
	rt.GitTag = RELEASE_TYPE_GIT_TAG
	rt.GitTagForce = RELEASE_TYPE_GIT_TAG_FORCE
	rt.GitTagMessage = RELEASE_TYPE_GIT_TAG_MESSAGE
	rt.GitTagNames = RELEASE_TYPE_GIT_TAG_NAMES
	rt.Identifiers = RELEASE_TYPE_IDENTIFIERS
	rt.MatchBranches = RELEASE_TYPE_MATCH_BRANCHES
	rt.MatchEnvironmentVariables = RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES
	rt.MatchWorkspaceStatus = RELEASE_TYPE_MATCH_WORKSPACE_STATUS
	rt.Publish = RELEASE_TYPE_PUBLISH
	rt.PublishDraft = RELEASE_TYPE_PUBLISH_DRAFT
	rt.PublishPreRelease = RELEASE_TYPE_PUBLISH_PRE_RELEASE
	rt.ReleaseName = RELEASE_TYPE_RELEASE_NAME
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
Returns the optional flag or the template to enable/disable the Git push operation. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitPushForce() *string {
	return rt.GitPushForce
}

/*
Sets the optional flag or the template to enable/disable the Git push operation. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitPushForce(gitPushForce *string) {
	rt.GitPushForce = gitPushForce
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
Returns the optional flag or the template to enable/disable the Git tag operation. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitTagForce() *string {
	return rt.GitTagForce
}

/*
Sets the optional flag or the template to enable/disable the Git tag operation. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitTagForce(gitTagForce *string) {
	rt.GitTagForce = gitTagForce
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
Returns the list of templates to use as tag names when tagging a commit. A nil value means undefined.
*/
func (rt *ReleaseType) GetGitTagNames() *[]*string {
	// This private field is set to true in SetGitTagNames(...) when user explicitly sets value for the GitTagNames field.
	// This is a workaround to Go not allowing default values for list/slice fields.
	// Since Go doesn't support default values for list/slice fields, when the GitTagNames is nil, this is the only way we have to know
	// if a user has explicitly set the GitTagNames to nil or not. If not, we will return the default value.
	// This is also required for backward compatibility as all existing configurations before the introduction of the GitTagNames
	// expect a behavior like the field is set to RELEASE_TYPE_GIT_TAG_NAMES.
	if rt.gitTagNamesUserOverwrite || rt.GitTagNames != nil {
		return rt.GitTagNames
	} else {
		return RELEASE_TYPE_GIT_TAG_NAMES
	}
}

/*
Sets the list of templates to use as tag names when tagging a commit. A nil value means undefined.
*/
func (rt *ReleaseType) SetGitTagNames(gitTagNames *[]*string) {
	// From now on, with this flag set, we will not return the default value for GitTagNames anymore
	// See GetGitTagNames() for more
	rt.gitTagNamesUserOverwrite = true
	rt.GitTagNames = gitTagNames
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
Returns the optional template to set the draft flag of releases published to remote services. A nil value means undefined.
*/
func (rt *ReleaseType) GetPublishDraft() *string {
	return rt.PublishDraft
}

/*
Sets the optional template to set the draft flag of releases published to remote services. A nil value means undefined.
*/
func (rt *ReleaseType) SetPublishDraft(publishDraft *string) {
	rt.PublishDraft = publishDraft
}

/*
Returns the optional template to set the pre-release flag of releases published to remote services. A nil value means undefined.
*/
func (rt *ReleaseType) GetPublishPreRelease() *string {
	return rt.PublishPreRelease
}

/*
Sets the optional template to set the pre-release flag of releases published to remote services. A nil value means undefined.
*/
func (rt *ReleaseType) SetPublishPreRelease(publishPreRelease *string) {
	rt.PublishPreRelease = publishPreRelease
}

/*
Returns the optional template to set the name of releases published to remote services.
*/
func (rt *ReleaseType) GetReleaseName() *string {
	return rt.ReleaseName
}

/*
Sets the optional template to set the name of releases published to remote services.
*/
func (rt *ReleaseType) SetReleaseName(releaseName *string) {
	rt.ReleaseName = releaseName
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
