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

import (
	"os" // https://pkg.go.dev/os

	utl "github.com/mooltiverse/nyx/modules/go/utils"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

// The following should be declared as constants but then Go wouldn't let us initialize them
var (
	// The default version identifier to bump. Value: nil
	BUMP *string = nil

	// The default changelog configuration block.
	CHANGELOG, _ = NewChangelogConfigurationWith(nil, &map[string]string{}, nil, &map[string]string{})

	// The default commit message conventions block.
	COMMIT_MESSAGE_CONVENTIONS, _ = NewCommitMessageConventionsWith(&[]*string{}, &map[string]*CommitMessageConvention{})

	// The default custom configuration file path. Value: nil
	CONFIGURATION_FILE *string = nil

	// The default working directory. Defaults to the current user directory returned by reading the os.Getwd()
	DIRECTORY *string = ignoreError(os.Getwd())

	// The default flag that prevents to alter any repository state and instead just log the actions that would be taken. Value: false
	DRY_RUN *bool = utl.PointerToBoolean(false)

	// The default Git configuration block.
	GIT, _ = NewGitConfigurationWith(&map[string]*GitRemoteConfiguration{})

	// The default initial version to use.
	// This strongly depends on the SCHEME and as long as it's SEMVER, we use that to select the initial version.
	INITIAL_VERSION *string = utl.PointerToString(ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)

	// The default preset configuration. Value: nil
	PRESET *string = nil

	// The release assets configuration block.
	RELEASE_ASSETS = &map[string]*Attachment{}

	// The default flag that alows reading releases from the history tolerating arbitrary prefixes and extra non critical characters. Value: true
	RELEASE_LENIENT *bool = utl.PointerToBoolean(true)

	// The default prefix to add at the beginning of a version identifier to generate the release identifier. Value: nil
	RELEASE_PREFIX *string = nil

	// The list of selected asset names to publish for the release type. Value: nil
	RELEASE_TYPE_ASSETS *[]*string = nil

	// The flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used. Value: false
	RELEASE_TYPE_COLLAPSE_VERSIONS *bool = utl.PointerToBoolean(false)

	// The optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed. Value: nil
	RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER *string = nil

	// The optional string or the template to render to use as the release description. Value: 'Release {{version}}'
	RELEASE_TYPE_DESCRIPTION *string = utl.PointerToString("Release {{version}}")

	// The optional template to render as a regular expression used to match tags from the commit history. Value: nil
	RELEASE_TYPE_FILTER_TAGS *string = nil

	// The optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated. Value: 'false'
	RELEASE_TYPE_GIT_COMMIT *string = utl.PointerToString("false")

	// The optional string or the template to render to use as the commit message if a commit has to be made. Value: 'Release version {{version}}'
	RELEASE_TYPE_GIT_COMMIT_MESSAGE *string = utl.PointerToString("Release version {{version}}")

	// The name of the default release type. Value: 'default'
	RELEASE_TYPE_NAME *string = utl.PointerToString("default")

	// The optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated. Value: 'false'
	RELEASE_TYPE_GIT_PUSH *string = utl.PointerToString("false")

	// The optional flag or the template to render indicating whether or not a new tag must be generated. Value: 'false'
	RELEASE_TYPE_GIT_TAG *string = utl.PointerToString("false")

	// The optional string or the template to render to use as the tag message if a tag has to be made. Value: nil
	RELEASE_TYPE_GIT_TAG_MESSAGE *string = nil

	// The list of templates to use as tag names when tagging a commit. Value: [ {{version}} ]
	RELEASE_TYPE_GIT_TAG_NAMES *[]*string = &[]*string{utl.PointerToString("{{version}}")}

	// The identifiers configuration block. Elements of this list must be of type Identifier. Value: nil
	RELEASE_TYPE_IDENTIFIERS *[]*Identifier = nil

	// The optional template to render as a regular expression used to match branch names. Value: nil
	RELEASE_TYPE_MATCH_BRANCHES *string = nil

	// The map of the match environment variables items, where keys are environment variable names and values are regular expressions.. Value: nil
	RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES *map[string]string

	// The identifier of a specific workspace status to be matched. Value: nil
	RELEASE_TYPE_MATCH_WORKSPACE_STATUS *WorkspaceStatus = nil

	// The optional flag or the template to render indicating whether or not releases must be published. Value: 'false'
	RELEASE_TYPE_PUBLISH *string = utl.PointerToString("false")

	// The optional template to set the name of releases published to remote services. Value: nil
	RELEASE_TYPE_RELEASE_NAME *string = nil

	// The optional template to render as a regular expression used to constrain versions issued by this release type. Value: nil
	RELEASE_TYPE_VERSION_RANGE *string = nil

	// The optional flag telling if the version range must be inferred from the branch name. Value: false
	RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME *bool = utl.PointerToBoolean(false)

	// The default release types block.
	RELEASE_TYPES, _ = NewReleaseTypesWith(&[]*string{RELEASE_TYPE_NAME}, &[]*string{}, &[]*string{}, &map[string]*ReleaseType{*RELEASE_TYPE_NAME: NewReleaseType()})

	// The default flag that enables loading a previously stored State file and resume operations from there. Value: false
	RESUME *bool = utl.PointerToBoolean(false)

	// The default versioning scheme to use. Value: SEMVER
	SCHEME *ver.Scheme = ver.PointerToScheme(ver.SEMVER)

	// The services configuration block.
	SERVICES *map[string]*ServiceConfiguration = &map[string]*ServiceConfiguration{}

	// The default shared custom configuration file path. Value: nil
	SHARED_CONFIGURATION_FILE *string = nil

	// The default path to the local state file. Value: nil
	STATE_FILE *string = nil

	// The default substitutions block.
	SUBSTITUTIONS, _ = NewSubstitutionsWith(&[]*string{}, &map[string]*Substitution{})

	// The default flag that tells when to print a summary to the console. Value: false
	SUMMARY *bool = utl.PointerToBoolean(false)

	// The default path to the local summary file. Value: nil
	SUMMARY_FILE *string = nil

	// The default logging level. Value: WARNING
	VERBOSITY *Verbosity = PointerToVerbosity(WARNING)

	// The default release version. Value: nil
	VERSION *string = nil
)

/*
Returns the string passed as parameter, ignoring the error, if any.

This is useful for inline assignment of a string value returned by a function that also returns an error.

Be careful when using this function as the error might be significant!
*/
func ignoreError(s string, err error) *string {
	return &s
}
