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

package configuration

import (
	"fmt" // https://pkg.go.dev/fmt
	"os"  // https://pkg.go.dev/os

	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings
	"sync"    // https://pkg.go.dev/sync

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"     // https://pkg.go.dev/github.com/sirupsen/logrus
	slices "golang.org/x/exp/slices"     // https://pkg.go.dev/golang.org/x/exp/slices

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

const (
	// The name of the argument to read for this value.
	BUMP_ARGUMENT_NAME = "--bump"

	// The short name of the argument to read for this value.
	BUMP_ARGUMENT_SHORT_NAME = "-b"

	// The name of the argument to read for this value.
	CHANGELOG_CONFIGURATION_ARGUMENT_NAME = "--changelog"

	// The name of the argument to read for this value.
	CHANGELOG_CONFIGURATION_PATH_ARGUMENT_NAME = CHANGELOG_CONFIGURATION_ARGUMENT_NAME + "-path"

	// The name of the argument to read for this value.
	CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_NAME = CHANGELOG_CONFIGURATION_ARGUMENT_NAME + "-sections"

	// The regular expression used to scan the name of a changelog section from an argument
	// name. This expression is used to detect if an argument is used to define
	// a changelog section.
	// This expression uses the 'name' capturing group which returns the section name, if detected.
	CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)$"

	// The parametrized name of the argument to read for the regular expression attribute of a
	// changelog section configuration.
	// This string is a prototype that contains a '%s' parameter for the section name
	// and must be rendered using fmt.Sprintf(CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the section with the given 'name'.
	CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_NAME + "-%s"

	// The name of the argument to read for this value.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_NAME = CHANGELOG_CONFIGURATION_ARGUMENT_NAME + "-substitutions"

	// The regular expression used to scan the name of a changelog substitution from an argument
	// name. This expression is used to detect if an argument is used to define
	// a changelog substitution.
	// This expression uses the 'name' capturing group which returns the substitution regex, if detected.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)$"

	// The parametrized name of the argument to read for the regular expression attribute of a
	// changelog substitution configuration.
	// This string is a prototype that contains a '%s' parameter for the substitution name
	// and must be rendered using fmt.Sprintf(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the substitution with the given 'name'.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_NAME + "-%s"

	// The name of the argument to read for this value.
	CHANGELOG_CONFIGURATION_TEMPLATE_ARGUMENT_NAME = CHANGELOG_CONFIGURATION_ARGUMENT_NAME + "-template"

	// The name of the argument to read for this value.
	COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_NAME = "--commit-message-conventions"

	// The name of the argument to read for this value.
	COMMIT_MESSAGE_CONVENTIONS_ENABLED_ARGUMENT_NAME = COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_NAME + "-enabled"

	// The regular expression used to scan the name of a commit message convention from an argument
	// name. This expression is used to detect if an argument is used to define
	// a commit message convention.
	// This expression uses the 'name' capturing group which returns the commit convention name, if detected.
	COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_NAME_REGEX = COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)-([a-zA-Z0-9-]+)$"

	// The parametrized name of the argument to read for the 'expression' attribute of a
	// commit message convention.
	// This string is a prototype that contains a '%s' parameter for the commit convention name
	// and must be rendered using fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_EXPRESSION_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the convention with the given 'name'.
	COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_EXPRESSION_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_NAME + "-%s-expression"

	// The parametrized name of the argument to read for the 'bumpExpressions' attribute of a
	// commit message convention.
	// This string is a prototype that contains a '%s' parameter for the commit convention name
	// and must be rendered using fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the convention with the given 'name'.
	COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_NAME + "-%s-bumpExpressions"

	// The name of the argument to read for this value.
	CONFIGURATION_FILE_ARGUMENT_NAME = "--configuration-file"

	// The short name of the argument to read for this value.
	CONFIGURATION_FILE_ARGUMENT_SHORT_NAME = "-c"

	// The name of the argument to read for this value.
	DIRECTORY_ARGUMENT_NAME = "--directory"

	// The short name of the argument to read for this value.
	DIRECTORY_ARGUMENT_SHORT_NAME = "-d"

	// The name of the argument to read for this value.
	DRY_RUN_ARGUMENT_NAME = "--dry-run"

	// The name of the argument to read for this value.
	GIT_CONFIGURATION_ARGUMENT_NAME = "--git"

	// The name of the argument to read for this value.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME = GIT_CONFIGURATION_ARGUMENT_NAME + "-remotes"

	// The regular expression used to scan the name of a Git remote configuration from an argument
	// name. This expression is used to detect if an argument is used to define
	// a Git remote configuration.
	// This expression uses the 'name' capturing group which returns the remote name, if detected.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_NAME_REGEX = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)-([a-zA-Z0-9-]+)$"

	// The parametrized name of the argument to read for the 'authenticationMethod' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-%s-authenticationMethod"

	// The parametrized name of the argument to read for the 'password' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSWORD_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSWORD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-%s-password"

	// The parametrized name of the argument to read for the 'user' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_USER_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_USER_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-%s-user"

	// The parametrized name of the argument to read for the 'privateKey' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PRIVATE_KEY_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PRIVATE_KEY_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-%s-privateKey"

	// The parametrized name of the argument to read for the 'passphrase' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSPHRASE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSPHRASE_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ARGUMENT_NAME + "-%s-passphrase"

	// The name of the argument to read for this value.
	HELP_ARGUMENT_NAME = "--help"

	// The name of the argument to read for this value.
	INITIAL_VERSION_ARGUMENT_NAME = "--initial-version"

	// The name of the argument to read for this value.
	PRESET_ARGUMENT_NAME = "--preset"

	// The name of the argument to read for this value.
	RELEASE_ASSETS_ARGUMENT_NAME = "--release-assets"

	// The regular expression used to scan the name of a release asset from an environment
	// variable name. This expression is used to detect if an argument is used to define
	// a release asset.
	// This expression uses the 'name' capturing group which returns the release asset name, if detected.
	RELEASE_ASSETS_ARGUMENT_ITEM_NAME_REGEX = RELEASE_ASSETS_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)-([a-zA-Z0-9-]+)$"

	// The parametrized name of the argument to read for the 'fileName' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_FILE_NAME_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release assetm file name with the given 'name'.
	RELEASE_ASSETS_ARGUMENT_ITEM_FILE_NAME_FORMAT_STRING = RELEASE_ASSETS_ARGUMENT_NAME + "-%s-fileName"

	// The parametrized name of the argument to read for the 'description' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release asset description with the given 'name'.
	RELEASE_ASSETS_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_ASSETS_ARGUMENT_NAME + "-%s-description"

	// The parametrized name of the argument to read for the 'path' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_PATH_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release asset path with the given 'name'.
	RELEASE_ASSETS_ARGUMENT_ITEM_PATH_FORMAT_STRING = RELEASE_ASSETS_ARGUMENT_NAME + "-%s-path"

	// The parametrized name of the argument to read for the 'type' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_TYPE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release asset type with the given 'name'.
	RELEASE_ASSETS_ARGUMENT_ITEM_TYPE_FORMAT_STRING = RELEASE_ASSETS_ARGUMENT_NAME + "-%s-type"

	// The name of the argument to read for this value.
	RELEASE_LENIENT_ARGUMENT_NAME = "--release-lenient"

	// The name of the argument to read for this value.
	RELEASE_PREFIX_ARGUMENT_NAME = "--release-prefix"

	// The name of the argument to read for this value.
	RELEASE_TYPES_ARGUMENT_NAME = "--release-types"

	// The name of the argument to read for this value.
	RELEASE_TYPES_ENABLED_ARGUMENT_NAME = RELEASE_TYPES_ARGUMENT_NAME + "-enabled"

	// The name of the argument to read for this value.
	RELEASE_TYPES_PUBLICATION_SERVICES_ARGUMENT_NAME = RELEASE_TYPES_ARGUMENT_NAME + "-publication-services"

	// The name of the argument to read for this value.
	RELEASE_TYPES_REMOTE_REPOSITORIES_ARGUMENT_NAME = RELEASE_TYPES_ARGUMENT_NAME + "-remote-repositories"

	// The regular expression used to scan the name of a release type from an argument
	// name. This expression is used to detect if an argument is used to define
	// a release type.
	// This expression uses the 'name' capturing group which returns the release type name, if detected.
	RELEASE_TYPES_ARGUMENT_ITEM_NAME_REGEX = RELEASE_TYPES_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)-([a-zA-Z0-9-]+)$"

	// The parametrized name of the argument to read for the 'assets' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_ASSETS_FORMAT_STRING, name)
	// in order to get the actual name of the argument variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_ASSETS_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-assets"

	// The parametrized name of the argument to read for the 'collapseVersions' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-collapse-versions"

	// The parametrized name of the argument to read for the 'collapsedVersionQualifier' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-collapsed-version-qualifier"

	// The parametrized name of the argument to read for the 'description' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-description"

	// The parametrized name of the argument to read for the 'filterTags' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_FILTER_TAGS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_FILTER_TAGS_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-filter-tags"

	// The parametrized name of the argument to read for the 'gitCommit' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-git-commit"

	// The parametrized name of the argument to read for the 'gitCommitMessage' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-git-commit-message"

	// The parametrized name of the argument to read for the 'gitPush' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_GIT_PUSH_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-git-push"

	// The parametrized name of the argument to read for the 'gitTag' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-git-tag"

	// The parametrized name of the argument to read for the 'gitTagMessage' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-git-tag-message"

	// The parametrized name of the argument to read for the 'identifiers' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the commit release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_IDENTIFIERS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_IDENTIFIERS_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-identifiers"

	// The parametrized name of the argument to read for the 'matchBranches' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_BRANCHES_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_MATCH_BRANCHES_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-match-branches"

	// The parametrized name of the argument to read for the 'matchEnvironmentVariables' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-match-environment-variables"

	// The parametrized name of the argument to read for the 'matchWorkspaceStatus' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-match-workspace-status"

	// The parametrized name of the argument to read for the 'publish' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_PUBLISH_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_PUBLISH_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-publish"

	// The parametrized name of the argument to read for the 'versionRange' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-version-range"

	// The parametrized name of the argument to read for the 'versionRangeFromBranchName' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING = RELEASE_TYPES_ARGUMENT_NAME + "-%s-version-range-from-branch-name"

	// The name of the argument to read for this value.
	RESUME_ARGUMENT_NAME = "--resume"

	// The name of the argument to read for this value.
	SCHEME_ARGUMENT_NAME = "--scheme"

	// The name of the argument to read for this value.
	SERVICES_ARGUMENT_NAME = "--services"

	// The regular expression used to scan the name of a service from an argument
	// name. This expression is used to detect if an argument is used to define
	// a service.
	// This expression uses the 'name' capturing group which returns the service name, if detected.
	SERVICES_ARGUMENT_ITEM_NAME_REGEX = SERVICES_ARGUMENT_NAME + "-(?<name>[a-zA-Z0-9]+)-([a-zA-Z0-9_]+)$"

	// The parametrized name of the argument to read for the 'options' attribute of a
	// service.
	// This string is a prototype that contains a '%s' parameter for the service name
	// and must be rendered using fmt.Sprintf(SERVICES_ARGUMENT_ITEM_OPTIONS_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the service with the given 'name'.
	SERVICES_ARGUMENT_ITEM_OPTIONS_FORMAT_STRING = SERVICES_ARGUMENT_NAME + "-%s-options"

	// The parametrized name of the argument to read for the 'type' attribute of a
	// service.
	// This string is a prototype that contains a '%s' parameter for the service name
	// and must be rendered using fmt.Sprintf(SERVICES_ARGUMENT_ITEM_TYPE_FORMAT_STRING, name)
	// in order to get the actual name of the argument that brings the value for the service type with the given 'name'.
	SERVICES_ARGUMENT_ITEM_TYPE_FORMAT_STRING = SERVICES_ARGUMENT_NAME + "-%s-type"

	// The name of the argument to read for this value.
	SHARED_CONFIGURATION_FILE_ARGUMENT_NAME = "--shared-configuration-file"

	// The name of the argument to read for this value.
	STATE_FILE_ARGUMENT_NAME = "--state-file"

	// The name of the argument to read for this value.
	VERBOSITY_ARGUMENT_NAME = "--verbosity"

	// The name of the argument to read for this value.
	VERBOSITY_FATAL_ARGUMENT_NAME = "--fatal"

	// The name of the argument to read for this value.
	VERBOSITY_ERROR_ARGUMENT_NAME = "--error"

	// The name of the argument to read for this value.
	VERBOSITY_WARNING_ARGUMENT_NAME = "--warning"

	// The name of the argument to read for this value.
	VERBOSITY_INFO_ARGUMENT_NAME = "--info"

	// The name of the argument to read for this value.
	VERBOSITY_DEBUG_ARGUMENT_NAME = "--debug"

	// The name of the argument to read for this value.
	VERBOSITY_TRACE_ARGUMENT_NAME = "--trace"

	// The name of the argument to read for this value.
	VERSION_ARGUMENT_NAME = "--version"

	// The short name of the argument to read for this value.
	VERSION_ARGUMENT_SHORT_NAME = "-v"
)

var (
	// the lock used to synchronize routines
	commandLineConfigurationLayerLock = &sync.Mutex{}

	// The single instance for this class.
	commandLineConfigurationLayerInstance *CommandLineConfigurationLayer
)

/*
A configuration layer that returns values read by command line arguments. Each argument must comply with
a well defined naming convention in order to be recognized.

This is a singleton class so instances are to be retrieved via the GetCommandLineConfigurationLayerInstance() method.

This object allows read only operations.
*/
type CommandLineConfigurationLayer struct {
	// The map of arguments keys and values
	argumentsMap map[string]string

	// The changelog configuration section.
	changelog *ent.ChangelogConfiguration

	// The commit message convention configuration section.
	commitMessageConventions *ent.CommitMessageConventions

	// The Git configuration section.
	git *ent.GitConfiguration

	// The release assets configuration section
	releaseAssets *map[string]*ent.Attachment

	// The release types configuration section.
	releaseTypes *ent.ReleaseTypes

	// The services configuration section
	services *map[string]*ent.ServiceConfiguration
}

/*
Returns a new instance of this class.

This constructor should be used for test purposes only, otherwise the singleton instance should be used
(see GetCommandLineConfigurationLayerInstance())
*/
func NewCommandLineConfigurationLayer() *CommandLineConfigurationLayer {
	return &CommandLineConfigurationLayer{}
}

/*
Returns the singleton instance of this class.
*/
func GetCommandLineConfigurationLayerInstance() *CommandLineConfigurationLayer {
	if commandLineConfigurationLayerInstance == nil {
		commandLineConfigurationLayerLock.Lock()
		defer commandLineConfigurationLayerLock.Unlock()
		if commandLineConfigurationLayerInstance == nil {
			commandLineConfigurationLayerInstance = NewCommandLineConfigurationLayer()
		}
	}

	return commandLineConfigurationLayerInstance
}

/*
Converts the given slice of strings into a slice of the same size with pointers to the input strings.

Arguments are as follows:

- items the slice of strrings to convert
*/
func (clcl *CommandLineConfigurationLayer) toSliceOfStringPointers(items []string) []*string {
	if items == nil {
		return nil
	}
	pointerItems := make([]*string, len(items))
	for i := 0; i < len(items); i++ {
		pointerItems[i] = &items[i]
	}
	return pointerItems
}

/*
Reads the given argument and parses it as a comma separated list of names that will
be returned as a list.

Arguments are as follows:

- attributeGroupName the name of the attribute group, used for log messages
- leafAttributeName the name of the specific attribute within the group, used for log messages
- argName the name of the argument to read and parse
*/
func (clcl *CommandLineConfigurationLayer) getItemNamesListFromArgument(attributeGroupName string, leafAttributeName string, argName string) []string {
	itemNames := make([]string, 0)
	argValue := clcl.getArgument(argName)
	if argValue == nil {
		log.Tracef("no argument named '%s' can be found, assuming the arguments do not set the '%s.%s' configuration option", argName, attributeGroupName, leafAttributeName)
	} else if "" == *argValue {
		log.Tracef("the argument named '%s' has been found but is empty, assuming the arguments do not set the '%s.%s' configuration option", argName, attributeGroupName, leafAttributeName)
	} else {
		log.Tracef("the argument named '%s' has been found with value '%s'. Parsing the value to infer the '%s.%s' configuration option", argName, *argValue, attributeGroupName, leafAttributeName)
		itemNames = append(itemNames, strings.Split(*argValue, ",")...)
		log.Tracef("the argument named '%s' has been parsed and yields to %d items: '%s'", argName, len(itemNames), itemNames)
	}
	return itemNames
}

/*
Scans all arguments trying to match their names against the given regular expression. If a match is found
then a capturing group named 'name' is used to extrapolate the item name, which will then be returned in the result.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - regex a regular expression used to match argument names. It must also contain a matching
    group named 'name' which will be used to extrapolate the name
  - ignores an optional collection of variable names to ignore. It may be nil

Errors can be:

- PatternSyntaxError if the given regular expression can't be compiled
- IllegalArgumentError if the given regular expression does not contain the 'name' capturing group
*/
func (clcl *CommandLineConfigurationLayer) scanItemNamesInArguments(attributeGroupName string, regex string, ignores []string) ([]string, error) {
	log.Tracef("scanning arguments searching for the names of configured elements belonging to the '%s' group using the regular expression: '%s'", attributeGroupName, regex)
	// Scan all arguments whose name matches the items regular expression
	// so that we can infer the name of the various items
	itemNames := make([]string, 0)
	for argName, _ := range clcl.getArgMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, argName)) {
			re, err := regexp2.Compile(regex, 0)
			if err != nil {
				return nil, &errs.PatternSyntaxError{Message: fmt.Sprintf("regular expression '%s' can't be compiled: %v", regex, err), Cause: err}
			}

			m, err := re.FindStringMatch(argName)
			if err != nil {
				return nil, &errs.PatternSyntaxError{Message: fmt.Sprintf("regular expression '%s' can't be matched: %v", regex, err), Cause: err}
			}

			if m != nil { // when this is nil it means no match was found, so skip this argument
				log.Tracef("the argument named '%s' denotes it configures a '%s' item", argName, attributeGroupName)
				g := m.GroupByName("name")
				if len(g.Captures) == 0 {
					log.Warnf("the argument named '%s' denotes it configures a '%s' item but the item name can't be extrapolated using the regular expression: '%s'", argName, attributeGroupName, regex)
				} else {
					name := g.Captures[0].String()
					if "" == name {
						log.Warnf("the argument named '%s' denotes it configures a '%s' item but the item name can't be extrapolated using the regular expression: '%s'", argName, attributeGroupName, regex)
					} else {
						log.Tracef("the argument named '%s' denotes it configures a '%s' item named '%s'", argName, attributeGroupName, name)
						itemNames = append(itemNames, name)
					}
				}
			}
		}
	}
	log.Tracef("the set of '%s' items configured using arguments has %d items: '%s'", attributeGroupName, len(itemNames), itemNames)
	return itemNames, nil
}

/*
Scans the arguments and takes all the ones starting with a certain prefix
and put them in the returned map. The key stored in the map is the trailing part of the variable name
after the given prefix, while values are the corresponding variable values.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - argNamePrefix the prefix of all argument names to be considered map items. This
    will be discarded from variable names in order to get item keys.
  - ignores an optional collection of variable names to ignore. It may be nil
*/
func (clcl *CommandLineConfigurationLayer) getAttributeMapFromArgument(attributeGroupName string, argNamePrefix string, ignores []string) map[string]string {
	log.Tracef("scanning arguments searching for the items belonging to the '%s' group using the prefix: '%s'", attributeGroupName, argNamePrefix)
	attributeMap := make(map[string]string)
	// Scan arguments in order to find the items whose name starts with the right prefix.
	// The trailing part is then supposed to be the map item name
	argNamePrefix = argNamePrefix + "-" // avoid false positives and consider that before the entry name has another separator '-', that we want to ignore hereafter

	for argName, _ := range clcl.getArgMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, argName)) {
			if strings.HasPrefix(argName, argNamePrefix) && len(argName) > len(argNamePrefix) {
				mapItemName := strings.Replace(argName, argNamePrefix, "", 1)
				mapItemValue := clcl.getArgument(argName)
				attributeMap[mapItemName] = *mapItemValue
				log.Tracef("the '%s' map has the following item: '%s'='%s'", attributeGroupName, mapItemName, *mapItemValue)
			}
		}
	}

	log.Tracef("the map of '%s' items configured using arguments has %d items: '%s'", attributeGroupName, len(attributeMap), attributeMap)
	return attributeMap
}

/*
Scans the arguments and takes all the ones starting with a certain prefix
and put them in the returned map. The key stored in the map is the trailing part of the variable name
after the given prefix, while values are the corresponding variable values.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - argNamePrefix the prefix of all argument names to be considered map items. This
    prefix will be discarded from variable names in order to get item keys.
  - ignores an optional collection of variable names to ignore. It may be nil

Errors can be:

- IllegalPropertyError if malformed items are encountered
*/
func (clcl *CommandLineConfigurationLayer) getIdentifiersListFromArgument(attributeGroupName string, argNamePrefix string, ignores []string) ([]*ent.Identifier, error) {
	log.Tracef("scanning arguments searching for the items belonging to the '%s' group using the prefix: '%s'", attributeGroupName, argNamePrefix)

	identifiersMap := make(map[int]*ent.Identifier)
	// Scan arguments in order to find the items whose name starts with the right prefix.
	// The trailing part is then supposed to be the concatenation of:
	// - the item ordinal within the list
	// - the separator '-'
	// - the name of the attribute of the item
	// We store them in a sorted map so we can return items ordered as they appeared (the order is based on their ordinals)
	argNamePrefix = argNamePrefix + "-" // avoid false positives and consider that before the ordinal each variable name has another separator '-', that we want to ignore hereafter
	for argName, _ := range clcl.getArgMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, argName)) {
			if strings.HasPrefix(argName, argNamePrefix) && len(argName) > len(argNamePrefix) {
				mapItemName := strings.Replace(argName, argNamePrefix, "", 1)
				mapItemValue := clcl.getArgument(argName)

				listItemNameComponents := strings.Split(mapItemName, "-")
				if len(listItemNameComponents) != 2 {
					return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument name %s is malformed as it is supposed to be in the form %s<INTEGER>-<ATTRIBUTE>", argName, argNamePrefix)}
				}

				// parse the first part as the integer ordinal
				ordinal, err := strconv.Atoi(listItemNameComponents[0])
				if err != nil {
					return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument name %s is malformed as it is supposed to be in the form %s<INTEGER>-<ATTRIBUTE>. In this case the integer part %s doesn't seem to be a valid integer", argName, argNamePrefix, listItemNameComponents[0])}
				}

				// create the object instance and store it in the map, if not already present
				if _, contains := identifiersMap[ordinal]; !contains {
					identifiersMap[ordinal] = ent.NewIdentifier()
				}

				// now set the attribute, remember that attributes are set with different iterations of this loop, one for each iteration
				switch strings.ToLower(listItemNameComponents[1]) {
				case "qualifier":
					identifiersMap[ordinal].SetQualifier(mapItemValue)
					break
				case "value":
					identifiersMap[ordinal].SetValue(mapItemValue)
					break
				case "position":
					position, err := ent.ValueOfPosition(*mapItemValue)
					if err != nil {
						return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", argName, *mapItemValue)}
					} else {
						identifiersMap[ordinal].SetPosition(&position)
					}
					break
				default:
					log.Warnf("the argument '%s' defines an unrecognized identifier attribute '%s'", argName, listItemNameComponents[1])
				}
			}
		}
	}
	log.Tracef("the map of '%s' items configured using arguments has %d items", attributeGroupName, len(identifiersMap))
	// Now produce a list from the sorted map so it keeps the item ordering
	ordinals := make([]int, 0)
	for k, _ := range identifiersMap {
		ordinals = append(ordinals, k)
	}
	slices.Sort(ordinals)
	identifiersList := make([]*ent.Identifier, 0)
	for _, o := range ordinals {
		identifiersList = append(identifiersList, identifiersMap[o])
	}
	return identifiersList, nil
}

/*
Parses the given slice of arguments, where each argument is in the form NAME=VALUE or simply NAME (for flags
with no value) and returns them in a map.

Arguments are as follows:

- arguments the slice of arguments to parse
*/
func (clcl *CommandLineConfigurationLayer) parseArguments(arguments []string) map[string]string {
	argumentsMap := make(map[string]string)

	// now parse all arguments and put them in the map
	for _, entry := range arguments {
		// options all start with a '-' or '--' so we can ignore all the rest (which is likely the command)
		if entry != "" && strings.HasPrefix(entry, "-") {
			entryValue := strings.Split(entry, "=")
			if len(entryValue) == 1 {
				argumentsMap[entryValue[0]] = "" // this is a flag, with no value
			} else if len(entryValue) == 2 {
				argumentsMap[entryValue[0]] = entryValue[1]
			} else if len(entryValue) > 2 {
				log.Errorf("malformed argument %s", entry)
			}
		}
	}

	return argumentsMap
}

/*
Returns the value of the arguments where keys are variable names and values are their values.

The returned map is saved and reused for future uses so it's initialized only once.
*/
func (clcl *CommandLineConfigurationLayer) getArgMap() map[string]string {
	if clcl.argumentsMap == nil {
		commandLineConfigurationLayerLock.Lock()
		defer commandLineConfigurationLayerLock.Unlock()
		if clcl.argumentsMap == nil {
			clcl.argumentsMap = clcl.parseArguments(os.Args[1:]) // first argument is always the executable name so we can ignore it
		}
	}

	return clcl.argumentsMap
}

/*
Returns true if an argument with the given name has been passed, false otherwise.

This method doesn't care about the value, which might not be there at all.

Arguments are as follows:

- name the name of the argument to get
*/
func (clcl *CommandLineConfigurationLayer) hasArgument(name string) bool {
	args := clcl.getArgMap() // this way we make sure the underlying map has been initialized
	_, ok := args[name]
	return ok
}

/*
Returns the value of the argument with the given name, or nil if no such variable has been defined.

Arguments are as follows:

- name the name of the argument to get
*/
func (clcl *CommandLineConfigurationLayer) getArgument(name string) *string {
	args := clcl.getArgMap() // this way we make sure the underlying map has been initialized
	val, ok := args[name]
	if ok {
		return &val
	} else {
		return nil
	}
}

/*
Sets the arguments for this configuration layer, replacing the ones coming from the command line arguments.
Each item in the given slide must be in the form NAME=VALUE.

This method should be used for test purposes only and allows to set name value pairs just as if they were
read from the operating system.

Arguments are as follows:

- arguments the slice of arguments to parse
*/
func (clcl *CommandLineConfigurationLayer) withArguments(arguments []string) {
	if clcl.argumentsMap == nil {
		commandLineConfigurationLayerLock.Lock()
		defer commandLineConfigurationLayerLock.Unlock()
		if clcl.argumentsMap == nil {
			clcl.argumentsMap = clcl.parseArguments(arguments)
		}
	}
}

/*
Returns the version identifier to bump as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetBump() (*string, error) {
	if clcl.getArgument(BUMP_ARGUMENT_SHORT_NAME) != nil {
		return clcl.getArgument(BUMP_ARGUMENT_SHORT_NAME), nil
	} else {
		return clcl.getArgument(BUMP_ARGUMENT_NAME), nil
	}
}

/*
Returns the changelog configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetChangelog() (*ent.ChangelogConfiguration, error) {
	if clcl.changelog == nil {
		// parse the 'sections' map
		sections := make(map[string]string)
		sectionNames, err := clcl.scanItemNamesInArguments("changelog", CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all section names configured through arguments and we can
		// query specific arguments
		for _, sectionName := range sectionNames {
			sectionValue := clcl.getArgument(fmt.Sprintf(CHANGELOG_CONFIGURATION_SECTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING, sectionName))
			sections[sectionName] = *sectionValue
		}

		// parse the 'substitutions' map
		substitutions := make(map[string]string)
		substitutionNames, err := clcl.scanItemNamesInArguments("changelog", CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all section names configured through arguments and we can
		// query specific arguments
		for _, substitutionName := range substitutionNames {
			substitutionValue := clcl.getArgument(fmt.Sprintf(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ARGUMENT_ITEM_REGEXP_FORMAT_STRING, substitutionName))
			substitutions[substitutionName] = *substitutionValue
		}

		clcl.changelog, err = ent.NewChangelogConfigurationWith(clcl.getArgument(CHANGELOG_CONFIGURATION_PATH_ARGUMENT_NAME), &sections, clcl.getArgument(CHANGELOG_CONFIGURATION_TEMPLATE_ARGUMENT_NAME), &substitutions)
		if err != nil {
			return nil, err
		}
	}
	return clcl.changelog, nil
}

/*
Returns the commit message convention configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetCommitMessageConventions() (*ent.CommitMessageConventions, error) {
	if clcl.commitMessageConventions == nil {
		// parse the 'enabled' items list
		enabled := clcl.getItemNamesListFromArgument("commitMessageConventions", "enabled", COMMIT_MESSAGE_CONVENTIONS_ENABLED_ARGUMENT_NAME)

		// parse the 'items' map
		items := make(map[string]*ent.CommitMessageConvention)

		itemNames, err := clcl.scanItemNamesInArguments("commitMessageConventions", COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through arguments and we can
		// query specific arguments
		for _, itemName := range itemNames {
			expression := clcl.getArgument(fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_EXPRESSION_FORMAT_STRING, itemName))
			bumpExpressions := clcl.getAttributeMapFromArgument("commitMessageConventions"+"."+itemName+"."+"bumpExpressions", fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ARGUMENT_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, itemName), nil)

			items[itemName] = ent.NewCommitMessageConventionWith(expression, &bumpExpressions)
		}
		enabledPointers := clcl.toSliceOfStringPointers(enabled)
		clcl.commitMessageConventions, err = ent.NewCommitMessageConventionsWith(&enabledPointers, &items)
		if err != nil {
			return nil, err
		}
	}
	return clcl.commitMessageConventions, nil
}

/*
Returns the path to a custom configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetConfigurationFile() (*string, error) {
	if clcl.getArgument(CONFIGURATION_FILE_ARGUMENT_SHORT_NAME) != nil {
		return clcl.getArgument(CONFIGURATION_FILE_ARGUMENT_SHORT_NAME), nil
	} else {
		return clcl.getArgument(CONFIGURATION_FILE_ARGUMENT_NAME), nil
	}
}

/*
Returns the directory to use as the working directory as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetDirectory() (*string, error) {
	if clcl.getArgument(DIRECTORY_ARGUMENT_SHORT_NAME) != nil {
		return clcl.getArgument(DIRECTORY_ARGUMENT_SHORT_NAME), nil
	} else {
		return clcl.getArgument(DIRECTORY_ARGUMENT_NAME), nil
	}
}

/*
Returns the value of the dry run flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetDryRun() (*bool, error) {
	dryRunString := clcl.getArgument(DRY_RUN_ARGUMENT_NAME)
	if dryRunString == nil || *dryRunString == "" {
		if clcl.hasArgument(DRY_RUN_ARGUMENT_NAME) {
			// this is a flag so the value may not be passed
			return utl.PointerToBoolean(true), nil
		} else {
			return nil, nil
		}
	}
	dryRun, err := strconv.ParseBool(*dryRunString)
	return &dryRun, err
}

/*
Returns the Git configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetGit() (*ent.GitConfiguration, error) {
	if clcl.git == nil {
		// parse the 'remotes' map
		remotes := make(map[string]*ent.GitRemoteConfiguration)

		itemNames, err := clcl.scanItemNamesInArguments("git", GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through arguments and we can
		// query specific arguments
		for _, itemName := range itemNames {
			var authenticationMethod *ent.AuthenticationMethod = nil
			authenticationMethodString := clcl.getArgument(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName))
			if authenticationMethodString != nil {
				am, err := ent.ValueOfAuthenticationMethod(*authenticationMethodString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName), *authenticationMethodString), Cause: err}
				}
				authenticationMethod = &am
			}
			password := clcl.getArgument(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSWORD_FORMAT_STRING, itemName))
			user := clcl.getArgument(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_USER_FORMAT_STRING, itemName))
			privateKey := clcl.getArgument(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PRIVATE_KEY_FORMAT_STRING, itemName))
			passphrase := clcl.getArgument(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ARGUMENT_ITEM_PASSPHRASE_FORMAT_STRING, itemName))

			remotes[itemName] = ent.NewGitRemoteConfigurationWith(authenticationMethod, user, password, privateKey, passphrase)
		}

		clcl.git, err = ent.NewGitConfigurationWith(&remotes)
		if err != nil {
			return nil, err
		}
	}
	return clcl.git, nil
}

/*
Returns the initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetInitialVersion() (*string, error) {
	return clcl.getArgument(INITIAL_VERSION_ARGUMENT_NAME), nil
}

/*
Returns the selected preset configuration as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetPreset() (*string, error) {
	return clcl.getArgument(PRESET_ARGUMENT_NAME), nil
}

/*
Returns the release assets configuration section. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetReleaseAssets() (*map[string]*ent.Attachment, error) {
	if clcl.releaseAssets == nil {
		ras := make(map[string]*ent.Attachment)

		itemNames, err := clcl.scanItemNamesInArguments("releaseAssets", RELEASE_ASSETS_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			fileName := clcl.getArgument(fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_FILE_NAME_FORMAT_STRING, itemName))
			description := clcl.getArgument(fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING, itemName))
			path := clcl.getArgument(fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_PATH_FORMAT_STRING, itemName))
			attachmentType := clcl.getArgument(fmt.Sprintf(RELEASE_ASSETS_ARGUMENT_ITEM_TYPE_FORMAT_STRING, itemName))

			ras[itemName] = ent.NewAttachmentWith(fileName, description, path, attachmentType)
		}
		clcl.releaseAssets = &ras
	}
	return clcl.releaseAssets, nil
}

/*
Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetReleaseLenient() (*bool, error) {
	releaseLenientString := clcl.getArgument(RELEASE_LENIENT_ARGUMENT_NAME)
	if releaseLenientString == nil || *releaseLenientString == "" {
		if clcl.hasArgument(RELEASE_LENIENT_ARGUMENT_NAME) {
			// this is a flag so the value may not be passed
			return utl.PointerToBoolean(true), nil
		} else {
			return nil, nil
		}
	}
	releaseLenient, err := strconv.ParseBool(*releaseLenientString)
	return &releaseLenient, err
}

/*
Returns the prefix to use in release name generation as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetReleasePrefix() (*string, error) {
	return clcl.getArgument(RELEASE_PREFIX_ARGUMENT_NAME), nil
}

/*
Returns the release types configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetReleaseTypes() (*ent.ReleaseTypes, error) {
	if clcl.releaseTypes == nil {
		// parse the 'enabled' items list
		enabled := clcl.getItemNamesListFromArgument("releaseTypes", "enabled", RELEASE_TYPES_ENABLED_ARGUMENT_NAME)

		// parse the 'publicationServices' items list
		publicationServices := clcl.getItemNamesListFromArgument("releaseTypes", "publicationServices", RELEASE_TYPES_PUBLICATION_SERVICES_ARGUMENT_NAME)

		// parse the 'remoteRepositories' items list
		remoteRepositories := clcl.getItemNamesListFromArgument("releaseTypes", "remoteRepositories", RELEASE_TYPES_REMOTE_REPOSITORIES_ARGUMENT_NAME)

		// parse the 'items' map
		items := make(map[string]*ent.ReleaseType)

		// ignore the RELEASE_TYPES_PUBLICATION_SERVICES_ARGUMENT_NAME and RELEASE_TYPES_REMOTE_REPOSITORIES_ARGUMENT_NAME variables or they're interpreted as 'PUBLICATION' items
		itemNames, err := clcl.scanItemNamesInArguments("releaseTypes", RELEASE_TYPES_ARGUMENT_ITEM_NAME_REGEX, []string{RELEASE_TYPES_PUBLICATION_SERVICES_ARGUMENT_NAME, RELEASE_TYPES_REMOTE_REPOSITORIES_ARGUMENT_NAME})
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through arguments and we can
		// query specific arguments
		for _, itemName := range itemNames {
			var collapseVersions *bool = nil
			collapseVersionsString := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName))
			if collapseVersionsString != nil {
				// empty string is considered 'false'
				if "" == *collapseVersionsString {
					cv := false
					collapseVersions = &cv
				} else {
					cv, err := strconv.ParseBool(*collapseVersionsString)
					if err != nil {
						return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName), *collapseVersionsString), Cause: err}
					}
					collapseVersions = &cv
				}
			}
			assetsList := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_ASSETS_FORMAT_STRING, itemName))
			var assets *[]*string
			if assetsList != nil {
				assetsSlice := strings.Split(*assetsList, ",")
				var assetsArray []*string
				for _, assetName := range assetsSlice {
					assetNameCopy := assetName
					assetsArray = append(assetsArray, &assetNameCopy)
				}
				assets = &assetsArray
			} else {
				assets = nil
			}
			collapseVersionQualifier := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, itemName))
			description := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_DESCRIPTION_FORMAT_STRING, itemName))
			filterTags := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_FILTER_TAGS_FORMAT_STRING, itemName))
			gitCommit := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_FORMAT_STRING, itemName))
			gitCommitMessage := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, itemName))
			gitPush := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_PUSH_FORMAT_STRING, itemName))
			gitTag := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_FORMAT_STRING, itemName))
			gitTagMessage := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, itemName))
			identifiers, err := clcl.getIdentifiersListFromArgument("releaseTypes"+"."+itemName+"."+"identifiers", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_IDENTIFIERS_FORMAT_STRING, itemName), nil)
			if err != nil {
				return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_IDENTIFIERS_FORMAT_STRING, itemName)), Cause: err}
			}
			matchBranches := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_BRANCHES_FORMAT_STRING, itemName))
			matchEnvironmentVariables := clcl.getAttributeMapFromArgument("releaseTypes"+"."+itemName+"."+"matchEnvironmentVariables", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, itemName), nil)
			var matchWorkspaceStatus *ent.WorkspaceStatus = nil
			matchWorkspaceStatusString := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName))
			if matchWorkspaceStatusString != nil {
				mws, err := ent.ValueOfWorkspaceStatus(*matchWorkspaceStatusString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName), *matchWorkspaceStatusString), Cause: err}
				}
				matchWorkspaceStatus = &mws
			}
			publish := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_PUBLISH_FORMAT_STRING, itemName))
			versionRange := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FORMAT_STRING, itemName))
			var versionRangeFromBranchName *bool = nil
			versionRangeFromBranchNameString := clcl.getArgument(fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName))
			if versionRangeFromBranchNameString != nil {
				vrfbn, err := strconv.ParseBool(*versionRangeFromBranchNameString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ARGUMENT_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName), *versionRangeFromBranchNameString), Cause: err}
				}
				versionRangeFromBranchName = &vrfbn
			}

			items[itemName] = ent.NewReleaseTypeWith(assets, collapseVersions, collapseVersionQualifier, description, filterTags, gitCommit, gitCommitMessage, gitPush, gitTag, gitTagMessage, &identifiers, matchBranches, &matchEnvironmentVariables, matchWorkspaceStatus, publish, versionRange, versionRangeFromBranchName)
		}

		enabledPointers := clcl.toSliceOfStringPointers(enabled)
		publicationServicesPointers := clcl.toSliceOfStringPointers(publicationServices)
		remoteRepositoriesPointers := clcl.toSliceOfStringPointers(remoteRepositories)
		clcl.releaseTypes, err = ent.NewReleaseTypesWith(&enabledPointers, &publicationServicesPointers, &remoteRepositoriesPointers, &items)
		if err != nil {
			return nil, err
		}
	}

	return clcl.releaseTypes, nil
}

/*
Returns the value of the resume flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetResume() (*bool, error) {
	resumeString := clcl.getArgument(RESUME_ARGUMENT_NAME)
	if resumeString == nil || *resumeString == "" {
		if clcl.hasArgument(RESUME_ARGUMENT_NAME) {
			// this is a flag so the value may not be passed
			return utl.PointerToBoolean(true), nil
		} else {
			return nil, nil
		}
	}
	resume, err := strconv.ParseBool(*resumeString)
	return &resume, err
}

/*
Returns the versioning scheme as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetScheme() (*ver.Scheme, error) {
	schemeString := clcl.getArgument(SCHEME_ARGUMENT_NAME)
	if schemeString == nil {
		return nil, nil
	}
	scheme, err := ver.ValueOfScheme("SEMVER")
	return &scheme, err
}

/*
Returns the services configuration section. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetServices() (*map[string]*ent.ServiceConfiguration, error) {
	if clcl.services == nil {
		svcs := make(map[string]*ent.ServiceConfiguration)

		itemNames, err := clcl.scanItemNamesInArguments("services", SERVICES_ARGUMENT_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through arguments and we can
		// query specific arguments
		for _, itemName := range itemNames {
			var serviceType ent.Provider
			typeString := clcl.getArgument(fmt.Sprintf(SERVICES_ARGUMENT_ITEM_TYPE_FORMAT_STRING, itemName))
			if typeString != nil {
				sType, err := ent.ValueOfProvider(*typeString)
				serviceType = sType
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(SERVICES_ARGUMENT_ITEM_TYPE_FORMAT_STRING, itemName), *typeString)}
				}
			}
			options := clcl.getAttributeMapFromArgument("services"+"."+itemName+"."+"options", fmt.Sprintf(SERVICES_ARGUMENT_ITEM_OPTIONS_FORMAT_STRING, itemName), nil)

			svcs[itemName] = ent.NewServiceConfigurationWith(&serviceType, &options)
		}
		clcl.services = &svcs
	}
	return clcl.services, nil
}

/*
Returns the path to a custom shared configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetSharedConfigurationFile() (*string, error) {
	return clcl.getArgument(SHARED_CONFIGURATION_FILE_ARGUMENT_NAME), nil
}

/*
Returns the path to the file where the Nyx State must be saved as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetStateFile() (*string, error) {
	return clcl.getArgument(STATE_FILE_ARGUMENT_NAME), nil
}

/*
Returns the logging verbosity level as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetVerbosity() (*ent.Verbosity, error) {
	if clcl.hasArgument(VERBOSITY_TRACE_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.TRACE), nil
	} else if clcl.hasArgument(VERBOSITY_DEBUG_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.DEBUG), nil
	} else if clcl.hasArgument(VERBOSITY_INFO_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.INFO), nil
	} else if clcl.hasArgument(VERBOSITY_WARNING_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.WARNING), nil
	} else if clcl.hasArgument(VERBOSITY_ERROR_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.ERROR), nil
	} else if clcl.hasArgument(VERBOSITY_FATAL_ARGUMENT_NAME) {
		return ent.PointerToVerbosity(ent.FATAL), nil
	}

	verbosityString := clcl.getArgument(VERBOSITY_ARGUMENT_NAME)
	if verbosityString == nil {
		return nil, nil
	} else {
		verbosity, err := ent.ValueOfVerbosity(*verbosityString)
		return &verbosity, err
	}
}

/*
Returns the version defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (clcl *CommandLineConfigurationLayer) GetVersion() (*string, error) {
	if clcl.getArgument(VERSION_ARGUMENT_SHORT_NAME) != nil {
		return clcl.getArgument(VERSION_ARGUMENT_SHORT_NAME), nil
	} else {
		return clcl.getArgument(VERSION_ARGUMENT_NAME), nil
	}
}
