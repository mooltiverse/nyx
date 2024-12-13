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
	"fmt"     // https://pkg.go.dev/fmt
	"os"      // https://pkg.go.dev/os
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings
	"sync"    // https://pkg.go.dev/sync

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"     // https://pkg.go.dev/github.com/sirupsen/logrus
	slices "golang.org/x/exp/slices"     // https://pkg.go.dev/golang.org/x/exp/slices

	errs "github.com/mooltiverse/nyx/src/go/errors"
	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	ver "github.com/mooltiverse/nyx/src/go/version"
)

const (
	// The prefix of all environment variables considered by this class.
	ENVVAR_NAME_GLOBAL_PREFIX = "NYX_"

	// The name of the environment variable to read for this value.
	BUMP_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "BUMP"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "CHANGELOG"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_APPEND_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME + "_APPEND"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_PATH_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME + "_PATH"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME + "_SECTIONS"

	// The regular expression used to scan the name of a changelog section from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a changelog section.
	// This expression uses the 'name' capturing group which returns the section name, if detected.
	CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)$"

	// The parametrized name of the environment variable to read for the regular expression attribute of a
	// changelog section configuration.
	// This string is a prototype that contains a '%s' parameter for the section name
	// and must be rendered using fmt.Sprintf(CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the section with the given 'name'.
	CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME + "_%s"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME + "_SUBSTITUTIONS"

	// The regular expression used to scan the name of a changelog substitution from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a changelog substitution.
	// This expression uses the 'name' capturing group which returns the substitution regex, if detected.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)$"

	// The parametrized name of the environment variable to read for the regular expression attribute of a
	// changelog substitution configuration.
	// This string is a prototype that contains a '%s' parameter for the substitution name
	// and must be rendered using fmt.Sprintf(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the substitution with the given 'name'.
	CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME + "_%s"

	// The name of the environment variable to read for this value.
	CHANGELOG_CONFIGURATION_TEMPLATE_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME + "_TEMPLATE"

	// The name of the environment variable to read for this value.
	COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "COMMIT_MESSAGE_CONVENTIONS"

	// The name of the environment variable to read for this value.
	COMMIT_MESSAGE_CONVENTIONS_ENABLED_ENVVAR_NAME = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME + "_ENABLED"

	// The regular expression used to scan the name of a commit message convention from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a commit message convention.
	// This expression uses the 'name' capturing group which returns the commit convention name, if detected.
	COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_NAME_REGEX = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'expression' attribute of a
	// commit message convention.
	// This string is a prototype that contains a '%s' parameter for the commit convention name
	// and must be rendered using fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_EXPRESSION_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the convention with the given 'name'.
	COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_EXPRESSION_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME + "_%s_EXPRESSION"

	// The parametrized name of the environment variable to read for the 'bumpExpressions' attribute of a
	// commit message convention.
	// This string is a prototype that contains a '%s' parameter for the commit convention name
	// and must be rendered using fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the convention with the given 'name'.
	COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME + "_%s_BUMP_EXPRESSIONS"

	// The name of the environment variable to read for this value.
	CONFIGURATION_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "CONFIGURATION_FILE"

	// The name of the environment variable to read for this value.
	DIRECTORY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "DIRECTORY"

	// The name of the environment variable to read for this value.
	DRY_RUN_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "DRY_RUN"

	// The name of the environment variable to read for this value.
	GIT_CONFIGURATION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "GIT"

	// The name of the environment variable to read for this value.
	GIT_CONFIGURATION_REMOTES_ENVVAR_NAME = GIT_CONFIGURATION_ENVVAR_NAME + "_REMOTES"

	// The regular expression used to scan the name of a Git remote configuration from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a Git remote configuration.
	// This expression uses the 'name' capturing group which returns the remote name, if detected.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_NAME_REGEX = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'authenticationMethod' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_%s_AUTHENTICATION_METHOD"

	// The parametrized name of the environment variable to read for the 'password' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSWORD_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSWORD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_%s_PASSWORD"

	// The parametrized name of the environment variable to read for the 'user' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_USER_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_USER_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_%s_USER"

	// The parametrized name of the environment variable to read for the 'privateKey' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PRIVATE_KEY_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PRIVATE_KEY_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_%s_PRIVATE_KEY"

	// The parametrized name of the environment variable to read for the 'passphrase' attribute of a
	// Git remote configuration.
	// This string is a prototype that contains a '%s' parameter for the remote name
	// and must be rendered using fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSPHRASE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the remote with the given 'name'.
	GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSPHRASE_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME + "_%s_PASSPHRASE"

	// The name of the environment variable to read for this value.
	INITIAL_VERSION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "INITIAL_VERSION"

	// The name of the environment variable to read for this value.
	PRESET_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "PRESET"

	// The name of the environment variable to read for this value.
	RELEASE_ASSETS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "RELEASE_ASSETS"

	// The regular expression used to scan the name of a release asset from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a release asset.
	// This expression uses the 'name' capturing group which returns the release asset name, if detected.
	RELEASE_ASSETS_ENVVAR_ITEM_NAME_REGEX = RELEASE_ASSETS_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'fileName' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_FILE_NAME_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release assetm file name with the given 'name'.
	RELEASE_ASSETS_ENVVAR_ITEM_FILE_NAME_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME + "_%s_FILE_NAME"

	// The parametrized name of the environment variable to read for the 'description' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release asset description with the given 'name'.
	RELEASE_ASSETS_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME + "_%s_DESCRIPTION"

	// The parametrized name of the environment variable to read for the 'path' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_PATH_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release asset path with the given 'name'.
	RELEASE_ASSETS_ENVVAR_ITEM_PATH_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME + "_%s_PATH"

	// The parametrized name of the environment variable to read for the 'type' attribute of a
	// release asset.
	// This string is a prototype that contains a '%s' parameter for the release asset name
	// and must be rendered using fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_TYPE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release asset type with the given 'name'.
	RELEASE_ASSETS_ENVVAR_ITEM_TYPE_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME + "_%s_TYPE"

	// The name of the environment variable to read for this value.
	RELEASE_LENIENT_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "RELEASE_LENIENT"

	// The name of the environment variable to read for this value.
	RELEASE_PREFIX_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "RELEASE_PREFIX"

	// The name of the environment variable to read for this value.
	RELEASE_TYPES_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "RELEASE_TYPES"

	// The name of the environment variable to read for this value.
	RELEASE_TYPES_ENABLED_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME + "_ENABLED"

	// The name of the environment variable to read for this value.
	RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME + "_PUBLICATION_SERVICES"

	// The name of the environment variable to read for this value.
	RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME + "_REMOTE_REPOSITORIES"

	// The regular expression used to scan the name of a release type from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a release type.
	// This expression uses the 'name' capturing group which returns the release type name, if detected.
	RELEASE_TYPES_ENVVAR_ITEM_NAME_REGEX = RELEASE_TYPES_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'assets' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_ASSETS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_ASSETS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_ASSETS"

	// The parametrized name of the environment variable to read for the 'collapseVersions' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_COLLAPSE_VERSIONS"

	// The parametrized name of the environment variable to read for the 'collapsedVersionQualifier' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_COLLAPSED_VERSION_QUALIFIER"

	// The parametrized name of the environment variable to read for the 'description' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_DESCRIPTION"

	// The parametrized name of the environment variable to read for the 'filterTags' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_FILTER_TAGS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_FILTER_TAGS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_FILTER_TAGS"

	// The parametrized name of the environment variable to read for the 'gitCommit' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_COMMIT"

	// The parametrized name of the environment variable to read for the 'gitCommitMessage' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_COMMIT_MESSAGE"

	// The parametrized name of the environment variable to read for the 'gitPush' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_PUSH"

	// The parametrized name of the environment variable to read for the 'gitPushForce' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORCE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORCE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_PUSH_FORCE"

	// The parametrized name of the environment variable to read for the 'gitTag' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_TAG"

	// The parametrized name of the environment variable to read for the 'gitTagForce' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORCE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORCE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_TAG_FORCE"

	// The parametrized name of the environment variable to read for the 'gitTagMessage' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_TAG_MESSAGE"

	// The parametrized name of the environment variable to read for the 'gitTagNames' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_NAMES_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_NAMES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_GIT_TAG_NAMES"

	// The parametrized name of the environment variable to read for the 'identifiers' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the commit release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_IDENTIFIERS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_IDENTIFIERS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_IDENTIFIERS"

	// The parametrized name of the environment variable to read for the 'matchBranches' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_BRANCHES_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_MATCH_BRANCHES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_MATCH_BRANCHES"

	// The parametrized name of the environment variable to read for the 'matchEnvironmentVariables' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_MATCH_ENVIRONMENT_VARIABLES"

	// The parametrized name of the environment variable to read for the 'matchWorkspaceStatus' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_MATCH_WORKSPACE_STATUS"

	// The parametrized name of the environment variable to read for the 'publish' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_PUBLISH"

	// The parametrized name of the environment variable to read for the 'publishDraft' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_DRAFT_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_DRAFT_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_PUBLISH_DRAFT"

	// The parametrized name of the environment variable to read for the 'publishPreRelease' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_PRE_RELEASE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_PRE_RELEASE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_PUBLISH_PRE_RELEASE"

	// The parametrized name of the environment variable to read for the 'releaseName' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_RELEASE_NAME_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_RELEASE_NAME_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_RELEASE_NAME"

	// The parametrized name of the environment variable to read for the 'versionRange' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_VERSION_RANGE"

	// The parametrized name of the environment variable to read for the 'versionRangeFromBranchName' attribute of a
	// release type.
	// This string is a prototype that contains a '%s' parameter for the release type name
	// and must be rendered using fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the release type with the given 'name'.
	RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME + "_%s_VERSION_RANGE_FROM_BRANCH_NAME"

	// The name of the environment variable to read for this value.
	RESUME_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "RESUME"

	// The name of the environment variable to read for this value.
	SCHEME_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SCHEME"

	// The name of the environment variable to read for this value.
	SERVICES_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SERVICES"

	// The regular expression used to scan the name of a service from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a service.
	// This expression uses the 'name' capturing group which returns the service name, if detected.
	SERVICES_ENVVAR_ITEM_NAME_REGEX = SERVICES_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'options' attribute of a
	// service.
	// This string is a prototype that contains a '%s' parameter for the service name
	// and must be rendered using fmt.Sprintf(SERVICES_ENVVAR_ITEM_OPTIONS_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the service with the given 'name'.
	SERVICES_ENVVAR_ITEM_OPTIONS_FORMAT_STRING = SERVICES_ENVVAR_NAME + "_%s_OPTIONS"

	// The parametrized name of the environment variable to read for the 'type' attribute of a
	// service.
	// This string is a prototype that contains a '%s' parameter for the service name
	// and must be rendered using fmt.Sprintf(SERVICES_ENVVAR_ITEM_TYPE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the service type with the given 'name'.
	SERVICES_ENVVAR_ITEM_TYPE_FORMAT_STRING = SERVICES_ENVVAR_NAME + "_%s_TYPE"

	// The name of the environment variable to read for this value.
	SHARED_CONFIGURATION_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SHARED_CONFIGURATION_FILE"

	// The name of the environment variable to read for this value.
	STATE_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "STATE_FILE"

	// The name of the environment variable to read for this value.
	SUBSTITUTIONS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SUBSTITUTIONS"

	// The name of the environment variable to read for this value.
	SUBSTITUTIONS_ENABLED_ENVVAR_NAME = SUBSTITUTIONS_ENVVAR_NAME + "_ENABLED"

	// The regular expression used to scan the name of a substitution from an environment
	// variable name. This expression is used to detect if an environment variable is used to define
	// a substitution.
	// This expression uses the 'name' capturing group which returns the commit substitution name, if detected.
	SUBSTITUTIONS_ENVVAR_ITEM_NAME_REGEX = SUBSTITUTIONS_ENVVAR_NAME + "_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$"

	// The parametrized name of the environment variable to read for the 'files' attribute of a
	// substitution.
	// This string is a prototype that contains a '%s' parameter for the substitution name
	// and must be rendered using fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_FILES_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the substitution with the given 'name'.
	SUBSTITUTIONS_ENVVAR_ITEM_FILES_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME + "_%s_FILES"

	// The parametrized name of the environment variable to read for the 'match' attribute of a
	// substitution.
	// This string is a prototype that contains a '%s' parameter for the substitution name
	// and must be rendered using fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_MATCH_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the substitution with the given 'name'.
	SUBSTITUTIONS_ENVVAR_ITEM_MATCH_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME + "_%s_MATCH"

	// The parametrized name of the environment variable to read for the 'replace' attribute of a
	// substitution.
	// This string is a prototype that contains a '%s' parameter for the substitution name
	// and must be rendered using fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_REPLACE_FORMAT_STRING, name)
	// in order to get the actual name of the environment variable that brings the value for the substitution with the given 'name'.
	SUBSTITUTIONS_ENVVAR_ITEM_REPLACE_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME + "_%s_REPLACE"

	// The name of the environment variable to read for this value.
	SUMMARY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SUMMARY"

	// The name of the environment variable to read for this value.
	SUMMARY_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "SUMMARY_FILE"

	// The name of the environment variable to read for this value.
	VERBOSITY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "VERBOSITY"

	// The name of the environment variable to read for this value.
	VERSION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX + "VERSION"
)

var (
	// the lock used to synchronize routines
	environmentConfigurationLayerLock = &sync.Mutex{}

	// The single instance for this class.
	environmentConfigurationLayerInstance *EnvironmentConfigurationLayer
)

/*
A configuration layer that returns values read by environment variables. Each environment variable must comply with
a well defined naming convention in order to be recognized.

This is a singleton class so instances are to be retrieved via the GetEnvironmentConfigurationLayerInstance() method.

This object allows read only operations.
*/
type EnvironmentConfigurationLayer struct {
	// The map of environment variables keys and values
	environmentVariablesMap map[string]string

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

	// The substitutions configuration section.
	substitutions *ent.Substitutions
}

/*
Returns the singleton instance of this class.
*/
func GetEnvironmentConfigurationLayerInstance() *EnvironmentConfigurationLayer {
	if environmentConfigurationLayerInstance == nil {
		environmentConfigurationLayerLock.Lock()
		defer environmentConfigurationLayerLock.Unlock()
		if environmentConfigurationLayerInstance == nil {
			environmentConfigurationLayerInstance = &EnvironmentConfigurationLayer{}
		}
	}

	return environmentConfigurationLayerInstance
}

/*
Converts the given slice of strings into a slice of the same size with pointers to the input strings.

Arguments are as follows:

- items the slice of strrings to convert
*/
func (ecl *EnvironmentConfigurationLayer) toSliceOfStringPointers(items []string) []*string {
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
Reads the given environment variable and parses it as a comma separated list of names that will
be returned as a list.

Arguments are as follows:

- attributeGroupName the name of the attribute group, used for log messages
- leafAttributeName the name of the specific attribute within the group, used for log messages
- envVarName the name of the environment variable to read and parse
*/
func (ecl *EnvironmentConfigurationLayer) getItemNamesListFromEnvironmentVariable(attributeGroupName string, leafAttributeName string, envVarName string) []string {
	itemNames := make([]string, 0)
	envVarValue := ecl.getEnvVar(envVarName)
	if envVarValue == nil {
		log.Tracef("no environment variable named '%s' can be found, assuming the environment variables do not set the '%s.%s' configuration option", envVarName, attributeGroupName, leafAttributeName)
	} else if "" == *envVarValue {
		log.Tracef("the environment variable named '%s' has been found but is empty, assuming the environment variables do not set the '%s.%s' configuration option", envVarName, attributeGroupName, leafAttributeName)
	} else {
		log.Tracef("the environment variable named '%s' has been found with value '%s'. Parsing the value to infer the '%s.%s' configuration option", envVarName, *envVarValue, attributeGroupName, leafAttributeName)
		itemNames = append(itemNames, strings.Split(*envVarValue, ",")...)
		log.Tracef("the environment variable named '%s' has been parsed and yields to %d items: '%s'", envVarName, len(itemNames), itemNames)
	}
	return itemNames
}

/*
Scans all environment variables trying to match their names against the given regular expression. If a match is found
then a capturing group named 'name' is used to extrapolate the item name, which will then be returned in the result.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - regex a regular expression used to match environment variable names. It must also contain a matching
    group named 'name' which will be used to extrapolate the name
  - ignores an optional collection of variable names to ignore. It may be nil

Errors can be:

- PatternSyntaxError if the given regular expression can't be compiled
- IllegalArgumentError if the given regular expression does not contain the 'name' capturing group
*/
func (ecl *EnvironmentConfigurationLayer) scanItemNamesInEnvironmentVariables(attributeGroupName string, regex string, ignores []string) ([]string, error) {
	log.Tracef("scanning environment variables searching for the names of configured elements belonging to the '%s' group using the regular expression: '%s'", attributeGroupName, regex)
	// Scan all environment variables whose name matches the items regular expression
	// so that we can infer the name of the various items
	itemNames := make([]string, 0)
	for envVarName, _ := range ecl.getEnvMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, envVarName)) {
			re, err := regexp2.Compile(regex, 0)
			if err != nil {
				return nil, &errs.PatternSyntaxError{Message: fmt.Sprintf("regular expression '%s' can't be compiled: %v", regex, err), Cause: err}
			}

			m, err := re.FindStringMatch(envVarName)
			if err != nil {
				return nil, &errs.PatternSyntaxError{Message: fmt.Sprintf("regular expression '%s' can't be matched: %v", regex, err), Cause: err}
			}

			if m != nil { // when this is nil it means no match was found, so skip this environment variable
				log.Tracef("the environment variable named '%s' denotes it configures a '%s' item", envVarName, attributeGroupName)
				g := m.GroupByName("name")
				if len(g.Captures) == 0 {
					log.Warnf("the environment variable named '%s' denotes it configures a '%s' item but the item name can't be extrapolated using the regular expression: '%s'", envVarName, attributeGroupName, regex)
				} else {
					name := g.Captures[0].String()
					if "" == name {
						log.Warnf("the environment variable named '%s' denotes it configures a '%s' item but the item name can't be extrapolated using the regular expression: '%s'", envVarName, attributeGroupName, regex)
					} else {
						log.Tracef("the environment variable named '%s' denotes it configures a '%s' item named '%s'", envVarName, attributeGroupName, name)
						itemNames = append(itemNames, name)
					}
				}
			}
		}
	}
	log.Tracef("the set of '%s' items configured using environment variables has %d items: '%s'", attributeGroupName, len(itemNames), itemNames)
	return itemNames, nil
}

/*
Scans the environment variables and takes all the ones starting with a certain prefix
and put them in the returned map. The key stored in the map is the trailing part of the variable name
after the given prefix, while values are the corresponding variable values.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - envVarNamePrefix the prefix of all environment variable names to be considered map items. This
    will be discarded from variable names in order to get item keys.
  - ignores an optional collection of variable names to ignore. It may be nil
*/
func (ecl *EnvironmentConfigurationLayer) getAttributeMapFromEnvironmentVariable(attributeGroupName string, envVarNamePrefix string, ignores []string) map[string]string {
	log.Tracef("scanning environment variables searching for the items belonging to the '%s' group using the prefix: '%s'", attributeGroupName, envVarNamePrefix)
	attributeMap := make(map[string]string)
	// Scan environment variables in order to find the items whose name starts with the right prefix.
	// The trailing part is then supposed to be the map item name
	envVarNamePrefix = envVarNamePrefix + "_" // avoid false positives and consider that before the entry name has another separator '_', that we want to ignore hereafter
	for envVarName, _ := range ecl.getEnvMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, envVarName)) {
			if strings.HasPrefix(envVarName, envVarNamePrefix) && len(envVarName) > len(envVarNamePrefix) {
				mapItemName := strings.Replace(envVarName, envVarNamePrefix, "", 1)
				mapItemValue := ecl.getEnvVar(envVarName)
				attributeMap[mapItemName] = *mapItemValue
				log.Tracef("the '%s' map has the following item: '%s'='%s'", attributeGroupName, mapItemName, *mapItemValue)
			}
		}
	}
	log.Tracef("the map of '%s' items configured using environment variables has %d items: '%s'", attributeGroupName, len(attributeMap), attributeMap)
	return attributeMap
}

/*
Scans the environment variables and takes all the ones starting with a certain prefix
and put them in the returned map. The key stored in the map is the trailing part of the variable name
after the given prefix, while values are the corresponding variable values.

Arguments are as follows:

  - attributeGroupName the name of the attribute group, used for log messages
  - envVarNamePrefix the prefix of all environment variable names to be considered map items. This
    prefix will be discarded from variable names in order to get item keys.
  - ignores an optional collection of variable names to ignore. It may be nil

Errors can be:

- IllegalPropertyError if malformed items are encountered
*/
func (ecl *EnvironmentConfigurationLayer) getIdentifiersListFromEnvironmentVariable(attributeGroupName string, envVarNamePrefix string, ignores []string) ([]*ent.Identifier, error) {
	log.Tracef("scanning environment variables searching for the items belonging to the '%s' group using the prefix: '%s'", attributeGroupName, envVarNamePrefix)

	identifiersMap := make(map[int]*ent.Identifier)
	// Scan environment variables in order to find the items whose name starts with the right prefix.
	// The trailing part is then supposed to be the concatenation of:
	// - the item ordinal within the list
	// - the separator '_'
	// - the name of the attribute of the item
	// We store them in a sorted map so we can return items ordered as they appeared (the order is based on their ordinals)
	envVarNamePrefix = envVarNamePrefix + "_" // avoid false positives and consider that before the ordinal each variable name has another separator '_', that we want to ignore hereafter
	for envVarName, _ := range ecl.getEnvMap() {
		if ignores == nil || (ignores != nil && !slices.Contains(ignores, envVarName)) {
			if strings.HasPrefix(envVarName, envVarNamePrefix) && len(envVarName) > len(envVarNamePrefix) {
				mapItemName := strings.Replace(envVarName, envVarNamePrefix, "", 1)
				mapItemValue := ecl.getEnvVar(envVarName)

				listItemNameComponents := strings.Split(mapItemName, "_")
				if len(listItemNameComponents) != 2 {
					return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable name %s is malformed as it is supposed to be in the form %s<INTEGER>_<ATTRIBUTE>", envVarName, envVarNamePrefix)}
				}

				// parse the first part as the integer ordinal
				ordinal, err := strconv.Atoi(listItemNameComponents[0])
				if err != nil {
					return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable name %s is malformed as it is supposed to be in the form %s<INTEGER>_<ATTRIBUTE>. In this case the integer part %s doesn't seem to be a valid integer", envVarName, envVarNamePrefix, listItemNameComponents[0])}
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
						return make([]*ent.Identifier, 0), &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value '%s'", envVarName, *mapItemValue)}
					} else {
						identifiersMap[ordinal].SetPosition(&position)
					}
					break
				default:
					log.Warnf("the environment variable '%s' defines an unrecognized identifier attribute '%s'", envVarName, listItemNameComponents[1])
				}
			}
		}
	}
	log.Tracef("the map of '%s' items configured using environment variables has %d items", attributeGroupName, len(identifiersMap))
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
Parses the given slice of environment variables, where each argument is in the form NAME=VALUE and returns them in a map.

Arguments are as follows:

- variables the slice of environment variables to parse
*/
func (ecl *EnvironmentConfigurationLayer) parseEnvironmentVariables(variables []string) map[string]string {
	environmentVariablesMap := make(map[string]string)

	// now parse all environment variables and put them in the map
	for _, entry := range variables {
		entryValue := strings.Split(entry, "=")
		environmentVariablesMap[entryValue[0]] = entryValue[1]
	}

	return environmentVariablesMap
}

/*
Returns the value of the environment variables where keys are variable names and values are their values.

The returned map is saved and reused for future uses so it's initialized only once.
*/
func (ecl *EnvironmentConfigurationLayer) getEnvMap() map[string]string {
	if ecl.environmentVariablesMap == nil {
		environmentConfigurationLayerLock.Lock()
		defer environmentConfigurationLayerLock.Unlock()
		if ecl.environmentVariablesMap == nil {
			ecl.environmentVariablesMap = ecl.parseEnvironmentVariables(os.Environ())
		}
	}

	return ecl.environmentVariablesMap
}

/*
Returns the value of the environment variable with the given name, or nil if no such variable has been defined.

Arguments are as follows:

- name the name of the environment variable to get
*/
func (ecl *EnvironmentConfigurationLayer) getEnvVar(name string) *string {
	envVars := ecl.getEnvMap() // this way we make sure the underlying map has been initialized
	val, ok := envVars[name]
	if ok {
		return &val
	} else {
		return nil
	}
}

/*
Sets the environment variables for this configuration layer, replacing the ones coming from the operating system.
Each item in the given slide must be in the form NAME=VALUE.

This method should be used for test purposes only and allows to set name value pairs just as if they were
read from the operating system.

Arguments are as follows:

- variables the slice of environment variables to parse
*/
func (ecl *EnvironmentConfigurationLayer) withEnvironmentVariables(variables []string) {
	if ecl.environmentVariablesMap == nil {
		environmentConfigurationLayerLock.Lock()
		defer environmentConfigurationLayerLock.Unlock()
		if ecl.environmentVariablesMap == nil {
			ecl.environmentVariablesMap = ecl.parseEnvironmentVariables(variables)
		}
	}
}

/*
Returns the version identifier to bump as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetBump() (*string, error) {
	return ecl.getEnvVar(BUMP_ENVVAR_NAME), nil
}

/*
Returns the changelog configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetChangelog() (*ent.ChangelogConfiguration, error) {
	if ecl.changelog == nil {
		// parse the 'sections' map
		sections := make(map[string]string)
		sectionNames, err := ecl.scanItemNamesInEnvironmentVariables("changelog", CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all section names configured through environment variables and we can
		// query specific environment variables
		for _, sectionName := range sectionNames {
			sectionValue := ecl.getEnvVar(fmt.Sprintf(CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING, sectionName))
			sections[sectionName] = *sectionValue
		}

		// parse the 'substitutions' map
		substitutions := make(map[string]string)
		substitutionNames, err := ecl.scanItemNamesInEnvironmentVariables("changelog", CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all section names configured through environment variables and we can
		// query specific environment variables
		for _, substitutionName := range substitutionNames {
			substitutionValue := ecl.getEnvVar(fmt.Sprintf(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_ITEM_REGEXP_FORMAT_STRING, substitutionName))
			substitutions[substitutionName] = *substitutionValue
		}

		ecl.changelog, err = ent.NewChangelogConfigurationWith(ecl.getEnvVar(CHANGELOG_CONFIGURATION_APPEND_ENVVAR_NAME), ecl.getEnvVar(CHANGELOG_CONFIGURATION_PATH_ENVVAR_NAME), &sections, ecl.getEnvVar(CHANGELOG_CONFIGURATION_TEMPLATE_ENVVAR_NAME), &substitutions)
		if err != nil {
			return nil, err
		}
	}
	return ecl.changelog, nil
}

/*
Returns the commit message convention configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetCommitMessageConventions() (*ent.CommitMessageConventions, error) {
	if ecl.commitMessageConventions == nil {
		// parse the 'enabled' items list
		enabled := ecl.getItemNamesListFromEnvironmentVariable("commitMessageConventions", "enabled", COMMIT_MESSAGE_CONVENTIONS_ENABLED_ENVVAR_NAME)

		// parse the 'items' map
		items := make(map[string]*ent.CommitMessageConvention)

		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("commitMessageConventions", COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			expression := ecl.getEnvVar(fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_EXPRESSION_FORMAT_STRING, itemName))
			bumpExpressions := ecl.getAttributeMapFromEnvironmentVariable("commitMessageConventions"+"."+itemName+"."+"bumpExpressions", fmt.Sprintf(COMMIT_MESSAGE_CONVENTIONS_ENVVAR_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, itemName), nil)

			items[itemName] = ent.NewCommitMessageConventionWith(expression, &bumpExpressions)
		}
		enabledPointers := ecl.toSliceOfStringPointers(enabled)
		ecl.commitMessageConventions, err = ent.NewCommitMessageConventionsWith(&enabledPointers, &items)
		if err != nil {
			return nil, err
		}
	}
	return ecl.commitMessageConventions, nil
}

/*
Returns the path to a custom configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetConfigurationFile() (*string, error) {
	return ecl.getEnvVar(CONFIGURATION_FILE_ENVVAR_NAME), nil
}

/*
Returns the directory to use as the working directory as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetDirectory() (*string, error) {
	return ecl.getEnvVar(DIRECTORY_ENVVAR_NAME), nil
}

/*
Returns the value of the dry run flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetDryRun() (*bool, error) {
	dryRunString := ecl.getEnvVar(DRY_RUN_ENVVAR_NAME)
	if dryRunString == nil {
		return nil, nil
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
func (ecl *EnvironmentConfigurationLayer) GetGit() (*ent.GitConfiguration, error) {
	if ecl.git == nil {
		// parse the 'remotes' map
		remotes := make(map[string]*ent.GitRemoteConfiguration)

		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("git", GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			var authenticationMethod *ent.AuthenticationMethod = nil
			authenticationMethodString := ecl.getEnvVar(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName))
			if authenticationMethodString != nil {
				am, err := ent.ValueOfAuthenticationMethod(*authenticationMethodString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The argument '%s' has an illegal value '%s'", fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName), *authenticationMethodString), Cause: err}
				}
				authenticationMethod = &am
			}
			password := ecl.getEnvVar(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSWORD_FORMAT_STRING, itemName))
			user := ecl.getEnvVar(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_USER_FORMAT_STRING, itemName))
			privateKey := ecl.getEnvVar(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PRIVATE_KEY_FORMAT_STRING, itemName))
			passphrase := ecl.getEnvVar(fmt.Sprintf(GIT_CONFIGURATION_REMOTES_ENVVAR_ITEM_PASSPHRASE_FORMAT_STRING, itemName))

			remotes[itemName] = ent.NewGitRemoteConfigurationWith(authenticationMethod, user, password, privateKey, passphrase)
		}

		ecl.git, err = ent.NewGitConfigurationWith(&remotes)
		if err != nil {
			return nil, err
		}
	}
	return ecl.git, nil
}

/*
Returns the initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetInitialVersion() (*string, error) {
	return ecl.getEnvVar(INITIAL_VERSION_ENVVAR_NAME), nil
}

/*
Returns the selected preset configuration as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetPreset() (*string, error) {
	return ecl.getEnvVar(PRESET_ENVVAR_NAME), nil
}

/*
Returns the release assets configuration section. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetReleaseAssets() (*map[string]*ent.Attachment, error) {
	if ecl.releaseAssets == nil {
		ras := make(map[string]*ent.Attachment)

		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("releaseAssets", RELEASE_ASSETS_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			fileName := ecl.getEnvVar(fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_FILE_NAME_FORMAT_STRING, itemName))
			description := ecl.getEnvVar(fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING, itemName))
			path := ecl.getEnvVar(fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_PATH_FORMAT_STRING, itemName))
			attachmentType := ecl.getEnvVar(fmt.Sprintf(RELEASE_ASSETS_ENVVAR_ITEM_TYPE_FORMAT_STRING, itemName))

			ras[itemName] = ent.NewAttachmentWith(fileName, description, path, attachmentType)
		}
		ecl.releaseAssets = &ras
	}
	return ecl.releaseAssets, nil
}

/*
Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetReleaseLenient() (*bool, error) {
	releaseLenientString := ecl.getEnvVar(RELEASE_LENIENT_ENVVAR_NAME)
	if releaseLenientString == nil {
		return nil, nil
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
func (ecl *EnvironmentConfigurationLayer) GetReleasePrefix() (*string, error) {
	return ecl.getEnvVar(RELEASE_PREFIX_ENVVAR_NAME), nil
}

/*
Returns the release types configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetReleaseTypes() (*ent.ReleaseTypes, error) {
	if ecl.releaseTypes == nil {
		// parse the 'enabled' items list
		enabled := ecl.getItemNamesListFromEnvironmentVariable("releaseTypes", "enabled", RELEASE_TYPES_ENABLED_ENVVAR_NAME)

		// parse the 'publicationServices' items list
		publicationServices := ecl.getItemNamesListFromEnvironmentVariable("releaseTypes", "publicationServices", RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME)

		// parse the 'remoteRepositories' items list
		remoteRepositories := ecl.getItemNamesListFromEnvironmentVariable("releaseTypes", "remoteRepositories", RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME)

		// parse the 'items' map
		items := make(map[string]*ent.ReleaseType)

		// ignore the RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME and RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME variables or they're interpreted as 'PUBLICATION' items
		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("releaseTypes", RELEASE_TYPES_ENVVAR_ITEM_NAME_REGEX, []string{RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME, RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME})
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			var collapseVersions *bool = nil
			collapseVersionsString := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName))
			if collapseVersionsString != nil {
				// empty string is considered 'false'
				if "" == *collapseVersionsString {
					cv := false
					collapseVersions = &cv
				} else {
					cv, err := strconv.ParseBool(*collapseVersionsString)
					if err != nil {
						return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName), *collapseVersionsString), Cause: err}
					}
					collapseVersions = &cv
				}
			}
			assetsList := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_ASSETS_FORMAT_STRING, itemName))
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
			collapseVersionQualifier := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, itemName))
			description := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_DESCRIPTION_FORMAT_STRING, itemName))
			filterTags := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_FILTER_TAGS_FORMAT_STRING, itemName))
			gitCommit := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_FORMAT_STRING, itemName))
			gitCommitMessage := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, itemName))
			gitPush := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORMAT_STRING, itemName))
			gitPushForce := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_PUSH_FORCE_FORMAT_STRING, itemName))
			gitTag := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORMAT_STRING, itemName))
			gitTagForce := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_FORCE_FORMAT_STRING, itemName))
			gitTagMessage := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, itemName))
			gitTagNamesList := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_GIT_TAG_NAMES_FORMAT_STRING, itemName))
			var gitTagNames *[]*string
			if gitTagNamesList != nil {
				gitTagNamesSlice := strings.Split(*gitTagNamesList, ",")
				var gitTagNamesArray []*string
				for _, tagName := range gitTagNamesSlice {
					tagNameCopy := tagName
					gitTagNamesArray = append(gitTagNamesArray, &tagNameCopy)
				}
				gitTagNames = &gitTagNamesArray
			} else {
				gitTagNames = nil
			}
			identifiers, err := ecl.getIdentifiersListFromEnvironmentVariable("releaseTypes"+"."+itemName+"."+"identifiers", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_IDENTIFIERS_FORMAT_STRING, itemName), nil)
			if err != nil {
				return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_IDENTIFIERS_FORMAT_STRING, itemName)), Cause: err}
			}
			matchBranches := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_BRANCHES_FORMAT_STRING, itemName))
			matchEnvironmentVariables := ecl.getAttributeMapFromEnvironmentVariable("releaseTypes"+"."+itemName+"."+"matchEnvironmentVariables", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, itemName), nil)
			var matchWorkspaceStatus *ent.WorkspaceStatus = nil
			matchWorkspaceStatusString := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName))
			if matchWorkspaceStatusString != nil {
				mws, err := ent.ValueOfWorkspaceStatus(*matchWorkspaceStatusString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName), *matchWorkspaceStatusString), Cause: err}
				}
				matchWorkspaceStatus = &mws
			}
			publish := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_FORMAT_STRING, itemName))
			publishDraft := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_DRAFT_FORMAT_STRING, itemName))
			publishPreRelease := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_PUBLISH_PRE_RELEASE_FORMAT_STRING, itemName))
			releaseName := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_RELEASE_NAME_FORMAT_STRING, itemName))
			versionRange := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FORMAT_STRING, itemName))
			var versionRangeFromBranchName *bool = nil
			versionRangeFromBranchNameString := ecl.getEnvVar(fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName))
			if versionRangeFromBranchNameString != nil {
				vrfbn, err := strconv.ParseBool(*versionRangeFromBranchNameString)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value '%s'", fmt.Sprintf(RELEASE_TYPES_ENVVAR_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName), *versionRangeFromBranchNameString), Cause: err}
				}
				versionRangeFromBranchName = &vrfbn
			}

			items[itemName] = ent.NewReleaseTypeWith(assets, collapseVersions, collapseVersionQualifier, description, filterTags, gitCommit, gitCommitMessage, gitPush, gitPushForce, gitTag, gitTagForce, gitTagMessage, gitTagNames, &identifiers, matchBranches, &matchEnvironmentVariables, matchWorkspaceStatus, publish, publishDraft, publishPreRelease, releaseName, versionRange, versionRangeFromBranchName)
		}

		enabledPointers := ecl.toSliceOfStringPointers(enabled)
		publicationServicesPointers := ecl.toSliceOfStringPointers(publicationServices)
		remoteRepositoriesPointers := ecl.toSliceOfStringPointers(remoteRepositories)
		ecl.releaseTypes, err = ent.NewReleaseTypesWith(&enabledPointers, &publicationServicesPointers, &remoteRepositoriesPointers, &items)
		if err != nil {
			return nil, err
		}
	}

	return ecl.releaseTypes, nil
}

/*
Returns the value of the resume flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetResume() (*bool, error) {
	resumeString := ecl.getEnvVar(RESUME_ENVVAR_NAME)
	if resumeString == nil {
		return nil, nil
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
func (ecl *EnvironmentConfigurationLayer) GetScheme() (*ver.Scheme, error) {
	schemeString := ecl.getEnvVar(SCHEME_ENVVAR_NAME)
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
func (ecl *EnvironmentConfigurationLayer) GetServices() (*map[string]*ent.ServiceConfiguration, error) {
	if ecl.services == nil {
		svcs := make(map[string]*ent.ServiceConfiguration)

		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("services", SERVICES_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			var serviceType ent.Provider
			typeString := ecl.getEnvVar(fmt.Sprintf(SERVICES_ENVVAR_ITEM_TYPE_FORMAT_STRING, itemName))
			if typeString != nil {
				sType, err := ent.ValueOfProvider(*typeString)
				serviceType = sType
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("The environment variable '%s' has an illegal value '%s'", fmt.Sprintf(SERVICES_ENVVAR_ITEM_TYPE_FORMAT_STRING, itemName), *typeString)}
				}
			}
			options := ecl.getAttributeMapFromEnvironmentVariable("services"+"."+itemName+"."+"options", fmt.Sprintf(SERVICES_ENVVAR_ITEM_OPTIONS_FORMAT_STRING, itemName), nil)

			svcs[itemName] = ent.NewServiceConfigurationWith(&serviceType, &options)
		}
		ecl.services = &svcs
	}
	return ecl.services, nil
}

/*
Returns the path to a custom shared configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetSharedConfigurationFile() (*string, error) {
	return ecl.getEnvVar(SHARED_CONFIGURATION_FILE_ENVVAR_NAME), nil
}

/*
Returns the path to the file where the Nyx State must be saved as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetStateFile() (*string, error) {
	return ecl.getEnvVar(STATE_FILE_ENVVAR_NAME), nil
}

/*
Returns the substitutions configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetSubstitutions() (*ent.Substitutions, error) {
	if ecl.substitutions == nil {
		// parse the 'enabled' items list
		enabled := ecl.getItemNamesListFromEnvironmentVariable("substitutions", "enabled", SUBSTITUTIONS_ENABLED_ENVVAR_NAME)

		// parse the 'items' map
		items := make(map[string]*ent.Substitution)

		itemNames, err := ecl.scanItemNamesInEnvironmentVariables("substitutions", SUBSTITUTIONS_ENVVAR_ITEM_NAME_REGEX, nil)
		if err != nil {
			return nil, err
		}
		// now we have the set of all item names configured through environment variables and we can
		// query specific environment variables
		for _, itemName := range itemNames {
			files := ecl.getEnvVar(fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_FILES_FORMAT_STRING, itemName))
			match := ecl.getEnvVar(fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_MATCH_FORMAT_STRING, itemName))
			replace := ecl.getEnvVar(fmt.Sprintf(SUBSTITUTIONS_ENVVAR_ITEM_REPLACE_FORMAT_STRING, itemName))

			items[itemName] = ent.NewSubstitutionWith(files, match, replace)
		}
		enabledPointers := ecl.toSliceOfStringPointers(enabled)
		ecl.substitutions, err = ent.NewSubstitutionsWith(&enabledPointers, &items)
		if err != nil {
			return nil, err
		}
	}
	return ecl.substitutions, nil
}

/*
Returns the value of the summary flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetSummary() (*bool, error) {
	summaryString := ecl.getEnvVar(SUMMARY_ENVVAR_NAME)
	if summaryString == nil {
		return nil, nil
	}
	summary, err := strconv.ParseBool(*summaryString)
	return &summary, err
}

/*
Returns the path to the file where the Nyx summary must be saved as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetSummaryFile() (*string, error) {
	return ecl.getEnvVar(SUMMARY_FILE_ENVVAR_NAME), nil
}

/*
Returns the logging verbosity level as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (ecl *EnvironmentConfigurationLayer) GetVerbosity() (*ent.Verbosity, error) {
	verbosityString := ecl.getEnvVar(VERBOSITY_ENVVAR_NAME)
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
func (ecl *EnvironmentConfigurationLayer) GetVersion() (*string, error) {
	return ecl.getEnvVar(VERSION_ENVVAR_NAME), nil
}
