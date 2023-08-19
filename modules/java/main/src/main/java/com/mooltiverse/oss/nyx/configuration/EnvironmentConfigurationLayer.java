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

import static com.mooltiverse.oss.nyx.log.Markers.CONFIGURATION;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.AuthenticationMethod;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Defaults;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.GitRemoteConfiguration;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.Provider;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Substitution;
import com.mooltiverse.oss.nyx.entities.Substitutions;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A configuration layer that returns values read by environment variables. Each environment variable must comply with
 * a well defined naming convention in order to be recognized.
 * 
 * This object allows read only operations.
 */
class EnvironmentConfigurationLayer implements ConfigurationLayer {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(EnvironmentConfigurationLayer.class);

    /**
     * The single instance for this class.
     */
    private static EnvironmentConfigurationLayer instance = null;

    /**
     * The prefix of all environment variables considered by this class. Value: {@value}
     */
    private static final String ENVVAR_NAME_GLOBAL_PREFIX = "NYX_";

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String BUMP_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("BUMP");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("CHANGELOG");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_PATH_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME.concat("_PATH");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME.concat("_SECTIONS");

    /**
     * The regular expression used to scan the name of a changelog section from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a changelog section.
     * This expression uses the 'name' capturing group which returns the section name, if detected.
     * Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_SECTIONS_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)$");

    /**
     * The parametrized name of the environment variable to read for the regular expression attribute of a
     * changelog section configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the section name
     * and must be rendered using {@link String#format(String, Object...) String.format(CHANGELOG_CONFIGURATION_SECTIONS_ITEM_REGEXP_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the section with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String CHANGELOG_CONFIGURATION_SECTIONS_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SECTIONS_ENVVAR_NAME.concat("_%s");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME.concat("_SUBSTITUTIONS");

    /**
     * The regular expression used to scan the name of a changelog substitution from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a changelog substitution.
     * This expression uses the 'name' capturing group which returns the substitution regex, if detected.
     * Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ITEM_NAME_REGEX = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)$");

    /**
     * The parametrized name of the environment variable to read for the regular expression attribute of a
     * changelog substitution configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the substitution name
     * and must be rendered using {@link String#format(String, Object...) String.format(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ITEM_REGEXP_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the substitution with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ITEM_REGEXP_FORMAT_STRING = CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ENVVAR_NAME.concat("_%s");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CHANGELOG_CONFIGURATION_TEMPLATE_ENVVAR_NAME = CHANGELOG_CONFIGURATION_ENVVAR_NAME.concat("_TEMPLATE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("COMMIT_MESSAGE_CONVENTIONS");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String COMMIT_MESSAGE_CONVENTIONS_ENABLED_ENVVAR_NAME = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME.concat("_ENABLED");

    /**
     * The regular expression used to scan the name of a commit message convention from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a commit message convention.
     * This expression uses the 'name' capturing group which returns the commit convention name, if detected.
     * Value: {@value}
     */
    private static final String COMMIT_MESSAGE_CONVENTIONS_ITEM_NAME_REGEX = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'expression' attribute of a
     * commit message convention.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the commit convention name
     * and must be rendered using {@link String#format(String, Object...) String.format(COMMIT_MESSAGE_CONVENTIONS_ITEM_EXPRESSION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the convention with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String COMMIT_MESSAGE_CONVENTIONS_ITEM_EXPRESSION_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME.concat("_%s_EXPRESSION");

    /**
     * The parametrized name of the environment variable to read for the 'bumpExpressions' attribute of a
     * commit message convention.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the commit convention name
     * and must be rendered using {@link String#format(String, Object...) String.format(COMMIT_MESSAGE_CONVENTIONS_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the convention with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String COMMIT_MESSAGE_CONVENTIONS_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING = COMMIT_MESSAGE_CONVENTIONS_ENVVAR_NAME.concat("_%s_BUMP_EXPRESSIONS");
    
    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String CONFIGURATION_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("CONFIGURATION_FILE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String DIRECTORY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("DIRECTORY");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String DRY_RUN_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("DRY_RUN");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String GIT_CONFIGURATION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("GIT");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String GIT_CONFIGURATION_REMOTES_ENVVAR_NAME = GIT_CONFIGURATION_ENVVAR_NAME.concat("_REMOTES");

    /**
     * The regular expression used to scan the name of a Git remote configuration from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a Git remote configuration.
     * This expression uses the 'name' capturing group which returns the remote name, if detected.
     * Value: {@value}
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_NAME_REGEX = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'authenticationMethod' attribute of a
     * Git remote configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the remote name
     * and must be rendered using {@link String#format(String, Object...) String.format(GIT_CONFIGURATION_REMOTES_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the remote with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_%s_AUTHENTICATION_METHOD");

    /**
     * The parametrized name of the environment variable to read for the 'password' attribute of a
     * Git remote configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the remote name
     * and must be rendered using {@link String#format(String, Object...) String.format(GIT_CONFIGURATION_REMOTES_ITEM_PASSWORD_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the remote with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_PASSWORD_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_%s_PASSWORD");

    /**
     * The parametrized name of the environment variable to read for the 'user' attribute of a
     * Git remote configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the remote name
     * and must be rendered using {@link String#format(String, Object...) String.format(GIT_CONFIGURATION_REMOTES_ITEM_USER_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the remote with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_USER_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_%s_USER");

    /**
     * The parametrized name of the environment variable to read for the 'privateKey' attribute of a
     * Git remote configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the remote name
     * and must be rendered using {@link String#format(String, Object...) String.format(GIT_CONFIGURATION_REMOTES_ITEM_PRIVATE_KEY_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the remote with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_PRIVATE_KEY_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_%s_PRIVATE_KEY");

    /**
     * The parametrized name of the environment variable to read for the 'passphrase' attribute of a
     * Git remote configuration.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the remote name
     * and must be rendered using {@link String#format(String, Object...) String.format(GIT_CONFIGURATION_REMOTES_ITEM_PASSPHRASE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the remote with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String GIT_CONFIGURATION_REMOTES_ITEM_PASSPHRASE_FORMAT_STRING = GIT_CONFIGURATION_REMOTES_ENVVAR_NAME.concat("_%s_PASSPHRASE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String INITIAL_VERSION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("INITIAL_VERSION");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String PRESET_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("PRESET");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_ASSETS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("RELEASE_ASSETS");

    /**
     * The regular expression used to scan the name of a release asset from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a release asset.
     * This expression uses the 'name' capturing group which returns the release asset name, if detected.
     * Value: {@value}
     */
    private static final String RELEASE_ASSETS_ITEM_NAME_REGEX = RELEASE_ASSETS_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'fileName' attribute of a
     * release asset.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release asset name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_ASSETS_ITEM_FILE_NAME_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release assetm file name with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_ASSETS_ITEM_FILE_NAME_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME.concat("_%s_FILE_NAME");

    /**
     * The parametrized name of the environment variable to read for the 'description' attribute of a
     * release asset.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release asset name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_ASSETS_ITEM_DESCRIPTION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release asset description with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_ASSETS_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME.concat("_%s_DESCRIPTION");

    /**
     * The parametrized name of the environment variable to read for the 'path' attribute of a
     * release asset.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release asset name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_ASSETS_ITEM_PATH_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release asset path with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_ASSETS_ITEM_PATH_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME.concat("_%s_PATH");

    /**
     * The parametrized name of the environment variable to read for the 'type' attribute of a
     * release asset.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release asset name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_ASSETS_ITEM_TYPE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release asset type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_ASSETS_ITEM_TYPE_FORMAT_STRING = RELEASE_ASSETS_ENVVAR_NAME.concat("_%s_TYPE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_LENIENT_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("RELEASE_LENIENT");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_PREFIX_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("RELEASE_PREFIX");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_TYPES_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("RELEASE_TYPES");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_TYPES_ENABLED_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME.concat("_ENABLED");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME.concat("_PUBLICATION_SERVICES");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME = RELEASE_TYPES_ENVVAR_NAME.concat("_REMOTE_REPOSITORIES");

    /**
     * The regular expression used to scan the name of a release type from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a release type.
     * This expression uses the 'name' capturing group which returns the release type name, if detected.
     * Value: {@value}
     */
    private static final String RELEASE_TYPES_ITEM_NAME_REGEX = RELEASE_TYPES_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'assets' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_ASSETS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_ASSETS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_ASSETS");

    /**
     * The parametrized name of the environment variable to read for the 'collapseVersions' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_COLLAPSE_VERSIONS");

    /**
     * The parametrized name of the environment variable to read for the 'collapsedVersionQualifier' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_COLLAPSED_VERSION_QUALIFIER");

    /**
     * The parametrized name of the environment variable to read for the 'description' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_DESCRIPTION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_DESCRIPTION_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_DESCRIPTION");

    /**
     * The parametrized name of the environment variable to read for the 'filterTags' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_FILTER_TAGS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_FILTER_TAGS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_FILTER_TAGS");

    /**
     * The parametrized name of the environment variable to read for the 'gitCommit' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_COMMIT_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_COMMIT_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_COMMIT");

    /**
     * The parametrized name of the environment variable to read for the 'gitCommitMessage' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_COMMIT_MESSAGE");

    /**
     * The parametrized name of the environment variable to read for the 'gitPush' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_COMMIT_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_PUSH_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_PUSH");

    /**
     * The parametrized name of the environment variable to read for the 'gitTag' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_TAG_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_TAG_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_TAG");

    /**
     * The parametrized name of the environment variable to read for the 'gitTagMessage' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_TAG_MESSAGE");

    /**
     * The parametrized name of the environment variable to read for the 'gitTagNames' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_GIT_TAG_NAMES_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_GIT_TAG_NAMES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_GIT_TAG_NAMES");

    /**
     * The parametrized name of the environment variable to read for the 'identifiers' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the commit release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_IDENTIFIERS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_IDENTIFIERS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_IDENTIFIERS");

    /**
     * The parametrized name of the environment variable to read for the 'matchBranches' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_MATCH_BRANCHES_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_MATCH_BRANCHES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_MATCH_BRANCHES");

    /**
     * The parametrized name of the environment variable to read for the 'matchEnvironmentVariables' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_MATCH_ENVIRONMENT_VARIABLES");

    /**
     * The parametrized name of the environment variable to read for the 'matchWorkspaceStatus' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_MATCH_WORKSPACE_STATUS");

    /**
     * The parametrized name of the environment variable to read for the 'publish' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_PUBLISH_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_PUBLISH_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_PUBLISH");

    /**
     * The parametrized name of the environment variable to read for the 'releaseName' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_RELEASE_NAME_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_RELEASE_NAME_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_RELEASE_NAME");

    /**
     * The parametrized name of the environment variable to read for the 'versionRange' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_VERSION_RANGE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_VERSION_RANGE_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_VERSION_RANGE");

    /**
     * The parametrized name of the environment variable to read for the 'versionRangeFromBranchName' attribute of a
     * release type.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the release type name
     * and must be rendered using {@link String#format(String, Object...) String.format(RELEASE_TYPES_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the release type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String RELEASE_TYPES_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING = RELEASE_TYPES_ENVVAR_NAME.concat("_%s_VERSION_RANGE_FROM_BRANCH_NAME");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String RESUME_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("RESUME");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SCHEME_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SCHEME");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SERVICES_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SERVICES");

    /**
     * The regular expression used to scan the name of a service from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a service.
     * This expression uses the 'name' capturing group which returns the service name, if detected.
     * Value: {@value}
     */
    private static final String SERVICES_ITEM_NAME_REGEX = SERVICES_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'options' attribute of a
     * service.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the service name
     * and must be rendered using {@link String#format(String, Object...) String.format(SERVICES_ITEM_OPTIONS_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the service with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String SERVICES_ITEM_OPTIONS_FORMAT_STRING = SERVICES_ENVVAR_NAME.concat("_%s_OPTIONS");

    /**
     * The parametrized name of the environment variable to read for the 'type' attribute of a
     * service.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the service name
     * and must be rendered using {@link String#format(String, Object...) String.format(SERVICES_ITEM_TYPE_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the service type with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String SERVICES_ITEM_TYPE_FORMAT_STRING = SERVICES_ENVVAR_NAME.concat("_%s_TYPE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SHARED_CONFIGURATION_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SHARED_CONFIGURATION_FILE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String STATE_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("STATE_FILE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SUBSTITUTIONS_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SUBSTITUTIONS");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SUBSTITUTIONS_ENABLED_ENVVAR_NAME = SUBSTITUTIONS_ENVVAR_NAME.concat("_ENABLED");

    /**
     * The regular expression used to scan the name of a substitution from an environment
     * variable name. This expression is used to detect if an environment variable is used to define
     * a substitution.
     * This expression uses the 'name' capturing group which returns the substitution name, if detected.
     * Value: {@value}
     */
    private static final String SUBSTITUTIONS_ITEM_NAME_REGEX = SUBSTITUTIONS_ENVVAR_NAME.concat("_(?<name>[a-zA-Z0-9]+)_([a-zA-Z0-9_]+)$");

    /**
     * The parametrized name of the environment variable to read for the 'files' attribute of a
     * substitution.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the substitution name
     * and must be rendered using {@link String#format(String, Object...) String.format(SUBSTITUTIONS_ITEM_EXPRESSION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the substitution with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String SUBSTITUTIONS_ITEM_FILES_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME.concat("_%s_FILES");

    /**
     * The parametrized name of the environment variable to read for the 'match' attribute of a
     * substitution.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the substitution name
     * and must be rendered using {@link String#format(String, Object...) String.format(SUBSTITUTIONS_ITEM_EXPRESSION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the substitution with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String SUBSTITUTIONS_ITEM_MATCH_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME.concat("_%s_MATCH");

    /**
     * The parametrized name of the environment variable to read for the 'replace' attribute of a
     * substitution.
     * This string is a {@link Formatter string} that contains a '%s' parameter for the substitution name
     * and must be rendered using {@link String#format(String, Object...) String.format(SUBSTITUTIONS_ITEM_EXPRESSION_FORMAT_STRING, name)}
     * in order to get the actual name of environment variable that brings the value for the substitution with the given {@code name}.
     * Value: {@value}
     * 
     * @see Formatter
     * @see String#format(String, Object...)
     */
    private static final String SUBSTITUTIONS_ITEM_REPLACE_FORMAT_STRING = SUBSTITUTIONS_ENVVAR_NAME.concat("_%s_REPLACE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SUMMARY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SUMMARY");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String SUMMARY_FILE_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("SUMMARY_FILE");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String VERBOSITY_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("VERBOSITY");

    /**
     * The name of the environment variable to read for this value. Value: {@value}
     */
    private static final String VERSION_ENVVAR_NAME = ENVVAR_NAME_GLOBAL_PREFIX.concat("VERSION");

    /**
     * The private instance of the changelog configuration section.
     */
    private ChangelogConfiguration changelogSection = null;

    /**
     * The private instance of the commit message convention configuration section.
     */
    private CommitMessageConventions commitMessageConventionsSection = null;

    /**
     * The private instance of the git configuration section.
     */
    private GitConfiguration gitSection = null;

    /**
     * The private instance of the release assets configuration section.
     */
    private Map<String,Attachment> releaseAssetsSection = null;

    /**
     * The private instance of the release types configuration section.
     */
    private ReleaseTypes releaseTypesSection = null;

    /**
     * The private instance of the services configuration section.
     */
    private Map<String,ServiceConfiguration> servicesSection = null;

    /**
     * The private instance of the substitutions configuration section.
     */
    private Substitutions substitutionsSection = null;

    /**
     * Default constructor is private on purpose.
     */
    protected EnvironmentConfigurationLayer() {
        super();
    }

    /**
     * Reads the given environment variable and parses it as a comma separated list of names that will
     * be returned as a list.
     * 
     * @param attributeGroupName the name of the attribute group, used for log messages
     * @param leafAttributeName the name of the specific attribute within the group, used for log messages
     * @param envVarName the name of the environment variable to read and parse
     * 
     * @return the list of item names parsed from the environment variable. It may be empty but not {@code null}
     */
    private List<String> getItemNamesListFromEnvironmentVariable(String attributeGroupName, String leafAttributeName, String envVarName) {
        List<String> itemNames = new ArrayList<String>();
        String envVarValue = getenv(envVarName);
        if (Objects.isNull(envVarValue))
            logger.trace(CONFIGURATION, "No environment variable named '{}' can be found, assuming the environment variables do not set the '{}.{}' configuration option", envVarName, attributeGroupName, leafAttributeName);
        else if (envVarValue.isBlank())
            logger.trace(CONFIGURATION, "The environment variable named '{}' has been found but is empty, assuming the environment variables do not set the '{}.{}' configuration option", envVarName, attributeGroupName, leafAttributeName);
        else {
            logger.trace(CONFIGURATION, "The environment variable named '{}' has been found with value '{}'. Parsing the value to infer the '{}.{}' configuration option", envVarName, envVarValue, attributeGroupName, leafAttributeName);
            itemNames.addAll(List.<String>of(envVarValue.split(",")));
            logger.trace(CONFIGURATION, "The environment variable named '{}' has been parsed and yields to {} items: '{}'", envVarName, itemNames.size(), String.join(", ", itemNames));
        }
        return itemNames;
    }

    /**
     * Scans all environment variables trying to match their names against the given regular expression. If a match is found
     * then a capturing group named {@code name} is used to extrapolate the item name, which will then be returned in the result.
     * 
     * @param attributeGroupName the name of the attribute group, used for log messages
     * @param regex a regular expression used to match environment variable names. It must also contain a matching
     * group named {@code name} which will be used to extrapolate the name
     * @param ignores an optional collection of variable names to ignore. It may be {@code null}
     * 
     * @return the set of item names parsed from the environment variables. It may be empty but not {@code null}
     * 
     * @throws PatternSyntaxException if the given regular expression can't be compiled
     * @throws IllegalArgumentException if the given regular expression does not contain the {@code name} capturing group
     */
    private Set<String> scanItemNamesInEnvironmentVariables(String attributeGroupName, String regex, Collection<String> ignores)
        throws PatternSyntaxException, IllegalArgumentException {
        logger.trace(CONFIGURATION, "Scanning environment variables searching for the names of configured elements belonging to the '{}' group using the regular expression: '{}'", attributeGroupName, regex);
        // Scan all environment variables whose name matches the items regular expression
        // so that we can infer the name of the various items
        Set<String> itemNames = new HashSet<String>();
        for (String envVarName: getenv().keySet()) {
            if (Objects.isNull(ignores) || (!Objects.isNull(ignores) && !ignores.contains(envVarName))) {
                Matcher m = Pattern.compile(regex).matcher(envVarName);
                try {
                    if (m.find()) {
                        logger.trace(CONFIGURATION, "The environment variable named '{}' denotes it configures a '{}' item", envVarName, attributeGroupName);
                        String name = m.group("name");
                        if (Objects.isNull(name) || name.isBlank()) {
                            logger.warn(CONFIGURATION, "The environment variable named '{}' denotes it configures a '{}' item but the item name can't be extrapolated using the regular expression: '{}'", envVarName, attributeGroupName, regex);
                        }
                        else {
                            logger.trace(CONFIGURATION, "The environment variable named '{}' denotes it configures a '{}' item named '{}'", envVarName, attributeGroupName, name);
                            itemNames.add(name);
                        }
                    }
                }
                catch (IllegalStateException | IllegalArgumentException ie) {
                    // the variable name does not define any item, just skip it
                }
            }
        }
        logger.trace(CONFIGURATION, "The set of '{}' items configured using environment variables has {} items: '{}'", attributeGroupName, itemNames.size(), String.join(", ", itemNames));
        return itemNames;
    }

    /**
     * Scans the environment variables and takes all the ones starting with a certain prefix
     * and put them in the returned map. The key stored in the map is the trailing part of the variable name
     * after the given prefix, while values are the corresponding variable values.
     * 
     * @param attributeGroupName the name of the attribute group, used for log messages
     * @param envVarNamePrefix the prefix of all environment variable names to be considered map items. This
     * prefix will be discarded from variable names in order to get item keys.
     * @param ignores an optional collection of variable names to ignore. It may be {@code null}
     * 
     * @return the map of the name value pairs obtained after scanning the environment variables
     */
    private Map<String,String> getAttributeMapFromEnvironmentVariable(String attributeGroupName, String envVarNamePrefix, Collection<String> ignores) {
        logger.trace(CONFIGURATION, "Scanning environment variables searching for the items belonging to the '{}' group using the prefix: '{}'", attributeGroupName, envVarNamePrefix);
        Map<String,String> attributeMap = new HashMap<String,String>();
        // Scan environment variables in order to find the items whose name starts with the right prefix.
        // The trailing part is then supposed to be the map item name
        envVarNamePrefix = envVarNamePrefix.concat("_"); // avoid false positives and consider that before the entry name has another separator '_', that we want to ignore hereafter
        for (String envVarName: getenv().keySet()) {
            if (Objects.isNull(ignores) || (!Objects.isNull(ignores) && !ignores.contains(envVarName))) {
                if (envVarName.startsWith(envVarNamePrefix) && (envVarName.length()>envVarNamePrefix.length())) {
                    String mapItemName = envVarName.replaceFirst(envVarNamePrefix, "");
                    String mapItemValue = getenv(envVarName);
                    attributeMap.put(mapItemName, mapItemValue);
                    logger.trace(CONFIGURATION, "The '{}' map has the following item: '{}'='{}'", attributeGroupName, mapItemName, mapItemValue);
                }
            }
        }
        logger.trace(CONFIGURATION, "The map of '{}' items configured using environment variables has {} items: '{}'", attributeGroupName, attributeMap.size(), String.join(", ", attributeMap.keySet()));
        return attributeMap;
    }

    /**
     * Scans the environment variables and takes all the ones starting with a certain prefix
     * and put them in the returned map. The key stored in the map is the trailing part of the variable name
     * after the given prefix, while values are the corresponding variable values.
     * 
     * @param attributeGroupName the name of the attribute group, used for log messages
     * @param envVarNamePrefix the prefix of all environment variable names to be considered map items. This
     * prefix will be discarded from variable names in order to get item keys.
     * @param ignores an optional collection of variable names to ignore. It may be {@code null}
     * 
     * @return the map of the name value pairs obtained after scanning the environment variables
     * 
     * @throws IllegalPropertyException if malformed items are encountered
     */
    private List<Identifier> getIdentifiersListFromEnvironmentVariable(String attributeGroupName, String envVarNamePrefix, Collection<String> ignores)
        throws IllegalPropertyException {
        logger.trace(CONFIGURATION, "Scanning environment variables searching for the items belonging to the '{}' group using the prefix: '{}'", attributeGroupName, envVarNamePrefix);
        SortedMap<Integer,Identifier> identifiersMap = new TreeMap<Integer,Identifier>();
        // Scan environment variables in order to find the items whose name starts with the right prefix.
        // The trailing part is then supposed to be the concatenation of:
        // - the item ordinal within the list
        // - the separator '_'
        // - the name of the attribute of the item
        // We store them in a sorted map so we can return items ordered as they appeared (the order is based on their ordinals)
        envVarNamePrefix = envVarNamePrefix.concat("_"); // avoid false positives and consider that before the ordinal each variable name has another separator '_', that we want to ignore hereafter
        for (String envVarName: getenv().keySet()) {
            if (Objects.isNull(ignores) || (!Objects.isNull(ignores) && !ignores.contains(envVarName))) {
                if (envVarName.startsWith(envVarNamePrefix) && (envVarName.length()>envVarNamePrefix.length())) {
                    String mapItemName = envVarName.replaceFirst(envVarNamePrefix, "");
                    String mapItemValue = getenv(envVarName);

                    String[] listItemNameComponents = mapItemName.split("_");
                    if (listItemNameComponents.length != 2)
                        throw new IllegalPropertyException(String.format("The environment variable name %s is malformed as it is supposed to be in the form %s<INTEGER>_<ATTRIBUTE>", envVarName, envVarNamePrefix));
                    
                    // parse the first part as the integer ordinal
                    Integer ordinal = null;
                    try {
                        ordinal = Integer.valueOf(listItemNameComponents[0]);
                    }
                    catch (NumberFormatException nfe) {
                        throw new IllegalPropertyException(String.format("The environment variable name %s is malformed as it is supposed to be in the form %s<INTEGER>_<ATTRIBUTE>. In this case the integer part %s doesn't seem to be a valid integer", envVarName, envVarNamePrefix, listItemNameComponents[0]), nfe);
                    }

                    // create the object instance and store it in the map, if not already present
                    if (!identifiersMap.containsKey(ordinal))
                        identifiersMap.put(ordinal, new Identifier());

                    // now set the attribute, remember that attributes are set with different iterations of this loop, one for each iteration
                    switch(listItemNameComponents[1].toLowerCase()) {
                        case "qualifier":
                            identifiersMap.get(ordinal).setQualifier(mapItemValue);
                            break;
                        case "value":
                            identifiersMap.get(ordinal).setValue(mapItemValue);
                            break;
                        case "position":
                            try {
                                identifiersMap.get(ordinal).setPosition(Identifier.Position.valueOf(mapItemValue));
                            }
                            catch (IllegalArgumentException iae) {
                                throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", envVarName, mapItemValue), iae);
                            }
                            break;
                        default: logger.warn(CONFIGURATION, "The environment variable '{}' defines an unrecognized identifier attribute '{}'", envVarName, listItemNameComponents[1]);
                    }
                }
            }
        }
        logger.trace(CONFIGURATION, "The map of '{}' items configured using environment variables has {} items", attributeGroupName, identifiersMap.size());
        // Now produce a list from the sorted map so it keeps the item ordering
        List<Identifier> identifiersList = new ArrayList<Identifier>();
        for (Integer key: identifiersMap.keySet())
            identifiersList.add(identifiersMap.get(key));
        return identifiersList;
    }

    /**
     * Returns the same value as {@link System#getenv()}. This method can be overridden for mocking.
     * 
     * @return the same value as {@link System#getenv()}.
     * 
     * @throws SecurityException if thrown by {@link System#getenv()}.
     */
    protected Map<String,String> getenv()
        throws SecurityException {
        return System.getenv();
    }

    /**
     * Returns the same value as {@link System#getenv(String)}. This method can be overridden for mocking.
     * 
     * @return the same value as {@link System#getenv(String)}.
     * 
     * @throws NullPointerException if thrown by {@link System#getenv(String)}.
     * @throws SecurityException if thrown by {@link System#getenv(String)}.
     */
    protected String getenv(String name)
        throws NullPointerException, SecurityException {
        return System.getenv(name);
    }

    /**
     * Returns the singleton instance of this class.
     * 
     * @return the singleton instance of this class
     */
    static EnvironmentConfigurationLayer getInstance() {
        if (Objects.isNull(instance))
            instance = new EnvironmentConfigurationLayer();
        return instance;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return getenv(BUMP_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ChangelogConfiguration getChangelog()
        throws IllegalPropertyException {
        if (Objects.isNull(changelogSection)) {
            // parse the 'sections' map
            Map<String,String> sections = new HashMap<String,String>();
            Set<String> sectionNames = scanItemNamesInEnvironmentVariables("changelog", CHANGELOG_CONFIGURATION_SECTIONS_ITEM_NAME_REGEX, null);
            // now we have the set of all section names configured through environment variables and we can
            // query specific environment variables
            for (String sectionName: sectionNames) {
                sections.put(sectionName, getenv(String.format(CHANGELOG_CONFIGURATION_SECTIONS_ITEM_REGEXP_FORMAT_STRING, sectionName)));
            }

            // parse the 'substitutions' map
            Map<String,String> substitutions = new HashMap<String,String>();
            Set<String> substitutionNames = scanItemNamesInEnvironmentVariables("changelog", CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ITEM_NAME_REGEX, null);
            // now we have the set of all section names configured through environment variables and we can
            // query specific environment variables
            for (String substitutionName: substitutionNames) {
                substitutions.put(substitutionName, getenv(String.format(CHANGELOG_CONFIGURATION_SUBSTITUTIONS_ITEM_REGEXP_FORMAT_STRING, substitutionName)));
            }

            changelogSection = new ChangelogConfiguration(getenv(CHANGELOG_CONFIGURATION_PATH_ENVVAR_NAME), sections, getenv(CHANGELOG_CONFIGURATION_TEMPLATE_ENVVAR_NAME), substitutions);
        }
        return changelogSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public CommitMessageConventions getCommitMessageConventions()
        throws IllegalPropertyException {
        if (Objects.isNull(commitMessageConventionsSection)) {
            // parse the 'enabled' items list
            List<String> enabled = getItemNamesListFromEnvironmentVariable("commitMessageConventions", "enabled", COMMIT_MESSAGE_CONVENTIONS_ENABLED_ENVVAR_NAME);
            
            // parse the 'items' map
            Map<String,CommitMessageConvention> items = new HashMap<String,CommitMessageConvention>();

            Set<String> itemNames = scanItemNamesInEnvironmentVariables("commitMessageConventions", COMMIT_MESSAGE_CONVENTIONS_ITEM_NAME_REGEX, null);
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                String expression = getenv(String.format(COMMIT_MESSAGE_CONVENTIONS_ITEM_EXPRESSION_FORMAT_STRING, itemName));
                Map<String,String> bumpExpressions = getAttributeMapFromEnvironmentVariable("commitMessageConventions".concat(".").concat(itemName).concat(".").concat("bumpExpressions"), String.format(COMMIT_MESSAGE_CONVENTIONS_ITEM_BUMP_EXPRESSIONS_FORMAT_STRING, itemName), null);
                
                items.put(itemName, new CommitMessageConvention(expression, bumpExpressions));
            }

            commitMessageConventionsSection = new CommitMessageConventions(enabled, items);
        }
        return commitMessageConventionsSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getConfigurationFile() {
        return getenv(CONFIGURATION_FILE_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDirectory() {
        return getenv(DIRECTORY_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(DRY_RUN_ENVVAR_NAME)) ? null : Boolean.valueOf(getenv(DRY_RUN_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", DRY_RUN_ENVVAR_NAME, getenv(DRY_RUN_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitConfiguration getGit()
        throws IllegalPropertyException {
        if (Objects.isNull(gitSection)) {
            // parse the 'remotes' map
            Map<String,GitRemoteConfiguration> remotes = new HashMap<String,GitRemoteConfiguration>();

            Set<String> itemNames = scanItemNamesInEnvironmentVariables("git", GIT_CONFIGURATION_REMOTES_ITEM_NAME_REGEX, null);
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                AuthenticationMethod authenticationMethod = null;
                String authenticationMethodString         = getenv(String.format(GIT_CONFIGURATION_REMOTES_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName));
                try {
                    if (!Objects.isNull(authenticationMethodString))
                    authenticationMethod = AuthenticationMethod.valueOf(authenticationMethodString);
                }
                catch (IllegalArgumentException iae) {
                    throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", String.format(GIT_CONFIGURATION_REMOTES_ITEM_AUTHENTICATION_METHOD_FORMAT_STRING, itemName), authenticationMethodString), iae);
                }
                String password                           = getenv(String.format(GIT_CONFIGURATION_REMOTES_ITEM_PASSWORD_FORMAT_STRING, itemName));
                String user                               = getenv(String.format(GIT_CONFIGURATION_REMOTES_ITEM_USER_FORMAT_STRING, itemName));
                String privateKey                         = getenv(String.format(GIT_CONFIGURATION_REMOTES_ITEM_PRIVATE_KEY_FORMAT_STRING, itemName));
                String passphrase                         = getenv(String.format(GIT_CONFIGURATION_REMOTES_ITEM_PASSPHRASE_FORMAT_STRING, itemName));
                
                remotes.put(itemName, new GitRemoteConfiguration(authenticationMethod, user, password, privateKey, passphrase));
            }

            gitSection = new GitConfiguration(remotes);
        }
        return gitSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInitialVersion() {
        return getenv(INITIAL_VERSION_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreset() {
        return getenv(PRESET_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,Attachment> getReleaseAssets()
        throws IllegalPropertyException {
        if (Objects.isNull(releaseAssetsSection)) {
            // parse the 'releaseAssets' map
            releaseAssetsSection = new HashMap<String,Attachment>();

            Set<String> itemNames = scanItemNamesInEnvironmentVariables("releaseAssets", RELEASE_ASSETS_ITEM_NAME_REGEX, null);
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                String fileName    = getenv(String.format(RELEASE_ASSETS_ITEM_FILE_NAME_FORMAT_STRING, itemName));
                String description = getenv(String.format(RELEASE_ASSETS_ITEM_DESCRIPTION_FORMAT_STRING, itemName));
                String path        = getenv(String.format(RELEASE_ASSETS_ITEM_PATH_FORMAT_STRING, itemName));
                String type        = getenv(String.format(RELEASE_ASSETS_ITEM_TYPE_FORMAT_STRING, itemName));

                releaseAssetsSection.put(itemName, new Attachment(fileName, description, type, path));
            }
        }
        return releaseAssetsSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(RELEASE_LENIENT_ENVVAR_NAME)) ? null : Boolean.valueOf(getenv(RELEASE_LENIENT_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", RELEASE_LENIENT_ENVVAR_NAME, getenv(RELEASE_LENIENT_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return getenv(RELEASE_PREFIX_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ReleaseTypes getReleaseTypes()
        throws IllegalPropertyException {
        if (Objects.isNull(releaseTypesSection)) {
            // parse the 'enabled' items list
            List<String> enabled = getItemNamesListFromEnvironmentVariable("releaseTypes", "enabled", RELEASE_TYPES_ENABLED_ENVVAR_NAME);

            // parse the 'publicationServices' items list
            List<String> publicationServices = getItemNamesListFromEnvironmentVariable("releaseTypes", "publicationServices", RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME);

            // parse the 'remoteRepositories' items list
            List<String> remoteRepositories = getItemNamesListFromEnvironmentVariable("releaseTypes", "remoteRepositories", RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME);

            // parse the 'items' map
            Map<String,ReleaseType> items = new HashMap<String,ReleaseType>();

            // ignore the RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME and RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME variables or they're interpreted as 'PUBLICATION' items
            Set<String> itemNames = scanItemNamesInEnvironmentVariables("releaseTypes", RELEASE_TYPES_ITEM_NAME_REGEX, Set.<String>of(RELEASE_TYPES_PUBLICATION_SERVICES_ENVVAR_NAME, RELEASE_TYPES_REMOTE_REPOSITORIES_ENVVAR_NAME));
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                Boolean collapseVersions                        = null;
                String collapseVersionsString                   = getenv(String.format(RELEASE_TYPES_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName));
                try {
                    if (!Objects.isNull(collapseVersionsString))
                        collapseVersions = Boolean.valueOf(collapseVersionsString);
                }
                catch (IllegalArgumentException iae) {
                    throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", String.format(RELEASE_TYPES_ITEM_COLLAPSE_VERSIONS_FORMAT_STRING, itemName), collapseVersionsString), iae);
                }
                String assetsList                               = getenv(String.format(RELEASE_TYPES_ITEM_ASSETS_FORMAT_STRING, itemName));
                List<String> assets                             = Objects.isNull(assetsList) ? null : List.<String>of(assetsList.split(","));
                String collapseVersionQualifier                 = getenv(String.format(RELEASE_TYPES_ITEM_COLLAPSED_VERSION_QUALIFIER_FORMAT_STRING, itemName));
                String description                              = getenv(String.format(RELEASE_TYPES_ITEM_DESCRIPTION_FORMAT_STRING, itemName));
                String filterTags                               = getenv(String.format(RELEASE_TYPES_ITEM_FILTER_TAGS_FORMAT_STRING, itemName));
                String gitCommit                                = getenv(String.format(RELEASE_TYPES_ITEM_GIT_COMMIT_FORMAT_STRING, itemName));
                String gitCommitMessage                         = getenv(String.format(RELEASE_TYPES_ITEM_GIT_COMMIT_MESSAGE_FORMAT_STRING, itemName));
                String gitPush                                  = getenv(String.format(RELEASE_TYPES_ITEM_GIT_PUSH_FORMAT_STRING, itemName));
                String gitTag                                   = getenv(String.format(RELEASE_TYPES_ITEM_GIT_TAG_FORMAT_STRING, itemName));
                String gitTagMessage                            = getenv(String.format(RELEASE_TYPES_ITEM_GIT_TAG_MESSAGE_FORMAT_STRING, itemName));
                String gitTagNamesList                          = getenv(String.format(RELEASE_TYPES_ITEM_GIT_TAG_NAMES_FORMAT_STRING, itemName));
                List<String> gitTagNames                        = Objects.isNull(gitTagNamesList) ? null : List.<String>of(gitTagNamesList.split(","));
                List<Identifier> identifiers                    = getIdentifiersListFromEnvironmentVariable("releaseTypes".concat(".").concat(itemName).concat(".").concat("identifiers"), String.format(RELEASE_TYPES_ITEM_IDENTIFIERS_FORMAT_STRING, itemName), null);
                String matchBranches                            = getenv(String.format(RELEASE_TYPES_ITEM_MATCH_BRANCHES_FORMAT_STRING, itemName));
                Map<String,String> matchEnvironmentVariables    = getAttributeMapFromEnvironmentVariable("releaseTypes".concat(".").concat(itemName).concat(".").concat("matchEnvironmentVariables"), String.format(RELEASE_TYPES_ITEM_MATCH_ENVIRONMENT_VARIABLES_FORMAT_STRING, itemName), null);
                WorkspaceStatus matchWorkspaceStatus            = null;
                String matchWorkspaceStatusString               = getenv(String.format(RELEASE_TYPES_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName));
                try {
                    if (!Objects.isNull(matchWorkspaceStatusString))
                        matchWorkspaceStatus = WorkspaceStatus.valueOf(matchWorkspaceStatusString);
                }
                catch (IllegalArgumentException iae) {
                    throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", String.format(RELEASE_TYPES_ITEM_MATCH_WORKSPACE_STATUS_FORMAT_STRING, itemName), matchWorkspaceStatusString), iae);
                }
                String publish                                  = getenv(String.format(RELEASE_TYPES_ITEM_PUBLISH_FORMAT_STRING, itemName));
                String releaseName                              = getenv(String.format(RELEASE_TYPES_ITEM_RELEASE_NAME_FORMAT_STRING, itemName));
                String versionRange                             = getenv(String.format(RELEASE_TYPES_ITEM_VERSION_RANGE_FORMAT_STRING, itemName));
                Boolean versionRangeFromBranchName              = null;
                String versionRangeFromBranchNameString         = getenv(String.format(RELEASE_TYPES_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName));
                try {
                    if (!Objects.isNull(versionRangeFromBranchNameString))
                        versionRangeFromBranchName = Boolean.valueOf(versionRangeFromBranchNameString);
                }
                catch (IllegalArgumentException iae) {
                    throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", String.format(RELEASE_TYPES_ITEM_VERSION_RANGE_FROM_BRANCH_NAME_FORMAT_STRING, itemName), versionRangeFromBranchNameString), iae);
                }

                items.put(itemName, new ReleaseType(assets, Objects.isNull(collapseVersions) ? Defaults.ReleaseType.COLLAPSE_VERSIONS : collapseVersions, collapseVersionQualifier, description, filterTags, gitCommit, gitCommitMessage, gitPush, gitTag, gitTagMessage, gitTagNames, identifiers, matchBranches, matchEnvironmentVariables, matchWorkspaceStatus, publish, releaseName, versionRange, versionRangeFromBranchName));
            }

            releaseTypesSection = new ReleaseTypes(enabled, publicationServices, remoteRepositories, items);
        }
        return releaseTypesSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getResume()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(RESUME_ENVVAR_NAME)) ? null : Boolean.valueOf(getenv(RESUME_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", RESUME_ENVVAR_NAME, getenv(RESUME_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(SCHEME_ENVVAR_NAME)) ? null : Scheme.valueOf(getenv(SCHEME_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", SCHEME_ENVVAR_NAME, getenv(SCHEME_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,ServiceConfiguration> getServices()
        throws IllegalPropertyException {
        if (Objects.isNull(servicesSection)) {
            // parse the 'services' map
            servicesSection = new HashMap<String,ServiceConfiguration>();

            Set<String> itemNames = scanItemNamesInEnvironmentVariables("services", SERVICES_ITEM_NAME_REGEX, null);
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                Provider type                                   = null;
                String typeString                               = getenv(String.format(SERVICES_ITEM_TYPE_FORMAT_STRING, itemName));
                try {
                    if (!Objects.isNull(typeString))
                        type = Provider.valueOf(typeString);
                }
                catch (IllegalArgumentException iae) {
                    throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", String.format(SERVICES_ITEM_TYPE_FORMAT_STRING, itemName), typeString), iae);
                }
                Map<String,String> options                      = getAttributeMapFromEnvironmentVariable("services".concat(".").concat(itemName).concat(".").concat("options"), String.format(SERVICES_ITEM_OPTIONS_FORMAT_STRING, itemName), null);

                servicesSection.put(itemName, new ServiceConfiguration(type, options));
            }
        }
        return servicesSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSharedConfigurationFile() {
        return getenv(SHARED_CONFIGURATION_FILE_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStateFile() {
        return getenv(STATE_FILE_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Substitutions getSubstitutions()
        throws IllegalPropertyException {
        if (Objects.isNull(substitutionsSection)) {
            // parse the 'enabled' items list
            List<String> enabled = getItemNamesListFromEnvironmentVariable("substitutions", "enabled", SUBSTITUTIONS_ENABLED_ENVVAR_NAME);
            
            // parse the 'items' map
            Map<String,Substitution> items = new HashMap<String,Substitution>();

            Set<String> itemNames = scanItemNamesInEnvironmentVariables("substitutions", SUBSTITUTIONS_ITEM_NAME_REGEX, null);
            // now we have the set of all item names configured through environment variables and we can
            // query specific environment variables
            for (String itemName: itemNames) {
                String files = getenv(String.format(SUBSTITUTIONS_ITEM_FILES_FORMAT_STRING, itemName));
                String match = getenv(String.format(SUBSTITUTIONS_ITEM_MATCH_FORMAT_STRING, itemName));
                String replace = getenv(String.format(SUBSTITUTIONS_ITEM_REPLACE_FORMAT_STRING, itemName));
                
                items.put(itemName, new Substitution(files, match, replace));
            }

            substitutionsSection = new Substitutions(enabled, items);
        }
        return substitutionsSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getSummary()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(SUMMARY_ENVVAR_NAME)) ? null : Boolean.valueOf(getenv(SUMMARY_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", SUMMARY_ENVVAR_NAME, getenv(SUMMARY_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSummaryFile() {
        return getenv(SUMMARY_FILE_ENVVAR_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws IllegalPropertyException {
        try {
            return Objects.isNull(getenv(VERBOSITY_ENVVAR_NAME)) ? null : Verbosity.valueOf(getenv(VERBOSITY_ENVVAR_NAME));
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalPropertyException(String.format("The environment variable '%s' has an illegal value '%s'", VERBOSITY_ENVVAR_NAME, getenv(VERBOSITY_ENVVAR_NAME)), iae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion() {
        return getenv(VERSION_ENVVAR_NAME);
    }
}
