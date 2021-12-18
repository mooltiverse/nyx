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

import java.util.List;
import java.util.Map;

import com.mooltiverse.oss.nyx.version.Versions;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A utility interface that collects default configuration values.
 */
public interface Defaults {
    /**
     * The default version identifier to bump. Value: {@code null}
     */
    public static final String BUMP = null;

    /**
     * The default commit message conventions block.
     */
    public static final CommitMessageConventions COMMIT_MESSAGE_CONVENTIONS = new CommitMessageConventions(List.<String>of(), Map.<String,CommitMessageConvention>of());

    /**
     * The default custom configuration file path. Value: {@code null}
     */
    public static final String CONFIGURATION_FILE = null;

    /**
     * The default working directory. Defaults to the current user directory returned by reading the
     * {@code user.dir} from {@link System#getProperty(String)}
     */
    public static final String DIRECTORY = System.getProperty("user.dir");

    /**
     * The default flag that prevents to alter any repository state and instead just log the actions that would be taken. Value: {@code false}
     */
    public static final Boolean DRY_RUN = Boolean.FALSE;

    /**
     * The default initial version to use. Value: {@link Scheme#SEMVER}
     * 
     * This strongly depends on the {@link #SCHEME} and as long as it's {@link Scheme#SEMVER}, we use that to select the initial version.
     */
    public static final String INITIAL_VERSION = Versions.defaultInitial(Scheme.SEMVER).toString();

    /**
     * The default preset configuration. Value: {@code null}
     */
    public static final String PRESET = null;

    /**
     * The default flag that alows reading releases from the history tolerating arbitrary prefixes and extra non critical characters. Value: {@code true}
     */
    public static final Boolean RELEASE_LENIENT = Boolean.TRUE;

    /**
     * The default prefix to add at the beginning of a version identifier to generate the release identifier. Value: {@code null}
     */
    public static final String RELEASE_PREFIX = null;

    /**
     * A utility interface that collects default configuration values for {@link com.mooltiverse.oss.nyx.entities.ReleaseType} objects.
     */
    public interface ReleaseType {
        /**
         * The flag indicating whether or not the 'collapsed' versioning (pre-release style) must be used. Value: {@code false}
         */
        public static final Boolean COLLAPSE_VERSIONS = Boolean.FALSE;

        /**
         * The optional qualifier or the template to render the qualifier to use for the pre-release identifier when versions are collapsed. Value: {@code null}
         */
        public static final String COLLAPSED_VERSION_QUALIFIER = null;

        /**
         * The optional string or the template to render to use as the release description. Value: {@code Release {{version}}}
         */
        public static final String DESCRIPTION = "Release {{version}}";

        /**
         * The optional template to render as a regular expression used to match tags from the commit history. Value: {@code null}
         */
        public static final String FILTER_TAGS = null;

        /**
         * The optional flag or the template to render indicating whether or not a new commit must be generated in case new artifacts are generated. Value: {@code false}
         */
        public static final String GIT_COMMIT = Boolean.FALSE.toString();

        /**
         * The optional string or the template to render to use as the commit message if a commit has to be made. Value: {@code Release version {{version}}}
         */
        public static final String GIT_COMMIT_MESSAGE = "Release version {{version}}";

        /**
         * The name of the default release type.
         */
        public static final String NAME = "default";

        /**
         * The optional flag or the template to render indicating whether or not a new commit must be generated and pushed in case new artifacts are generated. Value: {@code false}
         */
        public static final String GIT_PUSH = Boolean.FALSE.toString();

        /**
         * The optional flag or the template to render indicating whether or not a new tag must be generated. Value: {@code false}
         */
        public static final String GIT_TAG = Boolean.FALSE.toString();

        /**
         * The optional string or the template to render to use as the tag message if a tag has to be made. Value: {@code null}
         */
        public static final String GIT_TAG_MESSAGE = null;

        /**
         * The identifiers configuration block. Value: {@code null}
         */
        public static final List<Identifier> IDENTIFIERS = null;

        /**
         * The optional template to render as a regular expression used to match branch names. Value: {@code null}
         */
        public static final String MATCH_BRANCHES = null;

        /**
         * The map of the match environment variables items, where keys are environment variable names and values
         * are regular expressions. Value: {@code null}
         */
        public static final Map<String,String> MATCH_ENVIRONMENT_VARIABLES = null;

        /**
         * The identifier of a specific workspace status to be matched. Value: {@code null}
         */
        public static final WorkspaceStatus MATCH_WORKSPACE_STATUS = null;

        /**
         * The optional flag or the template to render indicating whether or not releases must be published. Value: {@code false}
         */
        public static final String PUBLISH = Boolean.FALSE.toString();

        /**
         * The optional template to render as a regular expression used to constrain versions issued by this release type. Value: {@code null}
         */
        public static final String VERSION_RANGE = null;

        /**
         * The optional flag telling if the version range must be inferred from the branch name. Value: {@code false}
         */
        public static final Boolean VERSION_RANGE_FROM_BRANCH_NAME = Boolean.FALSE;
    }

    /**
     * The default release types block.
     */
    public static final ReleaseTypes RELEASE_TYPES = new ReleaseTypes(List.<String>of(Defaults.ReleaseType.NAME), List.<String>of(), List.<String>of(), Map.<String,com.mooltiverse.oss.nyx.entities.ReleaseType>of(Defaults.ReleaseType.NAME, new com.mooltiverse.oss.nyx.entities.ReleaseType()));

    /**
     * The default flag that enables loading a previously stored State file and resume operations from there. Value: {@code false}
     */
    public static final Boolean RESUME = Boolean.FALSE;

    /**
     * The default versioning scheme to use. Value: {@link Scheme#SEMVER}
     */
    public static final Scheme SCHEME = Scheme.SEMVER;

    /**
     * The services configuration block.
     */
    public static final Map<String,ServiceConfiguration> SERVICES = Map.<String,ServiceConfiguration>of();

    /**
     * The default shared custom configuration file path. Value: {@code null}
     */
    public static final String SHARED_CONFIGURATION_FILE = null;

    /**
     * The default path to the local state file. Value: {@code null}
     */
    public static final String STATE_FILE = null;

    /**
     * The default logging level. Value: {@link Verbosity#WARNING}.
     * 
     * Please note that the verbosity option is actually ignored in this library implementation as the event filtering based
     * on the verbosity needs to be configured outside this library, depending on the logging framework deployed along with SLF4J.
     * See <a href="http://www.slf4j.org/manual.html#swapping">here</a> for more.
     */
    public static final Verbosity VERBOSITY = Verbosity.WARNING;

    /**
     * The default release version. Value: {@code null}
     */
    public static final String VERSION = null;
}
