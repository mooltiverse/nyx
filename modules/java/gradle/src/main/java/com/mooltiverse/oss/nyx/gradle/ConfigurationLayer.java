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
package com.mooltiverse.oss.nyx.gradle;

import static org.gradle.api.Project.DEFAULT_VERSION;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.configuration.Defaults;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.EnabledItemsMap;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * This class is an adapter to allow the extension to be used as a Nyx configuration layer.
 */
class ConfigurationLayer implements com.mooltiverse.oss.nyx.configuration.ConfigurationLayer {
    /**
     * The private instance of the extension object to adapt
     */
    private final NyxExtension extension;

    /**
     * The project version, as defined by the user in Gradle's {@code version} property.
     * May be {@code null} if the user has not defined any {@code version} property in the script
     */
    private final Object projectVersion;

    /**
     * The private instance of the commit message convention configuration section.
     */
    private EnabledItemsMap<CommitMessageConvention> commitMessageConventionsSection = null;

    /**
     * The private instance of the release types configuration section.
     */
    private EnabledItemsMap<ReleaseType> releaseTypesSection = null;

    /**
     * Standard constructor.
     * 
     * @param extension the extension instance to be adapted to a configuration layer
     * @param projectVersion the Gradle project version, which may be {@code null} if the user has not defined
     * any {@code version} property in the script
     */
    ConfigurationLayer(NyxExtension extension, Object projectVersion) {
        super();
        if (Objects.isNull(extension))
            throw new IllegalArgumentException("Cannot build a configuration layer adapter with a null extension");
        this.extension = extension;
        this.projectVersion = projectVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return extension.getBump().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EnabledItemsMap<CommitMessageConvention> getCommitMessageConventions() {
        if (Objects.isNull(commitMessageConventionsSection)) {
            // the list property is always present and never null but is empty when the user doesn't define its contents
            List<String> enabled = extension.getCommitMessageConventions().getEnabled().isPresent() && !extension.getCommitMessageConventions().getEnabled().get().isEmpty() ? extension.getCommitMessageConventions().getEnabled().get() : List.<String>of();

            Map<String, CommitMessageConvention> items = new HashMap<String, CommitMessageConvention>(extension.getCommitMessageConventions().getItems().size());
            // the map property is always present and never null but is empty when the user doesn't define its contents
            if (!extension.getCommitMessageConventions().getItems().isEmpty()) {
                for (NyxExtension.CommitMessageConventions.CommitMessageConvention convention: extension.getCommitMessageConventions().getItems()) {
                    if (!items.containsKey(convention.getName())) {
                        items.put(convention.getName(), new CommitMessageConvention(convention.getExpression().get(), convention.getBumpExpressions().isPresent() ? convention.getBumpExpressions().get() : Map.<String,String>of()));
                    }
                }
            }

            commitMessageConventionsSection = new EnabledItemsMap<CommitMessageConvention>(enabled, items);
        }
        return commitMessageConventionsSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getConfigurationFile() {
        return extension.getConfigurationFile().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDirectory() {
        return extension.getDirectory().isPresent() ? extension.getDirectory().get().getAbsolutePath() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return extension.getDryRun().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInitialVersion() {
        return extension.getInitialVersion().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreset() {
        return extension.getPreset().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return extension.getReleasePrefix().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient() {
        return extension.getReleaseLenient().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EnabledItemsMap<ReleaseType> getReleaseTypes() {
        if (Objects.isNull(releaseTypesSection)) {
            // the list property is always present and never null but is empty when the user doesn't define its contents
            List<String> enabled = extension.getReleaseTypes().getEnabled().isPresent() && !extension.getReleaseTypes().getEnabled().get().isEmpty() ? extension.getReleaseTypes().getEnabled().get() : List.<String>of();

            Map<String, ReleaseType> items = new HashMap<String, ReleaseType>(extension.getReleaseTypes().getItems().size());
            // the map property is always present and never null but is empty when the user doesn't define its contents
            if (!extension.getReleaseTypes().getItems().isEmpty()) {
                for (NyxExtension.ReleaseTypes.ReleaseType type: extension.getReleaseTypes().getItems()) {
                    if (!items.containsKey(type.getName())) {
                        List<Identifier> identifiers = new ArrayList<Identifier>();
                        if (!Objects.isNull(type.getIdentifiers()) && !type.getIdentifiers().isEmpty()) {
                            // Identifiers are modelled in a NamedDomainObjectContainer, which grants they are sorted by name
                            for (NyxExtension.ReleaseTypes.ReleaseType.Identifier identifier: type.getIdentifiers()) {
                                identifiers.add(new Identifier(
                                    identifier.getQualifier().isPresent() ? identifier.getQualifier().get() : null,
                                    identifier.getValue().isPresent() ? identifier.getValue().get() : null,
                                    identifier.getPosition().isPresent() ? Identifier.Position.valueOf(identifier.getPosition().get()) : null)
                                );
                            }
                        }
                        items.put(type.getName(), new ReleaseType(
                            type.getCollapseVersions().isPresent() ? type.getCollapseVersions().get() : Defaults.ReleaseType.COLLAPSE_VERSIONS,
                            type.getCollapsedVersionQualifier().isPresent() ? type.getCollapsedVersionQualifier().get() : Defaults.ReleaseType.COLLAPSED_VERSION_QUALIFIER,
                            type.getFilterTags().isPresent() ? type.getFilterTags().get() : Defaults.ReleaseType.FILTER_TAGS,
                            type.getGitCommit().isPresent() ? type.getGitCommit().get() : Defaults.ReleaseType.GIT_COMMIT,
                            type.getGitCommitMessage().isPresent() ? type.getGitCommitMessage().get() : Defaults.ReleaseType.GIT_COMMIT_MESSAGE,
                            type.getGitPush().isPresent() ? type.getGitPush().get() : Defaults.ReleaseType.GIT_PUSH,
                            type.getGitTag().isPresent() ? type.getGitTag().get() : Defaults.ReleaseType.GIT_TAG,
                            type.getGitTagMessage().isPresent() ? type.getGitTagMessage().get() : Defaults.ReleaseType.GIT_TAG_MESSAGE,
                            identifiers,
                            type.getMatchBranches().isPresent() ? type.getMatchBranches().get() : Defaults.ReleaseType.MATCH_BRANCHES,
                            type.getMatchEnvironmentVariables().isPresent() ? type.getMatchEnvironmentVariables().get() : Defaults.ReleaseType.MATCH_ENVIRONMENT_VARIABLES,
                            type.getMatchWorkspaceStatus().isPresent() ? WorkspaceStatus.valueOf(type.getMatchWorkspaceStatus().get()) : Defaults.ReleaseType.MATCH_WORKSPACE_STATUS,
                            type.getPublish().isPresent() ? type.getPublish().get() : Defaults.ReleaseType.PUBLISH,
                            type.getVersionRange().isPresent() ? type.getVersionRange().get() : Defaults.ReleaseType.VERSION_RANGE,
                            type.getVersionRangeFromBranchName().isPresent() ? type.getVersionRangeFromBranchName().get() : Defaults.ReleaseType.VERSION_RANGE_FROM_BRANCH_NAME
                        ));
                    }
                }
            }

            releaseTypesSection = new EnabledItemsMap<ReleaseType>(enabled, items);
        }
        return releaseTypesSection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getResume() {
        return extension.getResume().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws IllegalPropertyException {
        if (extension.getScheme().isPresent() && !Objects.isNull(extension.getScheme().getOrNull())) {
            try {
                return Scheme.valueOf(extension.getScheme().get());
            }
            catch (IllegalArgumentException iae) {
                throw new IllegalPropertyException(String.format("Illegal value '%s' provided for configuration option '%s'", extension.getScheme().get(), "scheme"), iae);
            }
        }
        else return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSharedConfigurationFile() {
        return extension.getSharedConfigurationFile().getOrNull();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStateFile() {
        return extension.getStateFile().isPresent() ? extension.getStateFile().get() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws IllegalPropertyException {
        if (extension.getVerbosity().isPresent() && !Objects.isNull(extension.getVerbosity().getOrNull())) {
            try {
                return Verbosity.valueOf(extension.getVerbosity().get());
            }
            catch (IllegalArgumentException iae) {
                throw new IllegalPropertyException(String.format("Illegal value '%s' provided for configuration option '%s'", extension.getVerbosity().get(), "verbosity"), iae);
            }
        }
        else return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion()
        throws IllegalPropertyException {
        // when to 'version' property is defined, Gradle does not return null but instead the 'unspecified' string which, to us,
        // means there is no version defined, just like it was null
        if (Objects.isNull(projectVersion) || DEFAULT_VERSION.equals(projectVersion))
            return null;
        else {
            return projectVersion.toString();
        }
    }
}
