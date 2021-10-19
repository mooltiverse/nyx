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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.CommitMessageConventions;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.data.Identifiers;
import com.mooltiverse.oss.nyx.data.IdentifierPosition;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.data.ReleaseTypes;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.data.WorkspaceStatus;
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
     * The private singleton instance of the commit message convention configuration block.
     */
    private CommitMessageConventionsBlock commitMessageConventionsBlock = null;

    /**
     * The private singleton instance of the release types configuration block.
     */
    private ReleaseTypesBlock releaseTypesBlock = null;

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
    public CommitMessageConventions getCommitMessageConventions() {
        if (Objects.isNull(commitMessageConventionsBlock)) {
            commitMessageConventionsBlock = new CommitMessageConventionsBlock();
        }
        return commitMessageConventionsBlock;
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
    public ReleaseTypes getReleaseTypes() {
        if (Objects.isNull(releaseTypesBlock)) {
            releaseTypesBlock = new ReleaseTypesBlock();
        }
        return releaseTypesBlock;
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

    /**
     * The class implementing the {@link CommitMessageConventions} configuration block.
     */
    private class CommitMessageConventionsBlock implements CommitMessageConventions {
        /**
         * The local cache of resolved convention items.
         */
        private Map<String, CommitMessageConvention> items = null;

        /**
         * Default constructor is private on purpose.
         */
        private CommitMessageConventionsBlock() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<String> getEnabled() {
            // the list property is always present and never null but is empty when the user doesn't define its contents
            return extension.getCommitMessageConventions().getEnabled().isPresent() && !extension.getCommitMessageConventions().getEnabled().get().isEmpty() ? extension.getCommitMessageConventions().getEnabled().get() : null;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String,CommitMessageConvention> getItems() {
            // the map property is always present and never null but is empty when the user doesn't define its contents
            if (extension.getCommitMessageConventions().getItems().isEmpty())
                return null;
            else {
                if (Objects.isNull(items))
                    items = new HashMap<String, CommitMessageConvention>(extension.getCommitMessageConventions().getItems().size());

                for (NyxExtension.CommitMessageConventions.CommitMessageConvention convention: extension.getCommitMessageConventions().getItems()) {
                    if (!items.containsKey(convention.getName()))
                        items.put(convention.getName(), getItem(convention.getName()));
                }

                return items;
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public CommitMessageConvention getItem(String name) {
            NyxExtension.CommitMessageConventions.CommitMessageConvention convention = extension.getCommitMessageConventions().getItems().findByName(name);
            return Objects.isNull(convention) ? null : new CommitMessageConvention(convention.getExpression().get(), convention.getBumpExpressions().isPresent() ? convention.getBumpExpressions().get() : Map.<String,String>of());
        }
    }

    /**
     * The class implementing the {@link ReleaseTypes} configuration block.
     */
    private class ReleaseTypesBlock implements ReleaseTypes {
        /**
         * The local cache of resolved release type items.
         */
        private Map<String, ReleaseType> items = null;

        /**
         * Default constructor is private on purpose.
         */
        private ReleaseTypesBlock() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<String> getEnabled() {
            // the list property is always present and never null but is empty when the user doesn't define its contents
            return extension.getReleaseTypes().getEnabled().isPresent() && !extension.getReleaseTypes().getEnabled().get().isEmpty() ? extension.getReleaseTypes().getEnabled().get() : null;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String,ReleaseType> getItems() {
            // the map property is always present and never null but is empty when the user doesn't define its contents
            if (extension.getReleaseTypes().getItems().isEmpty())
                return null;
            else {
                if (Objects.isNull(items))
                    items = new HashMap<String, ReleaseType>(extension.getReleaseTypes().getItems().size());

                for (NyxExtension.ReleaseTypes.ReleaseType type: extension.getReleaseTypes().getItems()) {
                    if (!items.containsKey(type.getName()))
                        items.put(type.getName(), getItem(type.getName()));
                }

                return items;
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public ReleaseType getItem(String name) {
            NyxExtension.ReleaseTypes.ReleaseType type = extension.getReleaseTypes().getItems().findByName(name);

            if (Objects.isNull(type))
                return null;
            else {
                Identifiers identifiers = null;
                if (!Objects.isNull(type.getIdentifiers())) {
                    Map<String,Identifier> items = null;
                    if (!type.getIdentifiers().getItems().isEmpty()) {
                        items = new HashMap<String,Identifier>(type.getIdentifiers().getItems().size());
                        for (NyxExtension.ReleaseTypes.ReleaseType.Identifiers.Identifier identifier: type.getIdentifiers().getItems()) {
                            if (!items.containsKey(identifier.getName()))
                                items.put(identifier.getName(), new Identifier(identifier.getQualifier().isPresent() ? identifier.getQualifier().get() : null, identifier.getValue().isPresent() ? identifier.getValue().get() : null, identifier.getPosition().isPresent() ? IdentifierPosition.valueOf(identifier.getPosition().get()) : null));
                        }
                    }
                    identifiers = new Identifiers(type.getIdentifiers().getEnabled().isPresent() ? type.getIdentifiers().getEnabled().get() : null, items);
                }
                return new ReleaseType(type.getCollapseVersions().isPresent() ? type.getCollapseVersions().get() : null, type.getCollapsedVersionQualifier().isPresent() ? type.getCollapsedVersionQualifier().get() : null, type.getFilterTags().isPresent() ? type.getFilterTags().get() : null, type.getGitCommit().isPresent() ? type.getGitCommit().get() : null, type.getGitCommitMessage().isPresent() ? type.getGitCommitMessage().get() : null, type.getGitPush().isPresent() ? type.getGitPush().get() : null, type.getGitTag().isPresent() ? type.getGitTag().get() : null, type.getGitTagMessage().isPresent() ? type.getGitTagMessage().get() : null, identifiers, type.getMatchBranches().isPresent() ? type.getMatchBranches().get() : null, type.getMatchEnvironmentVariables().isPresent() ? type.getMatchEnvironmentVariables().get() : null, type.getMatchWorkspaceStatus().isPresent() ? WorkspaceStatus.valueOf(type.getMatchWorkspaceStatus().get()) : null, type.getPublish().isPresent() ? type.getPublish().get() : null, type.getVersionRange().isPresent() ? type.getVersionRange().get() : null, type.getVersionRangeFromBranchName().isPresent() ? type.getVersionRangeFromBranchName().get() : null);
            }
        }
    }
}
