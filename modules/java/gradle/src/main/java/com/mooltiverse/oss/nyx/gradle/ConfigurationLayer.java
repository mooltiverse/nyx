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

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.CommitMessageConventions;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;

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
    public File getDirectory() {
        return extension.getDirectory().getOrNull();
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
    public String getInitialVersion()
        throws IllegalPropertyException {
        return extension.getInitialVersion().getOrNull();
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
    public Scheme getScheme()
        throws IllegalPropertyException {
        if (extension.getScheme().isPresent() && !Objects.isNull(extension.getScheme().getOrNull())) {
            try {
                return Scheme.from(extension.getScheme().get());
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
    public String getStateFile() {
        return extension.getStateFile().isPresent() ? extension.getStateFile().get().getAbsolutePath() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws IllegalPropertyException {
        if (extension.getVerbosity().isPresent() && !Objects.isNull(extension.getVerbosity().getOrNull())) {
            try {
                return Verbosity.from(extension.getVerbosity().get());
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
     * The class implementing the {@link CommitMessageConventions} confliguration block.
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
}
