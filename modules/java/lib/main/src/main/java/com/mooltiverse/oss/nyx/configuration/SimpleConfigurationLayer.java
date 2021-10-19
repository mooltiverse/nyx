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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.CommitMessageConventions;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.data.ReleaseTypes;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A simple configuration layer, acting as a value holder for configuration options.
 */
public class SimpleConfigurationLayer implements ConfigurationLayer {
    /**
     * The value held by this object.
     */
    private String bump = null;

    /**
     * The value held by this object.
     */
    private final SimpleCommitMessageConventions commitMessageConventions = new SimpleCommitMessageConventions();
    
    /**
     * The value held by this object.
     */
    private String configurationFile = null;

    /**
     * The value held by this object.
     */
    private String directory = null;

    /**
     * The value held by this object.
     */
    private Boolean dryRun = null;

    /**
     * The value held by this object.
     */
    private String initialVersion = null;

    /**
     * The value held by this object.
     */
    private String preset = null;

    /**
     * The value held by this object.
     */
    private String releasePrefix = null;

    /**
     * The value held by this object.
     */
    private Boolean releaseLenient = null;

    /**
     * The value held by this object.
     */
    private final SimpleReleaseTypes releaseTypes = new SimpleReleaseTypes();

    /**
     * The value held by this object.
     */
    private Boolean resume = null;

    /**
     * The value held by this object.
     */
    private Scheme scheme = null;

    /**
     * The value held by this object.
     */
    private String sharedConfigurationFile = null;

    /**
     * The value held by this object.
     */
    private String stateFile = null;

    /**
     * The value held by this object.
     */
    private Verbosity verbosity = null;

    /**
     * The value held by this object.
     */
    private String version = null;

    /**
     * Default constructor.
     */
    public SimpleConfigurationLayer() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return bump;
    }

    /**
     * Sets the value for this option.
     * 
     * @param bump the value for this option.
     */
    public void setBump(String bump) {
        this.bump = bump;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SimpleCommitMessageConventions getCommitMessageConventions() {
        return commitMessageConventions;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getConfigurationFile() {
        return configurationFile;
    }

    /**
     * Sets the value for this option.
     * 
     * @param configurationFile the value for this option.
     */
    public void setConfigurationFile(String configurationFile) {
        this.configurationFile = configurationFile;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDirectory() {
        return directory;
    }

    /**
     * Sets the value for this option.
     * 
     * @param directory the value for this option.
     */
    public void setDirectory(String directory) {
        this.directory = directory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return dryRun;
    }

    /**
     * Sets the value for this option.
     * 
     * @param dryRun the value for this option.
     */
    public void setDryRun(Boolean dryRun) {
        this.dryRun = dryRun;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInitialVersion(){
        return initialVersion;
    }

    /**
     * Sets the value for this option.
     * 
     * @param initialVersion the value for this option.
     */
    public void setInitialVersion(String initialVersion) {
        this.initialVersion = initialVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreset() {
        return preset;
    }

    /**
     * Sets the value for this option.
     * 
     * @param preset the value for this option.
     */
    public void setPreset(String preset) {
        this.preset = preset;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return releasePrefix;
    }

    /**
     * Sets the value for this option.
     * 
     * @param releasePrefix the value for this option.
     */
    public void setReleasePrefix(String releasePrefix) {
        this.releasePrefix = releasePrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient() {
        return releaseLenient;
    }

    /**
     * Sets the value for this option.
     * 
     * @param releaseLenient the value for this option.
     */
    public void setReleaseLenient(Boolean releaseLenient) {
        this.releaseLenient = releaseLenient;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SimpleReleaseTypes getReleaseTypes() {
        return releaseTypes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getResume() {
        return resume;
    }

    /**
     * Sets the value for this option.
     * 
     * @param resume the value for this option.
     */
    public void setResume(Boolean resume) {
        this.resume = resume;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme() {
        return scheme;
    }

    /**
     * Sets the value for this option.
     * 
     * @param scheme the value for this option.
     */
    public void setScheme(Scheme scheme) {
        this.scheme = scheme;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSharedConfigurationFile() {
        return sharedConfigurationFile;
    }

    /**
     * Sets the value for this option.
     * 
     * @param sharedConfigurationFile the value for this option.
     */
    public void setSharedConfigurationFile(String sharedConfigurationFile) {
        this.sharedConfigurationFile = sharedConfigurationFile;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStateFile() {
        return stateFile;
    }

    /**
     * Sets the value for this option.
     * 
     * @param stateFile the value for this option.
     */
    public void setStateFile(String stateFile) {
        this.stateFile = stateFile;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity(){
        return verbosity;
    }

    /**
     * Sets the value for this option.
     * 
     * @param verbosity the value for this option.
     */
    public void setVerbosity(Verbosity verbosity) {
        this.verbosity = verbosity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion(){
        return version;
    }

    /**
     * Sets the value for this option.
     * 
     * @param version the value for this option.
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * The value holder for commit message conventions.
     */
    public static class SimpleCommitMessageConventions implements CommitMessageConventions {
        /**
         * The value held by this object.
         */
        public List<String> enabled = null;

        /**
         * The value held by this object.
         */
        public final Map<String,CommitMessageConvention> items = new HashMap<String,CommitMessageConvention>();

        /**
         * Default constructor.
         */
        public SimpleCommitMessageConventions() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<String> getEnabled() {
            return enabled;
        }

        /**
         * Sets the value for this option.
         * 
         * @param enabled the value for this option.
         */
        public void setEnabled(List<String> enabled) {
            this.enabled = enabled;
        }
    
        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String,CommitMessageConvention> getItems() {
            return items;
        }
    
        /**
         * {@inheritDoc}
         */
        @Override
        public CommitMessageConvention getItem(String name) {
            return items.get(name);
        }
    }

    /**
     * The value holder for commit message conventions.
     */
    public static class SimpleReleaseTypes implements ReleaseTypes {
        /**
         * The value held by this object.
         */
        public List<String> enabled = null;

        /**
         * The value held by this object.
         */
        public final Map<String,ReleaseType> items = new HashMap<String,ReleaseType>();

        /**
         * Default constructor.
         */
        public SimpleReleaseTypes() {
            super();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public List<String> getEnabled() {
            return enabled;
        }

        /**
         * Sets the value for this option.
         * 
         * @param enabled the value for this option.
         */
        public void setEnabled(List<String> enabled) {
            this.enabled = enabled;
        }
    
        /**
         * {@inheritDoc}
         */
        @Override
        public Map<String,ReleaseType> getItems() {
            return items;
        }
    
        /**
         * {@inheritDoc}
         */
        @Override
        public ReleaseType getItem(String name) {
            return items.get(name);
        }
    }
}
