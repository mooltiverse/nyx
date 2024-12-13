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
import java.util.Map;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Substitutions;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A simple configuration layer, acting as a value holder for configuration options. This object allows read/write operations.
 */
public class SimpleConfigurationLayer implements ConfigurationLayer {
    /**
     * The value held by this object.
     */
    private String bump = null;

    /**
     * The value held by this object.
     */
    private ChangelogConfiguration changelog = new ChangelogConfiguration();

    /**
     * The value held by this object.
     */
    private CommitMessageConventions commitMessageConventions = new CommitMessageConventions();
    
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
    private GitConfiguration git = new GitConfiguration();

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
    private Map<String,Attachment> releaseAssets = new HashMap<String,Attachment>();

    /**
     * The value held by this object.
     */
    private Boolean releaseLenient = null;

    /**
     * The value held by this object.
     */
    private String releasePrefix = null;

    /**
     * The value held by this object.
     */
    private ReleaseTypes releaseTypes = new ReleaseTypes();

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
    private Map<String,ServiceConfiguration> services = new HashMap<String,ServiceConfiguration>();

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
    private Substitutions substitutions = new Substitutions();

    /**
     * The value held by this object.
     */
    private Boolean summary = null;

    /**
     * The value held by this object.
     */
    private String summaryFile = null;

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
    public ChangelogConfiguration getChangelog() {
        return changelog;
    }

    /**
     * Sets the value for this option.
     * 
     * @param changelog the value for this option.
     */
    public void setChangelog(ChangelogConfiguration changelog) {
        this.changelog = changelog;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public CommitMessageConventions getCommitMessageConventions() {
        return commitMessageConventions;
    }

    /**
     * Sets the value for this option.
     * 
     * @param commitMessageConventions the value for this option.
     */
    public void setCommitMessageConventions(CommitMessageConventions commitMessageConventions) {
        this.commitMessageConventions = commitMessageConventions;
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
    public GitConfiguration getGit() {
        return git;
    }

    /**
     * Sets the value for this option.
     * 
     * @param git the value for this option.
     */
    public void setGit(GitConfiguration git) {
        this.git = git;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInitialVersion() {
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
    public Map<String,Attachment> getReleaseAssets() {
        return releaseAssets;
    }

    /**
     * Sets the value for this option.
     * 
     * @param releaseAssets the value for this option.
     */
    public void setReleaseAssets(Map<String,Attachment> releaseAssets) {
        this.releaseAssets = releaseAssets;
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
    public ReleaseTypes getReleaseTypes() {
        return releaseTypes;
    }

    /**
     * Sets the value for this option.
     * 
     * @param releaseTypes the value for this option.
     */
    public void setReleaseTypes(ReleaseTypes releaseTypes) {
        this.releaseTypes = releaseTypes;
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
    public Map<String,ServiceConfiguration> getServices() {
        return services;
    }

    /**
     * Sets the value for this option.
     * 
     * @param services the value for this option.
     */
    public void setServices(Map<String,ServiceConfiguration> services) {
        this.services = services;
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
    public Substitutions getSubstitutions() {
        return substitutions;
    }

    /**
     * Sets the value for this option.
     * 
     * @param substitutions the value for this option.
     */
    public void setSubstitutions(Substitutions substitutions) {
        this.substitutions = substitutions;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getSummary() {
        return summary;
    }

    /**
     * Sets the value for this option.
     * 
     * @param summary the value for this option.
     */
    public void setSummary(Boolean summary) {
        this.summary = summary;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSummaryFile() {
        return summaryFile;
    }

    /**
     * Sets the value for this option.
     * 
     * @param summaryFile the value for this option.
     */
    public void setSummaryFile(String summaryFile) {
        this.summaryFile = summaryFile;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity() {
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
    public String getVersion() {
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
}
