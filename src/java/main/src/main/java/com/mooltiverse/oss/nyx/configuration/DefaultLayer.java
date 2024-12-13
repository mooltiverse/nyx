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

import static com.mooltiverse.oss.nyx.log.Markers.DEFAULT;

import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.Defaults;
import com.mooltiverse.oss.nyx.entities.GitConfiguration;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Substitutions;
import com.mooltiverse.oss.nyx.entities.Verbosity;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The default configuration layer. This is a singleton class so instances are to be retrieved via the static {@link #getInstance()} method.
 * 
 * The default configuration layer is used with the least priority when evaluating configuration options so it's queried
 * only if and when a certain ption doesn't appear in any other layer with higher priority.
 */
class DefaultLayer implements ConfigurationLayer, Defaults {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(DefaultLayer.class);

    /**
     * The single instance for this class.
     */
    private static DefaultLayer instance = null;

    /**
     * The directory returned by this layer.
     */
    private String directory = null;

    /**
     * Default constructor is private on purpose.
     */
    private DefaultLayer() {
        super();
    }

    /**
     * Returns the singleton instance of this class.
     * 
     * @return the singleton instance of this class
     */
    static DefaultLayer getInstance() {
        if (Objects.isNull(instance))
            instance = new DefaultLayer();
        return instance;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "bump", BUMP);
        return BUMP;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ChangelogConfiguration getChangelog()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "changelog");
        return CHANGELOG;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public CommitMessageConventions getCommitMessageConventions()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "commitMessageConventions");
        return COMMIT_MESSAGE_CONVENTIONS;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getConfigurationFile()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "configurationFile", CONFIGURATION_FILE);
        return CONFIGURATION_FILE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDirectory()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "directory", Objects.isNull(directory) ? DIRECTORY : directory);
        return Objects.isNull(directory) ? DIRECTORY : directory;
    }

    /**
     * This method allows to override the default directory that will be returned by {@link #getDirectory()}.
     * 
     * @param directory the new default directory. If {@code null} then the standard default directory will be used.
     */
    public void setDirectory(String directory) {
        logger.trace(DEFAULT, "Setting the default '{}' configuration option: '{}'", "directory", Objects.isNull(directory) ? "null" : directory);
        this.directory = directory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "dryRun", DRY_RUN);
        return DRY_RUN;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public GitConfiguration getGit()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "git");
        return GIT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getInitialVersion()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "initialVersion", INITIAL_VERSION);
        return INITIAL_VERSION;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPreset()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "preset", PRESET);
        return PRESET;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,Attachment> getReleaseAssets()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "releaseAssets");
        return RELEASE_ASSETS;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "releaseLenient", RELEASE_LENIENT);
        return RELEASE_LENIENT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "releasePrefix", RELEASE_PREFIX);
        return RELEASE_PREFIX;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ReleaseTypes getReleaseTypes()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "releaseTypes");
        return RELEASE_TYPES;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getResume()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "resume", RESUME);
        return RESUME;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "scheme", SCHEME);
        return SCHEME;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,ServiceConfiguration> getServices()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "services");
        return SERVICES;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSharedConfigurationFile()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "sharedConfigurationFile", SHARED_CONFIGURATION_FILE);
        return SHARED_CONFIGURATION_FILE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getStateFile()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "stateFile", STATE_FILE);
        return STATE_FILE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Substitutions getSubstitutions()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option", "substitutions");
        return SUBSTITUTIONS;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getSummary()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "summary", SUMMARY);
        return SUMMARY;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getSummaryFile()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "summaryFile", SUMMARY_FILE);
        return SUMMARY_FILE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "verbosity", VERBOSITY);
        return VERBOSITY;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion()
        throws DataAccessException, IllegalPropertyException {
        logger.trace(DEFAULT, "Retrieving the default '{}' configuration option: '{}'", "version", VERSION);
        return VERSION;
    }
}
