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
package com.mooltiverse.oss.nyx.state;

import static com.mooltiverse.oss.nyx.log.Markers.STATE;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.ReleaseScope;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.template.Templates;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The State class holds a number of attributes resulting from the execution of one or more command and so represents
 * the current status of a release process at a certain point in time.
 * 
 * Each command updates the state object with new or modified attributes so the same state instance is shared among
 * all commands.
 * 
 * There must be only one instance of this class for every execution and it's retrieved by {@link Nyx#state()}.
 */
public class State implements Root {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(State.class);

    /**
     * The current Git branch name.
     */
    private String branch = null;

    /**
     * The identifier to bump.
     */
    private String bump = null;

    /**
     * The private immutable instance of the configuration.
     */
    private Configuration configuration;

    /**
     * The map containing the internal attributes.
     */
    private  Map<String, String> internals = new HashMap<String, String>();

    /**
     * The private immutable instance of the release scope.
     */
    private ReleaseScope releaseScope = new ReleaseScope();

    /**
     * The private instance of the release type.
     */
    private ReleaseType releaseType = null;

    /**
     * The latest timestamp that was taken.
     */
    private Long timestamp = Long.valueOf(System.currentTimeMillis());

    /**
     * The version that has been inferred.
     */
    private String version = null;

    /**
     * The regular expression used to check the version against a range constraint.
     */
    private String versionRange = null;

    /**
     * Default constructor. <b>DO NOT USE THIS CONSTRUCTOR AS IT EXISTS ONLY FOR INTERNAL USE WHEN UNMARSHALLING</b>
     */
    @Deprecated
    public State() {
        super();
    }
    
    /**
     * Standard constructor.
     * 
     * @param configuration the configuration object held by this state
     * 
     * @throws NullPointerException if the given argument is {@code null}
     * 
     * @throws DataAccessException in case the state file is configured but cannot be read or accessed when resuming
     * from a previously saved state.
     * @throws IllegalPropertyException in case the state file is configured but has incorrect values or it can't be
     * resolved when resuming from a previously saved state
     */
    public State(Configuration configuration)
        throws DataAccessException, IllegalPropertyException {
        super();
        Objects.requireNonNull(configuration);
        this.configuration = configuration;
        logger.debug(STATE, "New state object");

        // initialize the timestamp
        touchTimestamp();
    }

    /**
     * Loads the state attributes from a previously saved state file. Note that not all attributes are loaded from the state file
     * as many of them will still be retrieved live from the configuration.
     * 
     * @param stateFile the file to load the state from
     * @param configuration the configuration object to read values from. This object will be set as the {@link #getConfiguration()}
     * of the newly instantiated object too.
     * 
     * @return the new state object deserialized from the given state file
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     */
    public static State resume(File stateFile, Configuration configuration) 
        throws DataAccessException, IllegalPropertyException {
        State state = FileMapper.load(stateFile, State.class);
        state.configuration = configuration;
        return state;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBranch()
        throws DataAccessException, IllegalPropertyException {
        return branch;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} branch name.
     * 
     * @return {@code true} if the scope has a non {@code null} branch name.
     */
    public boolean hasBranch() {
        return !Objects.isNull(branch);
    }

    /**
     * Sets the current Git branch name
     * 
     * @param branch the current Git branch name
     */
    public void setBranch(String branch) {
        this.branch = branch;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump()
        throws DataAccessException, IllegalPropertyException {
        return Objects.isNull(getConfiguration().getBump()) ? bump : getConfiguration().getBump();
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version identifier
     * to bump or bumped on the previous release to produce the new release.
     * 
     * @return {@code true} if the scope has a non {@code null} version identifier
     * to bump or bumped on the previous release to produce the new release.
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     */
    public boolean hasBump()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getBump());
    }

    /**
     * Sets the identifier to bump.
     * <br>
     * Since this option can be overridden by configuration this method can only be invoked when the
     * {@link #getConfiguration() configuration} doesn't already have a {@link Configuration#getBump() bump}
     * attribute otherwise an {@link IllegalStateException} is thrown.
     * 
     * @param bump the identifier to bump
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     * @throws IllegalStateException if the {@link #getConfiguration() configuration} has a value for the
     * {@link Configuration#getBump() bump} attribute.
     * 
     * @see Configuration#getBump()
     */
    public void setBump(String bump)
        throws DataAccessException, IllegalPropertyException, IllegalStateException {
        if (Objects.isNull(getConfiguration().getBump()))
            this.bump = bump;
        else throw new IllegalStateException(String.format("The state bump attribute can't be set when it's ovverridden by the configuration. Configuration bump attribute is '%s'", getConfiguration().getBump()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Configuration getConfiguration() {
        return configuration;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory()
        throws DataAccessException, IllegalPropertyException {
        return new File(getConfiguration().getDirectory());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getInternals() {
        return internals;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getNewRelease()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(releaseType))
            return Boolean.FALSE;
        try {
            return Boolean.valueOf(getNewVersion() && Templates.toBoolean(Templates.render(releaseType.getPublish(), this)));
        }
        catch (IOException ioe) {
            throw new IllegalPropertyException(String.format("Unable to render the template '%s' specified for the publish option in the release type", releaseType.getPublish()), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getNewVersion()
        throws DataAccessException, IllegalPropertyException {
        if (hasVersion()) {
            if (getVersion().equals(getReleaseScope().getPreviousVersion()))
                return false;
            else {
                if (hasReleaseType() && getReleaseType().getCollapseVersions()) {
                    if (getVersion().equals(getReleaseScope().getPrimeVersion())) {
                        return false;
                    }
                    else return true;
                }
                else return true;
            }
        }
        else return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ReleaseScope getReleaseScope() {
        return releaseScope;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ReleaseType getReleaseType() {
        return releaseType;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} release type set.
     * 
     * @return {@code true} if the scope has a non {@code null} release type set.
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     */
    public boolean hasReleaseType()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getReleaseType());
    }

    /**
     * Sets the selected release type.
     * 
     * @param releaseType the selected release type.
     */
    public void setReleaseType(ReleaseType releaseType) {
        this.releaseType = releaseType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException {
        return getConfiguration().getScheme();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Long getTimestamp() {
        return timestamp;
    }

    /**
     * Sets the state timestamp.
     * 
     * @param timestamp the state timestamp.
     */
    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion() 
        throws DataAccessException, IllegalPropertyException {
        return Objects.isNull(getConfiguration().getVersion()) ? version : getConfiguration().getVersion();
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version.
     * 
     * @return {@code true} if the scope has a non {@code null} version.
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     */
    public boolean hasVersion()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getVersion());
    }

    /**
     * Sets the version inferred by Nyx.
     * <br>
     * Since this option can be overridden by configuration this method can only be invoked when the
     * {@link #getConfiguration() configuration} doesn't already have a {@link Configuration#getVersion() version}
     * attribute otherwise an {@link IllegalStateException} is thrown.
     * 
     * @param version the version inferred by Nyx.
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     * @throws IllegalStateException if the {@link #getConfiguration() configuration} has a value for the
     * {@link Configuration#getVersion() version} attribute.
     * 
     * @see Configuration#getVersion()
     */
    public void setVersion(String version)
        throws DataAccessException, IllegalPropertyException, IllegalStateException {
        if (Objects.isNull(getConfiguration().getVersion()))
            this.version = version;
        else throw new IllegalStateException(String.format("The state version attribute can't be set when it's ovverridden by the configuration. Configuration version attribute is '%s'", getConfiguration().getVersion()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersionRange()
        throws DataAccessException, IllegalPropertyException {
        return versionRange;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version range.
     * 
     * @return {@code true} if the scope has a non {@code null} version range.
     */
    public boolean hasVersionRange() {
        return !Objects.isNull(versionRange);
    }

    /**
     * Sets the regular expression used to check the version against a range constraint.
     * 
     * @param versionRange the regular expression used to check the version against a range constraint.
     */
    public void setVersionRange(String versionRange) {
        this.versionRange = versionRange;
    }

    /**
     * Updates the current timestamp and returns the updated value.
     * 
     * @return the updated timestamp.
     * 
     * @see #getTimestamp()
     */
    public Long touchTimestamp() {
        timestamp = Long.valueOf(System.currentTimeMillis());
        return timestamp;
    }
}