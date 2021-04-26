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
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.FileMapper;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.ReleaseScope;
import com.mooltiverse.oss.nyx.data.Scheme;

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
     * The version identifier bumped on the previous release to produce the new release, if any.
     */
    private String bump = null;

    /**
     * The private immutable instance of the configuration.
     */
    private Configuration configuration;

    /**
     * The map containing the internal attributes.
     */
    private Map<String, String> internals = new HashMap<String, String>();

    /**
     * The private immutable instance of the release scope.
     */
    private ReleaseScope releaseScope = new ReleaseScope();

    /**
     * The latest timestamp that was taken.
     */
    private Long timestamp = Long.valueOf(System.currentTimeMillis());

    /**
     * The version that has been inferred.
     */
    private String version = null;

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
    public String getBump()
        throws DataAccessException, IllegalPropertyException {
        return bump;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version identifier bumped on the previous release to produce the new release.
     * 
     * @return {@code true} if the scope has a non {@code null} version identifier bumped on the previous release to produce the new release.
     */
    public boolean hasBump() {
        return !Objects.isNull(bump);
    }

    /**
     * Sets the version identifier bumped on the previous release to produce the new release, if any.
     * 
     * @param bump the version identifier bumped on the previous release to produce the new release, if any.
     * It may be {@code null}.
     * 
     * @see #getBump()
     */
    public void setBump(String bump) {
        this.bump = bump;
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
        return getConfiguration().getDirectory();
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
    public boolean getNewRelease()
        throws DataAccessException, IllegalPropertyException {
        // TODO: Also check the releaseType attribute telling if the release has to be published
        // (when available), and include it into the AND here
        return getNewVersion();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean getNewVersion()
        throws DataAccessException, IllegalPropertyException {
        return hasVersion() && !getVersion().equals(getReleaseScope().getPreviousVersion());
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
     * Returns the version inferred by Nyx, if any. If the version was overridden by configuration this will be the
     * same as {@link Configuration#getVersion()}. This value is only available after {@link Nyx#infer()} has run.
     * 
     * @return the current version inferred by Nyx. This is {@code null} until {@link Nyx#infer()} has run.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see Configuration#getVersion()
     * @see Nyx#infer()
     * @see Infer
     */
    @Override
    public String getVersion() 
        throws DataAccessException, IllegalPropertyException {
        return version;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version.
     * 
     * @return {@code true} if the scope has a non {@code null} version.
     */
    public boolean hasVersion() {
        return !Objects.isNull(version);
    }

    /**
     * Sets the version inferred by Nyx.
     * 
     * @param version the version inferred by Nyx.
     */
    public void setVersion(String version) {
        this.version = version;
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