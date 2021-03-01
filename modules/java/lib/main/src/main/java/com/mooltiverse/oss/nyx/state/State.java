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
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.version.Version;

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
     * The map containing the internal attributes.
     */
    private final Map<String, String> internals = new HashMap<String, String>();

    /**
     * The private immutable instance of the configuration.
     */
    private final Configuration configuration;

    /**
     * The latest timestamp that was taken.
     */
    private Long timestamp = Long.valueOf(System.currentTimeMillis());

    /**
     * The version that has been inferred.
     */
    private Version version = null;
    
    /**
     * Standard constructor.
     * 
     * @param configuration the configuration object held by this state
     * 
     * @throws NullPointerException if the given argument is {@code null}
     */
    public State(Configuration configuration) {
        super();
        Objects.requireNonNull(configuration);
        this.configuration = configuration;
        logger.debug(STATE, "New state object");

        // TODO: add the logic to load a previous state file, with special regard to the future 'resume' option (https://github.com/mooltiverse/nyx/issues/42)

        // restoring from file gets the timestamp from previous runs, so we need to update it
        touchTimestamp();
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
     * {@inheritDoc}
     */
    @Override
    public Version getVersion() 
        throws DataAccessException, IllegalPropertyException {
        return version;
    }

    /**
     * Sets the version attribute.
     * 
     * @param version the version attribute to set for this state.
     * 
     * @see #getVersion()
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see Version#getScheme()
     * @see Configuration#getScheme()
     */
    public void setVersion(Version version)
        throws DataAccessException, IllegalPropertyException {
        if (!getConfiguration().getScheme().getScheme().equals(version.getScheme())) {
            throw new IllegalPropertyException(String.format("The given version %s scheme %s (%s) does not match the configured scheme %s", version.toString(), version.getScheme(), Scheme.from(version.getScheme()), getConfiguration().getScheme()));
        }
        
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