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

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.Configuration;

/**
 * The State class holds a number of attributes resulting from the execution of one or more command and so represents
 * the current status of a release process at a certain point in time.
 * 
 * Each command updates the state object with new or modified attributes so the same state instance is shared among
 * all commands.
 * 
 * There must be only one instance of this class for every execution and it's retrieved by {@link Nyx#state()}.
 */
public class State {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(State.class);

    /**
     * The private immutable instance of the configuration.
     */
    private final Configuration configuration;
    
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
    }

    /**
     * Returns the configuration object. The configuration is a live reference.
     * 
     * @return the configuration object.
     */
    public Configuration getConfiguration() {
        return configuration;
    }
}