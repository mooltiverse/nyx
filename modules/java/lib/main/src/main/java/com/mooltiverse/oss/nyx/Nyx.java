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
package com.mooltiverse.oss.nyx;

import static com.mooltiverse.oss.nyx.log.Markers.MAIN;

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.command.Amend;
import com.mooltiverse.oss.nyx.command.Clean;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.command.Make;
import com.mooltiverse.oss.nyx.command.Publish;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.ConfigurationException;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.state.StateException;

/**
 * The Nyx entry point and main class.
 */
public class Nyx {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Nyx.class);

    /**
     * The internal configuration object.
     * 
     * This object is lazily initialized so in order to make sure you get a valid reference you should always use
     * {@link #configuration()} instead of reading this member.
     * 
     * @see #configuration()
     */
    private Configuration configuration = null;

    /**
     * The internal state object.
     * 
     * This object is lazily initialized so in order to make sure you get a valid reference you should always use
     * {@link #state()} instead of reading this member.
     * 
     * @see #state()
     */
    private State state = null;

    /**
     * Default constructor.
     */
    public Nyx() {
        super();
        logger.trace(MAIN, "New Nyx instance");
    }

    /**
     * Returns the configuration.
     * 
     * @return the configuration
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     */
    public synchronized Configuration configuration()
        throws ConfigurationException {
        if (Objects.isNull(configuration)) {
            logger.debug(MAIN, "Instantiating the initial configuration");
            configuration = new Configuration();
        }
        return configuration;
    }

    /**
     * Returns the state.
     * 
     * @return the state
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public synchronized State state()
        throws ConfigurationException, StateException {
        if (Objects.isNull(state)) {
            logger.debug(MAIN, "Instantiating the initial state");
            state = new State(configuration());
        }
        return state;
    }

    /**
     * TODO: write the docs here
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public State amend()
        throws ConfigurationException, StateException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.amend()");
        return new Amend(state()).run();
    }

    /**
     * TODO: write the docs here
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public void clean()
        throws ConfigurationException, StateException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.clean()");
    }

    /**
     * TODO: write the docs here
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public State infer()
        throws ConfigurationException, StateException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.infer()");
        return null;
    }

    /**
     * TODO: write the docs here
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public State make()
        throws ConfigurationException, StateException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.make()");
        return null;
    }

    /**
     * TODO: write the docs here
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public State publish()
        throws ConfigurationException, StateException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.publish()");
        return null;
    }
}