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

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.ConfigurationException;

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
     * This object is lazily initialized by {@link #configuration()}.
     */
    private Configuration configuration = null;

    /**
     * Default constructor hidden on purpose.
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
     * TODO: write the docs here
     */
    public void amend() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.amend()");
    }

    /**
     * TODO: write the docs here
     */
    public void clean() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.clean()");
    }

    /**
     * TODO: write the docs here
     */
    public void infer() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.infer()");
    }

    /**
     * TODO: write the docs here
     */
    public void make() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.make()");
    }

    /**
     * TODO: write the docs here
     */
    public void publish() {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(MAIN, "Nyx.publish()");
    }
}