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

import java.util.Objects;

import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.ConfigurationException;

/**
 * The Nyx entry point and main class.
 */
public class Nyx {
    /**
     * The internal configuration object.
     * 
     * This object is lazily initialized by {@link #configuration()}.
     */
    private Configuration configuration = null;

    /**
     * Default constructor hidden on purpose.
     */
    private Nyx() {
        super();
    }

    /**
     * Returns a new instance of this class.
     * 
     * @return a new instance of this class.
     */
    public static Nyx newInstance() {
        return new Nyx();
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
            configuration = Configuration.initial();
        }
        return configuration;
    }

    /**
     * TODO: write the docs here
     */
    public void amend() {
        // TODO: implement this method
        System.out.println(">>> Nyx.amend() invoked on backing instance "+this);System.out.flush(); // This is just a smoke detection output
    }

    /**
     * TODO: write the docs here
     */
    public void clean() {
        // TODO: implement this method
        System.out.println(">>> Nyx.clean() invoked on backing instance "+this);System.out.flush(); // This is just a smoke detection output
    }

    /**
     * TODO: write the docs here
     */
    public void infer() {
        // TODO: implement this method
        System.out.println(">>> Nyx.infer() invoked on backing instance "+this);System.out.flush(); // This is just a smoke detection output
    }

    /**
     * TODO: write the docs here
     */
    public void make() {
        // TODO: implement this method
        System.out.println(">>> Nyx.make() invoked on backing instance "+this);System.out.flush(); // This is just a smoke detection output
    }

    /**
     * TODO: write the docs here
     */
    public void publish() {
        // TODO: implement this method
        System.out.println(">>> Nyx.publish() invoked on backing instance "+this);System.out.flush(); // This is just a smoke detection output
    }
}