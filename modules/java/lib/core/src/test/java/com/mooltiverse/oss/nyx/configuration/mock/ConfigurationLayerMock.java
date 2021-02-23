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
package com.mooltiverse.oss.nyx.configuration.mock;

import java.io.File;

import org.slf4j.event.Level;

import com.mooltiverse.oss.nyx.configuration.ConfigurationLayer;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A fake configuration layer, with (possibly) unique values that allow the tests to distinguish
 * when values come from this object or others. Baseline is the default values as this class tries
 * to have all of its values different from that, allowing tests to compare they are not equal.
 */
public class ConfigurationLayerMock implements ConfigurationLayer {
    /**
     * The value returned by this mock object.
     */
    public static final String BUMP = "mockIdentifier";

    /**
     * The value returned by this mock object. This is an abstract path and does not exists on the file system.
     */
    public static final File DIRECTORY = new File(System.getProperty("java.io.tmpdir"), "mockDirectory");

    /**
     * The value returned by this mock object.
     */
    public static final Boolean DRY_RUN = Boolean.TRUE;

    /**
     * The value returned by this mock object.
     */
    public static final String RELEASE_PREFIX = "mock";

    /**
     * The value returned by this mock object.
     */
    public static final Boolean RELEASE_PREFIX_LENIENT = Boolean.FALSE;

    /**
     * The value returned by this mock object.
     */
    public static final Scheme SCHEME = Scheme.SEMVER;

    /**
     * The value returned by this mock object.
     */
    public static final Level VERBOSITY = Level.TRACE;

    /**
     * Default constructor
     */
    public ConfigurationLayerMock() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    public String getBump() {
        return BUMP;
    }

    /**
     * {@inheritDoc}
     */
    public File getDirectory() {
        return DIRECTORY;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getDryRun() {
        return DRY_RUN;
    }

    /**
     * {@inheritDoc}
     */
    public String getReleasePrefix() {
        return RELEASE_PREFIX;
    }

    /**
     * {@inheritDoc}
     */
    public Boolean getReleasePrefixLenient() {
        return RELEASE_PREFIX_LENIENT;
    }

    /**
     * {@inheritDoc}
     */
    public Scheme getScheme() {
        return Scheme.SEMVER;
    }

    /**
     * {@inheritDoc}
     */
    public Level getVerbosity(){
        return VERBOSITY;
    }
}
