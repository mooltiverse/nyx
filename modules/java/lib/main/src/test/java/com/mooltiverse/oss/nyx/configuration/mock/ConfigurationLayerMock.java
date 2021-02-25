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

import com.mooltiverse.oss.nyx.configuration.ConfigurationLayer;
import com.mooltiverse.oss.nyx.configuration.Scheme;
import com.mooltiverse.oss.nyx.configuration.Verbosity;
import com.mooltiverse.oss.nyx.version.SemanticVersion;
import com.mooltiverse.oss.nyx.version.Version;

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
    public static final Verbosity VERBOSITY = Verbosity.TRACE;

    /**
     * The value returned by this mock object.
     */
    public static final Version VERSION = SemanticVersion.valueOf("11.12.13");

    /**
     * Default constructor
     */
    public ConfigurationLayerMock() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getBump() {
        return BUMP;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory() {
        return DIRECTORY;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return DRY_RUN;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return RELEASE_PREFIX;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleasePrefixLenient() {
        return RELEASE_PREFIX_LENIENT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme() {
        return Scheme.SEMVER;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity(){
        return VERBOSITY;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getVersion(){
        return VERSION;
    }
}
