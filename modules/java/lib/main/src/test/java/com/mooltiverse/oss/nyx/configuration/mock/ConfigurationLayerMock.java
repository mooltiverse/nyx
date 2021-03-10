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
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.SemanticVersion;
import com.mooltiverse.oss.nyx.version.Version;

/**
 * A fake configuration layer, with (possibly) unique default values that allow the tests to distinguish
 * when values come from this object or others. Baseline is the default values as this class tries
 * to have all of its values different from that, allowing tests to compare they are not equal.
 * 
 * Values can also be set for more complex tests. Please note that Nyx doesn't resolve values twice
 * so you should set values only before you pass this object to Nyx.
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
    public static final Version INITIAL_VERSION = SemanticVersion.valueOf("0.9.9");

    /**
     * The value returned by this mock object.
     */
    public static final String RELEASE_PREFIX = "mock";

    /**
     * The value returned by this mock object.
     */
    public static final Boolean RELEASE_LENIENT = Boolean.FALSE;

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
     * The value returned by this mock object.
     */
    public String bump = BUMP;

    /**
     * The value returned by this mock object. This is an abstract path and does not exists on the file system.
     */
    public File directory = DIRECTORY;

    /**
     * The value returned by this mock object.
     */
    public Boolean dryRun = DRY_RUN;

    /**
     * The value returned by this mock object.
     */
    public Version initialVersion = INITIAL_VERSION;

    /**
     * The value returned by this mock object.
     */
    public String releasePrefix = RELEASE_PREFIX;

    /**
     * The value returned by this mock object.
     */
    public Boolean releaseLenient = RELEASE_LENIENT;

    /**
     * The value returned by this mock object.
     */
    public Scheme scheme = SCHEME;

    /**
     * The value returned by this mock object.
     */
    public Verbosity verbosity = VERBOSITY;

    /**
     * The value returned by this mock object.
     */
    public Version version = VERSION;

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
        return bump;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getDirectory() {
        return directory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getDryRun() {
        return dryRun;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getInitialVersion(){
        return initialVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getReleasePrefix() {
        return releasePrefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Boolean getReleaseLenient() {
        return releaseLenient;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Scheme getScheme() {
        return scheme;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Verbosity getVerbosity(){
        return verbosity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Version getVersion(){
        return version;
    }
}
