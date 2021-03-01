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
package com.mooltiverse.oss.nyx.configuration;

import java.io.File;

import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.version.Version;

/**
 * A utility interface that collects default configuration values.
 */
public interface Defaults {
    /**
     * The default version identifier to bump. Value: {@code null}
     */
    public static final String BUMP = null;

    /**
     * The default working directory. Defaults to the current user directory returned by reading the
     * {@code user.dir} from {@link System#getProperty(String)}
     */
    public static final File DIRECTORY = new File(System.getProperty("user.dir"));

    /**
     * The flag that prevents to alter any repository state and instead just log the actions that would be taken. Value: {@code false}
     */
    public static final Boolean DRY_RUN = Boolean.FALSE;

    /**
     * The default prefix to add at the beginning of a version identifier to generate the release identifier. Value: {@value}
     */
    public static final String RELEASE_PREFIX = "v";

    /**
     * The flag that alows reading releases from the history tolerating arbitrary prefixes. Value: {@code true}
     */
    public static final Boolean RELEASE_PREFIX_LENIENT = Boolean.TRUE;

    /**
     * The versioning scheme to use. Value: {@link Scheme#SEMVER}
     */
    public static final Scheme SCHEME = Scheme.SEMVER;

    /**
     * The logging level. Value: {@link Verbosity#WARNING}.
     * 
     * Please note that the verbosity option is actually ignored in this library implementation as the event filtering based
     * on the verbosity needs to be configured outside this library, depending on the logging framework deployed along with SLF4J.
     * See <a href="http://www.slf4j.org/manual.html#swapping">here</a> for more.
     */
    public static final Verbosity VERBOSITY = Verbosity.WARNING;

    /**
     * The release version. Value: {@code null}
     */
    public static final Version VERSION = null;
}
