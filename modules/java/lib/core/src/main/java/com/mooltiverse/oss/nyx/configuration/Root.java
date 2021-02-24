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

import com.mooltiverse.oss.nyx.version.Version;

/**
 * This interface models the root configuration block, with globl options.
 */
public interface Root extends Block {
    /**
     * Returns the version identifier to bump as it's defined by this configuration.
     * 
     * @return the version identifier to bump as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public String getBump()
        throws ConfigurationException;

    /**
     * Returns the directory to use as the working directory as it's defined by this configuration.
     * 
     * @return the directory to use as the working directory as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public File getDirectory()
        throws ConfigurationException;

    /**
     * Returns the value of the dry run flag as it's defined by this configuration.
     * 
     * @return the value of the dry run flag as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public Boolean getDryRun()
        throws ConfigurationException;

    /**
     * Returns the prefix to use in release name generation as it's defined by this configuration.
     * 
     * @return the prefix to use in release name generation as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public String getReleasePrefix()
        throws ConfigurationException;

    /**
     * Returns the flag that enables tolerance in reading release names with arbitrary prefixes
     * as it's defined by this configuration.
     * 
     * @return the flag that enables tolerance in reading release names with arbitrary prefixes
     * as it's defined by this configuration or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public Boolean getReleasePrefixLenient()
        throws ConfigurationException;

    /**
     * Returns the versioning scheme to use as it's defined by this configuration.
     * 
     * @return the versioning scheme to use as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public Scheme getScheme()
        throws ConfigurationException;

    /**
     * Returns the logging verbosity level as it's defined by this configuration.
     * 
     * Please note that the verbosity option is actually ignored in this library implementation as the event filtering based
     * on the verbosity needs to be configured outside this library, depending on the logging framework deployed along with SLF4J.
     * See <a href="http://www.slf4j.org/manual.html#swapping">here</a> for more.
     * 
     * @return the logging verbosity level as it's defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public Verbosity getVerbosity()
        throws ConfigurationException;

    /**
     * Returns the version defined by this configuration.
     * 
     * @return the version defined by this configuration
     * or {@code null} if the value hasn't been defined.
     * 
     * @throws ConfigurationException in case the option has been defined but has
     * incorrect values or it can't be resolved.
     */
    public Version getVersion()
        throws ConfigurationException;
}
