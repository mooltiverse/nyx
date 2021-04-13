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

import com.mooltiverse.oss.nyx.data.Block;
import com.mooltiverse.oss.nyx.data.CommitMessageConventions;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.state.State;

/**
 * This interface models the configuration root block, with global options.
 */
public interface Root extends Block {
    /**
     * Returns the version identifier to bump as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public String getBump()
        throws DataAccessException, IllegalPropertyException;
    
    /**
     * Returns the commit message convention configuration block.
     * 
     * @return the commit message convention configuration block. Never {@code null}.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public CommitMessageConventions getCommitMessageConventions()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the directory to use as the working directory as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     * 
     * @see State#getDirectory()
     */
    public File getDirectory()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the initial version defined by this configuration to use when no past version is available in the commit history.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public String getInitialVersion()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the value of the dry run flag as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public Boolean getDryRun()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the prefix to use in release name generation as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public String getReleasePrefix()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
     * as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public Boolean getReleaseLenient()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the versioning scheme to use as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the path to the file where the Nyx {@link State} must be saved as it's defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public String getStateFile()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the logging verbosity level as it's defined by this configuration.
     * 
     * Please note that the verbosity option is actually ignored in this library implementation as the event filtering based
     * on the verbosity needs to be configured outside this library, depending on the logging framework deployed along with SLF4J.
     * See <a href="http://www.slf4j.org/manual.html#swapping">here</a> for more.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public Verbosity getVerbosity()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the version defined by this configuration.
     * 
     * @return the configured value for this option or {@code null} if the value hasn't been defined.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public String getVersion()
        throws DataAccessException, IllegalPropertyException;
}
