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

import java.io.File;
import java.util.Map;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.Scheme;
import com.mooltiverse.oss.nyx.version.Version;

/**
 * This interface models the root state block, with global options.
 */
public interface Root extends Block {
    /**
     * Returns the configuration object. The configuration is a live reference.
     * 
     * @return the configuration object.
     */
    public Configuration getConfiguration();

    /**
     * Returns the directory used as the working directory as it's defined by the configuration.
     * 
     * @return the current value for this option.
     * 
     * @throws StateException in case the attribute has incorrect values or it can't be resolved.
     * 
     * @see Configuration#getDirectory()
     */
    public File getDirectory()
        throws StateException;

    /**
     * Returns the live map of internal attributes.
     * 
     * Internal attributes are not documented so they must not be used by users as the implementation may change
     * them at any time. Commands and other implementation objects are free to store and remove their own
     * attributes here (i.e. for caching or store their internal state).
     * 
     * When handling these attributes, entities must make sure the names (keys) do not overlap, unless for
     * shared attributes.
     * 
     * This object takes no control over the values stored in this map.
     * 
     * Sensitive informations must not be stored here as they would be exposed when marshalling the attributes
     * to files.
     * 
     * @return the live map of internal attributes. The returned map is never {@code null}
     */
    public Map<String, String> getInternals();

    /**
     * Returns the versioning scheme used as it's defined by this configuration.
     * 
     * @return the current value for this option.
     * 
     * @throws StateException in case the attribute has incorrect values or it can't be resolved.
     */
    public Scheme getScheme()
        throws StateException;

    /**
     * Returns the current timestamp.
     * 
     * @return the current timestamp.
     */
    public Long getTimestamp();

    /**
     * Returns the version inferred by Nyx, if any. If the version was overridden by configuration this will be the
     * same as {@link Configuration#getVersion()}. This value is only available after {@link Nyx#infer()} has run.
     * 
     * @return the current version inferred by Nyx. This is {@code null} until {@link Nyx#infer()} has run.
     * 
     * @throws StateException in case the attribute has incorrect values or it can't be resolved.
     * 
     * @see Configuration#getVersion()
     * @see Nyx#infer()
     * @see Infer
     */
    public Version getVersion()
        throws StateException;
}
