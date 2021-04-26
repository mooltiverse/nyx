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
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.data.Block;
import com.mooltiverse.oss.nyx.data.ReleaseScope;
import com.mooltiverse.oss.nyx.data.Scheme;

/**
 * This interface models the state root block, with global attributes.
 */
public interface Root extends Block {
    /**
     * Returns the version identifier bumped on the previous release to produce the new release, if any.
     * This value is only available after {@link Nyx#infer()} has run.
     * 
     * @return the version identifier bumped on the previous release to produce the new release, if any.
     * It may be {@code null} if no identifier has been bumped (i.e. because no significant changes have
     * been detected in the release scope or because inference was inhibited by values overridden by user).
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see #getReleaseScope()
     * @see ReleaseScope#getSignificant()
     */
    public String getBump()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the configuration object. The configuration is a live reference.
     * 
     * @return the configuration object.
     */
    public Configuration getConfiguration();

    /**
     * Returns the directory used as the working directory as it's defined by the configuration.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see Configuration#getDirectory()
     */
    public File getDirectory()
        throws DataAccessException, IllegalPropertyException;

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
     * Returns {@code true} if the version ({@link #getVersion()}) is different than the previous version
     * ({@link #getReleaseScope()}{@link ReleaseScope#getPreviousVersion()}) and a new release has to be
     * published on the new version.
     * 
     * @return {@code true} if the version is new and has to be released.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see #getVersion()
     * @see #getReleaseScope()
     * @see ReleaseScope#getPreviousVersion()
     * @see Configuration#getVersion()
     * @see Nyx#infer()
     * @see Infer
     */
    public boolean getNewRelease()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns {@code true} if the version ({@link #getVersion()}) is different than the previous version
     * ({@link #getReleaseScope()}{@link ReleaseScope#getPreviousVersion()}).
     * 
     * @return {@code true} if the version is new.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see #getVersion()
     * @see #getReleaseScope()
     * @see ReleaseScope#getPreviousVersion()
     * @see Configuration#getVersion()
     * @see Nyx#infer()
     * @see Infer
     */
    public boolean getNewVersion()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the object modelling the attributes defining the scope of the release.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public ReleaseScope getReleaseScope()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the versioning scheme used as it's defined by the configuration.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the current timestamp.
     * 
     * @return the current timestamp.
     */
    public Long getTimestamp();

    /**
     * Returns the version inferred by Nyx, if any. If the version was overridden by configuration this will be the
     * same as {@link Configuration#getVersion()}. This value is only available after {@link Nyx#infer()} has run.
     * <br>
     * The returned version also has the configured prefix, if any.
     * 
     * @return the current version inferred by Nyx. This is {@code null} until {@link Nyx#infer()} has run.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see #getNewRelease()
     * @see #getNewVersion()
     * @see Configuration#getVersion()
     * @see Nyx#infer()
     * @see Infer
     */
    public String getVersion()
        throws DataAccessException, IllegalPropertyException;
}
