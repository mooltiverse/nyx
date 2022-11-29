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

import static com.mooltiverse.oss.nyx.log.Markers.STATE;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.Nyx;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.entities.Changelog;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.entities.ReleaseScope;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.FileMapper;
import com.mooltiverse.oss.nyx.template.Templates;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * The State class holds a number of attributes resulting from the execution of one or more command and so represents
 * the current status of a release process at a certain point in time.
 * <br>
 * Each command updates the state object with new or modified attributes so the same state instance is shared among
 * all commands.
 * <br>
 * There must be only one instance of this class for every execution and it's retrieved by {@link Nyx#state()}.
 */
public class State {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(State.class);

    /**
     * The current Git branch name.
     */
    private String branch = null;

    /**
     * The identifier to bump.
     */
    private String bump = null;

    /**
     * The private instance of the configuration.
     */
    private Changelog changelog = null;

    /**
     * The private instance of the configuration.
     */
    private Configuration configuration = null;

    /**
     * The map containing the internal attributes.
     */
    private  Map<String, String> internals = new HashMap<String, String>();

    /**
     * The list containing the released assets.
     */
    private List<Attachment> releaseAssets = new ArrayList<Attachment>();

    /**
     * The private instance of the release scope.
     */
    private ReleaseScope releaseScope = new ReleaseScope();

    /**
     * The private instance of the release type.
     */
    private ReleaseType releaseType = null;

    /**
     * The latest timestamp that was taken. This is initialized by default to the date and
     * time the instance of this class has been created.
     */
    private Long timestamp = Long.valueOf(System.currentTimeMillis());

    /**
     * The version that has been inferred.
     */
    private String version = null;

    /**
     * The regular expression used to check the version against a range constraint.
     */
    private String versionRange = null;

    /**
     * Default constructor. <b>DO NOT USE THIS CONSTRUCTOR AS IT EXISTS ONLY FOR INTERNAL USE WHEN UNMARSHALLING</b>
     */
    @Deprecated
    public State() {
        super();
    }
    
    /**
     * Standard constructor.
     * 
     * @param configuration the configuration object held by this state
     * 
     * @throws NullPointerException if the given argument is {@code null}
     * 
     * @throws DataAccessException in case the state file is configured but cannot be read or accessed when resuming
     * from a previously saved state.
     * @throws IllegalPropertyException in case the state file is configured but has incorrect values or it can't be
     * resolved when resuming from a previously saved state
     */
    public State(Configuration configuration)
        throws DataAccessException, IllegalPropertyException {
        super();
        Objects.requireNonNull(configuration);
        this.configuration = configuration;
        logger.trace(STATE, "New state object");
    }

    /**
     * Loads the state attributes from a previously saved state file. All attributes are loaded from the given file
     * but the nested configuration is replaced by the given one.
     * 
     * @param stateFile the file to load the state from
     * @param configuration the configuration object to use for the resumed state. This object will be set as the
     * {@link #getConfiguration() configuration} of the returned instance.
     * 
     * @return the new state object deserialized from the given state file
     * 
     * @throws DataAccessException in case the state file cannot be read or accessed.
     * @throws IllegalPropertyException in case the state file has incorrect values.
     */
    public static State resume(File stateFile, Configuration configuration) 
        throws DataAccessException, IllegalPropertyException {
        State state = FileMapper.load(stateFile, State.class);
        state.configuration = configuration;

        // The values that are overridden by the configuration must be set to null after deserialization so that
        // their getter/setter methods will consistently check if they've been defined by the configuration and return those
        // instead of local ones.
        // Consider that setter methods for those values need to be permissive because when the State is unmarshalled
        // the internal reference to the configuration is not yet set.
        if (!Objects.isNull(configuration.getBump()))
            state.bump = null;
        if (!Objects.isNull(configuration.getVersion()))
            state.version = null;

        return state;
    }

    /**
     * Returns the current Git branch name.
     * 
     * @return the current Git branch name. This is {@code null} until {@link Nyx#infer()} has run.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public String getBranch()
        throws DataAccessException, IllegalPropertyException {
        return branch;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} branch name.
     * 
     * @return {@code true} if the scope has a non {@code null} branch name.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasBranch()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getBranch());
    }

    /**
     * Sets the current Git branch name
     * 
     * @param branch the current Git branch name. It may be {@code null} to reset any previous value.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     */
    public void setBranch(String branch)
        throws DataAccessException, IllegalPropertyException {
        this.branch = branch;
    }

    /**
     * Returns the version identifier to bump or bumped on the previous release to produce the new release, if any.
     * This value is only available after {@link Nyx#infer()} has run unless it's overridden by the configuration,
     * in which case the configuration value is returned.
     * 
     * @return the version identifier to bump or bumped on the previous release to produce the new release, if any.
     * It may be {@code null} if no identifier has been bumped (i.e. because no significant changes have
     * been detected in the release scope or because inference was inhibited by values overridden by user) and the
     * configuration does not override this value.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see Configuration#getBump()
     */
    public String getBump()
        throws DataAccessException, IllegalPropertyException {
        return Objects.isNull(getConfiguration().getBump()) ? bump : getConfiguration().getBump();
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} bump identifier
     * to bump or bumped on the previous release to produce the new release.
     * 
     * @return {@code true} if the scope has a non {@code null} bump identifier
     * to bump or bumped on the previous release to produce the new release.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasBump()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getBump());
    }

    /**
     * Sets the identifier to bump.
     * <br>
     * Since this option can be overridden by configuration this method can only be invoked when the
     * {@link #getConfiguration() configuration} doesn't already have a {@link Configuration#getBump() bump}
     * attribute otherwise an {@link IllegalStateException} is thrown.
     * 
     * @param bump the identifier to bump
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     * @throws IllegalStateException if the {@link #getConfiguration() configuration} already has a value for the
     * {@link Configuration#getBump() bump} attribute.
     */
    public void setBump(String bump)
        throws DataAccessException, IllegalPropertyException, IllegalStateException {
        // We need to be permissive here as when this method is called by the unmarshaller the configuration object is not set yet
        // but on the other hand the 'resume' method will handle this situation right after unmarshalling by checking if the
        // configuration overrides this value and, if so, set the local reference to null.
        // This causes a temporary inconsistency until the unmarshalling is finished, but this has no consequences
        if (Objects.isNull(getConfiguration()) || Objects.isNull(getConfiguration().getBump()))
            this.bump = bump;
        else throw new IllegalStateException(String.format("The state bump attribute can't be set when it's ovverridden by the configuration. Configuration bump attribute is '%s'", getConfiguration().getBump()));
    }

    /**
     * Returns the current changelog data model.
     * 
     * @return the current changelog data model. This is {@code null} until {@link Nyx#make()} has run.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public Changelog getChangelog()
        throws DataAccessException, IllegalPropertyException {
        return changelog;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} changelog data model.
     * 
     * @return {@code true} if the scope has a non {@code null} changelog data model.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasChangelog()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getChangelog());
    }

    /**
     * Sets the changelog data model
     * 
     * @param changelog the current changelog data model. It may be {@code null} to reset any previous value.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     */
    public void setChangelog(Changelog changelog)
        throws DataAccessException, IllegalPropertyException {
        this.changelog = changelog;
    }

    /**
     * Returns the configuration object. The configuration is a live reference.
     * 
     * @return the configuration object.
     */
    public Configuration getConfiguration() {
        return configuration;
    }

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
        throws DataAccessException, IllegalPropertyException {
        return new File(getConfiguration().getDirectory());
    }

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
    public Map<String, String> getInternals() {
        return internals;
    }

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
     */
    public Boolean getNewRelease()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(releaseType))
            return Boolean.FALSE;
        try {
            return Boolean.valueOf(getNewVersion() && Templates.toBoolean(Templates.render(releaseType.getPublish(), this)));
        }
        catch (IOException ioe) {
            throw new IllegalPropertyException(String.format("Unable to render the template '%s' specified for the publish option in the release type", releaseType.getPublish()), ioe);
        }
    }

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
     */
    public Boolean getNewVersion()
        throws DataAccessException, IllegalPropertyException {
        if (hasVersion()) {
            if (getVersion().equals(getReleaseScope().getPreviousVersion()))
                return false;
            else {
                if (hasReleaseType() && getReleaseType().getCollapseVersions()) {
                    if (getVersion().equals(getReleaseScope().getPrimeVersion())) {
                        return false;
                    }
                    else return true;
                }
                else return true;
            }
        }
        else return false;
    }

    /**
     * Returns the list of assets published with the release. The returned object is a live collection whose contents can be changed.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public List<Attachment> getReleaseAssets()
        throws DataAccessException, IllegalPropertyException {
        return releaseAssets;
    }

    /**
     * Returns the object modelling the attributes defining the scope of the release.
     * 
     * @return the current value for this attribute.
     */
    public ReleaseScope getReleaseScope() {
        return releaseScope;
    }

    /**
     * Returns the object modelling the attributes defining the type of the release.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public ReleaseType getReleaseType()
        throws DataAccessException, IllegalPropertyException {
        return releaseType;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} release type set.
     * 
     * @return {@code true} if the scope has a non {@code null} release type set.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasReleaseType()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getReleaseType());
    }

    /**
     * Sets the selected release type.
     * 
     * @param releaseType the selected release type.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     */
    public void setReleaseType(ReleaseType releaseType)
        throws DataAccessException, IllegalPropertyException {
        this.releaseType = releaseType;
    }

    /**
     * Returns the versioning scheme used as it's defined by the configuration.
     * 
     * @return the current value for this attribute.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public Scheme getScheme()
        throws DataAccessException, IllegalPropertyException {
        return getConfiguration().getScheme();
    }

    /**
     * Returns the current timestamp.
     * 
     * @return the current timestamp.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public Long getTimestamp()
        throws DataAccessException, IllegalPropertyException {
        return timestamp;
    }

    /**
     * Sets the state timestamp.
     * 
     * @param timestamp the state timestamp.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     */
    public void setTimestamp(Long timestamp)
        throws DataAccessException, IllegalPropertyException {
        this.timestamp = timestamp;
    }

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
     */
    public String getVersion() 
        throws DataAccessException, IllegalPropertyException {
        return Objects.isNull(getConfiguration().getVersion()) ? version : getConfiguration().getVersion();
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version.
     * 
     * @return {@code true} if the scope has a non {@code null} version.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasVersion()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(getVersion());
    }

    /**
     * Sets the version inferred by Nyx.
     * <br>
     * Since this option can be overridden by configuration this method can only be invoked when the
     * {@link #getConfiguration() configuration} doesn't already have a {@link Configuration#getVersion() version}
     * attribute otherwise an {@link IllegalStateException} is thrown.
     * 
     * @param version the version inferred by Nyx.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     * @throws IllegalStateException if the {@link #getConfiguration() configuration} has a value for the
     * {@link Configuration#getVersion() version} attribute.
     * 
     * @see Configuration#getVersion()
     */
    public void setVersion(String version)
        throws DataAccessException, IllegalPropertyException, IllegalStateException {
        // We need to be permissive here as when this method is called by the unmarshaller the configuration object is not set yet
        // but on the other hand the 'resume' method will handle this situation right after unmarshalling by checking if the
        // configuration overrides this value and, if so, set the local reference to null.
        // This causes a temporary inconsistency until the unmarshalling is finished, but this has no consequences
        if (Objects.isNull(getConfiguration()) || Objects.isNull(getConfiguration().getVersion()))
            this.version = version;
        else throw new IllegalStateException(String.format("The state version attribute can't be set when it's ovverridden by the configuration. Configuration version attribute is '%s'", getConfiguration().getVersion()));
    }

    /**
     * Returns the regular expression that is used to check whether or not the {@link #getVersion() version} is within a certain
     * range. This value is only available after {@link Nyx#infer()} has run.
     * <br>
     * This attribute has a value only if {@link ReleaseType#getVersionRange() version range} or
     * {@link ReleaseType#getVersionRangeFromBranchName() version range from branch name} are enabled. If
     * {@link ReleaseType#getVersionRange() version range} has a value then this attribute has the same value, otherwise if
     * {@link ReleaseType#getVersionRangeFromBranchName() version range from branch name} is {@code true} this value has the dynamically
     * generated regular expression inferred from the branch name.
     * 
     * @return the regular expression that is used to check whether or not the {@link #getVersion() version} is within a certain
     * range. This is {@code null} until {@link Nyx#infer()} has run.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     * 
     * @see ReleaseType#getVersionRange()
     * @see ReleaseType#getVersionRangeFromBranchName()
     */
    public String getVersionRange()
        throws DataAccessException, IllegalPropertyException {
        return versionRange;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version range.
     * 
     * @return {@code true} if the scope has a non {@code null} version range.
     * 
     * @throws DataAccessException in case the attribute cannot be read or accessed.
     * @throws IllegalPropertyException in case the attribute has been defined but has incorrect values or it can't be resolved.
     */
    public boolean hasVersionRange()
        throws DataAccessException, IllegalPropertyException {
        return !Objects.isNull(versionRange);
    }

    /**
     * Sets the regular expression used to check the version against a range constraint.
     * 
     * @param versionRange the regular expression used to check the version against a range constraint.
     * 
     * @throws DataAccessException in case the attribute cannot be written or accessed.
     * @throws IllegalPropertyException in case the attribute has incorrect values or it can't be resolved.
     */
    public void setVersionRange(String versionRange)
        throws DataAccessException, IllegalPropertyException {
        this.versionRange = versionRange;
    }

    /**
     * Updates the current timestamp and returns the updated value.
     * 
     * @return the updated timestamp.
     * 
     * @see #getTimestamp()
     */
    public Long touchTimestamp() {
        timestamp = Long.valueOf(System.currentTimeMillis());
        return timestamp;
    }
}