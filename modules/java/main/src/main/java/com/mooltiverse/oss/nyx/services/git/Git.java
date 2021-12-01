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
package com.mooltiverse.oss.nyx.services.git;

import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.mooltiverse.oss.nyx.services.GitRemoteService;
import com.mooltiverse.oss.nyx.services.SecurityException;
import com.mooltiverse.oss.nyx.services.Service;

/**
 * The entry point to the generic Git service.
 */
public class Git implements GitRemoteService {
    /**
     * The list of supported remote repository names supported by this service. It may be {@code null} or empty.
     */
    private List<String> remotes = null;

    /**
     * The user name to authenticate to the remote service.
     * If {@code null} or invalid all authentication protected operations will fail with a
     * {@link SecurityException}
     */
    private String user = null;

    /**
     * The password to authenticate to the remote service.
     * If {@code null} or invalid all authentication protected operations will fail with a
     * {@link SecurityException}
     */
    private String password = null;

    /**
     * The name of the option used to pass the list of supported remotes to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to perform some of its operations.
     * Value: {@value}
     */
    public static final String REMOTES_OPTION_NAME = "REMOTES";

    /**
     * The name of the option used to pass the user name to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to authenticate and perform any
     * of the authentication protected operations.
     * Value: {@value}
     */
    public static final String USER_OPTION_NAME = "USER";

    /**
     * The name of the option used to pass the password to this object instance.
     * This is the value of the key inside the options passed to get a new instance of this class.
     * If this option is not passed the service will not be able to authenticate and perform any
     * of the authentication protected operations.
     * Value: {@value}
     */
    public static final String PASSWORD_OPTION_NAME = "PASSWORD";

    /**
     * The {@code SERVICE} marker, used when logging command events.
     */
    static final Marker SERVICE = MarkerFactory.getMarker("SERVICE");

    /**
     * The logger instance.
     */
    static final Logger logger = LoggerFactory.getLogger(Git.class);

    /**
     * Builds an instance.
     * 
     * @param user the user name to authenticate to the remote service. It may be {@code null} or empty.
     * @param password the password to authenticate to the remote service. It may be {@code null} or empty.
     * @param remotes the list of supported remote repository names supported by this service. It may be {@code null} or empty.
     * 
     * @throws NullPointerException if the given API is {@code null}.
     */
    private Git(String user, String password, List<String> remotes) {
        super();
        this.user = user;
        this.password = password;
        this.remotes = remotes;
    }

    /**
     * Returns an instance using the given options.
     * 
     * @param options the map of options for the requested service. It can't be {@code null}.
     * Valid options are documented as constants on this class.
     * 
     * @return an instance using the given options.
     * 
     * @throws NullPointerException if the given options map is {@code null}
     * @throws IllegalArgumentException if some entries in the given options map are missing or illegal for some reason
     */
    public static Git instance(Map<String,String> options) {
        Objects.requireNonNull(options, "Can't create a new instance with a null options map");

        String user = null;
        if (Objects.isNull(options.get(USER_OPTION_NAME)))
            logger.warn(SERVICE, "No user name passed to the '{}' service, no authentication protected operation will be available. Use the '{}' option to set this value", Git.class.getSimpleName(), USER_OPTION_NAME);
        else user = options.get(USER_OPTION_NAME);

        String password = null;
        if (Objects.isNull(options.get(PASSWORD_OPTION_NAME)))
            logger.warn(SERVICE, "No password passed to the '{}' service, no authentication protected operation will be available. Use the '{}' option to set this value", Git.class.getSimpleName(), PASSWORD_OPTION_NAME);
        else password = options.get(PASSWORD_OPTION_NAME);

        List<String> remotes = null;
        if (Objects.isNull(options.get(REMOTES_OPTION_NAME)))
            logger.warn(SERVICE, "No supported remotes list passed to the '{}' service, assuming the service can be used for any remote. Use the '{}' option to set this value", Git.class.getSimpleName(), REMOTES_OPTION_NAME);
        else remotes = List.<String>of(options.get(REMOTES_OPTION_NAME).split(","));
        
        // we have only one API and version so far so we just use that, otherwise we'd need to read additional
        // options to understand which API and version to use
        return new Git(user, password, remotes);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedRemoteNames() {
        return remotes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getUserForRemote() {
        return user;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getPasswordForRemote() {
        return password;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean supports(Service.Feature feature) {
        Objects.requireNonNull(feature, "Can't check if the feature is supported for a null feature");
        switch (feature)
        {
            case GIT_REMOTE:    return true;
            default:            return false;
        }
    }
}
