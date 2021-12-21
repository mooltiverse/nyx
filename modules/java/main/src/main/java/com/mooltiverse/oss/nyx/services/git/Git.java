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

import java.io.File;
import java.io.IOException;
import java.net.URI;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.mooltiverse.oss.nyx.services.GitException;
import com.mooltiverse.oss.nyx.services.SecurityException;

/**
 * The entry point to the Git local and remote service. This is also the main entry point to retrieve {@link Repository} instances
 */
public class Git {
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
     * Default constructor is private on purpose.
     */
    private Git() {
        super();
    }

    /**
     * Returns an instance using default options.
     * 
     * @return an instance using default options.
     */
    public static Git instance() {
        return new Git();
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * If the instance has some credentials set, those are used to perform the operation, otherwise
     * anonymous access will be used.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(File directory, URI uri)
        throws GitException {
        return clone(directory, uri, getUser(), getPassword());
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * @param user the user name to use when credentials are required. If this and {@code password} are both {@code null}
     * then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * @param password the password to use when credentials are required. If this and {@code user} are both {@code null}
     * then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(File directory, URI uri, String user, String password)
        throws GitException {
        return JGitRepository.clone(directory, uri, user, password);
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * If the instance has some credentials set, those are used to perform the operation, otherwise
     * anonymous access will be used.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(String directory, String uri) 
        throws GitException {
        return clone(directory, uri, getUser(), getPassword());
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * @param user the user name to use when credentials are required. If this and {@code password} are both {@code null}
     * then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * @param password the password to use when credentials are required. If this and {@code user} are both {@code null}
     * then no credentials is used. When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(String directory, String uri, String user, String password) 
        throws GitException {
        return JGitRepository.clone(directory, uri, user, password);
    }

    /**
     * Returns the user name to be used when connecting to remote repositories.
     * 
     * @return the user name to be used when connecting to remote repositories.
     * When {@code null} the remote repositories are meant to allow anonymous access and,
     * in this case, also the {@link #getPassword() password} is {@code null}.
     */
    public String getUser() {
        return user;
    }

    /**
     * Returns the password to be used when connecting to remote repositories.
     * 
     * @return the password to be used when connecting to remote repositories.
     * When {@link #getUser()} returns {@code null} this method also returns
     * {@code null} amd the remote repositories are meant to allow anonymous access.
     */
    public String getPassword() {
        return password;
    }

    /**
     * Returns a repository instance working in the given directory.
     * 
     * @param directory the directory where the repository is.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if the given object is {@code null}
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws IOException in case of any I/O issue accessing the repository
     */
    public Repository open(File directory)
        throws IOException {
        return JGitRepository.open(directory);
    }

    /**
     * Returns a repository instance working in the given directory.
     * 
     * @param directory the directory where the repository is.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if the given object is {@code null}
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws IOException in case of any I/O issue accessing the repository
     */
    public Repository open(String directory) 
        throws IOException {
        return JGitRepository.open(directory);
    }

    /**
     * Sets the user name to be used when connecting to remote repositories.
     * 
     * @param user the user name to be used when connecting to remote repositories.
     * When {@code null} the remote repositories are meant to allow anonymous access.
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * Sets the password to be used when connecting to remote repositories.
     * 
     * @param password the password to be used when connecting to remote repositories.
     */
    public void setPassword(String password) {
        this.password = password;
    }
}
