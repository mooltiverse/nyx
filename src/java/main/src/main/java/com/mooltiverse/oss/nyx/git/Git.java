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
package com.mooltiverse.oss.nyx.git;

import java.io.File;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The entry point to the Git local and remote service. This is also the main entry point to retrieve {@link Repository} instances
 */
public class Git {
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
     * This method uses no authentication.
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
    public Repository clone(File directory, String uri)
        throws GitException {
            return JGitRepository.clone(directory, uri);
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
    public Repository clone(File directory, String uri, String user, String password)
        throws GitException {
        return JGitRepository.clone(directory, uri, user, password);
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * @param privateKey the SSH private key. If {@code null} the private key will be searched in its default location
     * (i.e. in the users' {@code $HOME/.ssh} directory).
     * @param passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
     * This is required when the private key is password protected as this implementation does not support prompting
     * the user interactively for entering the password.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(File directory, String uri, String privateKey, byte[] passphrase)
        throws GitException {
        return JGitRepository.clone(directory, uri, privateKey, passphrase);
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * This method uses no authentication.
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
        return JGitRepository.clone(directory, uri);
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
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * @param privateKey the SSH private key. If {@code null} the private key will be searched in its default location
     * (i.e. in the users' {@code $HOME/.ssh} directory).
     * @param passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
     * This is required when the private key is password protected as this implementation does not support prompting
     * the user interactively for entering the password.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    public Repository clone(String directory, String uri, String privateKey, byte[] passphrase) 
        throws GitException {
        return JGitRepository.clone(directory, uri, privateKey, passphrase);
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
}
