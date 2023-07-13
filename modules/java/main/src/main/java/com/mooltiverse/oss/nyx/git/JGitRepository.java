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

import static com.mooltiverse.oss.nyx.log.Markers.GIT;

import java.io.File;
import java.io.IOException;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;

import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Tag;

import com.jcraft.jsch.AgentIdentityRepository;
import com.jcraft.jsch.AgentProxyException;
import com.jcraft.jsch.IdentityRepository;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SSHAgentConnector;
import com.jcraft.jsch.UserInfo;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.AddCommand;
import org.eclipse.jgit.api.CommitCommand;
import org.eclipse.jgit.api.PushCommand;
import org.eclipse.jgit.api.TagCommand;
import org.eclipse.jgit.api.TransportConfigCallback;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.JGitInternalException;
import org.eclipse.jgit.errors.AmbiguousObjectException;
import org.eclipse.jgit.errors.MissingObjectException;
import org.eclipse.jgit.errors.NoWorkTreeException;
import org.eclipse.jgit.errors.RevisionSyntaxException;
import org.eclipse.jgit.errors.RevWalkException;
import org.eclipse.jgit.errors.UnsupportedCredentialItem;
import org.eclipse.jgit.internal.transport.ssh.jsch.CredentialsProviderUserInfo;
import org.eclipse.jgit.lib.AnyObjectId;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.PersonIdent;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.RefDatabase;
import org.eclipse.jgit.revwalk.RevSort;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.transport.CredentialsProvider;
import org.eclipse.jgit.transport.CredentialItem;
import org.eclipse.jgit.transport.RefSpec;
import org.eclipse.jgit.transport.SshTransport;
import org.eclipse.jgit.transport.Transport;
import org.eclipse.jgit.transport.URIish;
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider;
import org.eclipse.jgit.transport.ssh.jsch.JschConfigSessionFactory;
import org.eclipse.jgit.transport.ssh.jsch.OpenSshConfig.Host;
import org.eclipse.jgit.util.FS;
import org.slf4j.event.Level;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A local repository implementation that encapsulates the backing <a href="https://www.eclipse.org/jgit/">JGit</a> library.
 */
class JGitRepository implements Repository {
    /**
     * The private logger instance
     */
    private static Logger logger = LoggerFactory.getLogger(Repository.class);

    /**
     * The private instance of the underlying Git object.
     */
    private final Git jGit;

    /**
     * Builds the instance using the given backing object.
     * 
     * @param jGit the backing JGit object.
     * 
     * @throws NullPointerException if the given object is {@code null}
     * @throws IllegalArgumentException if a given object is illegal for some reason, like referring to an illegal repository
     * @throws IOException in case of any I/O issue accessing the repository
     */
    private JGitRepository(Git jGit)
        throws IOException {
        super();
        Objects.requireNonNull(jGit, "Can't create a repository instance with a null backing JGit object");
        this.jGit = jGit;
    }

    /**
     * Returns a new credentials provider using the given user name and password.
     * 
     * @param user the user name to use when credentials are required. It may be {@code null}.
     * When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * @param password the password to use when credentials are required. It may be {@code null}.
     * When using single token authentication (i.e. OAuth or Personal Access Tokens)
     * this value may be the token or something other than a token, depending on the remote provider.
     * 
     * @return the credentials provider built on the given credentials. It is {@code null} if both the given credentials
     * are {@code null}.
     */
    private static CredentialsProvider getCredentialsProvider(String user, String password) {
        return (Objects.isNull(user) && Objects.isNull(password)) ? null : new UsernamePasswordCredentialsProvider(user, password);
    }

    /**
     * Returns a new transport config callback to use when using Git SSH authentication.
     * 
     * @param privateKey the SSH private key. If {@code null} the private key will be searched in its default location
     * (i.e. in the users' {@code $HOME/.ssh} directory).
     * @param passphrase the optional password to use to open the private key, in case it's protected by a passphrase.
     * This is required when the private key is password protected as this implementation does not support prompting
     * the user interactively for entering the password.
     * 
     * @return the transport config callback built on the given credentials. It is {@code null} if both the given credentials
     * are {@code null}.
     * 
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    private static TransportConfigCallback getTransportConfigCallback(String privateKey, byte[] passphrase) 
        throws GitException {
        // These code examples have been taken from
        // - https://www.codeaffine.com/2014/12/09/jgit-authentication/ and then elaborated
        // - https://stackoverflow.com/questions/31931574/java-sftp-client-that-takes-private-key-as-a-string
        // - https://stackoverflow.com/questions/56758040/loading-private-key-from-string-or-resource-in-java-jsch-in-android-app
        // - https://stackoverflow.com/questions/33637481/jsch-to-add-private-key-from-a-string
        JschConfigSessionFactory sshSessionFactory = new JschConfigSessionFactory() {
            // Override this method as per the Javadoc instructions
            @Override
            protected void configure(Host host, Session session) {
                // Configure in order to use the passphrase (when using the private key from the default location,
                // which means the user passed the passphrase but not the private key).
                // When the private key is passed in memory instead of using the default location, the passphrase
                // is passed along with it (see below in createDefaultJSch(FS fs)).
                if ((Objects.isNull(privateKey) || privateKey.isEmpty()) && ((!Objects.isNull(passphrase)) && (passphrase.length > 0))) {
                    // Implement an anonymous credentials provider to provide the passphrase when needed.
                    // An alternative could be to just implement an anonymous UserInfo object and make its getPassphrase()
                    // method return the passphrase.
                    CredentialsProvider credentialsProvider = new CredentialsProvider() {
                        /**
                         * Ask for the credential items to be populated.
                         * This method sets the passphrase for any given credential item, regardless the URI.
                         * 
                         * @param uri the URI of the remote resource that needs authentication.
                         * @param items the items the application requires to complete authentication.
                         * 
                         * @return {@code true}
                         */
                        @Override
                        public boolean get(URIish uri, CredentialItem... items)
                            throws UnsupportedCredentialItem {
                            for (CredentialItem item : items) {
                                // set the value for string types and ignore others
                                if (CredentialItem.StringType.class.isInstance(item)) {
                                    CredentialItem.StringType.class.cast(item).setValue(new String(passphrase));
                                }
                            }
                            return true;
                        }

                        /**
                         * Check if the provider is interactive with the end-user.
                         * An interactive provider may try to open a dialog box, or prompt for input on the terminal,
                         * and will wait for a user response. A non-interactive provider will either populate CredentialItems, or fail.
                         * 
                         * @return {@code false}
                         */
                        @Override
                        public boolean isInteractive() {
                            return false;
                        }

                        /**
                         * Check if the provider can supply the necessary CredentialItems.
                         * 
                         * This method always returns {@code true}
                         * 
                         * @param items the items the application requires to complete authentication.
                         * 
                         * @return {@code true} regardless the input
                         */
                        @Override
                        public boolean supports(CredentialItem... items) {
                            // just return true for any type of items, maybe this needs to be refined
                            return true;
                        }
                    };
                    UserInfo userInfo = new CredentialsProviderUserInfo(session, credentialsProvider);
                    session.setUserInfo(userInfo);
                }

                Properties sessionProperties = new Properties();
                //sessionProperties.put("PreferredAuthentications", "publickey");
                if (!Objects.isNull(privateKey) && !privateKey.isEmpty()) {
                    // disable host key checking if the in-memory key is used
                    sessionProperties.put("StrictHostKeyChecking", "no");
                }
                session.setConfig(sessionProperties);
            }

            // Override this method in order to load the in-memory key, as the default one only loads them from
            // default locations on the local filesystem
            @Override
            protected JSch createDefaultJSch(FS fs)
                throws JSchException {
                JSch.setLogger(new JschLogger());
                JSch.setConfig("PreferredAuthentications", "publickey");
                JSch defaultJSch = super.createDefaultJSch(fs);
                if (!Objects.isNull(privateKey) && !privateKey.isEmpty()) {
                    logger.debug(GIT, "Git uses public key authentication (SSH) using a custom key");
                    // the key name is not relevant
                    // the public key is not required
                    defaultJSch.addIdentity("git", privateKey.getBytes(), null, passphrase);

                    // disable host key checking
                    JSch.setConfig("StrictHostKeyChecking", "no");
                }
                else {
                    logger.debug(GIT, "Git uses public key authentication (SSH) using keys from default locations");
                
                    if (Objects.isNull(passphrase) || passphrase.length == 0) {
                        // In order to support the ssh-agent we can use: https://github.com/mwiede/jsch/blob/master/examples/JSchWithAgentProxy.java
                        logger.debug(GIT, "No passphrase has been provided so Git uses ssh-agent for passphrase-protected private keys");
                        try {
                            IdentityRepository irepo = new AgentIdentityRepository(new SSHAgentConnector());
                            defaultJSch.setIdentityRepository(irepo);
                        } catch (AgentProxyException ape) {
                            logger.error(GIT, "Attemp to attach to ssh-agent failed", ape);
                            //throw new GitException("Failed to attach to ssh-agent", ape);
                        }
                    } else logger.debug(GIT, "A passphrase has been provided so ssh-agent support is not enabled");
                }

                return defaultJSch;                  
            }
        };

        return new TransportConfigCallback() {
            @Override
            public void configure(Transport transport) {
                // How can we be sure the given Transport is an SshTransport? Docs don't explain...
                if (SshTransport.class.isInstance(transport)) {
                    SshTransport.class.cast(transport).setSshSessionFactory(sshSessionFactory);
                } else logger.warn(GIT, "The transport object received to set up the Git SSH authentication is of type '{}' and can't be cast to '{}'", transport.getClass().getName(), SshTransport.class.getName());
            }
        };
    }

    /**
     * The logger adapter class we use for Jsch in order to capture its logs and send them out to SLF4J.
     */
    public static class JschLogger implements com.jcraft.jsch.Logger {
        /**
         * A map mapping Jsch debug levels to SLF4J debug levels
         */
        static Map<Integer, Level> levelMapping = Map.of(
            Integer.valueOf(com.jcraft.jsch.Logger.DEBUG), Level.DEBUG,
            Integer.valueOf(com.jcraft.jsch.Logger.INFO), Level.DEBUG, // map Jsch's INFO to DEBUG to avoid log churns
            Integer.valueOf(com.jcraft.jsch.Logger.WARN), Level.WARN,
            Integer.valueOf(com.jcraft.jsch.Logger.ERROR), Level.ERROR,
            Integer.valueOf(com.jcraft.jsch.Logger.FATAL), Level.ERROR // FATAL is not available in SLF4J
        );

        /**
         * Checks if logging of some level is actually enabled.
         * This will be called by the library before each call to the {@link #log(int, String)} method.
         * 
         * @param level one of the log level constants DEBUG, INFO, WARN, ERROR and FATAL
         * 
         * @return {@code true} if the logging for the given level is enabled
         */
        @Override
        public boolean isEnabled(int level){
            // Commented due to issue at https://github.com/mooltiverse/nyx/issues/236
            // What follows is a workaround
            // Thanks n8ebel (https://github.com/n8ebel) for the hint
            //return logger.isEnabledForLevel(levelMapping.get(Integer.valueOf(level)));
            Level logLevel = levelMapping.get(Integer.valueOf(level));
            switch (logLevel) {
                case ERROR:
                    return logger.isErrorEnabled();
                case WARN:
                    return logger.isWarnEnabled();
                case INFO:
                    return logger.isInfoEnabled();
                case DEBUG:
                    return logger.isDebugEnabled();
                case TRACE:
                    return logger.isTraceEnabled();
            }

            return false;
        }

        /**
         * Logs the given message at the given level, relaying to SLF4J.
         * 
         * @param level one of the log level constants DEBUG, INFO, WARN, ERROR and FATAL
         * 
         * @return {@code true} if the logging for the given level is enabled
         */
        @Override
        public void log(int level, String message){
            // Commented due to issue at https://github.com/mooltiverse/nyx/issues/236
            // What follows is a workaround
            // Thanks n8ebel (https://github.com/n8ebel) for the hint
            //logger.atLevel(levelMapping.get(Integer.valueOf(level))).addMarker(GIT).log(message);
            Level logLevel = levelMapping.get(Integer.valueOf(level));
            switch (logLevel) {
                case ERROR:
                    logger.error(message);
                    break;
                case WARN:
                    logger.warn(message);
                    break;
                case INFO:
                    logger.info(message);
                    break;
                case DEBUG:
                    logger.debug(message);
                    break;
                case TRACE:
                    logger.trace(message);
                    break;
            }
        }
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
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
    static JGitRepository clone(File directory, String uri)
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");

        logger.debug(GIT, "Cloning repository in directory '{}' from URI '{}'", directory.getAbsolutePath(), uri);

        try {
            return new JGitRepository(Git.cloneRepository().setDirectory(directory).setURI(uri).call());
        }
        catch (IOException | GitAPIException | JGitInternalException e) {
            throw new GitException(String.format("Unable to clone the '%s' repository into '%s'", uri, directory.getAbsolutePath()), e);
        }
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
    static JGitRepository clone(File directory, String uri, String user, String password)
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");

        logger.debug(GIT, "Cloning repository in directory '{}' from URI '{}' using username and password", directory.getAbsolutePath(), uri);

        try {
            return new JGitRepository(Git.cloneRepository().setDirectory(directory).setURI(uri).setCredentialsProvider(getCredentialsProvider(user, password)).call());
        }
        catch (IOException | GitAPIException | JGitInternalException e) {
            throw new GitException(String.format("Unable to clone the '%s' repository into '%s'", uri, directory.getAbsolutePath()), e);
        }
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
    static JGitRepository clone(File directory, String uri, String privateKey, byte[] passphrase)
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");

        logger.debug(GIT, "Cloning repository in directory '{}' from URI '{}' using public key (SSH) authentication", directory.getAbsolutePath(), uri);

        try {
            return new JGitRepository(Git.cloneRepository().setDirectory(directory).setURI(uri).setTransportConfigCallback(getTransportConfigCallback(privateKey, passphrase)).call());
        }
        catch (IOException | GitAPIException | JGitInternalException e) {
            throw new GitException(String.format("Unable to clone the '%s' repository into '%s'", uri, directory.getAbsolutePath()), e);
        }
    }

    /**
     * Returns a repository instance working in the given directory after cloning from the given URI.
     * 
     * @param directory the directory where the repository has to be cloned. It is created if it doesn't exist.
     * @param uri the URI of the remote repository to clone.
     * 
     * @return the new repository object.
     * 
     * @throws NullPointerException if any of the given objects is {@code null}
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    static JGitRepository clone(String directory, String uri) 
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");
        if (directory.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank directory");
        if (uri.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank URI");

        return clone(new File(directory), uri);
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
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    static JGitRepository clone(String directory, String uri, String user, String password) 
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");
        if (directory.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank directory");
        if (uri.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank URI");

        return clone(new File(directory), uri, user, password);
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
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws GitException in case the operation fails for some reason, including when authentication fails
     */
    static JGitRepository clone(String directory, String uri, String privateKey, byte[] passphrase) 
        throws GitException {
        Objects.requireNonNull(directory, "Can't clone a repository instance with a null directory");
        Objects.requireNonNull(uri, "Can't clone a repository instance from a null URI");
        if (directory.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank directory");
        if (uri.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank URI");

        return clone(new File(directory), uri, privateKey, passphrase);
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
    static JGitRepository open(File directory)
        throws IOException {
        Objects.requireNonNull(directory, "Can't create a repository instance with a null directory");

        logger.debug(GIT, "Opening repository in directory '{}'", directory.getAbsolutePath());

        return new JGitRepository(Git.open(directory));
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
    static JGitRepository open(String directory) 
        throws IOException {
        Objects.requireNonNull(directory, "Can't create a repository instance with a null directory");
        if (directory.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank directory");

        return open(new File(directory));
    }

    /**
     * Resolves the commit with the given {@code id} using the repository object and returns it as a typed object.
     * In case you need to use the returned object with a {@link RevWalk} use the {@link #parseCommit(RevWalk, String)}
     * version of this method.
     * 
     * This method is an utility wrapper around {@link org.eclipse.jgit.lib.Repository#parseCommit(AnyObjectId)} which never returns
     * {@code null} and throws {@link GitException} if the identifier cannot be resolved or any other exception occurs.
     * 
     * @param id the commit identifier to resolve. It must be a long or abbreviated SHA-1 but not {@code null}.
     * 
     * @return the parsed commit object for the given identifier, never {@code null}
     * 
     * @throws GitException in case the given identifier cannot be resolved or any other issue is encountered
     * 
     * @see #resolve(String)
     * @see #parseCommit(RevWalk, String)
     * @see org.eclipse.jgit.lib.Repository#parseCommit(AnyObjectId)
     */
    private RevCommit parseCommit(String id)
        throws GitException {
        Objects.requireNonNull(id, "Cannot parse a commit from a null identifier");

        logger.trace(GIT, "Parsing commit '{}'", id);
        try {
            return jGit.getRepository().parseCommit(resolve(id));
        }
        catch (MissingObjectException moe) {
            throw new GitException(String.format("The '%s' commit identifier cannot be resolved as there is no such commit.", id), moe);
        }
        catch (IOException ioe) {
            throw new GitException(String.format("The '%s' commit identifier cannot be resolved to a valid commit", id), ioe);
        }
    }

    /**
     * Resolves the commit with the given {@code id} using the given {@link RevWalk} object and returns it as a typed object.
     * In case you need to use the returned object with a {@link RevWalk} instance you should use this version 
     * as {@link RevWalk} would throw exceptions if the object is parsed elsewhere.
     * 
     * This method is an utility wrapper around {@link RevWalk#parseCommit(AnyObjectId)} which never returns
     * {@code null} and throws {@link GitException} if the identifier cannot be resolved or any other exception occurs.
     * 
     * @param rw the {@link RevWalk} instance to use to parse the commit, cannot be {@code null}.
     * @param id the commit identifier to resolve. It must be a long or abbreviated SHA-1 but not {@code null}.
     * 
     * @return the parsed commit object for the given identifier, never {@code null}
     * 
     * @throws GitException in case the given identifier cannot be resolved or any other issue is encountered
     * 
     * @see #resolve(String)
     * @see RevWalk#parseCommit(AnyObjectId)
     */
    private RevCommit parseCommit(RevWalk rw, String id)
        throws GitException {
        Objects.requireNonNull(rw, "The RevWalk cannot be null");
        Objects.requireNonNull(id, "Cannot parse a commit from a null identifier");

        try {
            return rw.parseCommit(resolve(id));
        }
        catch (MissingObjectException moe) {
            throw new GitException(String.format("The '%s' commit identifier cannot be resolved as there is no such commit.", id), moe);
        }
        catch (IOException ioe) {
            throw new GitException(String.format("The '%s' commit identifier cannot be resolved to a valid commit", id), ioe);
        }
    }

    /**
     * Returns the first commit starting from the given revision. Different sort options can be provided (i.e. to get the last commit instead of the first).
     * 
     * @param startFrom a name that will be resolved first when looking for the commit. This is resolved using {@link org.eclipse.jgit.lib.Repository#resolve(String)} and may
     * be a branch name a commit SHA or anything supported by such method. For example, to use the current branch, you can use {@code HEAD} here ({@link Constants#HEAD}).
     * @param sort the sort option. Pass {@code null} for the natural ordering (from most recent to oldest) or {@link RevSort#REVERSE} for the reverse order.
     * 
     * @return the selected commit in the repository or {@code null} if such commit cannot be found (i.e. if the repository has no commit yet).
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository or when {@code startFrom} can't be resolved
     * to a valid repository object
     * 
     * @see org.eclipse.jgit.lib.Repository#resolve(String)
     */
    private RevCommit peekCommit(String startFrom, RevSort sort)
        throws GitException {
        Objects.requireNonNull(startFrom, "The starting revision object is required");

        logger.trace(GIT, "Peeking commit");
        RevWalk rw = new RevWalk(jGit.getRepository());
        try {
            if (!Objects.isNull(sort))
                rw.sort(sort);
            rw.markStart(parseCommit(rw, startFrom));
            return rw.next();
        }
        catch (MissingObjectException moe) {
            throw new GitException(String.format("Cannot peek the '%s' commit likely because of a broken link in the object database.", startFrom), moe);
        }
        catch (RevisionSyntaxException | JGitInternalException | IOException e) {
            throw new GitException(String.format("Cannot peek the '%s' commit.", startFrom), e);
        }
        finally {
            rw.close();
        }
    }

    /**
     * Resolves the object with the given {@code id} in the repository.
     * 
     * This method is an utility wrapper around {@link org.eclipse.jgit.lib.Repository#resolve(String)} which never returns
     * {@code null} and throws {@link GitException} if the identifier cannot be resolved or any other exception occurs.
     * 
     * @param id the object identifier to resolve. It can't be {@code null}. If it's a SHA-1 it can be long or abbreviated.
     * For allowed values see {@link org.eclipse.jgit.lib.Repository#resolve(String)}
     * 
     * @return the resolved object for the given identifier, never {@code null}
     * 
     * @throws GitException in case the given identifier cannot be resolved or any other issue is encountered
     * 
     * @see org.eclipse.jgit.lib.Repository#resolve(String)
     */
    private ObjectId resolve(String id)
        throws GitException {
        Objects.requireNonNull(id, "Cannot resolve null identifiers");

        logger.trace(GIT, "Resolving '{}'", id);
        try {
            ObjectId res = jGit.getRepository().resolve(id);
            if (Objects.isNull(res))
            {
                if (Constants.HEAD.equals(id))
                    logger.warn(GIT, "Repository identifier '{}' cannot be resolved. This means that the repository has just been initialized and has no commits yet or the repository is in a 'detached HEAD' state. See the documentation to fix this.", Constants.HEAD);
                throw new GitException(String.format("Identifier '%s' cannot be resolved", id));
            }
            else return res;
        }
        catch (AmbiguousObjectException aoe) {
            throw new GitException(String.format("The '%s' identifier cannot be resolved uniquely as it resolves to multiple objects in the repository. If this is a shortened SHA identifier try using more charachers to disambiguate.", id), aoe);
        }
        catch (RevisionSyntaxException rse) {
            throw new GitException(String.format("The '%s' identifier cannot be resolved as the expression is not supported by this implementation.", id), rse);
        }
        catch (IOException ioe) {
            throw new GitException(String.format("The '%s' identifier cannot be resolved", id), ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void add(Collection<String> paths)
        throws GitException {
        logger.debug(GIT, "Adding contents to repository staging area");
        try {
            AddCommand command = jGit.add();
            command.setUpdate(false); // match all files, not only those already in the index
            for (String path: paths)
                command.addFilepattern(path);

            command.call();
        }
        catch (GitAPIException gae) {
            throw new GitException("An error occurred when trying to add paths to the staging area", gae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Commit commit(String message)
        throws GitException {
        return commit(message, null, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Commit commit(String message, Identity author, Identity committer)
        throws GitException {
        logger.debug(GIT, "Committing changes to repository");
        try {
            CommitCommand command = jGit.commit();
            command.setMessage(message);
            command.setAllowEmpty(false); // we don't want to create empty commits
            if (!Objects.isNull(author))
                command.setAuthor(author.getName(), author.getEmail());
            if (!Objects.isNull(committer))
                command.setCommitter(committer.getName(), committer.getEmail());

            return ObjectFactory.commitFrom(command.call(), Set.<Tag>of());
        }
        catch (GitAPIException gae) {
            throw new GitException("An error occurred when trying to commit", gae);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Commit commit(Collection<String> paths, String message)
        throws GitException {
        return commit(paths, message, null, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Commit commit(Collection<String> paths, String message, Identity author, Identity committer)
        throws GitException {
        add(paths);
        return commit(message, author, committer);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Tag> getCommitTags(String commit)
        throws GitException {
        logger.debug(GIT, "Retrieving tags for commit '{}'", commit);
        Set<Tag> res = new HashSet<Tag>();
        try {
            RefDatabase refDatabase = jGit.getRepository().getRefDatabase();
            for (Ref tagRef: refDatabase.getRefsByPrefix(Constants.R_TAGS)) {
                // refs must be peeled in order to see if they're annoteted or lightweight
                tagRef = refDatabase.peel(tagRef);
                // when it's an annotated tag tagRef.getPeeledObjectId() is not null,
                // while for lightweight tags tagRef.getPeeledObjectId() is null
                if (Objects.isNull(tagRef.getPeeledObjectId()))
                {
                    if (tagRef.getObjectId().getName().startsWith(commit))
                        res.add(ObjectFactory.tagFrom(tagRef));
                }
                else {
                    // it's an annotated tag
                    if (tagRef.getPeeledObjectId().getName().startsWith(commit))
                        res.add(ObjectFactory.tagFrom(tagRef));
                }
            }
        }
        catch (IOException e) {
            throw new GitException("Cannot list repository tags", e);
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getCurrentBranch()
        throws GitException {
        try {
            return jGit.getRepository().getBranch();
        }
        catch (IOException ioe) {
            throw new GitException("Unable to retrieve the current branch name", ioe);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLatestCommit()
        throws GitException {
        String commitSHA = peekCommit(Constants.HEAD, null).getName();
        logger.debug(GIT, "Repository latest commit in HEAD branch is '{}'", commitSHA);
        return commitSHA;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getRootCommit()
        throws GitException {
        String commitSHA = peekCommit(Constants.HEAD, RevSort.REVERSE).getName();
        logger.debug(GIT, "Repository root commit is '{}'", commitSHA);
        return commitSHA;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getRemoteNames()
        throws GitException {
        logger.debug(GIT, "Repository remote names are '{}'", String.join(", ", jGit.getRepository().getRemoteNames()));
        return jGit.getRepository().getRemoteNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Tag> getTags()
        throws GitException {
        logger.debug(GIT, "Retrieving all tags");
        Set<Tag> res = new HashSet<Tag>();
        try {
            RefDatabase refDatabase = jGit.getRepository().getRefDatabase();
            for (Ref tagRef: refDatabase.getRefsByPrefix(Constants.R_TAGS)) {
                // refs must be peeled in order to see if they're annoteted or lightweight
                tagRef = refDatabase.peel(tagRef);
                res.add(ObjectFactory.tagFrom(tagRef));
            }
        }
        catch (IOException e) {
            throw new GitException("Cannot list repository tags", e);
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isClean()
        throws GitException {
        logger.debug(GIT, "Checking repository clean status");
        try {
            return jGit.status().call().isClean();
        }
        catch (GitAPIException | NoWorkTreeException e) {
            throw new GitException("Unable to query the repository status.", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String push(String user, String password)
        throws GitException {
        return push(DEFAULT_REMOTE_NAME, user, password);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String push(String privateKey, byte[] passphrase)
        throws GitException {
        return push(DEFAULT_REMOTE_NAME, privateKey, passphrase);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String push(String remote, String user, String password)
        throws GitException {
        logger.debug(GIT, "Pushing changes to remote repository '{}' using username and password", remote);
        try {
            // get the current branch name
            String currentBranchRef = jGit.getRepository().getFullBranch();
            // the refspec is in the localBranch:remoteBranch form, and we assume they both have the same name here
            RefSpec refSpec = new RefSpec(currentBranchRef.concat(":").concat(currentBranchRef));
            PushCommand pushCommand = jGit.push().setRefSpecs(refSpec);
            if (!Objects.isNull(remote) && !remote.isBlank())
                pushCommand.setRemote(remote);
            pushCommand.setPushTags();
            pushCommand.setCredentialsProvider(getCredentialsProvider(user, password));
            pushCommand.call();
            return pushCommand.getRemote();
        }
        catch (GitAPIException | IOException e) {
            throw new GitException("An error occurred when trying to push", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String push(String remote, String privateKey, byte[] passphrase)
        throws GitException {
        logger.debug(GIT, "Pushing changes to remote repository '{}' using public key (SSH) authentication", remote);
        try {
            // get the current branch name
            String currentBranchRef = jGit.getRepository().getFullBranch();
            // the refspec is in the localBranch:remoteBranch form, and we assume they both have the same name here
            RefSpec refSpec = new RefSpec(currentBranchRef.concat(":").concat(currentBranchRef));
            PushCommand pushCommand = jGit.push().setRefSpecs(refSpec);
            if (!Objects.isNull(remote) && !remote.isBlank())
                pushCommand.setRemote(remote);
            pushCommand.setPushTags();
            pushCommand.setTransportConfigCallback(getTransportConfigCallback(privateKey, passphrase));
            pushCommand.call();
            return pushCommand.getRemote();
        }
        catch (GitAPIException | IOException e) {
            throw new GitException("An error occurred when trying to push", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> push(Collection<String> remotes, String user, String password)
        throws GitException {
        logger.debug(GIT, "Pushing changes to '{}' remote repositories using username and password", remotes.size());
        Set<String> res = new HashSet<String>();
        for (String remote: remotes) {
            res.add(push(remote, user, password));
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> push(Collection<String> remotes, String privateKey, byte[] passphrase)
        throws GitException {
        logger.debug(GIT, "Pushing changes to '{}' remote repositories using public key (SSH) authentication", remotes.size());
        Set<String> res = new HashSet<String>();
        for (String remote: remotes) {
            res.add(push(remote, privateKey, passphrase));
        }
        return res;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Tag tag(String name)
        throws GitException {
        return tag(name, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Tag tag(String name, String message)
        throws GitException {
        return tag(name, message, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Tag tag(String name, String message, Identity tagger)
        throws GitException {
        return tag(null, name, message, tagger);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Tag tag(String target, String name, String message, Identity tagger)
        throws GitException {
        logger.debug(GIT, "Tagging as '{}'", name);
        try {
            TagCommand command = jGit.tag().setObjectId(parseCommit(Objects.isNull(target) ? getLatestCommit() : target));
            if (Objects.isNull(message))
                command.setAnnotated(false);
            else {
                command.setAnnotated(true);
                command.setMessage(message);
                if (!Objects.isNull(tagger)) {
                    command.setTagger(new PersonIdent(tagger.getName(), tagger.getEmail()));
                }
            }
            return ObjectFactory.tagFrom(jGit.getRepository().getRefDatabase().peel(command.setName(name).call()));
        }
        catch (GitAPIException | JGitInternalException | IOException e) {
            throw new GitException("Unable to create Git tag", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void walkHistory(String start, String end, CommitVisitor visitor)
        throws GitException {
        if (Objects.isNull(visitor))
            return;
        logger.debug(GIT, "Walking commit history. Start commit boundary is '{}'. End commit boundary is '{}'", Objects.isNull(start) ? "not defined" : start, Objects.isNull(end) ? "not defined" : end);

        RevWalk rw = new RevWalk(jGit.getRepository());
        try {
            // follow the first parent upon merge commits
            rw.setFirstParent(true); // this must always be called before markStart
            logger.debug(GIT, "Upon merge commits only the first parent is considered.");

            RevCommit startCommit = parseCommit(rw, Objects.isNull(start) ? Constants.HEAD : start);
            logger.trace(GIT, "Start boundary resolved to commit '{}'", startCommit.getId().getName());
            rw.markStart(startCommit);

            // make sure the end commit can be resolved, if not null, or throw an exception
            RevCommit endCommit = Objects.isNull(end) ? null : parseCommit(rw, end);
            logger.trace(GIT, "End boundary resolved to commit '{}'", Objects.isNull(endCommit) ? "not defined" : endCommit.getId().getName());

            Iterator<RevCommit> commitIterator = rw.iterator();
            while (commitIterator.hasNext()) {
                RevCommit commit = commitIterator.next();
                logger.trace(GIT, "Visiting commit '{}'", commit.getId().getName());
                boolean visitorContinues = visitor.visit(ObjectFactory.commitFrom(commit, getCommitTags(commit.getId().getName())));
                
                if (!visitorContinues) {
                    logger.debug(GIT, "Commit history walk interrupted by visitor");
                    break;
                }
                else if (!Objects.isNull(end) && commit.getId().getName().startsWith(end)) {
                    logger.debug(GIT, "Commit history walk reached the end boundary '{}'", end);
                    break;
                }
                else if (!commitIterator.hasNext()) {
                    logger.debug(GIT, "Commit history walk reached the end");
                }
            }
        }
        catch (RevWalkException rwe) {
            throw new GitException("Cannot walk through commits.", rwe);
        }
        catch (RevisionSyntaxException | JGitInternalException | IOException e) {
            throw new GitException("An error occurred while walking through commits", e);
        }
        finally {
            rw.close();
        }
    }
}
