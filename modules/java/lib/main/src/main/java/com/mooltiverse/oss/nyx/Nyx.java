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
package com.mooltiverse.oss.nyx;

import static com.mooltiverse.oss.nyx.log.Markers.MAIN;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Objects;
import java.util.Map;
import java.util.HashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.command.AbstractCommand;
import com.mooltiverse.oss.nyx.command.Amend;
import com.mooltiverse.oss.nyx.command.Clean;
import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.command.Make;
import com.mooltiverse.oss.nyx.command.Mark;
import com.mooltiverse.oss.nyx.command.Publish;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.configuration.ConfigurationException;
import com.mooltiverse.oss.nyx.git.local.Repository;
import com.mooltiverse.oss.nyx.state.State;
import com.mooltiverse.oss.nyx.state.StateException;

/**
 * The Nyx entry point and main class.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Nyx {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Nyx.class);

    /**
     * The internal configuration object.
     * 
     * This object is lazily initialized so in order to make sure you get a valid reference you should always use
     * {@link #configuration()} instead of reading this member.
     * 
     * @see #configuration()
     */
    private Configuration configuration = null;

    /**
     * The internal Git repository object.
     * 
     * This object is lazily initialized so in order to make sure you get a valid reference you should always use
     * {@link #repository()} instead of reading this member.
     * 
     * @see #repository()
     */
    private Repository repository = null;

    /**
     * The internal state object.
     * 
     * This object is lazily initialized so in order to make sure you get a valid reference you should always use
     * {@link #state()} instead of reading this member.
     * 
     * @see #state()
     */
    private State state = null;

    /**
     * This map stores instances of commands so that they can be reused.
     * 
     * Instances are lazily created and stored here in their command methods.
     */
    private final Map<Class<? extends AbstractCommand>, Command> commands = new HashMap<Class<? extends AbstractCommand>, Command>();

    /**
     * Default constructor.
     */
    public Nyx() {
        super();
        logger.trace(MAIN, "New Nyx instance");
    }

    /**
     * Returns the configuration.
     * 
     * @return the configuration
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     */
    public Configuration configuration()
        throws ConfigurationException {
        if (Objects.isNull(configuration)) {
            logger.debug(MAIN, "Instantiating the initial configuration");
            configuration = new Configuration();
        }
        return configuration;
    }

    /**
     * Returns the state.
     * 
     * @return the state
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     */
    public Repository repository()
        throws ConfigurationException {
        if (Objects.isNull(repository)) {
            File repoDir = configuration().getDirectory();
            logger.debug(MAIN, "Instantiating the Git repository in {}", repoDir);
            try {
                repository = Repository.open(repoDir);
            }
            catch (IOException ioe) {
                throw new ConfigurationException(String.format("The directory %s is not accessible or does not contain a valid Git repository", repoDir.getAbsolutePath()), ioe);
            }
        }
        return repository;
    }

    /**
     * Returns the state.
     * 
     * @return the state
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    public State state()
        throws ConfigurationException, StateException {
        if (Objects.isNull(state)) {
            logger.debug(MAIN, "Instantiating the initial state");
            state = new State(configuration());
        }
        return state;
    }

    /**
     * Runs the command implemented by the given class through its {@link AbstractCommand#run()} and returns its result.
     * If {@code useCache} is {@code true} the command instance is only created if not already cached from previous runs.
     * 
     * @param <T> the command type (class)
     * 
     * @param clazz the command class
     * @param useCache enables or disables the use of command instances
     * 
     * @return the return value of the {@link AbstractCommand#run()} method, invoked against the given command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     */
    private <T extends AbstractCommand> State runCommand(Class<T> clazz, boolean useCache)
        throws ConfigurationException, StateException {
        
        Objects.requireNonNull(clazz, "Cannot instantiate the command from a null class");

        Command command = null;

        if (useCache) {
            logger.debug(MAIN, "Trying to retrieve {} command from cache", clazz.getSimpleName());
            command = commands.get(clazz);
            logger.debug(MAIN, "No cached instances of the {} command found", clazz.getSimpleName());
        }

        if (Objects.isNull(command)) {
            try {
                logger.debug(MAIN, "Instantiating new command from {}", clazz.getSimpleName());
                Constructor<T> constructor = clazz.getConstructor(State.class, Repository.class);
                command = constructor.newInstance(state(), repository());
                logger.debug(MAIN, "New command from {}: {}", clazz.getSimpleName(), command.hashCode());

                if (useCache) {
                    logger.debug(MAIN, "Storing command {} in cache", clazz.getSimpleName());
                    commands.put(clazz, command);
                }
            }
            catch (NoSuchMethodException nsme) {
                String msg = String.format("Class %s does not implement the required constructor with 2 arguments %s and %s. This must be considered a Nyx internal error and reported to maintainers.", clazz.getName(), State.class.getName(), Repository.class.getName());
                logger.error(msg, nsme);
                throw new NoSuchMethodError(msg);
            }
            catch (SecurityException | IllegalAccessException | IllegalArgumentException | InstantiationException | InvocationTargetException | ExceptionInInitializerError t) {
                String msg = String.format("Class %s cannot be instantiated. This must be considered a Nyx internal error and reported to maintainers.", clazz.getName());
                logger.error(msg, t);
                throw new InstantiationError(msg);
            }
        }

        logger.debug(MAIN, "Running command {}", clazz.getSimpleName());
        if (command.isUpToDate()) {
            logger.debug(MAIN, "Command {} is up to date, skipping.", clazz.getSimpleName());
            return state();
        }
        else {
            logger.debug(MAIN, "Command {} is not up to date, running...", clazz.getSimpleName());
            return command.run();
        }
    }

    /**
     * Runs the {@link Amend} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @See Amend
     */
    public State amend()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.amend()");

        // this command has no dependencies

        // run the command
        return runCommand(Amend.class, true);
    }

    /**
     * Runs the {@link Clean} command and removes all the cached instances of internally referenced objects to restore the
     * state of this class to its initial state.
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @see Clean
     */
    public void clean()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.clean()");

        // this command has no dependencies

        // run the command (never cached)
        runCommand(Clean.class, false);

        // clean the cache and remove lazy instances
        repository = null;
        configuration = null;
        state = null;
    }

    /**
     * Runs the {@link Infer} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @See Infer
     */
    public State infer()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.infer()");

        // run dependent tasks first
        amend();

        // run the command
        return runCommand(Infer.class, true);
    }

    /**
     * Runs the {@link Make} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @See Make
     */
    public State make()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.make()");

        // run dependent tasks first
        infer();

        // run the command
        return runCommand(Make.class, true);
    }

    /**
     * Runs the {@link Mark} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @See Mark
     */
    public State mark()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.mark()");

        // run dependent tasks first
        make();

        // run the command
        return runCommand(Mark.class, true);
    }

    /**
     * Runs the {@link Publish} command and returns the updated state. Dependencies of this command are also executed first.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws ConfigurationException in case the configuration can't be loaded for some reason.
     * @throws StateException in case the state is invalid for some reason.
     * 
     * @See Publish
     */
    public State publish()
        throws ConfigurationException, StateException {
        logger.debug(MAIN, "Nyx.publish()");

        // run dependent tasks first
        mark();

        // run the command
        return runCommand(Publish.class, true);
    }
}