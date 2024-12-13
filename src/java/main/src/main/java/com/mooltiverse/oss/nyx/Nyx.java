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
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Objects;
import java.util.EnumMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.command.Clean;
import com.mooltiverse.oss.nyx.command.Command;
import com.mooltiverse.oss.nyx.command.Commands;
import com.mooltiverse.oss.nyx.command.Infer;
import com.mooltiverse.oss.nyx.command.Make;
import com.mooltiverse.oss.nyx.command.Mark;
import com.mooltiverse.oss.nyx.command.Publish;
import com.mooltiverse.oss.nyx.configuration.Configuration;
import com.mooltiverse.oss.nyx.entities.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.Git;
import com.mooltiverse.oss.nyx.git.GitException;
import com.mooltiverse.oss.nyx.git.Repository;
import com.mooltiverse.oss.nyx.io.DataAccessException;
import com.mooltiverse.oss.nyx.io.FileMapper;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The Nyx entry point and main class.
 * 
 * This class is not thread safe.
 */
public class Nyx {
    /**
     * The private logger instance.
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
     * Instances are lazily created and stored here.
     */
    private final EnumMap<Commands, Command> commands = new EnumMap<Commands,Command>(Commands.class);

    /**
     * Default constructor.
     */
    public Nyx() {
        super();
        logger.trace(MAIN, "New Nyx instance");
        logger.debug(MAIN, "Nyx version {}", this.getClass().getPackage().getImplementationVersion());

        // this is not actually needed for production but some tests may leave the default directory dirty, so this is safer
        Configuration.setDefaultDirectory(null);
    }

    /**
     * Creates a new Nyx instance using the given directory as the base directory.
     * 
     * @param directory the default directory. If not {@code null} this overrides the configuration
     * ({@link Configuration#getDirectory()}).
     * 
     * @see Configuration#getDirectory()
     */
    public Nyx(File directory) {
        super();
        logger.trace(MAIN, "New Nyx instance in directory '{}'", Objects.isNull(directory) ? "null" : directory.getAbsolutePath());
        // just override the default directory right away in the singleton default layer
        Configuration.setDefaultDirectory(directory);
    }

    /**
     * Returns the configuration.
     * 
     * @return the configuration
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     */
    public Configuration configuration()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(configuration)) {
            logger.debug(MAIN, "Instantiating the initial configuration");
            configuration = new Configuration();
        }
        return configuration;
    }

    /**
     * Returns the repository.
     * 
     * @return the repository
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     */
    public Repository repository()
        throws DataAccessException, IllegalPropertyException, GitException {
        if (Objects.isNull(repository)) {
            File repoDir = new File(configuration().getDirectory());
            logger.debug(MAIN, "Instantiating the Git repository in '{}'", repoDir);
            try {
                repository = Git.instance().open(repoDir);
            }
            catch (IOException ioe) {
                throw new DataAccessException(String.format("The directory '%s' is not accessible or does not contain a valid Git repository", repoDir.getAbsolutePath()), ioe);
            }
        }
        return repository;
    }

    /**
     * Returns the state. The state may be created from scratch or loaded from a previously saved file, if the configuration says so.
     * 
     * @return the state
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     */
    public State state()
        throws DataAccessException, IllegalPropertyException {
        if (Objects.isNull(state)) {
            if (configuration().getResume()) {
                if (Objects.isNull(configuration().getStateFile())) {
                    logger.warn(MAIN, "The resume flag has been set but no state file has been configured. The state file will not be resumed.");
                }
                else {
                    File stateFile = new File(configuration().getStateFile());
                    // if the file path is relative make it relative to the configured directory
                    if (!stateFile.isAbsolute())
                        stateFile = new File(configuration().getDirectory(), configuration().getStateFile());
                    if (stateFile.exists()) {
                        logger.debug(MAIN, "Resuming the state from file '{}'", configuration().getStateFile());
                        state = State.resume(stateFile, configuration());
                        logger.debug(MAIN, "State has been resumed from file '{}'", configuration().getStateFile());
                    }
                    else {
                        logger.warn(MAIN, "The resume flag has been set and the state file has been configured but no state file exists at the given location {}. The state file will not be resumed.", configuration().getStateFile());
                    }
                }
            }

            // if the state was not resumed from a file, instantiate a new one
            if (Objects.isNull(state)) {
                logger.debug(MAIN, "Instantiating the initial state");
                state = new State(configuration());
            }
        }
        return state;
    }

    /**
     * Creates a command instance from the given command identifier.
     * 
     * @param command the identifier of the command to create the instance for
     * 
     * @return the instance of the command, never {@code null}
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     */
    private Command newCommandInstance(Commands command)
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(MAIN, "Creating new command instance '{}'", command);
        switch (command) {
            case CLEAN:   return new Clean(state(), repository());
            case INFER:   return new Infer(state(), repository());
            case MAKE:    return new Make(state(), repository());
            case MARK:    return new Mark(state(), repository());
            case PUBLISH: return new Publish(state(), repository());
            default:      throw new IllegalArgumentException(String.format("Unknown command '%s'", command.toString()));
        }
    }

    /**
     * Gets a command instance. If the command is already in the internal cache that instance is returned,
     * otherwise a new instance is created and stored for later use, before it's returned.
     * 
     * @param command the command
     * 
     * @return the instance of the command, never {@code null}
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     */
    private Command getCommandInstance(Commands command)
        throws DataAccessException, IllegalPropertyException, GitException {
        logger.debug(MAIN, "Looking up command instance '{}' from cache", command);
        if (commands.containsKey(command)) {
            logger.debug(MAIN, "Command instance '{}' found in cache", command);
            return commands.get(command);
        }
        else {
            logger.debug(MAIN, "No command instance '{}' found in cache", command);
        }
        
        Command res = newCommandInstance(command);
        commands.put(command, res);
        return res;
    }

    /**
     * Runs the given command through its {@link Command#run()}.
     * 
     * @param command the command to run
     * @@param saveStateAndSummary a boolean that, when {@code true} saves the {@link State} to the configured state file
     * ({@link Configuration#getStateFile()}), if not {@code null}, and the summary to the configured summary file
     * ({@link Configuration#getSummaryFile()}), if not {@code null}
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     */
    private void runCommand(Commands command, boolean saveStateAndSummary)
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        Objects.requireNonNull(command, "Cannot run a null command");

        logger.debug(MAIN, "Running command '{}'", command);
        Command commandInstance = getCommandInstance(command);
        if (commandInstance.isUpToDate()) {
            logger.debug(MAIN, "Command '{}' is up to date, skipping.", command.toString());
        }
        else {
            logger.debug(MAIN, "Command '{}' is not up to date, running...", command.toString());
            commandInstance.run();
            logger.debug(MAIN, "Command '{}' finished.", command.toString());

            // optionally save the state file
            if (saveStateAndSummary && !Objects.isNull(configuration().getStateFile()) && !configuration().getStateFile().isBlank()) {
                File stateFile = new File(configuration().getStateFile());
                // if the file path is relative make it relative to the configured directory
                if (!stateFile.isAbsolute())
                    stateFile = new File(configuration().getDirectory(), configuration().getStateFile());
                logger.debug(MAIN, "Storing the state to '{}'", configuration().getStateFile());
                FileMapper.save(stateFile.getAbsolutePath(), state());
                logger.debug(MAIN, "State stored to to '{}'", configuration().getStateFile());
            }

            // optionally save the summary file
            if (saveStateAndSummary && !Objects.isNull(configuration().getSummaryFile()) && !configuration().getSummaryFile().isBlank()) {
                File summaryFile = new File(configuration().getSummaryFile());
                // if the file path is relative make it relative to the configured directory
                if (!summaryFile.isAbsolute())
                summaryFile = new File(configuration().getDirectory(), configuration().getSummaryFile());
                logger.debug(MAIN, "Storing the summary to '{}'", configuration().getSummaryFile());
                try {
                    FileOutputStream fos = new FileOutputStream(summaryFile);
                    fos.write(state().summary().getBytes());
                    fos.flush();
                    fos.close();
                }
                catch (IOException ioe) {
                    throw new DataAccessException(String.format("Unable to write the summary file '%s'", configuration().getSummaryFile()), ioe);
                }

                logger.debug(MAIN, "Summary stored to to '{}'", configuration().getSummaryFile());
            }
        }
    }

    /**
     * Runs {@code true} if the given command has already run and is up to date, {@code false} otherwise.
     * 
     * @param command the command to query the status for
     * 
     * @return {@code true} if the given command has already run and is up to date, {@code false} otherwise.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     */
    public boolean isUpToDate(Commands command)
        throws DataAccessException, IllegalPropertyException, GitException {
        Objects.requireNonNull(command, "Command cannot be null");

        boolean res = commands.containsKey(command) && commands.get(command).isUpToDate();

        logger.debug(MAIN, "Checking if command '{}' is up to date: '{}'", command, res);
        return res;
    }

    /**
     * Runs the given command.
     * 
     * @param command the identifier of the command to run.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see #clean()
     * @see #infer()
     * @see #make()
     * @see #mark()
     * @see #publish()
     * @see #state()
     */
    public void run(Commands command)
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        Objects.requireNonNull(command, "Command cannot be null");
        
        logger.debug(MAIN, "Nyx.run({})", command.toString());
        switch (command) {
            case CLEAN:   clean(); break;
            case INFER:   infer(); break;
            case MAKE:    make(); break;
            case MARK:    mark(); break;
            case PUBLISH: publish(); break;
            default:      throw new IllegalArgumentException(String.format("Unknown command %", command));
        }
    }

    /**
     * Runs the {@link Clean} command to restore the state of the workspace to ints initial state.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see Clean
     * @see #run(Commands)
     */
    public void clean()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(MAIN, "Nyx.clean()");

        // this command has no dependencies

        // run the command
        runCommand(Commands.CLEAN, false);
    }

    /**
     * Runs the {@link Infer} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see Infer
     * @see #run(Commands)
     */
    public State infer()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(MAIN, "Nyx.infer()");

        // this command has no dependencies

        // run the command
        runCommand(Commands.INFER, true);

        return state();
    }

    /**
     * Runs the {@link Make} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see Make
     * @see #run(Commands)
     */
    public State make()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(MAIN, "Nyx.make()");

        // run dependent tasks first
        infer();

        // run the command
        runCommand(Commands.MAKE, true);

        return state();
    }

    /**
     * Runs the {@link Mark} command and returns the updated state.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see Mark
     * @see #run(Commands)
     */
    public State mark()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(MAIN, "Nyx.mark()");

        // run dependent tasks first
        make();

        // run the command
        runCommand(Commands.MARK, true);

        return state();
    }

    /**
     * Runs the {@link Publish} command and returns the updated state. Dependencies of this command are also executed first.
     * 
     * @return the same state object reference returned by {@link #state()}, which might have been updated by this command
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws GitException in case of unexpected issues when accessing the Git repository.
     * @throws ReleaseException if the task is unable to complete for reasons due to the release process.
     * 
     * @see Publish
     * @see #run(Commands)
     */
    public State publish()
        throws DataAccessException, IllegalPropertyException, GitException, ReleaseException {
        logger.debug(MAIN, "Nyx.publish()");

        // run dependent tasks first
        mark();

        // run the command
        runCommand(Commands.PUBLISH, true);

        return state();
    }
}