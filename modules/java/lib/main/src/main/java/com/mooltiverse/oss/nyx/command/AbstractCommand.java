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
package com.mooltiverse.oss.nyx.command;

import static com.mooltiverse.oss.nyx.log.Markers.COMMAND;

import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mooltiverse.oss.nyx.git.local.Repository;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The common superclass for Nyx commands.
 * 
 * This class is not meant to be used in multi-threaded environments.
 * 
 * All implementing classes must have a public constructor that accept a {@link State} and a {@link Repository} parameter.
 */
public abstract class AbstractCommand implements Command {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(AbstractCommand.class);

    /**
     * The private instance of the Git repository.
     */
    private final Repository repository;

    /**
     * The private instance of the state.
     */
    private final State state;

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    protected AbstractCommand(State state, Repository repository) {
        super();
        Objects.requireNonNull(state, "The State object cannot be null");
        Objects.requireNonNull(repository, "The Repository object cannot be null");
        this.state = state;
        this.repository = repository;
        logger.debug(COMMAND, "New command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final State state() {
        return state;
    }

    /**
     * Returns the repository object.
     * 
     * @return the repository object.
     */
    public final Repository repository() {
        return repository;
    }
}