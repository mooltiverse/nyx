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

import com.mooltiverse.oss.nyx.RepositoryException;
import com.mooltiverse.oss.nyx.data.DataAccessException;
import com.mooltiverse.oss.nyx.data.IllegalPropertyException;
import com.mooltiverse.oss.nyx.git.local.Repository;
import com.mooltiverse.oss.nyx.state.State;

/**
 * The Infer command takes care of inferring and computing informations in order to make a new release.
 * 
 * This class is not meant to be used in multi-threaded environments.
 */
public class Infer extends AbstractCommand {
    /**
     * The private logger instance
     */
    private static final Logger logger = LoggerFactory.getLogger(Infer.class);

    /**
     * The name used for the internal state attribute where we store the SHA-1 of the last
     * commit in the current branch by the time this command was last executed.
     * 
     * The name is prefixed with this class name to avoid clashes with other attributes.
     * 
     * @see State#getInternals()
     */
    private static final String INTERNAL_LAST_COMMIT = Infer.class.getSimpleName().concat(".").concat("last").concat(".").concat("commit");

    /**
     * Standard constructor.
     * 
     * @param state the state reference
     * @param repository the repository reference
     * 
     * @throws NullPointerException if a given argument is {@code null}
     */
    public Infer(State state, Repository repository) {
        super(state, repository);
        logger.debug(COMMAND, "New Infer command object");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isUpToDate()
        throws DataAccessException, IllegalPropertyException, RepositoryException {
        // this command is considered up to date only when the repository is clean and the latest
        // commit (there must be at least one) didn't change
        return isRepositoryClean() && !Objects.isNull(state().getInternals().get(INTERNAL_LAST_COMMIT)) && state().getInternals().get(INTERNAL_LAST_COMMIT).equals(getlatestCommit());
    }

    /**
     * This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
     * of the {@link #isUpToDate()} method can find them and determine if the command is already up to date.
     * 
     * This method is meant to be invoked at the end of a succesful {@link #run()}.
     * 
     * @throws DataAccessException in case the configuration can't be loaded for some reason.
     * @throws IllegalPropertyException in case the configuration has some illegal options.
     * @throws RepositoryException in case of unexpected issues when accessing the Git repository.
     * 
     * @see #isUpToDate()
     * @see State#getInternals()
     */
    private void storeStatusAttributes()
        throws DataAccessException, IllegalPropertyException, RepositoryException {
        // store the last commit SHA-1
        String latestCommit = getlatestCommit();
        if (!Objects.isNull(latestCommit))
            state().getInternals().put(INTERNAL_LAST_COMMIT, latestCommit);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public State run()
        throws DataAccessException, IllegalPropertyException, RepositoryException {
        // TODO: implement this method
        // the following are just temporary smoke detection outputs
        logger.info(COMMAND, "Infer.run()");

        storeStatusAttributes();
        return state();
    }
}