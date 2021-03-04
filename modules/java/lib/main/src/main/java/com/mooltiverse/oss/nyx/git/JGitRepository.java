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

import java.util.Objects;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.JGitInternalException;
import org.eclipse.jgit.errors.NoWorkTreeException;
import org.eclipse.jgit.errors.RevisionSyntaxException;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevSort;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

/**
 * A local repository implementation that encapsulates the backing <a href="https://www.eclipse.org/jgit/">JGit</a> library.
 */
class JGitRepository implements Repository {
    /**
     * The {@code GIT} marker, used when logging events.
     */
    private static Marker GIT = MarkerFactory.getMarker("GIT");

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
     * @throws IllegalArgumentException if the given object is illegal for some reason, like referring to an illegal repository
     * @throws IOException in case of any I/O issue accessing the repository
     */
    private JGitRepository(Git jGit)
        throws IOException {
        super();
        Objects.requireNonNull(jGit, "Can't create a repository instance with a null backing JGit object");
        this.jGit = jGit;
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

        logger.debug(GIT, "Opening repository in directory {}", directory.getAbsolutePath());

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
        RevWalk rw = new RevWalk(jGit.getRepository());
        try {
            ObjectId start = jGit.getRepository().resolve(startFrom);
            if (Objects.isNull(start))
                throw new GitException(String.format("Repository cannot resolve %s. Unable to peek the commit.", startFrom));

            RevCommit base = rw.parseCommit(start);
            if (!Objects.isNull(sort))
                rw.sort(sort);
            rw.markStart(base);
            return rw.next();
        }
        catch (RevisionSyntaxException | JGitInternalException | IOException e) {
            throw new GitException(e);
        }
        finally {
            rw.close();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getLatestCommit()
        throws GitException {
        String commitSHA = peekCommit(Constants.HEAD, null).getName();
        logger.debug(GIT, "Repository root commit is {}", commitSHA);
        return commitSHA;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getRootCommit()
        throws GitException {
        String commitSHA = peekCommit(Constants.HEAD, RevSort.REVERSE).getName();
        logger.debug(GIT, "Repository root commit is {}", commitSHA);
        return commitSHA;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isClean()
        throws GitException {
        try {
            return jGit.status().call().isClean();
        }
        catch (GitAPIException | NoWorkTreeException e) {
            throw new GitException(e);
        }
    }
}
