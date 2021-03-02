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
package com.mooltiverse.oss.nyx.git.local;

import java.io.File;
import java.io.IOException;

import java.util.Iterator;
import java.util.Objects;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.JGitInternalException;
import org.eclipse.jgit.errors.NoWorkTreeException;
import org.eclipse.jgit.errors.RevisionSyntaxException;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevCommit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

import com.mooltiverse.oss.nyx.git.GitException;

/**
 * A local repository implementation that encapsulates the backing <a href="https://www.eclipse.org/jgit/">JGit</a> library.
 * 
 * This class just provide coarse grained methods functional to Nyx purposes.
 */
public class Repository {
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
    private Repository(Git jGit)
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
    public static Repository open(File directory)
        throws IOException {
        Objects.requireNonNull(directory, "Can't create a repository instance with a null directory");

        logger.debug(GIT, "Opening repository in directory {}", directory.getAbsolutePath());

        return new Repository(Git.open(directory));
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
    public static Repository open(String directory) 
        throws IOException {
        Objects.requireNonNull(directory, "Can't create a repository instance with a null directory");
        if (directory.isBlank())
            throw new IllegalArgumentException("Can't create a repository instance with a blank directory");

        return open(new File(directory));
    }

    /**
     * Returns the SHA-1 identifier of the last commit in the current branch.
     * 
     * @return the SHA-1 identifier of the last commit in the current branch or {@code code} if the repository has no commits yet.
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository
     */
    public String getLatestCommit()
        throws GitException {
        try {
            ObjectId head = jGit.getRepository().resolve(Constants.HEAD);
            if (Objects.isNull(head)) {
                logger.debug(GIT, "Repository cannot resolve HEAD. Unable to find the latest commit.");
                return null;
            }
            Iterator<RevCommit> iterator = jGit.log().add(head).setMaxCount(1).call().iterator();
            if (iterator.hasNext()) {
                String commitSHA = iterator.next().getName();
                logger.debug(GIT, "Repository latest commit is {}", commitSHA);
                return commitSHA;
            }
            else {
                logger.debug(GIT, "Repository has no commits.");
                return null;
            }
        }
        catch (RevisionSyntaxException | JGitInternalException | GitAPIException | IOException e) {
            throw new GitException(e);
        }
    }

    /**
     * Returns {@code true} if the repository is clean, which is when no differences exist between the working tree, the index,
     * and the current {@code HEAD}.
     * 
     * @return {@code true} if the repository is clean, {@code false} otherwise.
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository
     */
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
