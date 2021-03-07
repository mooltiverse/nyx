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
package com.mooltiverse.oss.nyx.data;

import java.util.Collections;
import java.util.Objects;
import java.util.List;
import java.util.Set;

/**
 * This object is a Git commit value holder independent from the underlying Git implementation.
 */
public class Commit {
    /**
     * The author data
     */
    private final Action authorAction;

    /**
     * The committer data
     */
    private final Action commitAction;

    /**
     * The commit date.
     */
    private final int date;

    /**
     * The commit message
     */
    private final Message message;

    /**
     * The parents SHA's.
     */
    private final List<String> parents;

    /**
     * The tags associated to the commit.
     */
    private final Set<Tag> tags;

    /**
     * The commit SHA-1 identifier.
     */
    private final String sha;

    /**
     * Constructor.
     * 
     * @param sha the commit SHA-1 identifier. Cannot be {@code null}
     * @param date the commit date
     * @param parents the SHA-1 identifiers of parent commits. If the commit has no parents an empty,
     * non {@code null} list, must be passed
     * @param commitAction the value holder about the author. Cannot be {@code null}
     * @param commitAction the value holder about the committer. Cannot be {@code null}
     * @oaram message the commit message. Cannot be {@code null}
     * @param tags the tags applied to this commit. If the commit has no tags an empty,
     * non {@code null} set, must be passed
     */
    public Commit(String sha, int date, List<String> parents, Action authorAction, Action commitAction, Message message, Set<Tag> tags) {
        super();
        Objects.requireNonNull(sha);
        Objects.requireNonNull(parents);
        Objects.requireNonNull(authorAction);
        Objects.requireNonNull(commitAction);
        Objects.requireNonNull(message);
        Objects.requireNonNull(tags);
        this.sha = sha;
        this.date = date;
        this.parents = Collections.unmodifiableList(parents);
        this.authorAction = authorAction;
        this.commitAction = commitAction;
        this.message = message;
        this.tags = Collections.unmodifiableSet(tags);
    }

    /**
     * Returns the author data.
     * 
     * @return the author data. Never {@code null}.
     */
    public Action getAuthorAction() {
        return authorAction;
    }

    /**
     * Returns the committer data.
     * 
     * @return the committer data. Never {@code null}.
     */
    public Action getCommitAction() {
        return commitAction;
    }

    /**
     * Returns the commit date.
     * 
     * @return the commit date.
     */
    public int getDate() {
        return date;
    }

    /**
     * Returns the commit message.
     * 
     * @return the commit message. Never {@code null}.
     */
    public Message getMessage() {
        return message;
    }

    /**
     * Returns the immutable list of parent commit SHA-1 identifiers.
     * 
     * @return the immutable list of parent commit SHA-1 identifiers. May be empty but not {@code null}.
     */
    public List<String> getParents() {
        return parents;
    }

    /**
     * Returns the immutable list of tags pointing to this commit.
     * 
     * @return the immutable list of tags pointing to this commit. May be empty but not {@code null}.
     */
    public Set<Tag> getTags() {
        return tags;
    }

    /**
     * Returns the SHA-1 identifier for the commit.
     * 
     * @return the SHA-1 identifier for the commit. Never {@code null}.
     */
    public String getSHA() {
        return sha;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return sha;
    }
}
