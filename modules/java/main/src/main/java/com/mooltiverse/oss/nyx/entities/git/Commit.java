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
package com.mooltiverse.oss.nyx.entities.git;

import java.io.Serializable;
import java.util.Collections;
import java.util.Objects;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This object is a Git commit value holder independent from the underlying Git implementation.
 */
public class Commit implements Comparable<Commit>, Cloneable, Serializable {
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
    private final long date;

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
     * @param authorAction the value holder about the author. Cannot be {@code null}
     * @param commitAction the value holder about the committer. Cannot be {@code null}
     * @param message the commit message. Cannot be {@code null}
     * @param tags the tags applied to this commit. If the commit has no tags an empty,
     * non {@code null} set, must be passed
     */
    @JsonCreator
    public Commit(@JsonProperty("sha") String sha, @JsonProperty("date") long date, @JsonProperty("parents") List<String> parents, @JsonProperty("authorAction") Action authorAction, @JsonProperty("commitAction") Action commitAction, @JsonProperty("message") Message message, @JsonProperty("tags") Set<Tag> tags) {
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
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return 41 * sha.hashCode() * 37 * Long.valueOf(date).intValue() * 31 * parents.hashCode() * 29 * authorAction.hashCode() * 23 * commitAction.hashCode() * 19 * message.hashCode() * 17 * tags.hashCode();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (this == obj)
            return true;
        if (!this.getClass().isInstance(obj))
            return false;

        Commit other = Commit.class.cast(obj);
        return getSHA().equals(other.getSHA()) && getDate() == other.getDate() && getParents().equals(other.getParents()) && getAuthorAction().equals(other.getAuthorAction()) && getCommitAction().equals(other.getCommitAction()) && getMessage().equals(other.getMessage()) && getTags().equals(other.getTags());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int compareTo(Commit c) {
        if (c == null)
            return 1;

        if (getSHA().compareTo(c.getSHA()) == 0) {
            if (getDate() == c.getDate()) {
                if (getParents().size() == c.getParents().size()) {
                    if (getAuthorAction().compareTo(c.getAuthorAction()) == 0) {
                        if (getCommitAction().compareTo(c.getCommitAction()) == 0) {
                            if (getMessage().compareTo(c.getMessage()) == 0) {
                                return getTags().size()-c.getTags().size();
                            }
                            else return getMessage().compareTo(c.getMessage());
                        }
                        else return getCommitAction().compareTo(c.getCommitAction());
                    }
                    else return getAuthorAction().compareTo(c.getAuthorAction());
                }
                else return getParents().size() - c.getParents().size();
            }
            else return Long.valueOf(getDate()).intValue() - Long.valueOf(c.getDate()).intValue();
        }
        else return getSHA().compareTo(c.getSHA());
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
    public long getDate() {
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
