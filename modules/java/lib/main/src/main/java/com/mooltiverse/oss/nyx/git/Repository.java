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

import java.util.Collection;
import java.util.Set;

import com.mooltiverse.oss.nyx.entities.git.Commit;
import com.mooltiverse.oss.nyx.entities.git.Identity;
import com.mooltiverse.oss.nyx.entities.git.Tag;

/**
 * This interface models coarse grained, implementation independent methods used by Nyx to access a Git repository.
 */
public interface Repository {
    /**
     * Adds the given paths to the staging area.
     * 
     * @param paths the file patterns of the contents to add to stage. Cannot be {@code null} or empty. The path "{@code .}" represents
     * all files in the working area so with that you can add all locally changed files.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to add paths.
     * 
     * @see #commit(String)
     * @see #commit(String, Identity, Identity)
     * @see #commit(Collection, String)
     * @see #commit(Collection, String, Identity, Identity)
     */
    public void add(Collection<String> paths)
        throws GitException;

    /**
     * Commits changes to the repository. Files to commit must be staged separately using {@link #add(Collection)}.
     * 
     * @param message the commit message. Cannot be {@code null}.
     * 
     * @return the object modelling the new commit that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to commit.
     * 
     * @see #add(Collection)
     */
    public Commit commit(String message)
        throws GitException;

    /**
     * Commits changes to the repository. Files to commit must be staged separately using {@link #add(Collection)}.
     * 
     * @param message the commit message. Cannot be {@code null}.
     * @param author the object modelling the commit author informations. It may be {@code null}, in which case the default
     * for the repository will be used
     * @param committer the object modelling the committer informations. It may be {@code null}, in which case the default
     * for the repository will be used
     * 
     * @return the object modelling the new commit that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to commit.
     * 
     * @see #add(Collection)
     */
    public Commit commit(String message, Identity author, Identity committer)
        throws GitException;

    /**
     * Adds the given files to the staging area and commits changes to the repository. This method is a shorthand
     * for {@link #add(Collection)} and {@link #commit(String)}.
     * 
     * @param paths the file patterns of the contents to add to stage. Cannot be {@code null} or empty. The path "{@code .}" represents
     * all files in the working area so with that you can add all locally changed files.
     * @param message the commit message. Cannot be {@code null}.
     * 
     * @return the object modelling the new commit that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to commit.
     */
    public Commit commit(Collection<String> paths, String message)
        throws GitException;

    /**
     * Adds the given files to the staging area and commits changes to the repository. This method is a shorthand
     * for {@link #add(Collection)} and {@link #commit(String, Identity, Identity)}.
     * 
     * @param paths the file patterns of the contents to add to stage. Cannot be {@code null} or empty. The path "{@code .}" represents
     * all files in the working area so with that you can add all locally changed files.
     * @param message the commit message. Cannot be {@code null}.
     * @param author the object modelling the commit author informations. It may be {@code null}, in which case the default
     * for the repository will be used
     * @param committer the object modelling the committer informations. It may be {@code null}, in which case the default
     * for the repository will be used
     * 
     * @return the object modelling the new commit that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to commit.
     */
    public Commit commit(Collection<String> paths, String message, Identity author, Identity committer)
        throws GitException;

    /**
     * Pushes local changes in the current branch to the default remote {@code origin}.
     * 
     * @return the local name of the remotes that has been pushed
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to push.
     */
    public String push()
        throws GitException;

    /**
     * Pushes local changes in the current branch to the given remote.
     * 
     * @param remote the name of the remote to push to. If {@code null} or empty the default remote name ({@code origin})
     * is used.
     * 
     * @return the local name of the remotes that has been pushed
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to push.
     */
    public String push(String remote)
        throws GitException;

    /**
     * Pushes local changes in the current branch to the given remotes.
     * 
     * @param remotes the names of remotes to push to. If {@code null} or empty the default remote name ({@code origin})
     * is used.
     * 
     * @return a collection with the local names of remotes that have been pushed
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to push.
     */
    public Set<String> push(Collection<String> remotes)
        throws GitException;

    /**
     * Tags the latest commit in the current branch with a tag with the given name. The resulting tag is lightweight.
     * 
     * @param name the name of the tag. Cannot be {@code null}
     * 
     * @return the object modelling the new tag that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to tag
     * (i.e. when the tag name is {@code null} or there is already a tag with the given name in the repository).
     */
    public Tag tag(String name)
        throws GitException;

    /**
     * Tags the latest commit in the current branch with a tag with the given name and optional message.
     * 
     * @param name the name of the tag. Cannot be {@code null}
     * @param message the optional tag message. If {@code null} the new tag will be lightweight, otherwise it will be an
     * annotated tag
     * 
     * @return the object modelling the new tag that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to tag
     * (i.e. when the tag name is {@code null} or there is already a tag with the given name in the repository).
     */
    public Tag tag(String name, String message)
        throws GitException;

    /**
     * Tags the latest commit in the current branch with a tag with the given name and optional message using the optional
     * tagger identity.
     * 
     * @param name the name of the tag. Cannot be {@code null}
     * @param message the optional tag message. If {@code null} the new tag will be lightweight, otherwise it will be an
     * annotated tag
     * @param tagger the optional identity of the tagger. If {@code null} Git defaults are used. If {@code message} is {@code null}
     * this is ignored.
     * 
     * @return the object modelling the new tag that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to tag
     * (i.e. when the tag name is {@code null} or there is already a tag with the given name in the repository).
     */
    public Tag tag(String name, String message, Identity tagger)
        throws GitException;

    /**
     * Tags the object represented by the given SHA-1 with a tag with the given name and optional message using the optional
     * tagger identity.
     * 
     * @param target the SHA-1 identifier of the object to tag. If {@code null} the latest commit in the current branch is tagged.
     * @param name the name of the tag. Cannot be {@code null}
     * @param message the optional tag message. If {@code null} the new tag will be lightweight, otherwise it will be an
     * annotated tag
     * @param tagger the optional identity of the tagger. If {@code null} Git defaults are used. If {@code message} is {@code null}
     * this is ignored.
     * 
     * @return the object modelling the new tag that was created. Never {@code null}.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, preventing to tag
     * (i.e. when the tag name is {@code null} or there is already a tag with the given name in the repository).
     */
    public Tag tag(String target, String name, String message, Identity tagger)
        throws GitException;

    /**
     * Returns the name of the current branch or a commit SHA-1 if the repository is in the detached head state.
     * 
     * @return the name of the current branch or a commit SHA-1 if the repository is in the detached head state.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, including when
     * the repository has no commits yet or is in the 'detached HEAD' state.
     */
    public String getCurrentBranch()
        throws GitException;

    /**
     * Returns the SHA-1 identifier of the last commit in the current branch.
     * 
     * @return the SHA-1 identifier of the last commit in the current branch.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, including when
     * the repository has no commits yet or is in the 'detached HEAD' state.
     */
    public String getLatestCommit()
        throws GitException;

    /**
     * Returns the SHA-1 identifier of the first commit in the repository (the only commit with no parents).
     * 
     * @return the SHA-1 identifier of the first commit in the repository.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, including when
     * the repository has no commits yet or is in the 'detached HEAD' state.
     */
    public String getRootCommit()
        throws GitException;

    /**
     * Returns a set of abjects representing all the tags for the given commit.
     * 
     * @param commit the SHA-1 identifier of the commit to get the tags for. It can be a full or abbreviated SHA-1.
     * 
     * @return the set of abjects representing all the tags for the given commit.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository.
     */
    public Set<Tag> getCommitTags(String commit)
        throws GitException;

    /**
     * Returns {@code true} if the repository is clean, which is when no differences exist between the working tree, the index,
     * and the current {@code HEAD}.
     * 
     * @return {@code true} if the repository is clean, {@code false} otherwise.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, including when
     * the repository has no commits yet or is in the 'detached HEAD' state.
     */
    public boolean isClean()
        throws GitException;

    /**
     * Browse the repository commit history using the given {@code visitor} to inspect each commit. Commits are
     * evaluated in Git's natural order, from the most recent to oldest.
     * 
     * @param start the optional SHA-1 id of the commit to start from. If {@code null} the latest commit in the
     * current branch ({@code HEAD}) is used. This can be a long or abbreviated SHA-1. If this commit cannot be
     * resolved within the repository a {@link GitException} is thrown.
     * @param end the optional SHA-1 id of the commit to end with, included. If {@code null} the repository root
     * commit is used (until the given {@code visitor} returns {@code false}). If this commit is not reachable
     * from the start it will be ignored. This can be a long or abbreviated SHA-1. If this commit cannot be resolved
     * within the repository a {@link GitException} is thrown.
     * @param visitor the visitor function that will receive commit data to evaluate. If {@code null} this method
     * takes no action.
     * 
     * @throws GitException in case some problem is encountered with the underlying Git repository, including when
     * the repository has no commits yet or a given commit identifier cannot be resolved.
     */
    public void walkHistory(String start, String end, CommitVisitor visitor)
        throws GitException;
}
