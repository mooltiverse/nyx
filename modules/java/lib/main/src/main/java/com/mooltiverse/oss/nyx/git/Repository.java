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

import java.util.Set;

import com.mooltiverse.oss.nyx.data.Tag;

/**
 * This interface models coarse grained, implementation independent methods used by Nyx to access a Git repository.
 */
public interface Repository {
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
}
