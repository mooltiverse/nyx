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

/**
 * This interface models coarse grained, implementation independent methods used by Nyx to access a Git repository.
 */
public interface Repository {
    /**
     * Returns the SHA-1 identifier of the last commit in the current branch.
     * 
     * @return the SHA-1 identifier of the last commit in the current branch.
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository, including when
     * the repository has no commits yet.
     */
    public String getLatestCommit()
        throws GitException;

    /**
     * Returns the SHA-1 identifier of the first commit in the repository (the only commit with no parents).
     * 
     * @return the SHA-1 identifier of the first commit in the repository.
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository, including when
     * the repository has no commits yet.
     */
    public String getRootCommit()
        throws GitException;

    /**
     * Returns {@code true} if the repository is clean, which is when no differences exist between the working tree, the index,
     * and the current {@code HEAD}.
     * 
     * @return {@code true} if the repository is clean, {@code false} otherwise.
     * 
     * @throws GitException in case some propblem is encountered with the underlying Git repository
     */
    public boolean isClean()
        throws GitException;
}
