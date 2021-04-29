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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * This is a value object that models the summary data about the scope of a release.
 */
public class ReleaseScope {
    /**
     * The internal list of SHA-1 identifiers of all commits in the scope. The items are in reverse
     * order so the newest commit is at position 0 and the oldest is in the final position.
     */
    private final List<String> commits = new ArrayList<String>();

    /**
     * The SHA-1 identifier of the most recent past release commit.
     */
    private String previousVersionCommit = null;

    /**
     * The version identifier of the most recent past release.
     */
    private String previousVersion = null;

    /**
     * Default constructor.
     */
    public ReleaseScope() {
        super();
    }

    /**
     * Returns the live list of SHA-1 identifiers of all commits in the scope. The items are in reverse
     * order so the newest commit is at position 0 and the oldest is in the final position.
     * 
     * @return the live list of SHA-1 identifiers of all commits in the scope.
     */
    public List<String> getCommits() {
        return commits;
    }

    /**
     * Returns the version identifier of the most recent past release.
     * 
     * @return the version identifier of the most recent past release. It may be {@code null}.
     */
    public String getPreviousVersion() {
        return previousVersion;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} version identifier of the most recent past release.
     * 
     * @return {@code true} if the scope has a non {@code null} version identifier of the most recent past release. It may be {@code null}.
     */
    public boolean hasPreviousVersion() {
        return !Objects.isNull(previousVersion);
    }

    /**
     * Sets the version identifier of the most recent past release.
     * 
     * @param previousVersion the version identifier of the most recent past release. It may be {@code null}.
     */
    public void setPreviousVersion(String previousVersion) {
        this.previousVersion = previousVersion;
    }

    /**
     * Returns the SHA-1 identifier of the most recent past release commit.
     * 
     * @return the SHA-1 identifier of the most recent past release commit. It may be {@code null}.
     */
    public String getPreviousVersionCommit() {
        return previousVersionCommit;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} SHA-1 identifier of the most recent past release commit.
     * 
     * @return {@code true} if the scope has a non {@code null} SHA-1 identifier of the most recent past release commit.
     */
    public boolean hasPreviousVersionCommit() {
        return !Objects.isNull(previousVersionCommit);
    }

    /**
     * Sets the SHA-1 identifier of the most recent past release commit.
     * 
     * @param previousVersionCommit the SHA-1 identifier of the most recent past release commit. It may be {@code null}.
     */
    public void setPreviousVersionCommit(String previousVersionCommit) {
        this.previousVersionCommit = previousVersionCommit;
    }

    /**
     * Returns the SHA-1 identifier of the first commit within the scope.
     * 
     * @return the SHA-1 identifier of the first commit within the scope. It may be {@code null}.
     */
    public String getInitialCommit() {
        return commits.isEmpty() ? null : commits.get(commits.size()-1);
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} SHA-1 identifier of the first commit within the scope.
     * 
     * @return {@code true} if the scope has a non {@code null} SHA-1 identifier of the first commit within the scope.
     */
    public boolean hasInitialCommit() {
        return !commits.isEmpty();
    }

    /**
     * Returns the SHA-1 identifier of the last commit within the scope.
     * 
     * @return the SHA-1 identifier of the last commit within the scope. It may be {@code null}.
     */
    public String getFinalCommit() {
        return commits.isEmpty() ? null : commits.get(0);
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} SHA-1 identifier of the last commit within the scope.
     * 
     * @return {@code true} if the scope has a non {@code null} SHA-1 identifier of the last commit within the scope. It may be {@code null}.
     */
    public boolean hasFinalCommit() {
        return !commits.isEmpty();
    }

    /**
     * Returns the flag telling if the release scope contains significant commits.
     * 
     * @return the flag telling if the release scope contains significant commits. It may be {@code null}.
     */
    public Boolean getSignificant() {
        return !commits.isEmpty();
    }
}
