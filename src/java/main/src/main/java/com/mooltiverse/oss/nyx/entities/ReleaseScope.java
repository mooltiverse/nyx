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
package com.mooltiverse.oss.nyx.entities;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.mooltiverse.oss.nyx.entities.git.Commit;

/**
 * This is a value object that models the summary data about the scope of a release.
 */
public class ReleaseScope {
    /**
     * The internal list of commits in the scope.
     * Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
     */
    private final List<Commit> commits = new ArrayList<Commit>();

    /**
     * The version identifier of the most recent past release.
     */
    private String previousVersion = null;

    /**
     * The most recent past release commit.
     */
    private Commit previousVersionCommit = null;

    /**
     * The version identifier of the most recent past release with only core identifiers.
     */
    private String primeVersion = null;

    /**
     * The most recent past release commit with only core identifiers.
     */
    private Commit primeVersionCommit = null;

    /**
     * The list of significant commits (those commits causing the version number to be bumped).
     * Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
     */
    private final List<Commit> significantCommits = new ArrayList<Commit>();

    /**
     * Default constructor.
     */
    public ReleaseScope() {
        super();
    }

    /**
     * Returns the list of commits in the scope.
     * Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
     * 
     * @return the list of commits in the scope.
     */
    public List<Commit> getCommits() {
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
     * Returns {@code true} if the scope has a non {@code null} most recent past release.
     * 
     * @return {@code true} if the scope has a non {@code null} most recent past release. It may be {@code null}.
     */
    public boolean hasPreviousVersion() {
        return !Objects.isNull(previousVersion);
    }

    /**
     * Sets the most recent past release.
     * 
     * @param previousVersion the most recent past release. It may be {@code null}.
     */
    public void setPreviousVersion(String previousVersion) {
        this.previousVersion = previousVersion;
    }

    /**
     * Returns the most recent past release commit.
     * 
     * @return the most recent past release commit. It may be {@code null}.
     */
    public Commit getPreviousVersionCommit() {
        return previousVersionCommit;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} most recent past release commit.
     * 
     * @return {@code true} if the scope has a non {@code null} most recent past release commit.
     */
    public boolean hasPreviousVersionCommit() {
        return !Objects.isNull(previousVersionCommit);
    }

    /**
     * Sets the most recent past release commit.
     * 
     * @param previousVersionCommit the most recent past release commit. It may be {@code null}.
     */
    public void setPreviousVersionCommit(Commit previousVersionCommit) {
        this.previousVersionCommit = previousVersionCommit;
    }

    /**
     * Returns the version identifier of the most recent past release with only core identifiers.
     * 
     * @return the version identifier of the most recent past release with only core identifiers.
     * It may be {@code null}.
     */
    public String getPrimeVersion() {
        return primeVersion;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} most recent past release
     * with only core identifiers.
     * 
     * @return {@code true} if the scope has a non {@code null} most recent past release
     * with only core identifiers. It may be {@code null}.
     */
    public boolean hasPrimeVersion() {
        return !Objects.isNull(primeVersion);
    }

    /**
     * Sets the version identifier of the most recent past release with only core identifiers.
     * 
     * @param primeVersion the version identifier of the most recent past release with only core identifiers.
     * It may be {@code null}.
     */
    public void setPrimeVersion(String primeVersion) {
        this.primeVersion = primeVersion;
    }

    /**
     * Returns the most recent past release commit with only core identifiers.
     * 
     * @return the most recent past release commit with only core identifiers.
     * It may be {@code null}.
     */
    public Commit getPrimeVersionCommit() {
        return primeVersionCommit;
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} most recent past release commit
     * with only core identifiers.
     * 
     * @return {@code true} if the scope has a non {@code null} most recent past release commit
     * with only core identifiers.
     */
    public boolean hasPrimeVersionCommit() {
        return !Objects.isNull(primeVersionCommit);
    }

    /**
     * Sets the most recent past release commit with only core identifiers.
     * 
     * @param primeVersionCommit the most recent past release commit with only core identifiers.
     * It may be {@code null}.
     */
    public void setPrimeVersionCommit(Commit primeVersionCommit) {
        this.primeVersionCommit = primeVersionCommit;
    }

    /**
     * Returns the first commit within the scope.
     * 
     * @return the first commit within the scope. It may be {@code null}.
     */
    public Commit getInitialCommit() {
        return commits.isEmpty() ? null : commits.get(commits.size()-1);
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} first commit within the scope.
     * 
     * @return {@code true} if the scope has a non {@code null} first commit within the scope.
     */
    public boolean hasInitialCommit() {
        return !commits.isEmpty();
    }

    /**
     * Returns the last commit within the scope.
     * 
     * @return the last commit within the scope. It may be {@code null}.
     */
    public Commit getFinalCommit() {
        return commits.isEmpty() ? null : commits.get(0);
    }

    /**
     * Returns {@code true} if the scope has a non {@code null} last commit within the scope.
     * 
     * @return {@code true} if the scope has a non {@code null} last commit within the scope.
     * It may be {@code null}.
     */
    public boolean hasFinalCommit() {
        return !commits.isEmpty();
    }

    /**
     * Returns the list of significant commits (those commits causing the version number to be bumped).
     * Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
     * 
     * @return the list of significant commits (those commits causing the version number to be bumped).
     */
    public List<Commit> getSignificantCommits() {
        return significantCommits;
    }
}
