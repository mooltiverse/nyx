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

import com.mooltiverse.oss.nyx.version.Version;

/**
 * This is a value object that models the summary data about the scope of a release.
 */
public class ReleaseScope {
    /**
     * The SHA-1 identifier of the first commit within the scope.
     */
    private String initialCommit = null;

    /**
     * The SHA-1 identifier of the last commit within the scope.
     */
    private String finalCommit = null;

    /**
     * The SHA-1 identifier of the most recent past release commit.
     */
    private String previousVersionCommit = null;

    /**
     * The version identifier of the most recent past release.
     */
    private Version previousVersion = null;

    /**
     * Default constructor.
     */
    public ReleaseScope() {
        super();
    }

    /**
     * Constructor.
     * 
     * @param previousVersion the version identifier of the most recent past release. It may be {@code null}.
     * @param previousVersionCommit the SHA-1 identifier of the most recent past release commit. It may be {@code null}.
     * @param initialCommit the SHA-1 identifier of the first commit within the scope. It may be {@code null}.
     * @param finalCommit the SHA-1 identifier of the last commit within the scope. It may be {@code null}.
     */
    public ReleaseScope(Version previousVersion, String previousVersionCommit, String initialCommit, String finalCommit) {
        super();
        this.previousVersion = previousVersion;
        this.previousVersionCommit = previousVersionCommit;
        this.initialCommit = initialCommit;
        this.finalCommit = finalCommit;
    }

    /**
     * Returns the version identifier of the most recent past release.
     * 
     * @return the version identifier of the most recent past release. It may be {@code null}.
     */
    public Version getPreviousVersion() {
        return previousVersion;
    }

    /**
     * Sets the version identifier of the most recent past release.
     * 
     * @param previousVersion the version identifier of the most recent past release. It may be {@code null}.
     */
    public void setPreviousVersion(Version previousVersion) {
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
        return initialCommit;
    }

    /**
     * Sets the SHA-1 identifier of the first commit within the scope.
     * 
     * @param initialCommit the SHA-1 identifier of the first commit within the scope. It may be {@code null}.
     */
    public void setInitialCommit(String initialCommit) {
        this.initialCommit = initialCommit;
    }

    /**
     * Returns the SHA-1 identifier of the last commit within the scope.
     * 
     * @return the SHA-1 identifier of the last commit within the scope. It may be {@code null}.
     */
    public String getFinalCommit() {
        return finalCommit;
    }

    /**
     * Sets the SHA-1 identifier of the last commit within the scope.
     * 
     * @param finalCommit the SHA-1 identifier of the last commit within the scope. It may be {@code null}.
     */
    public void setFinalCommit(String finalCommit) {
        this.finalCommit = finalCommit;
    }
}
