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
package com.mooltiverse.oss.nyx.configuration.presets;

import java.util.List;

import com.mooltiverse.oss.nyx.entities.Identifier;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.WorkspaceStatus;

/**
 * This class provides reusable configuration chunks for release types.
 */
public class ReleaseTypes {
    /**
     * The release type used for feature branches
     */
    public static final ReleaseType FEATURE = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.FALSE.toString());
            setGitTag(Boolean.FALSE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(null);
            setPublish(Boolean.FALSE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used for fix branches
     */
    public static final ReleaseType FIX = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-fix(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.FALSE.toString());
            setGitTag(Boolean.FALSE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^fix((-|\\/)[0-9a-zA-Z-_]+)?$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.FALSE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used for hotfix branches
     */
    public static final ReleaseType HOTFIX = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-hotfix(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^hotfix((-|\\/)[0-9a-zA-Z-_]+)?$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.TRUE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used for integration branches
     */
    public static final ReleaseType INTEGRATION = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(develop|development|integration|latest)(\\.([0-9]\\d*))?)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^(develop|development|integration|latest)$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.TRUE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The fallback release type used for releases not fitting other, more specific, types
     */
    public static final ReleaseType INTERNAL = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("internal");
            setDescription(null);
            setFilterTags(null);
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.FALSE.toString());
            setGitTag(Boolean.FALSE.toString());
            setGitTagMessage(null);
            setIdentifiers(List.<Identifier>of(new Identifier("timestamp", "{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}", Identifier.Position.BUILD)));
            setMatchBranches(null);
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(null);
            setPublish(Boolean.FALSE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used to issue official releases from the main branch.
     */
    public static final ReleaseType MAINLINE = new ReleaseType() {
        {
            setCollapseVersions(Boolean.FALSE);
            setCollapsedVersionQualifier(null);
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^(master|main)$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.TRUE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used for maintenance branches
     */
    public static final ReleaseType MAINTENANCE = new ReleaseType() {
        {
            setCollapseVersions(Boolean.FALSE);
            setCollapsedVersionQualifier(null);
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^[a-zA-Z]*([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.TRUE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.TRUE);
        }
    };

    /**
     * The release type used for maturity branches
     */
    public static final ReleaseType MATURITY = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.TRUE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.FALSE);
        }
    };

    /**
     * The release type used for release branches
     */
    public static final ReleaseType RELEASE = new ReleaseType() {
        {
            setCollapseVersions(Boolean.TRUE);
            setCollapsedVersionQualifier("{{#firstLower}}{{branch}}{{/firstLower}}");
            setDescription(null);
            setFilterTags("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(rel|release)((\\.([0-9]\\d*))?)?)$");
            setGitCommit(Boolean.FALSE.toString());
            setGitCommitMessage(null);
            setGitPush(Boolean.TRUE.toString());
            setGitTag(Boolean.TRUE.toString());
            setGitTagMessage(null);
            setIdentifiers(null);
            setMatchBranches("^(rel|release)(-|\\/)({{configuration.releasePrefix}})?([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$");
            setMatchEnvironmentVariables(null);
            setMatchWorkspaceStatus(WorkspaceStatus.CLEAN);
            setPublish(Boolean.FALSE.toString());
            setVersionRange(null);
            setVersionRangeFromBranchName(Boolean.TRUE);
        }
    };

    /**
     * Default constructor.
     */
    public ReleaseTypes() {
        super();
    }
}
