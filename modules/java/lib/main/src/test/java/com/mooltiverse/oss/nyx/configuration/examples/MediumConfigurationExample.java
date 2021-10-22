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
package com.mooltiverse.oss.nyx.configuration.examples;

import java.util.List;
import java.util.Map;

import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.data.CommitMessageConvention;
import com.mooltiverse.oss.nyx.data.Identifier;
import com.mooltiverse.oss.nyx.data.Identifiers;
import com.mooltiverse.oss.nyx.data.IdentifierPosition;
import com.mooltiverse.oss.nyx.data.ReleaseType;
import com.mooltiverse.oss.nyx.data.Verbosity;
import com.mooltiverse.oss.nyx.data.WorkspaceStatus;
import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * A simple configuration.
 */
public class MediumConfigurationExample extends SimpleConfigurationLayer {
    /**
     * Default constructor.
     */
    public MediumConfigurationExample() {
        super();

        setConfigurationFile("example.config.json");

        getCommitMessageConventions().setEnabled(List.<String>of("conventionalCommits"));
        getCommitMessageConventions().getItems().put("conventionalCommits", new CommitMessageConvention("(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*", Map.<String,String>of("major", "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*", "minor", "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*", "patch", "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*")));

        setInitialVersion("1.0.0");
        setReleaseLenient(Boolean.TRUE);
        setReleasePrefix("v");

        getReleaseTypes().setEnabled(List.<String>of("mainline", "internal"));
        getReleaseTypes().getItems().put(
            "mainline",
            new ReleaseType(
                false,
                null,
                "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                Boolean.TRUE.toString(),
                "Release version {{version}}",
                Boolean.TRUE.toString(),
                Boolean.TRUE.toString(),
                "Tag release {{version}}",
                null,
                "^(master|main)$",
                Map.<String,String>of("CI","^true$"),
                WorkspaceStatus.CLEAN,
                Boolean.TRUE.toString(),
                null,
                Boolean.FALSE
            )
        );
        getReleaseTypes().getItems().put(
            "internal",
            new ReleaseType(
                false,
                "internal",
                "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$",
                Boolean.FALSE.toString(),
                null,
                Boolean.FALSE.toString(),
                Boolean.FALSE.toString(),
                null,
                new Identifiers(
                    List.<String>of("branch", "commit", "user", "timestamp"),
                    Map.<String,Identifier>of(
                        "branch",    new Identifier("branch", "{{#sanitize}}{{branch}}{{/sanitize}}", IdentifierPosition.BUILD),
                        "commit",    new Identifier("commit", "{{#short7}}{{releaseScope.finalCommit}}{{/short7}}", IdentifierPosition.BUILD),
                        "user",      new Identifier("user", "{{#sanitizeLower}}{{environment.user}}{{/sanitizeLower}}", IdentifierPosition.BUILD),
                        "timestamp", new Identifier("timestamp", "{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}", IdentifierPosition.BUILD)
                    )
                ),
                null,
                null,
                null,
                Boolean.FALSE.toString(),
                null,
                Boolean.FALSE
            )
        );

        setScheme(Scheme.SEMVER);
        setStateFile(".nyx-state.yml");
        setVerbosity(Verbosity.INFO);
    }
}
