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
        setDirectory("project/directory");
        setInitialVersion("1.0.0");
        setReleasePrefix("v");
        setReleaseLenient(Boolean.TRUE);
        setScheme(Scheme.SEMVER);
        setStateFile(".nyx-state.yml");
        setVerbosity(Verbosity.INFO);

        getCommitMessageConventions().setEnabled(List.<String>of("conventionalCommits"));
        getCommitMessageConventions().getItems().put("conventionalCommits", new CommitMessageConvention("(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*", Map.<String,String>of("major", "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*", "minor", "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*", "patch", "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*")));

        getReleaseTypes().setEnabled(List.<String>of("type1"));
        getReleaseTypes().getItems().put("type1", new ReleaseType(true, "{{ branch }}", "^({{ configuration.releasePrefix }})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", Boolean.TRUE.toString(), "Committing {{ version }}", Boolean.TRUE.toString(), Boolean.TRUE.toString(), "Tagging {{ version }}", new Identifiers(List.<String>of("build"),  Map.<String,Identifier>of("build", new Identifier("build", "12", IdentifierPosition.BUILD))), "^(release[-\\/](0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)|feature[-\\/][[:alnum:]]+)$", Map.<String,String>of("PATH",".*"), WorkspaceStatus.CLEAN, Boolean.TRUE.toString(), "^1\\.2\\.(0|[1-9]\\d*)?$", Boolean.FALSE));
    }
}
