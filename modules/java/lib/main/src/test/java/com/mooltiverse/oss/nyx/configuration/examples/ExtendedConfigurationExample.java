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
import com.mooltiverse.oss.nyx.data.Scheme;
import com.mooltiverse.oss.nyx.data.Verbosity;

/**
 * A simple configuration.
 */
public class ExtendedConfigurationExample extends SimpleConfigurationLayer {
    /**
     * Default constructor.
     */
    public ExtendedConfigurationExample() {
        super();

        setBump("minor");
        setConfigurationFile("example.config.json");
        setDirectory("project/directory");
        setDryRun(Boolean.FALSE);
        setInitialVersion("1.0.0");
        setPreset("example");
        setReleasePrefix("v");
        setReleaseLenient(Boolean.TRUE);
        setResume(Boolean.TRUE);
        setScheme(Scheme.SEMVER);
        setSharedConfigurationFile("example-shared.config.json");
        setStateFile(".nyx-state.yml");
        setVerbosity(Verbosity.INFO);
        setVersion("1.8.12");

        getCommitMessageConventions().setEnabled(List.<String>of("conventionalCommits"));
        getCommitMessageConventions().getItems().put("conventionalCommits", new CommitMessageConvention("(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*", Map.<String,String>of("major", "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*", "minor", "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*", "patch", "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*")));
    }
}
