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
import java.util.Map;

import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;
import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;
import com.mooltiverse.oss.nyx.entities.CommitMessageConventions;
import com.mooltiverse.oss.nyx.entities.ReleaseType;
import com.mooltiverse.oss.nyx.entities.ReleaseTypes;
import com.mooltiverse.oss.nyx.entities.ServiceConfiguration;
import com.mooltiverse.oss.nyx.entities.Substitution;

/**
 * The extended configuration preset.
 */
public class Extended extends SimpleConfigurationLayer {
    /**
     * The name identifier for this preset.
     */
    public static final String NAME = "extended";

    /**
     * Default constructor.
     */
    public Extended() {
        super();

        // add a changelog configuration suitable for all the conventions used in this preset
        setChangelog(Changelogs.ANY);

        // add the 'conventionalCommits' and 'gitmoji' conventions
        setCommitMessageConventions(
            new CommitMessageConventions(
                List.<String>of("conventionalCommits", "gitmoji"),
                Map.<String,CommitMessageConvention>of("conventionalCommits", com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS, "conventionalCommitsForMerge", com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE, "gitmoji", com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventions.GITMOJI))
        );

        // add the 'mainline' and 'internal' release types
        setReleaseTypes(
            new ReleaseTypes(
                List.<String>of("mainline", "integration", "maturity", "feature", "fix", "hotfix", "release", "maintenance", "internal"),
                List.<String>of(),
                List.<String>of(),
                Map.<String,ReleaseType>of(
                    "mainline", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.MAINLINE,
                    "integration", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.INTEGRATION,
                    "maturity", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.MATURITY,
                    "feature", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.FEATURE,
                    "fix", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.FIX,
                    "hotfix", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.HOTFIX,
                    "release", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.RELEASE,
                    "maintenance", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.MAINTENANCE,
                    "internal", com.mooltiverse.oss.nyx.configuration.presets.ReleaseTypes.INTERNAL
                )
            )
        );

        // add the 'github' and 'gitlab' service configurations
        setServices(Map.<String,ServiceConfiguration>of(
            "github", ServiceConfigurations.GITHUB,
            "gitlab", ServiceConfigurations.GITLAB
        ));

        // add the 'cargo', 'composer', 'dart', 'elixir', 'expo', 'helm', 'node' and 'text' substitution configurations
        // no substitution is enabled by default
        setSubstitutions(
            new com.mooltiverse.oss.nyx.entities.Substitutions(
                List.<String>of(),
                Map.<String,Substitution>of(
                    "cargo_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.CARGO_VERSION,
                    "composer_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.COMPOSER_VERSION,
                    "dart_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.DART_VERSION,
                    "elixir_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.ELIXIR_VERSION,
                    "expo_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.EXPO_VERSION,
                    "helm_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.HELM_VERSION,
                    "node_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.NODE_VERSION,
                    "text_version", com.mooltiverse.oss.nyx.configuration.presets.Substitutions.TEXT_VERSION
                )
            )
        );
    }
}
