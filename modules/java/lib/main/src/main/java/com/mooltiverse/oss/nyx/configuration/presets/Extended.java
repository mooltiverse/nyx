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
import com.mooltiverse.oss.nyx.entities.EnabledItemsMap;
import com.mooltiverse.oss.nyx.entities.ReleaseType;

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

        // add the 'conventionalCommits' and 'gitmoji' conventions
        setCommitMessageConventions(
            new EnabledItemsMap<CommitMessageConvention>(
                List.<String>of("conventionalCommits", "gitmoji"),
                Map.<String,CommitMessageConvention>of("conventionalCommits", CommitMessageConventions.CONVENTIONAL_COMMITS, "gitmoji", CommitMessageConventions.GITMOJI))
        );

        // add the 'mainline' and 'internal' release types
        setReleaseTypes(
            new EnabledItemsMap<ReleaseType>(
                List.<String>of("mainline", "integration", "maturity", "feature", "hotfix", "release", "maintenance", "internal"),
                Map.<String,ReleaseType>of(
                    "mainline", ReleaseTypes.MAINLINE,
                    "integration", ReleaseTypes.INTEGRATION,
                    "maturity", ReleaseTypes.MATURITY,
                    "feature", ReleaseTypes.FEATURE,
                    "hotfix", ReleaseTypes.HOTFIX,
                    "release", ReleaseTypes.RELEASE,
                    "maintenance", ReleaseTypes.MAINTENANCE,
                    "internal", ReleaseTypes.INTERNAL
                )
            )
        );
    }
}
