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
 * The simple configuration preset.
 */
public class Simple extends SimpleConfigurationLayer {
    /**
     * The name identifier for this preset.
     */
    public static final String NAME = "simple";

    /**
     * Default constructor.
     */
    public Simple() {
        super();

        // add the 'conventionalCommits' convention
        setCommitMessageConventions(
            new EnabledItemsMap<CommitMessageConvention>(
                List.<String>of("conventionalCommits"),
                Map.<String,CommitMessageConvention>of("conventionalCommits", CommitMessageConventions.CONVENTIONAL_COMMITS))
        );

        // add the 'mainline' and 'internal' release types
        setReleaseTypes(
            new EnabledItemsMap<ReleaseType>(
                List.<String>of("mainline", "internal"),
                Map.<String,ReleaseType>of(
                    "mainline", ReleaseTypes.MAINLINE,
                    "internal", ReleaseTypes.INTERNAL
                )
            )
        );
    }
}
