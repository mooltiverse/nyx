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

import com.mooltiverse.oss.nyx.configuration.SimpleConfigurationLayer;

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
        getCommitMessageConventions().setEnabled(List.<String>of("conventionalCommits"));
        getCommitMessageConventions().getItems().put("conventionalCommits", CommitMessageConventions.CONVENTIONAL_COMMITS);
    }
}
