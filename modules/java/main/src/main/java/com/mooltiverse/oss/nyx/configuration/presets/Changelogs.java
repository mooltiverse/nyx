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

import java.util.Map;

import com.mooltiverse.oss.nyx.entities.ChangelogConfiguration;

/**
 * This class provides reusable configuration chunks for changelog configurations.
 */
public class Changelogs {
    /**
     * The changelog configuration that is suitable when using any commit message convention
     */
    public static final ChangelogConfiguration ANY = new ChangelogConfiguration() {
        {
            setIncludeUnreleased(Boolean.TRUE);
            setPath("CHANGELOG.md");
            setSections(Map.<String,String>of("Added", "^(feat|:boom:|:sparkles:)$", "Fixed", "^(fix|:bug:|:ambulance:)$", "Removed", "^:fire:$", "Security", "^:lock:$"));
        }
    };

    /**
     * The changelog configuration that is suitable when using <a href="https://www.conventionalcommits.org/">Conventional Commits</a> as the commit message convention
     */
    public static final ChangelogConfiguration CONVENTIONAL_COMMITS = new ChangelogConfiguration() {
        {
            setIncludeUnreleased(Boolean.TRUE);
            setPath("CHANGELOG.md");
            setSections(Map.<String,String>of("Added", "^feat$", "Fixed", "^fix$"));
        }
    };

    /**
     * The changelog configuration that is suitable when using <a href="https://gitmoji.dev/">gitmoji</a> as the commit message convention
     */
    public static final ChangelogConfiguration GITMOJI = new ChangelogConfiguration() {
        {
            setIncludeUnreleased(Boolean.TRUE);
            setPath("CHANGELOG.md");
            setSections(Map.<String,String>of("Added", "^(:boom:|:sparkles:)$", "Fixed", "^(:bug:|:ambulance:)$", "Removed", "^:fire:$", "Security", "^:lock:$"));
        }
    };

    /**
     * Default constructor is hidden on purpose.
     */
    public Changelogs() {
        super();
    }
}
