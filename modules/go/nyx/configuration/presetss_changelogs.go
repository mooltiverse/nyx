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

package configuration

import (
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

var (
	// The changelog configuration that is suitable when using any commit message convention.
	CHANGELOGS_ANY, _ = ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Added": "^(feat|:boom:|:sparkles:)$", "Fixed": "^(fix|:bug:|:ambulance:)$", "Removed": "^:fire:$", "Security": "^:lock:$"}, nil, nil)

	// The changelog configuration that is suitable when using Conventional Commits as the commit message convention.
	CHANGELOGS_CONVENTIONAL_COMMITS, _ = ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Added": "^feat$", "Fixed": "^fix$"}, nil, nil)

	// The changelog configuration that is suitable when using gitmoji as the commit message convention.
	CHANGELOGS_GITMOJI, _ = ent.NewChangelogConfigurationWith(utl.PointerToString("CHANGELOG.md"), &map[string]string{"Added": "^(:boom:|:sparkles:)$", "Fixed": "^(:bug:|:ambulance:)$", "Removed": "^:fire:$", "Security": "^:lock:$"}, nil, nil)
)
