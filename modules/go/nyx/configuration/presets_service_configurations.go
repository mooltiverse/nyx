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
)

var (
	// The GitHub service configuration
	SERVICE_CONFIGURATIONS_GITHUB = ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}"})

	// The GitLab service configuration
	SERVICE_CONFIGURATIONS_GITLAB = ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}"})
)
