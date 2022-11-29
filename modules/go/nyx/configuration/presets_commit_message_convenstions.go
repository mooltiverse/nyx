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
	// The Conventional Commits configuration.
	COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS = ent.NewCommitMessageConventionWith(utl.PointerToString("(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*"), &map[string]string{"major": "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*", "minor": "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*", "patch": "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"})

	// The gitmoji configuration.
	COMMIT_MESSAGE_CONVENTIONS_GITMOJI = ent.NewCommitMessageConventionWith(utl.PointerToString("(?m)^(:(?<type>[a-zA-Z0-9_]+):)( (?<title>.+))?$(?s).*"), &map[string]string{"major": "(?m)^:boom:(?s).*", "minor": "(?m)^:sparkles:(?s).*", "patch": "(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*"})
)
