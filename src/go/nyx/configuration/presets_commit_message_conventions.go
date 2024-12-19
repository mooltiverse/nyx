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
	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

var (
	/*
		The Conventional Commits configuration.
	*/
	COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS = ent.NewCommitMessageConventionWith(utl.PointerToString("(?m)^(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-zA-Z0-9 \\-_]+)\\))?(!)?:( (?<title>.+))$(?s).*"), &map[string]string{"major": "(?s)(?m)^[a-zA-Z0-9_]+(\\([a-zA-Z0-9 \\-_]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*", "minor": "(?s)(?m)^feat(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*", "patch": "(?s)(?m)^fix(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*"})

	/*
		An unofficial extension for the Conventional Commits configuration which also parses the commit message body to detect which changes have occurred.

		This convention is useful in merge commits whose messages have been automatically generated listing all the commits
		that have been merged (i.e. when squashing) so the bump identifiers are scanned in the body, which may contain multiple
		significant rows, rather than just the first line.
	*/
	COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS_FOR_MERGE = ent.NewCommitMessageConventionWith(utl.PointerToString("(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-zA-Z0-9 \\-_]+)\\))?:( (?<title>.+))"), &map[string]string{"major": "(?s)(?m)[a-zA-Z0-9_]+(!: .*|.*^(BREAKING( |-)CHANGE: )).*", "minor": "(?s)(?m)feat(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*", "patch": "(?s)(?m)fix(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*"})

	/*
		The gitmoji configuration.
	*/
	COMMIT_MESSAGE_CONVENTIONS_GITMOJI = ent.NewCommitMessageConventionWith(utl.PointerToString("(?m)^(?<type>:[a-zA-Z0-9_]+:)( (?<title>.+))?$(?s).*"), &map[string]string{"major": "(?m)^:boom:(?s).*", "minor": "(?m)^:sparkles:(?s).*", "patch": "(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*"})
)
