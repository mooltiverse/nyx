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

import com.mooltiverse.oss.nyx.entities.CommitMessageConvention;

/**
 * This class provides reusable configuration chunks for commit message conventions.
 */
public class CommitMessageConventions {
    /**
     * The <a href="https://www.conventionalcommits.org/">Conventional Commits</a> configuration.
     */
    public static final CommitMessageConvention CONVENTIONAL_COMMITS = new CommitMessageConvention() {
        {
            setExpression("(?m)^(?<type>[a-zA-Z0-9_]+)(\\((?<scope>[a-zA-Z0-9 \\-_]+)\\))?(!)?:( (?<title>.+))$(?s).*");

            getBumpExpressions().put("major", "(?s)(?m)^[a-zA-Z0-9_]+(\\([a-zA-Z0-9 \\-_]+\\))?(!: .*|.*^(BREAKING( |-)CHANGE: )).*");
            getBumpExpressions().put("minor", "(?s)(?m)^feat(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*");
            getBumpExpressions().put("patch", "(?s)(?m)^fix(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*");
        }
    };

    /**
     * An unofficial extension for the <a href="https://www.conventionalcommits.org/">Conventional Commits</a> configuration
     * which also parses the commit message body to detect which changes have occurred.
     * 
     * This convention is useful in merge commits whose messages have been automatically generated listing all the commits
     * that have been merged (i.e. when squashing) so the bump identifiers are scanned in the body, which may contain multiple
     * significant rows, rather than just the first line.
     */
    public static final CommitMessageConvention CONVENTIONAL_COMMITS_FOR_MERGE = new CommitMessageConvention() {
        {
            setExpression("(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-zA-Z0-9 \\-_]+)\\))?:( (?<title>.+))");

            getBumpExpressions().put("major", "(?s)(?m)[a-zA-Z0-9_]+(!: .*|.*^(BREAKING( |-)CHANGE: )).*");
            getBumpExpressions().put("minor", "(?s)(?m)feat(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*");
            getBumpExpressions().put("patch", "(?s)(?m)fix(!{0})(\\([a-zA-Z0-9 \\-_]+\\))?: (?!.*^(BREAKING( |-)CHANGE: )).*");
        }
    };

    /**
     * The <a href="https://gitmoji.dev/">gitmoji</a> configuration.
     */
    public static final CommitMessageConvention GITMOJI = new CommitMessageConvention() {
        {
            setExpression("(?m)^(?<type>:[a-zA-Z0-9_]+:)( (?<title>.+))?$(?s).*"); 

            getBumpExpressions().put("major", "(?m)^:boom:(?s).*");
            getBumpExpressions().put("minor", "(?m)^:sparkles:(?s).*");
            getBumpExpressions().put("patch", "(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*");
        }
    };

    /**
     * Default constructor.
     */
    public CommitMessageConventions() {
        super();
    }
}
