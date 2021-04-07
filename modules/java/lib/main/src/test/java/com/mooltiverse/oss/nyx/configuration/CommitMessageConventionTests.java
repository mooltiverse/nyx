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
package com.mooltiverse.oss.nyx.configuration;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

@DisplayName("CommitMessageConvention")
public class CommitMessageConventionTests {
    /**
     * The regular expression that can be used to match headline fields in commit messages. The regex defines 4 named groups:<br>
     * - 'type' is the commit type<br>
     * - 'breaking' is the optional exlamation mark after the type<br>
     * - 'scope' is the commit scope (optional)<br>
     * - 'title' is the commit short description<br>
     * <br>
     * This expression only considers the first line while others are ignored.
     * <br>
     * IMPLEMENTATION NOTE:<br>
     * If Java regex supported duplicated named group|pattern names we could transform this expression into:
     *      ^(?<type>[a-zA-Z0-9_]+)(?<breaking>!)?(\((?<scope>[a-z ]+)\))?:(\s(?<title>.+))?(^(?<breaking>BREAKING(\s|-)CHANGE:\s.+)|\X)*
     * so we could have just one expression, avoiding using also {@link #BREAKING_CHANGE_REGEX}. But this uses the 'breaking' named group
     * twice, which indeed is not supported by Java.
     * If we could do this the single 'breaking' group could match the exclamation mark after the {@code type} AND the BREAKING CHANGE
     * footers, making the regex a little more complicated, but the code lighter.
     * Conclusion is: in order to cope with Conventional Commit breaking changes, we must have and evaluate 2 expressions.
     */
    private static final String CONVENTIONAL_COMMITS_REGEX = "(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\\((?<scope>[a-z ]+)\\))?:( (?<title>.+))$(?s).*";

    /**
     * The map of regular expressions used to determine which version identifier to bump, if any.
     * Each map entry key is the name of the version identifier if the expression (the entry value) matches the expression.
     */
    private static final Map<String,String> CONVENTIONAL_COMMITS_BUMP_REGEXES = Map.<String,String>of(
        "major", "(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*",
        "minor", "(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*",
        "patch", "(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*"
    );

    /**
     * The regular expression that can be used to match Gitmojis in any message.
     * <br>
     * This expression defines two named groups that can be used to retrieve fields:<br>
     * - 'type': the emoji identifier (without the leading and trailing colons)
     * - 'title': the message title (the content of the first line, without the leading emoji)
     */
    private static final String GITMOJI_REGEX = "(?m)^(:(?<type>[a-zA-Z0-9_]+):)( (?<title>.+))?$(?s).*";

    /**
     * The map of regular expressions used to determine which version identifier to bump, if any.
     * Each map entry key is the name of the version identifier if the expression (the entry value) matches the expression.
     * 
     * These entries are taken from <a href="https://github.com/carloscuesta/gitmoji/blob/master/src/data/gitmojis.json"/>.
     */
    private static final Map<String,String> GITMOJI_BUMP_REGEXES = Map.<String,String>of(
        "major", "(?m)^:boom:(?s).*",
        "minor", "(?m)^:sparkles:(?s).*",
        "patch", "(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*"
    );

    /**
     * A {@link MethodSource} method that returns valid structured data to test commit messages using Conventional Commits.
     * Each returned argument has the fields:<br>
     * - message: the entire message<br>
     * - type: the type that is expected to be returned as the commit message type<br>
     * - scope: the scope that is expected to be returned as the commit message scope, or null if not present<br>
     * - title: the description that is expected to be returned as the commit message description<br>
     * - bump: the version identifier that is expected to be bumped by the given commit message, or null
     */
    static Stream<Arguments> wellKnownValidConventionalCommitsMessages() {
        return Stream.of(
            arguments("feat: allow provided config object to extend other configs\nBREAKING CHANGE: `extends` key in config file is now used for extending other config files", "feat", null, "allow provided config object to extend other configs", "major"),
            arguments("refactor!: drop support 1 for Node 6", "refactor", null, "drop support 1 for Node 6", "major"),
            arguments("refactor!: drop support 2 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 2 for Node 6", "major"),
            arguments("refactor: drop support 3 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 3 for Node 6", "major"),
            arguments("refactor: drop support 4 for Node 6\nBREAKING-CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 4 for Node 6", "major"),
            arguments("refactor: drop support 5 for Node 6\nBREAKING CHANGE:", "refactor", null, "drop support 5 for Node 6", null),
            arguments("refactor: drop support 6 for Node 6\nBREAKING CHANGE refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 6 for Node 6", null),
            arguments("refactor: drop support 7 for Node 6\nbreaking change: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 7 for Node 6", null),
            arguments("docs: correct spelling of CHANGELOG", "docs", null, "correct spelling of CHANGELOG", null),
            arguments("feat(lang): add polish language", "feat", "lang", "add polish language", "minor"),
            arguments("fix: correct minor typos in code\n\nsee the issue for details\n\non typos fixed.\n\nReviewed-by: Z\nRefs #133", "fix", null, "correct minor typos in code", "patch"),
            arguments("feat(shopping cart): add the amazing button", "feat", "shopping cart", "add the amazing button", "minor"),
            arguments("feat: remove ticket list endpoint\n\nrefers to JIRA-1337\nBREAKING CHANGES: ticket enpoints no longer supports list all entites.", "feat", null, "remove ticket list endpoint", "minor"),
            arguments("fix: add missing parameter to service call\n\nThe error occurred because of <reasons>.", "fix", null, "add missing parameter to service call", "patch"),
            arguments("build: update dependencies", "build", null, "update dependencies", null)
        );
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test commit messages using Conventional Commits.
     * Each returned argument has one field with a commit message that doesn't match the Conventional Commits specification.
     */
    static Stream<Arguments> wellKnownInvalidConventionalCommitsMessages() {
        return Stream.of(
            arguments(""),
            arguments("A free text headline"),
            arguments("A free text message\non two lines"),
            arguments("A (free): text message\non two lines")
        );
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test commit messages using Gitmojis.
     * Each returned argument has the fields:<br>
     * - message: the entire message<br>
     * - type: the gitmoji that is expected to be returned as the commit message type<br>
     * - scope: the scope that is expected to be returned as the commit message scope (always null)<br>
     * - title: the description that is expected to be returned as the commit message description<br>
     * - bump: the version identifier that is expected to be bumped by the given commit message, or null
     */
    static Stream<Arguments> wellKnownValidGitMojiMessages() {
        return Stream.of(
            arguments(":boom:", "boom", null, null, "major"),
            arguments(":zap: A single line message :art:", "zap", null, "A single line message :art:", "patch"),
            arguments(":zap: A double line message :art:\nSecond line", "zap", null, "A double line message :art:", "patch"),
            arguments(":zap: A double line message :art:\r\nSecond line", "zap", null, "A double line message :art:", "patch"),
            arguments(":unknownemoji:", "unknownemoji", null, null, null),
            arguments(":unknownemoji: A commit with unknown type", "unknownemoji", null, "A commit with unknown type", null),
            arguments(":unknownemoji: A commit with unknown type\nAnd a second line", "unknownemoji", null, "A commit with unknown type", null)
        );
    }

    /**
     * A {@link MethodSource} method that returns valid structured data to test commit messages using Gitmojis.
     * Each returned argument has one field with a commit message that doesn't match the Gitmoji specification.
     */
    static Stream<Arguments> wellKnownInvalidGitMojiMessages() {
        return Stream.of(
            arguments(""),
            arguments("art"),
            arguments(":art"),
            arguments("art:"),
            arguments("A single line message"),
            arguments("A single :boom: line message :art:"),
            arguments("A single line message :bug:"),
            arguments("A double :fire: line message :art:\nSecond line"),
            arguments("A double line message :bug:\nSecond line"),
            arguments("A double :fire: line message :art:\r\nSecond line")
        );
    }

    @Nested
    @DisplayName("ConventionalCommits")
    class ConventionalCommitsTests {
        @Nested
        @DisplayName("ConventionalCommits positive match")
        class MatchTests {
            @ParameterizedTest(name = "Conventional commits convention regex matches")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidConventionalCommitsMessages")
            void positiveMatch(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CONVENTIONAL_COMMITS_REGEX).matcher(message);
                assertTrue(m.matches());
            }

            @ParameterizedTest(name = "Conventional commits convention regex does not match")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownInvalidConventionalCommitsMessages")
            void negativeMatch(String message) {
                Matcher m = Pattern.compile(CONVENTIONAL_COMMITS_REGEX).matcher(message);
                assertFalse(m.matches());
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit type")
        class CommitTypeTests {
            @ParameterizedTest(name = "Conventional commits convention regex type == ''{1}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidConventionalCommitsMessages")
            void getCommitType(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CONVENTIONAL_COMMITS_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(type))
                    assertEquals(type, m.group("type"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit scope")
        class CommitScopeTests {
            @ParameterizedTest(name = "Conventional commits convention regex scope == ''{2}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidConventionalCommitsMessages")
            void getCommitScope(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CONVENTIONAL_COMMITS_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(scope))
                    assertEquals(scope, m.group("scope"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit title")
        class CommitTitleTests {
            @ParameterizedTest(name = "Conventional commits convention regex title == ''{3}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidConventionalCommitsMessages")
            void getCommitTitle(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CONVENTIONAL_COMMITS_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(title))
                    assertEquals(title, m.group("title"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits bump component")
        class BumpComponentTests {
            @ParameterizedTest(name = "Conventional commits convention regex bump component == ''{4}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidConventionalCommitsMessages")
            void getBumpComponent(String message, String type, String scope, String title, String bump) {
                if (!Objects.isNull(bump)) {
                    for (Map.Entry<String,String> entry: CONVENTIONAL_COMMITS_BUMP_REGEXES.entrySet()) {
                        Matcher m = Pattern.compile(entry.getValue()).matcher(message);
                        if (bump.equals(entry.getKey()))
                            assertTrue(m.matches());
                        else assertFalse(m.matches(), String.format("expression %s (%s) was not expected to match but it did", entry.getKey(), entry.getValue()));
                    }
                }
            }
        }
    }

    @Nested
    @DisplayName("Gitmoji")
    class GitmojiTests {
        @Nested
        @DisplayName("Gitmoji positive match")
        class MatchTests {
            @ParameterizedTest(name = "Gitmoji convention regex matches")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidGitMojiMessages")
            void positiveMatch(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(GITMOJI_REGEX).matcher(message);
                assertTrue(m.matches());
            }

            @ParameterizedTest(name = "Gitmoji convention regex does not match")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownInvalidGitMojiMessages")
            void negativeMatch(String message) {
                Matcher m = Pattern.compile(GITMOJI_REGEX).matcher(message);
                assertFalse(m.matches());
            }
        }

        @Nested
        @DisplayName("Gitmoji commit type")
        class CommitTypeTests {
            @ParameterizedTest(name = "Gitmoji convention regex type == ''{1}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidGitMojiMessages")
            void getCommitType(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(GITMOJI_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(type))
                    assertEquals(type, m.group("type"));
            }
        }

        @Nested
        @DisplayName("Gitmoji commit scope")
        class CommitScopeTests {
            @ParameterizedTest(name = "Gitmoji convention regex scope == ''{2}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidGitMojiMessages")
            void getCommitScope(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(GITMOJI_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(scope))
                    assertEquals(scope, m.group("scope"));
            }
        }
        
        @Nested
        @DisplayName("Gitmoji commit title")
        class CommitTitleTests {
            @ParameterizedTest(name = "Gitmoji convention regex title == ''{3}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidGitMojiMessages")
            void getCommitTitle(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(GITMOJI_REGEX).matcher(message);
                m.find();
                if (!Objects.isNull(title))
                    assertEquals(title, m.group("title"));
            }
        }

        @Nested
        @DisplayName("Gitmoji bump component")
        class BumpComponentTests {
            @ParameterizedTest(name = "Gitmoji convention regex bump component == ''{4}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.CommitMessageConventionTests#wellKnownValidGitMojiMessages")
            void getBumpComponent(String message, String type, String scope, String title, String bump) {
                if (!Objects.isNull(bump)) {
                    for (Map.Entry<String,String> entry: GITMOJI_BUMP_REGEXES.entrySet()) {
                        Matcher m = Pattern.compile(entry.getValue()).matcher(message);
                        if (bump.equals(entry.getKey()))
                            assertTrue(m.matches());
                        else assertFalse(m.matches(), String.format("expression %s (%s) was not expected to match but it did", entry.getKey(), entry.getValue()));
                    }
                }
            }
        }
    }
}