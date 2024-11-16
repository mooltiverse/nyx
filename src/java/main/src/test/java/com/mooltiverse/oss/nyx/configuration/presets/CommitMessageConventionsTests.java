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

@DisplayName("CommitMessageConventions")
public class CommitMessageConventionsTests {
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
     * A {@link MethodSource} method that returns invalid structured data to test commit messages using Conventional Commits.
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
     * A {@link MethodSource} method that returns valid structured data to test commit messages using Conventional Commits For Merge.
     * Each returned argument has the fields:<br>
     * - message: the entire message<br>
     * - type: the type that is expected to be returned as the commit message type<br>
     * - scope: the scope that is expected to be returned as the commit message scope, or null if not present<br>
     * - title: the description that is expected to be returned as the commit message description<br>
     * - bump: the version identifier that is expected to be bumped by the given commit message, or null
     */
    static Stream<Arguments> wellKnownValidConventionalCommitsForMergeMessages() {
        // This convention must also comply with the standard Conventional Commits so we concatenate the two stream
        return Stream.concat(wellKnownValidConventionalCommitsMessages(), 
            Stream.of(
                arguments("\nfeat: allow provided config object to extend other configs\nBREAKING CHANGE: `extends` key in config file is now used for extending other config files", "feat", null, "allow provided config object to extend other configs", "major"),
                arguments("Anything\nfeat: allow provided config object to extend other configs\nBREAKING CHANGE: `extends` key in config file is now used for extending other config files", "feat", null, "allow provided config object to extend other configs", "major"),
                arguments("Anything\n\nfeat: allow provided config object to extend other configs\nBREAKING CHANGE: `extends` key in config file is now used for extending other config files", "feat", null, "allow provided config object to extend other configs", "major"),
                arguments("Anything\n\nrefactor!: drop support 1 for Node 6", "refactor", null, "drop support 1 for Node 6", "major"),
                arguments("Anything\n\nrefactor!: drop support 2 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 2 for Node 6", "major"),
                arguments("Anything\n\nrefactor: drop support 3 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 3 for Node 6", "major"),
                arguments("Anything\n\nrefactor: drop support 4 for Node 6\nBREAKING-CHANGE: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 4 for Node 6", "major"),
                arguments("Anything\n\nrefactor: drop support 5 for Node 6\nBREAKING CHANGE:", "refactor", null, "drop support 5 for Node 6", null),
                arguments("Anything\n\nrefactor: drop support 6 for Node 6\nBREAKING CHANGE refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 6 for Node 6", null),
                arguments("Anything\n\nrefactor: drop support 7 for Node 6\nbreaking change: refactor to use JavaScript features not available in Node 6.", "refactor", null, "drop support 7 for Node 6", null),
                arguments("Anything\n\ndocs: correct spelling of CHANGELOG", "docs", null, "correct spelling of CHANGELOG", null),
                arguments("Anything\n\nfeat(lang): add polish language", "feat", "lang", "add polish language", "minor"),
                arguments("Anything\n\nfix: correct minor typos in code\n\nsee the issue for details\n\non typos fixed.\n\nReviewed-by: Z\nRefs #133", "fix", null, "correct minor typos in code", "patch"),
                arguments("Anything\n\nfeat(shopping cart): add the amazing button", "feat", "shopping cart", "add the amazing button", "minor"),
                arguments("Anything\n\nfeat: remove ticket list endpoint\n\nrefers to JIRA-1337\nBREAKING CHANGES: ticket enpoints no longer supports list all entites.", "feat", null, "remove ticket list endpoint", "minor"),
                arguments("Anything\n\nfix: add missing parameter to service call\n\nThe error occurred because of <reasons>.", "fix", null, "add missing parameter to service call", "patch"),
                arguments("Anything\n\nbuild: update dependencies", "build", null, "update dependencies", null)
            )
        );
    }

    /**
     * A {@link MethodSource} method that returns invalid structured data to test commit messages using Conventional Commits For Merge.
     * Each returned argument has one field with a commit message that doesn't match the Conventional Commits specification.
     */
    static Stream<Arguments> wellKnownInvalidConventionalCommitsForMergeMessages() {
        // This convention must also comply with the standard Conventional Commits so we concatenate the two stream
        return Stream.concat(wellKnownInvalidConventionalCommitsMessages(), 
            Stream.of(
                // no further elements to add here, elements for the standard Conventional Commit are sufficient
            )
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
            arguments(":boom:", ":boom:", null, null, "major"),
            arguments(":zap: A single line message :art:", ":zap:", null, "A single line message :art:", "patch"),
            arguments(":zap: A double line message :art:\nSecond line", ":zap:", null, "A double line message :art:", "patch"),
            arguments(":zap: A double line message :art:\r\nSecond line", ":zap:", null, "A double line message :art:", "patch"),
            arguments(":unknownemoji:", ":unknownemoji:", null, null, null),
            arguments(":unknownemoji: A commit with unknown type", ":unknownemoji:", null, "A commit with unknown type", null),
            arguments(":unknownemoji: A commit with unknown type\nAnd a second line", ":unknownemoji:", null, "A commit with unknown type", null)
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
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsMessages")
            void positiveMatch(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression()).matcher(message);
                assertTrue(m.find(), String.format("Expression %s was supposed to match '%s' but it didn't", CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression(), message));
            }

            @ParameterizedTest(name = "Conventional commits convention regex does not match")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownInvalidConventionalCommitsMessages")
            void negativeMatch(String message) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression()).matcher(message);
                assertFalse(m.find(), String.format("Expression %s was not supposed to match '%s' but it did", CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression(), message));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit type")
        class CommitTypeTests {
            @ParameterizedTest(name = "Conventional commits convention regex type == ''{1}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsMessages")
            void getCommitType(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(type))
                    assertEquals(type, m.group("type"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit scope")
        class CommitScopeTests {
            @ParameterizedTest(name = "Conventional commits convention regex scope == ''{2}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsMessages")
            void getCommitScope(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(scope))
                    assertEquals(scope, m.group("scope"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits commit title")
        class CommitTitleTests {
            @ParameterizedTest(name = "Conventional commits convention regex title == ''{3}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsMessages")
            void getCommitTitle(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(title))
                    assertEquals(title, m.group("title"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommits bump component")
        class BumpComponentTests {
            @ParameterizedTest(name = "Conventional commits convention regex bump component == ''{4}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsMessages")
            void getBumpComponent(String message, String type, String scope, String title, String bump) {
                if (!Objects.isNull(bump)) {
                    for (Map.Entry<String,String> entry: CommitMessageConventions.CONVENTIONAL_COMMITS.getBumpExpressions().entrySet()) {
                        Matcher m = Pattern.compile(entry.getValue()).matcher(message);
                        if (bump.equals(entry.getKey()))
                            assertTrue(m.find(), String.format("expression '%s' ('%s') was expected to match '%s' but it didn't", entry.getKey(), entry.getValue(), message));
                        else assertFalse(m.find(), String.format("expression '%s' ('%s') was not expected to match '%s' but it did", entry.getKey(), entry.getValue(), message));
                    }
                }
            }
        }
    }

    @Nested
    @DisplayName("ConventionalCommitsForMerge")
    class ConventionalCommitsForMergeTests {
        @Nested
        @DisplayName("ConventionalCommitsForMerge positive match")
        class MatchTests {
            @ParameterizedTest(name = "Conventional commits for merge convention regex matches")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsForMergeMessages")
            void positiveMatch(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression()).matcher(message);
                assertTrue(m.find(), String.format("Expression %s was supposed to match '%s' but it didn't", CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression(), message));
            }

            @ParameterizedTest(name = "Conventional commits for merge convention regex does not match")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownInvalidConventionalCommitsForMergeMessages")
            void negativeMatch(String message) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression()).matcher(message);
                assertFalse(m.find(), String.format("Expression %s was not supposed to match '%s' but it did", CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression(), message));
            }
        }

        @Nested
        @DisplayName("ConventionalCommitsForMerge commit type")
        class CommitTypeTests {
            @ParameterizedTest(name = "Conventional commits for merge convention regex type == ''{1}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsForMergeMessages")
            void getCommitType(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(type))
                    assertEquals(type, m.group("type"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommitsForMerge commit scope")
        class CommitScopeTests {
            @ParameterizedTest(name = "Conventional commits for merge convention regex scope == ''{2}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsForMergeMessages")
            void getCommitScope(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(scope))
                    assertEquals(scope, m.group("scope"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommitsForMerge commit title")
        class CommitTitleTests {
            @ParameterizedTest(name = "Conventional commits for merge convention regex title == ''{3}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsForMergeMessages")
            void getCommitTitle(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(title))
                    assertEquals(title, m.group("title"));
            }
        }

        @Nested
        @DisplayName("ConventionalCommitsForMerge bump component")
        class BumpComponentTests {
            @ParameterizedTest(name = "Conventional commits for merge convention regex bump component == ''{4}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidConventionalCommitsForMergeMessages")
            void getBumpComponent(String message, String type, String scope, String title, String bump) {
                if (!Objects.isNull(bump)) {
                    for (Map.Entry<String,String> entry: CommitMessageConventions.CONVENTIONAL_COMMITS_FOR_MERGE.getBumpExpressions().entrySet()) {
                        Matcher m = Pattern.compile(entry.getValue()).matcher(message);
                        if (bump.equals(entry.getKey()))
                            assertTrue(m.find(), String.format("expression '%s' ('%s') was expected to match '%s' but it didn't", entry.getKey(), entry.getValue(), message));
                        else assertFalse(m.find(), String.format("expression '%s' ('%s') was not expected to match '%s' but it did", entry.getKey(), entry.getValue(), message));
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
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidGitMojiMessages")
            void positiveMatch(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.GITMOJI.getExpression()).matcher(message);
                assertTrue(m.find(), String.format("Expression %s was supposed to match '%s' but it didn't", CommitMessageConventions.GITMOJI.getExpression(), message));
            }

            @ParameterizedTest(name = "Gitmoji convention regex does not match")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownInvalidGitMojiMessages")
            void negativeMatch(String message) {
                Matcher m = Pattern.compile(CommitMessageConventions.GITMOJI.getExpression()).matcher(message);
                assertFalse(m.find(), String.format("Expression %s was not supposed to match '%s' but it did", CommitMessageConventions.GITMOJI.getExpression(), message));
            }
        }

        @Nested
        @DisplayName("Gitmoji commit type")
        class CommitTypeTests {
            @ParameterizedTest(name = "Gitmoji convention regex type == ''{1}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidGitMojiMessages")
            void getCommitType(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.GITMOJI.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(type))
                    assertEquals(type, m.group("type"));
            }
        }

        @Nested
        @DisplayName("Gitmoji commit scope")
        class CommitScopeTests {
            @ParameterizedTest(name = "Gitmoji convention regex scope == ''{2}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidGitMojiMessages")
            void getCommitScope(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.GITMOJI.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(scope))
                    assertEquals(scope, m.group("scope"));
            }
        }
        
        @Nested
        @DisplayName("Gitmoji commit title")
        class CommitTitleTests {
            @ParameterizedTest(name = "Gitmoji convention regex title == ''{3}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidGitMojiMessages")
            void getCommitTitle(String message, String type, String scope, String title, String bump) {
                Matcher m = Pattern.compile(CommitMessageConventions.GITMOJI.getExpression()).matcher(message);
                m.find();
                if (!Objects.isNull(title))
                    assertEquals(title, m.group("title"));
            }
        }

        @Nested
        @DisplayName("Gitmoji bump component")
        class BumpComponentTests {
            @ParameterizedTest(name = "Gitmoji convention regex bump component == ''{4}''")
            @MethodSource("com.mooltiverse.oss.nyx.configuration.presets.CommitMessageConventionsTests#wellKnownValidGitMojiMessages")
            void getBumpComponent(String message, String type, String scope, String title, String bump) {
                if (!Objects.isNull(bump)) {
                    for (Map.Entry<String,String> entry: CommitMessageConventions.GITMOJI.getBumpExpressions().entrySet()) {
                        Matcher m = Pattern.compile(entry.getValue()).matcher(message);
                        if (bump.equals(entry.getKey()))
                            assertTrue(m.find(), String.format("expression '%s' ('%s') was expected to match '%s' but it didn't", entry.getKey(), entry.getValue(), message));
                        else assertFalse(m.find(), String.format("expression '%s' ('%s') was not expected to match '%s' but it did", entry.getKey(), entry.getValue(), message));
                    }
                }
            }
        }
    }
}