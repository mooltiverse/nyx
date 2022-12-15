//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.p:
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
	"strconv" // https://pkg.go.dev/strconv
	"testing" // https://pkg.go.dev/testing

	regexp2 "github.com/dlclark/regexp2"        // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

var (
	/*
	   A fixture with valid structured data to test commit messages using Conventional Commits.

	   Each returned argument has the fields:
	   - message: the entire message
	   - type: the type that is expected to be returned as the commit message type
	   - scope: the scope that is expected to be returned as the commit message scope, or nil if not present
	   - title: the description that is expected to be returned as the commit message description
	   - bump: the version identifier that is expected to be bumped by the given commit message, or nil
	*/
	wellKnownValidConventionalCommitsMessages = []struct {
		message      *string
		returnedType *string
		scope        *string
		title        *string
		bump         *string
	}{
		{message: utl.PointerToString("feat: allow provided config object to extend other configs\nBREAKING CHANGE: `extends` key in config file is now used for extending other config files"), returnedType: utl.PointerToString("feat"), scope: nil, title: utl.PointerToString("allow provided config object to extend other configs"), bump: utl.PointerToString("major")},
		{message: utl.PointerToString("refactor!: drop support 1 for Node 6"), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 1 for Node 6"), bump: utl.PointerToString("major")},
		{message: utl.PointerToString("refactor!: drop support 2 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6."), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 2 for Node 6"), bump: utl.PointerToString("major")},
		{message: utl.PointerToString("refactor: drop support 3 for Node 6\nBREAKING CHANGE: refactor to use JavaScript features not available in Node 6."), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 3 for Node 6"), bump: utl.PointerToString("major")},
		{message: utl.PointerToString("refactor: drop support 4 for Node 6\nBREAKING-CHANGE: refactor to use JavaScript features not available in Node 6."), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 4 for Node 6"), bump: utl.PointerToString("major")},
		{message: utl.PointerToString("refactor: drop support 5 for Node 6\nBREAKING CHANGE:"), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 5 for Node 6"), bump: nil},
		{message: utl.PointerToString("refactor: drop support 6 for Node 6\nBREAKING CHANGE refactor to use JavaScript features not available in Node 6."), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 6 for Node 6"), bump: nil},
		{message: utl.PointerToString("refactor: drop support 7 for Node 6\nbreaking change: refactor to use JavaScript features not available in Node 6."), returnedType: utl.PointerToString("refactor"), scope: nil, title: utl.PointerToString("drop support 7 for Node 6"), bump: nil},
		{message: utl.PointerToString("docs: correct spelling of CHANGELOG"), returnedType: utl.PointerToString("docs"), scope: nil, title: utl.PointerToString("correct spelling of CHANGELOG"), bump: nil},
		{message: utl.PointerToString("feat(lang): add polish language"), returnedType: utl.PointerToString("feat"), scope: utl.PointerToString("lang"), title: utl.PointerToString("add polish language"), bump: utl.PointerToString("minor")},
		{message: utl.PointerToString("fix: correct minor typos in code\n\nsee the issue for details\n\non typos fixed.\n\nReviewed-by: Z\nRefs #133"), returnedType: utl.PointerToString("fix"), scope: nil, title: utl.PointerToString("correct minor typos in code"), bump: utl.PointerToString("patch")},
		{message: utl.PointerToString("feat(shopping cart): add the amazing button"), returnedType: utl.PointerToString("feat"), scope: utl.PointerToString("shopping cart"), title: utl.PointerToString("add the amazing button"), bump: utl.PointerToString("minor")},
		{message: utl.PointerToString("feat: remove ticket list endpoint\n\nrefers to JIRA-1337\nBREAKING CHANGES: ticket enpoints no longer supports list all entites."), returnedType: utl.PointerToString("feat"), scope: nil, title: utl.PointerToString("remove ticket list endpoint"), bump: utl.PointerToString("minor")},
		{message: utl.PointerToString("fix: add missing parameter to service call\n\nThe error occurred because of <reasons>."), returnedType: utl.PointerToString("fix"), scope: nil, title: utl.PointerToString("add missing parameter to service call"), bump: utl.PointerToString("patch")},
		{message: utl.PointerToString("build: update dependencies"), returnedType: utl.PointerToString("build"), scope: nil, title: utl.PointerToString("update dependencies"), bump: nil},
	}

	/*
	   A fixture with invalid structured data to test commit messages using Conventional Commits.
	   Each returned argument has one field with a commit message that doesn't match the Conventional Commits specification.

	   Each returned argument has the fields:
	   - message: the entire message
	*/
	wellKnownInvalidConventionalCommitsMessages = []struct {
		message      *string
		returnedType *string
		scope        *string
		title        *string
		bump         *string
	}{
		{message: utl.PointerToString("")},
		{message: utl.PointerToString("A free text headline")},
		{message: utl.PointerToString("A free text message\non two lines")},
		{message: utl.PointerToString("A (free): text message\non two lines")},
	}

	/*
	   A fixture with valid structured data to test commit messages using GitMoji.

	   Each returned argument has the fields:
	   - message: the entire message
	   - type: the type that is expected to be returned as the commit message type
	   - scope: the scope that is expected to be returned as the commit message scope, or nil if not present
	   - title: the description that is expected to be returned as the commit message description
	   - bump: the version identifier that is expected to be bumped by the given commit message, or nil
	*/
	wellKnownValidGitMojiMessages = []struct {
		message      *string
		returnedType *string
		scope        *string
		title        *string
		bump         *string
	}{
		{message: utl.PointerToString(":boom:"), returnedType: utl.PointerToString(":boom:"), scope: nil, title: nil, bump: utl.PointerToString("major")},
		{message: utl.PointerToString(":zap: A single line message :art:"), returnedType: utl.PointerToString(":zap:"), scope: nil, title: utl.PointerToString("A single line message :art:"), bump: utl.PointerToString("patch")},
		{message: utl.PointerToString(":zap: A double line message :art:\nSecond line"), returnedType: utl.PointerToString(":zap:"), scope: nil, title: utl.PointerToString("A double line message :art:"), bump: utl.PointerToString("patch")},
		//{message: utl.PointerToString(":zap: A double line message :art:\r\nSecond line"), returnedType: utl.PointerToString(":zap:"), scope: nil, title: utl.PointerToString("A double line message :art:"), bump: utl.PointerToString("patch")},
		{message: utl.PointerToString(":unknownemoji:"), returnedType: utl.PointerToString(":unknownemoji:"), scope: nil, title: nil, bump: nil},
		{message: utl.PointerToString(":unknownemoji: A commit with unknown type"), returnedType: utl.PointerToString(":unknownemoji:"), scope: nil, title: utl.PointerToString("A commit with unknown type"), bump: nil},
		{message: utl.PointerToString(":unknownemoji: A commit with unknown type\nAnd a second line"), returnedType: utl.PointerToString(":unknownemoji:"), scope: nil, title: utl.PointerToString("A commit with unknown type"), bump: nil},
	}

	/*
	   A fixture with invalid structured data to test commit messages using GitMoji.
	   Each returned argument has one field with a commit message that doesn't match the Conventional Commits specification.

	   Each returned argument has the fields:
	   - message: the entire message
	*/
	wellKnownInvalidGitMojiMessages = []struct {
		message      *string
		returnedType *string
		scope        *string
		title        *string
		bump         *string
	}{
		{message: utl.PointerToString("")},
		{message: utl.PointerToString("art")},
		{message: utl.PointerToString(":art")},
		{message: utl.PointerToString("art:")},
		{message: utl.PointerToString("A single line message")},
		{message: utl.PointerToString("A single :boom: line message :art:")},
		{message: utl.PointerToString("A single line message :bug:")},
		{message: utl.PointerToString("A double :fire: line message :art:\nSecond line")},
		{message: utl.PointerToString("A double line message :bug:\nSecond line")},
		{message: utl.PointerToString("A double :fire: line message :art:\r\nSecond line")},
	}
)

func TestCommitMessageConventionConventionalCommitsMatchPositiveMatch(t *testing.T) {
	for i, tc := range wellKnownValidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.MatchString(*tc.message)
			assert.NoError(t, err)
			assert.True(t, match)
		})
	}
}

func TestCommitMessageConventionConventionalCommitsMatchNegativeMatch(t *testing.T) {
	for i, tc := range wellKnownInvalidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.MatchString(*tc.message)
			assert.NoError(t, err)
			assert.False(t, match)
		})
	}
}

func TestCommitMessageConventionConventionalCommitsCommitType(t *testing.T) {
	for i, tc := range wellKnownValidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("type")
			if tc.returnedType != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.returnedType, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionConventionalCommitsCommitScope(t *testing.T) {
	for i, tc := range wellKnownValidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("scope")
			if tc.scope != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.scope, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionConventionalCommitsCommitTitle(t *testing.T) {
	for i, tc := range wellKnownValidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("title")
			if tc.title != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.title, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionConventionalCommitsCommitBumpComponent(t *testing.T) {
	for i, tc := range wellKnownValidConventionalCommitsMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			if tc.bump != nil {
				for key, value := range *COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS.GetBumpExpressions() {
					re, err := regexp2.Compile(value, 0)
					assert.NoError(t, err)
					match, err := re.MatchString(*tc.message)
					assert.NoError(t, err)
					if key == *tc.bump {
						assert.True(t, match)
					} else {
						assert.Falsef(t, match, "expression '%s' ('%s') was not expected to match but it did", key, value)
					}
				}
			}
		})
	}
}

func TestCommitMessageConventionGitmojiMatchPositiveMatch(t *testing.T) {
	for i, tc := range wellKnownValidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.MatchString(*tc.message)
			assert.NoError(t, err)
			assert.True(t, match)
		})
	}
}

func TestCommitMessageConventionGitmojiMatchNegativeMatch(t *testing.T) {
	for i, tc := range wellKnownInvalidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.MatchString(*tc.message)
			assert.NoError(t, err)
			assert.False(t, match)
		})
	}
}

func TestCommitMessageConventionGitmojiCommitType(t *testing.T) {
	for i, tc := range wellKnownValidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("type")
			if tc.returnedType != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.returnedType, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionGitmojiCommitScope(t *testing.T) {
	for i, tc := range wellKnownValidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("scope")
			if tc.scope != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.scope, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionGitmojiCommitTitle(t *testing.T) {
	for i, tc := range wellKnownValidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			re, err := regexp2.Compile(*COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetExpression(), 0)
			assert.NoError(t, err)
			match, err := re.FindStringMatch(*tc.message)
			assert.NoError(t, err)
			g := match.GroupByName("title")
			if tc.title != nil {
				assert.NotNil(t, g)
				assert.Equal(t, *tc.title, g.Captures[0].String())
			}
		})
	}
}

func TestCommitMessageConventionGitmojiCommitBumpComponent(t *testing.T) {
	for i, tc := range wellKnownValidGitMojiMessages {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			if tc.bump != nil {
				for key, value := range *COMMIT_MESSAGE_CONVENTIONS_GITMOJI.GetBumpExpressions() {
					re, err := regexp2.Compile(value, 0)
					assert.NoError(t, err)
					match, err := re.MatchString(*tc.message)
					assert.NoError(t, err)
					if key == *tc.bump {
						assert.True(t, match)
					} else {
						assert.Falsef(t, match, "expression '%s' ('%s') was not expected to match but it did", key, value)
					}
				}
			}
		})
	}
}
