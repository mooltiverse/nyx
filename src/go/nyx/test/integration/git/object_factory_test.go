//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package git_test

import (
	"os"      // https://pkg.go.dev/os
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	gitent "github.com/mooltiverse/nyx/src/go/nyx/entities/git"
	. "github.com/mooltiverse/nyx/src/go/nyx/git"
	gittools "github.com/mooltiverse/nyx/src/go/nyx/test/integration/git/tools"
)

func TestObjectFactoryActionFrom(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	commit := script.AndAddFiles().Commit("A message")
	action := ActionFrom(commit.Author)

	assert.Equal(t, commit.Author.Name, action.Identity.Name)
	assert.Equal(t, commit.Author.Email, action.Identity.Email)

	assert.Equal(t, commit.Committer.When.UnixMilli(), action.TimeStamp.TimeStamp)
	_, offset := commit.Committer.When.Zone()
	// the time package uses seconds for the offset, we need to convert in minutes
	offset = offset / 60
	assert.Equal(t, offset, *action.TimeStamp.Offset)
}

func TestObjectFactoryTimeStampFrom(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	commit := script.AndAddFiles().Commit("A message")
	timestamp := TimeStampFrom(commit.Committer)

	assert.Equal(t, commit.Committer.When.UnixMilli(), timestamp.TimeStamp)
	_, offset := commit.Committer.When.Zone()
	// the time package uses seconds for the offset, we need to convert in minutes
	offset = offset / 60
	assert.Equal(t, offset, *timestamp.Offset)
}

func TestObjectFactoryIdentityFrom(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	commit := script.AndAddFiles().Commit("A message")
	identity := IdentityFrom(commit.Committer)

	assert.Equal(t, commit.Author.Name, identity.Name)
	assert.Equal(t, commit.Author.Email, identity.Email)
}

func TestObjectFactoryMessageFrom(t *testing.T) {
	messageHeader := "Commit message header"
	fullCommitMessage := `Commit message header

Body row 1
Body row 2

Body row 3
Body row 4

Reviewed-By: John Doe
Issue: 98765
`

	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	commit := script.AndAddFiles().Commit(messageHeader)
	message := MessageFrom(commit)

	assert.Equal(t, messageHeader, message.FullMessage)
	assert.Equal(t, messageHeader, message.ShortMessage)
	assert.Equal(t, 0, len(message.Footers))

	commit = script.AndAddFiles().Commit(fullCommitMessage)
	message = MessageFrom(commit)

	assert.Equal(t, fullCommitMessage, message.FullMessage)
	assert.Equal(t, messageHeader, message.ShortMessage)
	assert.Equal(t, 2, len(message.Footers))
	assert.Equal(t, "John Doe", message.Footers["Reviewed-By"])
	assert.Equal(t, "98765", message.Footers["Issue"])
}

func TestObjectFactoryTagFrom(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())
	commit := script.AndAddFiles().Commit("Commit 1")

	// test a lightweight tag
	refTag1 := script.Tag("t1", nil)
	tag1 := TagFrom(&script.Repository, refTag1)
	assert.Equal(t, "t1", tag1.Name)
	assert.Equal(t, commit.Hash.String(), tag1.Target)
	assert.Equal(t, false, tag1.Annotated)

	// test an annotated tag
	msg := "Tag message"
	refTag2 := script.Tag("t2", &msg)
	tag2 := TagFrom(&script.Repository, refTag2)
	assert.Equal(t, "t2", tag2.Name)
	assert.Equal(t, commit.Hash.String(), tag2.Target)
	assert.Equal(t, true, tag2.Annotated)
}

func TestObjectFactoryCommitFrom(t *testing.T) {
	script := gittools.FROM_SCRATCH().Realize()
	defer os.RemoveAll(script.GetWorkingDirectory())

	revCommit1 := script.AndAddFiles().Commit("Commit 1")
	refTag1 := script.Tag("t1", nil)
	tag1 := TagFrom(&script.Repository, refTag1)
	commit1 := CommitFrom(revCommit1, []gitent.Tag{tag1})

	assert.Equal(t, revCommit1.Hash.String(), commit1.GetSHA())
	assert.Equal(t, revCommit1.Committer.When.UnixMilli(), commit1.GetDate())
	assert.Equal(t, 0, len(commit1.GetParents()))
	assert.Equal(t, revCommit1.Author.Name, commit1.GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, revCommit1.Author.Email, commit1.GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, revCommit1.Committer.Name, commit1.GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, revCommit1.Committer.Email, commit1.GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, revCommit1.Message, commit1.GetMessage().GetShortMessage())
	assert.Equal(t, revCommit1.Message, commit1.GetMessage().GetFullMessage())
	assert.Equal(t, 1, len(commit1.GetTags()))
	assert.Equal(t, "t1", commit1.GetTags()[0].GetName())

	revCommit2 := script.AndAddFiles().Commit("Commit 2")
	msg := "Tag message"
	refTag2 := script.Tag("t2", &msg)
	tag2 := TagFrom(&script.Repository, refTag2)
	commit2 := CommitFrom(revCommit2, []gitent.Tag{tag2})

	assert.Equal(t, revCommit2.Hash.String(), commit2.GetSHA())
	assert.Equal(t, revCommit2.Committer.When.UnixMilli(), commit2.GetDate())
	assert.Equal(t, 1, len(commit2.GetParents()))
	assert.Equal(t, revCommit2.Author.Name, commit2.GetAuthorAction().GetIdentity().GetName())
	assert.Equal(t, revCommit2.Author.Email, commit2.GetAuthorAction().GetIdentity().GetEmail())
	assert.Equal(t, revCommit2.Committer.Name, commit2.GetCommitAction().GetIdentity().GetName())
	assert.Equal(t, revCommit2.Committer.Email, commit2.GetCommitAction().GetIdentity().GetEmail())
	assert.Equal(t, revCommit2.Message, commit2.GetMessage().GetShortMessage())
	assert.Equal(t, revCommit2.Message, commit2.GetMessage().GetFullMessage())
	assert.Equal(t, 1, len(commit2.GetTags()))
	assert.Equal(t, "t2", commit2.GetTags()[0].GetName())
}
