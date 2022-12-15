//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

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

package git

import (
	"testing" // https://pkg.go.dev/testing
	"time"    // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestNewCommitWith(t *testing.T) {
	lightweightTag := NewTagWith("t1", "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", false)
	annotatedTag := NewTagWith("t2", "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", true)
	tags := []Tag{*lightweightTag, *annotatedTag}
	parents := []string{"e7c4419c1a9635a264b1d6c573ac2af71e1eeea6"}
	timeStamp := NewTimeStampFrom(time.Now())

	authorIdentity := NewIdentityWith("author", "")
	committerIdentity := NewIdentityWith("committer", "")
	authorAction := NewActionWith(*authorIdentity, *timeStamp)
	commitAction := NewActionWith(*committerIdentity, *timeStamp)

	footers := map[string]string{"k1": "v1", "k2": "v2"}
	message := NewMessageWith("full", "short", footers)

	commit := NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 999999, parents, *authorAction, *commitAction, *message, tags)

	assert.Equal(t, "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", commit.GetSHA())
	assert.Equal(t, int64(999999), commit.GetDate())
	assert.Equal(t, 1, len(commit.GetParents()))
	assert.Equal(t, "e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", commit.GetParents()[0])
	authorAction1 := commit.GetAuthorAction()
	authorIdentity1 := authorAction1.GetIdentity()
	assert.Equal(t, "author", authorIdentity1.GetName())
	commitAction1 := commit.GetCommitAction()
	committerIdentity1 := commitAction1.GetIdentity()
	assert.Equal(t, "committer", committerIdentity1.GetName())
	message1 := commit.GetMessage()
	assert.Equal(t, "full", message1.GetFullMessage())
	assert.Equal(t, 2, len(commit.GetTags()))
}
