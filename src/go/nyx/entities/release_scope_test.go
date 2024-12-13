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

package entities

import (
	"testing" // https://pkg.go.dev/testing
	"time"    // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	gitent "github.com/mooltiverse/nyx/src/go/nyx/entities/git"
	utl "github.com/mooltiverse/nyx/src/go/utils"
	ver "github.com/mooltiverse/nyx/src/go/version"
)

func TestReleaseScopeNewReleaseScope(t *testing.T) {
	rs := NewReleaseScope()

	// default constructor has its fields set to default values
	assert.Equal(t, make([]*gitent.Commit, 0), rs.GetCommits())
	assert.Nil(t, rs.GetPreviousVersion())
	assert.False(t, rs.HasPreviousVersion())
	assert.Nil(t, rs.GetPreviousVersionCommit())
	assert.False(t, rs.HasPreviousVersionCommit())
	assert.Nil(t, rs.GetPrimeVersion())
	assert.False(t, rs.HasPrimeVersion())
	assert.Nil(t, rs.GetPrimeVersionCommit())
	assert.False(t, rs.HasPrimeVersionCommit())
	assert.Nil(t, rs.GetInitialCommit())
	assert.False(t, rs.HasInitialCommit())
	assert.Nil(t, rs.GetFinalCommit())
	assert.False(t, rs.HasFinalCommit())
	assert.Equal(t, make([]*gitent.Commit, 0), rs.GetSignificantCommits())
}

func TestReleaseScopeGetCommits(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 1, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 2, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 3, len(releaseScope.GetCommits()))
}

func TestReleaseScopeGetPreviousVersion(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetPreviousVersion())
	releaseScope.SetPreviousVersion(utl.PointerToString(ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION))
	pv := releaseScope.GetPreviousVersion()
	assert.Equal(t, ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, *pv)
}

func TestReleaseScopeHasPreviousVersion(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasPreviousVersion())
	releaseScope.SetPreviousVersion(utl.PointerToString(ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION))
	assert.True(t, releaseScope.HasPreviousVersion())
}

func TestReleaseScopeGetPreviousVersionCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetPreviousVersionCommit())
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	pvc := releaseScope.GetPreviousVersionCommit()
	assert.Equal(t, "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", pvc.GetSHA())
}

func TestReleaseScopeHasPreviousVersionCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasPreviousVersionCommit())
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	assert.True(t, releaseScope.HasPreviousVersionCommit())
}

func TestReleaseScopeGetPrimeVersion(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetPrimeVersion())
	releaseScope.SetPrimeVersion(utl.PointerToString(ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION))
	pv := releaseScope.GetPrimeVersion()
	assert.Equal(t, ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, *pv)
}

func TestReleaseScopeHasPrimeVersion(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasPrimeVersion())
	releaseScope.SetPrimeVersion(utl.PointerToString(ver.SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION))
	assert.True(t, releaseScope.HasPrimeVersion())
}

func TestReleaseScopeGetPrimeVersionCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetPrimeVersionCommit())
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	pvc := releaseScope.GetPrimeVersionCommit()
	assert.Equal(t, "f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", pvc.GetSHA())
}

func TestReleaseScopeHasPrimeVersionCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasPrimeVersionCommit())
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	assert.True(t, releaseScope.HasPrimeVersionCommit())
}

func TestReleaseScopeGetInitialCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetInitialCommit())
	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 1, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 2, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	ic := releaseScope.GetInitialCommit()
	assert.Equal(t, "d0a19fc5776dc0c0b1a8d869c1117dac71065870", ic.GetSHA())
}

func TestReleaseScopeHasInitialCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasInitialCommit())
	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.True(t, releaseScope.HasInitialCommit())
}

func TestReleaseScopeGetFinalCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Nil(t, releaseScope.GetFinalCommit())
	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 1, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.Equal(t, 2, len(releaseScope.GetCommits()))
	newCommits = append(releaseScope.GetCommits(), gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	ic := releaseScope.GetFinalCommit()
	assert.Equal(t, "e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", ic.GetSHA())
}

func TestReleaseScopeHasFinalCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.False(t, releaseScope.HasFinalCommit())
	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(newCommits)
	assert.True(t, releaseScope.HasFinalCommit())
}

func TestReleaseScopeGetSignificantCommit(t *testing.T) {
	releaseScope := NewReleaseScope()

	assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
	assert.Equal(t, make([]*gitent.Commit, 0), releaseScope.GetSignificantCommits())
	newCommits := append(releaseScope.GetCommits(), gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetSignificantCommits(newCommits)
	assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
	newCommits = append(releaseScope.GetSignificantCommits(), gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetSignificantCommits(newCommits)
	assert.Equal(t, 2, len(releaseScope.GetSignificantCommits()))
	newCommits = append(releaseScope.GetSignificantCommits(), gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetSignificantCommits(newCommits)
	assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
}
