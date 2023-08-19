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

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestReleaseTypeNewReleaseType(t *testing.T) {
	rt := NewReleaseType()

	// default constructor has its fields set to default values
	assert.Equal(t, RELEASE_TYPE_COLLAPSE_VERSIONS, rt.GetCollapseVersions())
	assert.Equal(t, RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, rt.GetCollapsedVersionQualifier())
	assert.Equal(t, RELEASE_TYPE_DESCRIPTION, rt.GetDescription())
	assert.Equal(t, RELEASE_TYPE_FILTER_TAGS, rt.GetFilterTags())
	assert.Equal(t, RELEASE_TYPE_GIT_COMMIT, rt.GetGitCommit())
	assert.Equal(t, RELEASE_TYPE_GIT_COMMIT_MESSAGE, rt.GetGitCommitMessage())
	assert.Equal(t, RELEASE_TYPE_GIT_PUSH, rt.GetGitPush())
	assert.Equal(t, RELEASE_TYPE_GIT_TAG, rt.GetGitTag())
	assert.Equal(t, RELEASE_TYPE_GIT_TAG_MESSAGE, rt.GetGitTagMessage())
	assert.Equal(t, RELEASE_TYPE_IDENTIFIERS, rt.GetIdentifiers())
	assert.Equal(t, RELEASE_TYPE_MATCH_BRANCHES, rt.GetMatchBranches())
	assert.Equal(t, RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, rt.GetMatchEnvironmentVariables())
	assert.Equal(t, RELEASE_TYPE_MATCH_WORKSPACE_STATUS, rt.GetMatchWorkspaceStatus())
	assert.Equal(t, RELEASE_TYPE_PUBLISH, rt.GetPublish())
	assert.Equal(t, RELEASE_TYPE_VERSION_RANGE, rt.GetVersionRange())
	assert.Equal(t, RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, rt.GetVersionRangeFromBranchName())
}

func TestReleaseTypeNewReleaseTypeWith(t *testing.T) {
	al := []*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}

	m := make(map[string]string)
	m["PATH"] = ".*"

	i1 := NewIdentifierWith(utl.PointerToString("alpha"), utl.PointerToString("any"), PointerToPosition(PRE_RELEASE))
	i2 := NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("123"), PointerToPosition(BUILD))
	l := []*Identifier{i1, i2}

	rt := NewReleaseTypeWith(&al, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), utl.PointerToString("Release description"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{}, &l, utl.PointerToString(""), &m, nil, utl.PointerToString("true"), utl.PointerToString(""), utl.PointerToBoolean(false))

	a := rt.GetAssets()
	assert.Equal(t, 2, len(*a))
	assert.Equal(t, "asset1", *(*a)[0])
	assert.Equal(t, "asset2", *(*a)[1])
	cv := rt.GetCollapseVersions()
	assert.Equal(t, true, *cv)
	cvq := rt.GetCollapsedVersionQualifier()
	assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *cvq)
	d := rt.GetDescription()
	assert.Equal(t, "Release description", *d)
	ft := rt.GetFilterTags()
	assert.Equal(t, "^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$", *ft)
	gc := rt.GetGitCommit()
	assert.Equal(t, "true", *gc)
	gcm := rt.GetGitCommitMessage()
	assert.Equal(t, "Committing {{version}}", *gcm)
	gp := rt.GetGitPush()
	assert.Equal(t, "true", *gp)
	gt := rt.GetGitTag()
	assert.Equal(t, "true", *gt)
	gtm := rt.GetGitTagMessage()
	assert.Equal(t, "Tagging {{version}}", *gtm)
	i := rt.GetIdentifiers()
	assert.Equal(t, l, *i)
	mb := rt.GetMatchBranches()
	assert.Equal(t, "", *mb)
	mev := rt.GetMatchEnvironmentVariables()
	assert.Equal(t, &m, mev)
	mws := rt.GetMatchWorkspaceStatus()
	assert.Nil(t, mws)
	p := rt.GetPublish()
	assert.Equal(t, "true", *p)
	vr := rt.GetVersionRange()
	assert.Equal(t, "", *vr)
	vrfbn := rt.GetVersionRangeFromBranchName()
	assert.Equal(t, false, *vrfbn)
}

func TestReleaseTypeGetAssets(t *testing.T) {
	al := []*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}

	releaseType := NewReleaseType()

	releaseType.SetAssets(&al)
	a := releaseType.GetAssets()
	assert.Equal(t, 2, len(*a))
	assert.Equal(t, "asset1", *(*a)[0])
	assert.Equal(t, "asset2", *(*a)[1])
}

func TestReleaseTypeGetCollapseVersions(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetCollapseVersions(utl.PointerToBoolean(true))
	cv := releaseType.GetCollapseVersions()
	assert.Equal(t, true, *cv)
}

func TestReleaseTypeGetCollapsedVersionQualifier(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetCollapsedVersionQualifier(utl.PointerToString("vqualifier"))
	cvq := releaseType.GetCollapsedVersionQualifier()
	assert.Equal(t, "vqualifier", *cvq)
}

func TestReleaseTypeGetDescription(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetDescription(utl.PointerToString("description"))
	d := releaseType.GetDescription()
	assert.Equal(t, "description", *d)
}

func TestReleaseTypeGetFilterTags(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetFilterTags(utl.PointerToString("true"))
	gft := releaseType.GetFilterTags()
	assert.Equal(t, "true", *gft)
}

func TestReleaseTypeGetGitCommit(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetGitCommit(utl.PointerToString("true"))
	gc := releaseType.GetGitCommit()
	assert.Equal(t, "true", *gc)
}

func TestReleaseTypeGetGitCommitMessage(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetGitCommitMessage(utl.PointerToString("message"))
	gcm := releaseType.GetGitCommitMessage()
	assert.Equal(t, "message", *gcm)
}

func TestReleaseTypeGetGitPush(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetGitPush(utl.PointerToString("true"))
	gp := releaseType.GetGitPush()
	assert.Equal(t, "true", *gp)
}

func TestReleaseTypeGetGitTag(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetGitTag(utl.PointerToString("tag"))
	gt := releaseType.GetGitTag()
	assert.Equal(t, "tag", *gt)
}

func TestReleaseTypeGetGitTagMessage(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetGitTagMessage(utl.PointerToString("message"))
	gtm := releaseType.GetGitTagMessage()
	assert.Equal(t, "message", *gtm)
}

func TestReleaseTypeGetIdentifiers(t *testing.T) {
	i1 := NewIdentifierWith(utl.PointerToString("alpha"), utl.PointerToString("any"), PointerToPosition(PRE_RELEASE))
	i2 := NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("123"), PointerToPosition(BUILD))
	l := []*Identifier{i1, i2}

	releaseType := NewReleaseType()

	releaseType.SetIdentifiers(&l)
	mev := releaseType.GetIdentifiers()
	assert.Equal(t, l, *mev)
}

func TestReleaseTypeGetMatchBranches(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetMatchBranches(utl.PointerToString("true"))
	mb := releaseType.GetMatchBranches()
	assert.Equal(t, "true", *mb)
}

func TestReleaseTypeGetMatchEnvironmentVariables(t *testing.T) {
	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	releaseType := NewReleaseType()

	releaseType.SetMatchEnvironmentVariables(&m)
	mev := releaseType.GetMatchEnvironmentVariables()
	assert.Equal(t, &m, mev)
}

func TestReleaseTypeGetMatchWorkspaceStatus(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetMatchWorkspaceStatus(PointerToWorkspaceStatus(DIRTY))
	ws := releaseType.GetMatchWorkspaceStatus()
	assert.Equal(t, DIRTY, *ws)
}

func TestReleaseTypeGetPublish(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetPublish(utl.PointerToString("true"))
	p := releaseType.GetPublish()
	assert.Equal(t, "true", *p)
}

func TestReleaseTypeGetVersionRange(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetVersionRange(utl.PointerToString("1.x"))
	vr := releaseType.GetVersionRange()
	assert.Equal(t, "1.x", *vr)
}

func TestReleaseTypeGetVersionRangeFromBranchName(t *testing.T) {
	releaseType := NewReleaseType()

	releaseType.SetVersionRangeFromBranchName(utl.PointerToBoolean(true))
	vfbn := releaseType.GetVersionRangeFromBranchName()
	assert.Equal(t, true, *vfbn)
}
