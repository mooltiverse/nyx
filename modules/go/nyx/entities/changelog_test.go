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

	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestChangelogNewChangelog(t *testing.T) {
	changelog := NewChangelog()

	// default constructor has its fields set to default values
	assert.Nil(t, changelog.GetReleases())
}

func TestChangelogNewChangelogWith(t *testing.T) {
	releases := make([]*Release, 0)
	releases = append(releases, NewReleaseWith(utl.PointerToString("name1"), utl.PointerToString("date1")))
	releases = append(releases, NewReleaseWith(utl.PointerToString("name2"), utl.PointerToString("date2")))

	changelog := NewChangelogWith(releases)

	r := changelog.GetReleases()
	assert.Equal(t, releases, r)
	assert.Equal(t, 2, len(changelog.GetReleases()))
}

func TestChangelogGetReleases(t *testing.T) {
	releases := make([]*Release, 0)
	releases = append(releases, NewReleaseWith(utl.PointerToString("name1"), utl.PointerToString("date1")))
	releases = append(releases, NewReleaseWith(utl.PointerToString("name2"), utl.PointerToString("date2")))

	changelog := NewChangelog()

	changelog.SetReleases(releases)

	r := changelog.GetReleases()
	assert.Equal(t, releases, r)
	assert.Equal(t, 2, len(changelog.GetReleases()))
}

func TestReleaseNewRelease(t *testing.T) {
	release := NewRelease()

	// default constructor has its fields set to default values
	assert.Nil(t, release.GetDate())
	assert.Nil(t, release.GetName())
	assert.Equal(t, 0, len(release.GetSections()))
}

func TestReleaseNewReleaseWith(t *testing.T) {
	release := NewReleaseWith(utl.PointerToString("name"), utl.PointerToString("date"))

	n := release.GetName()
	d := release.GetDate()
	assert.Equal(t, "name", *n)
	assert.Equal(t, "date", *d)
	assert.Equal(t, 0, len(release.GetSections()))
}

func TestReleaseGetName(t *testing.T) {
	release := NewRelease()

	release.SetName(utl.PointerToString("name"))
	n := release.GetName()
	assert.Equal(t, "name", *n)
}

func TestReleaseGetDate(t *testing.T) {
	release := NewRelease()

	release.SetDate(utl.PointerToString("date"))
	d := release.GetDate()
	assert.Equal(t, "date", *d)
}

func TestReleaseGetSections(t *testing.T) {
	sections := make([]*Section, 0)
	sections = append(sections, NewSectionWith(utl.PointerToString("one"), nil))
	sections = append(sections, NewSectionWith(utl.PointerToString("two"), nil))

	release := NewRelease()

	release.SetSections(sections)
	s := release.GetSections()
	assert.Equal(t, sections, s)
}

func TestReleaseGetSection(t *testing.T) {
	release := NewRelease()

	s := release.GetSection("missing", false)
	assert.Nil(t, s)
	assert.Equal(t, 0, len(release.GetSections()))
	s = release.GetSection("missing", true)
	assert.Equal(t, "missing", *s.GetName())
	assert.Equal(t, 1, len(release.GetSections()))
}

func TestSectionNewSection(t *testing.T) {
	section := NewSection()

	// default constructor has its fields set to default values
	assert.Nil(t, section.GetName())
	assert.Equal(t, 0, len(section.GetCommits()))
}

func TestSectionNewSectionWith(t *testing.T) {
	commits := make([]*gitent.Commit, 0)
	commits = append(commits, gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))

	section := NewSectionWith(utl.PointerToString("section"), commits)

	n := section.GetName()
	assert.Equal(t, "section", *n)
	c := section.GetCommits()
	assert.Equal(t, commits, c)
}

func TestSectionGetName(t *testing.T) {
	section := NewSection()

	section.SetName(utl.PointerToString("name"))
	n := section.GetName()
	assert.Equal(t, "name", *n)
}

func TestSectionGetCommits(t *testing.T) {
	commits := make([]*gitent.Commit, 0)
	commits = append(commits, gitent.NewCommitWith("e7c4419c1a9635a264b1d6c573ac2af71e1eeea6", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("f9422bd6e5b0ac0ab0df2bffc280c3d4caa11b44", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("d0a19fc5776dc0c0b1a8d869c1117dac71065870", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))

	section := NewSection()

	section.SetCommits(commits)
	c := section.GetCommits()
	assert.Equal(t, commits, c)
}
