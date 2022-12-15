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
	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
)

/*
This object models the data model of a changelog.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Changelog struct {
	// The internal list of releases.
	Releases []*Release `json:"releases,omitempty" yaml:"releases,omitempty"`
}

/*
Default constructor
*/
func NewChangelog() *Changelog {
	return &Changelog{}
}

/*
Standard constructor.

Arguments are as follows:

- releases the list of releases
*/
func NewChangelogWith(releases []*Release) *Changelog {
	changelog := Changelog{}

	changelog.Releases = releases

	return &changelog
}

/*
Returns the list of releases.
*/
func (c *Changelog) GetReleases() []*Release {
	return c.Releases
}

/*
Sets the list of releases.
*/
func (c *Changelog) SetReleases(releases []*Release) {
	c.Releases = releases
}

/*
This object models a single release in a changelog.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Release struct {
	// The release date attribute
	Date *string `json:"date,omitempty" yaml:"date,omitempty"`

	// The release name attribute
	Name *string `json:"name,omitempty" yaml:"name,omitempty"`

	// The changelog release sections.
	Sections []*Section `json:"sections,omitempty" yaml:"sections,omitempty"`
}

/*
Default constructor
*/
func NewRelease() *Release {
	release := Release{}

	release.Sections = make([]*Section, 0)

	return &release
}

/*
Standard constructor.

Arguments are as follows:

- name the release name
- date the release date in a string format
*/
func NewReleaseWith(name *string, date *string) *Release {
	release := Release{}

	release.Name = name
	release.Date = date
	release.Sections = make([]*Section, 0)

	return &release
}

/*
Returns the release date in a string format.
*/
func (r *Release) GetDate() *string {
	return r.Date
}

/*
Sets the release date in a string format.
*/
func (r *Release) SetDate(date *string) {
	r.Date = date
}

/*
Returns the release name.
*/
func (r *Release) GetName() *string {
	return r.Name
}

/*
Sets the release name.
*/
func (r *Release) SetName(name *string) {
	r.Name = name
}

/*
Returns the changelog release sections.
*/
func (r *Release) GetSections() []*Section {
	return r.Sections
}

/*
Returns the changelog release section with the given name, if any, optionally creating if it doesn't exists and create is true.

Arguments are as follows:

  - name the name of the section to return
  - create when true and no section with the given name exists, the section is created, appended to the end of existing sections, and returned,
    otherwise nil is returned when no section with the given name exists.
*/
func (r *Release) GetSection(name string, create bool) *Section {
	for _, s := range r.Sections {
		if name == *s.GetName() {
			return s
		}
	}
	if create {
		s := NewSection()
		s.SetName(&name)
		r.Sections = append(r.Sections, s)
		return s
	}

	return nil
}

/*
Sets the changelog release sections.
*/
func (r *Release) SetSections(sections []*Section) {
	r.Sections = sections
}

/*
This object models a single section in a changelog release.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Section struct {
	// The section name attribute
	Name *string `json:"name,omitempty" yaml:"name,omitempty"`

	// The changelog section commits.
	Commits []*gitent.Commit `json:"commits,omitempty" yaml:"commits,omitempty"`
}

/*
Default constructor
*/
func NewSection() *Section {
	section := Section{}

	section.Commits = make([]*gitent.Commit, 0)

	return &section
}

/*
Standard constructor.

Arguments are as follows:

- name the section name
- commits the section commits
*/
func NewSectionWith(name *string, commits []*gitent.Commit) *Section {
	section := Section{}

	section.Name = name
	section.Commits = commits

	if section.Commits == nil {
		section.Commits = make([]*gitent.Commit, 0)
	}

	return &section
}

/*
Returns the section name.
*/
func (s *Section) GetName() *string {
	return s.Name
}

/*
Sets the section name.
*/
func (s *Section) SetName(name *string) {
	s.Name = name
}

/*
Returns the section commits.
*/
func (s *Section) GetCommits() []*gitent.Commit {
	return s.Commits
}

/*
Sets the section commits.
*/
func (s *Section) SetCommits(commits []*gitent.Commit) {
	s.Commits = commits
}
