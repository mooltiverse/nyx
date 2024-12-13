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
	"encoding/json" // https://pkg.go.dev/encoding/json

	gitent "github.com/mooltiverse/nyx/src/go/nyx/entities/git"
)

/*
This is a value object that models the summary data about the scope of a release.
*/
type ReleaseScope struct {
	// The internal list of commits in the scope. Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
	Commits []*gitent.Commit `json:"commits,omitempty" yaml:"commits,omitempty" handlebars:"commits"`

	// The version identifier of the most recent past release.
	PreviousVersion *string `json:"previousVersion,omitempty" yaml:"previousVersion,omitempty" handlebars:"previousVersion"`

	// The most recent past release commit.
	PreviousVersionCommit *gitent.Commit `json:"previousVersionCommit,omitempty" yaml:"previousVersionCommit,omitempty" handlebars:"previousVersionCommit"`

	// The version identifier of the most recent past release with only core identifiers.
	PrimeVersion *string `json:"primeVersion,omitempty" yaml:"primeVersion,omitempty" handlebars:"primeVersion"`

	// The most recent past release commit with only core identifiers.
	PrimeVersionCommit *gitent.Commit `json:"primeVersionCommit,omitempty" yaml:"primeVersionCommit,omitempty" handlebars:"primeVersionCommit"`

	// The list of significant commits (those commits causing the version number to be bumped). Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
	SignificantCommits []*gitent.Commit `json:"significantCommits,omitempty" yaml:"significantCommits,omitempty" handlebars:"significantCommits"`
}

/*
A flat representation of the ReleaseScope structure where al values are resolved and stored as local fiends instead of retrieving
them through getter methods.

This structure is detached from the ReleaseScope but is useful when marshalling or rendering the ReleaseScope.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.

Marshalling is handled by the MarshalJSON() and MarshalYAML() methods. Unmarshalling does not require any custom handling
because transient fields are to be ignored when unmarshalling.

Objects of this type can be retrieved using Flatten().
*/
type FlatReleaseScope struct {
	// The internal list of commits in the scope. Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
	Commits []*gitent.Commit `json:"commits,omitempty" yaml:"commits,omitempty" handlebars:"commits"`

	// The cached value for the final commit within the scope. It's required to cache this value or marshalling/unmarshalling won't work
	FinalCommitCache *gitent.Commit `json:"finalCommit,omitempty" yaml:"finalCommit,omitempty" handlebars:"finalCommit"`

	// The cached value for the initial commit within the scope. It's required to cache this value or marshalling/unmarshalling won't work
	InitialCommitCache *gitent.Commit `json:"initialCommit,omitempty" yaml:"initialCommit,omitempty" handlebars:"initialCommit"`

	// The version identifier of the most recent past release.
	PreviousVersion *string `json:"previousVersion,omitempty" yaml:"previousVersion,omitempty" handlebars:"previousVersion"`

	// The most recent past release commit.
	PreviousVersionCommit *gitent.Commit `json:"previousVersionCommit,omitempty" yaml:"previousVersionCommit,omitempty" handlebars:"previousVersionCommit"`

	// The version identifier of the most recent past release with only core identifiers.
	PrimeVersion *string `json:"primeVersion,omitempty" yaml:"primeVersion,omitempty" handlebars:"primeVersion"`

	// The most recent past release commit with only core identifiers.
	PrimeVersionCommit *gitent.Commit `json:"primeVersionCommit,omitempty" yaml:"primeVersionCommit,omitempty" handlebars:"primeVersionCommit"`

	// The list of significant commits (those commits causing the version number to be bumped). Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
	SignificantCommits []*gitent.Commit `json:"significantCommits,omitempty" yaml:"significantCommits,omitempty" handlebars:"significantCommits"`
}

/*
Default constructor
*/
func NewReleaseScope() *ReleaseScope {
	releaseScope := ReleaseScope{}

	releaseScope.Commits = make([]*gitent.Commit, 0)
	releaseScope.SignificantCommits = make([]*gitent.Commit, 0)

	return &releaseScope
}

/*
Resolves the cached values so they're up to date when marshalling occurs.

The returned object is a copy of this one with all of the values resolved.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (r *ReleaseScope) Flatten() (*FlatReleaseScope, error) {
	// Invoking all the getter methods causes this object to resolve all fields, even those that weren't
	// resolved before.
	resolvedReleaseScope := &FlatReleaseScope{}

	resolvedReleaseScope.Commits = r.GetCommits()
	resolvedReleaseScope.FinalCommitCache = r.GetFinalCommit()
	resolvedReleaseScope.InitialCommitCache = r.GetInitialCommit()
	resolvedReleaseScope.PreviousVersion = r.GetPreviousVersion()
	resolvedReleaseScope.PreviousVersionCommit = r.GetPreviousVersionCommit()
	resolvedReleaseScope.PrimeVersion = r.GetPrimeVersion()
	resolvedReleaseScope.PrimeVersionCommit = r.GetPrimeVersionCommit()
	resolvedReleaseScope.SignificantCommits = r.GetSignificantCommits()

	return resolvedReleaseScope, nil
}

/*
Adds the JSON marshalling feature to the release scope object

This method implements the Marshaler interface in the JSON package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (r *ReleaseScope) MarshalJSON() ([]byte, error) {
	flatReleaseScope, err := r.Flatten()
	if err != nil {
		return nil, err
	}

	// marshal the resolved copy to avoid recursion and a stack overflow
	return json.Marshal(flatReleaseScope)
}

/*
Adds the YAML marshalling feature to the release scope object.

This method implements the Marshaler interface in the YAML package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (r *ReleaseScope) MarshalYAML() (interface{}, error) {
	flatReleaseScope, err := r.Flatten()
	if err != nil {
		return nil, err
	}

	// marshal the resolved copy to avoid recursion and a stack overflow
	return flatReleaseScope, nil
}

/*
Returns the list of commits in the scope.
Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
*/
func (rs *ReleaseScope) GetCommits() []*gitent.Commit {
	return rs.Commits
}

/*
Sets the list of commits in the scope.
Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
*/
func (rs *ReleaseScope) SetCommits(commits []*gitent.Commit) {
	rs.Commits = commits
}

/*
Returns the version identifier of the most recent past release.
*/
func (rs *ReleaseScope) GetPreviousVersion() *string {
	return rs.PreviousVersion
}

/*
Returns true if the scope has a non nil most recent past release.
*/
func (rs *ReleaseScope) HasPreviousVersion() bool {
	if rs.PreviousVersion == nil {
		return false
	} else {
		return true
	}
}

/*
Sets the version identifier of the most recent past release.
*/
func (rs *ReleaseScope) SetPreviousVersion(previousVersion *string) {
	rs.PreviousVersion = previousVersion
}

/*
Returns the most recent past release commit.
*/
func (rs *ReleaseScope) GetPreviousVersionCommit() *gitent.Commit {
	return rs.PreviousVersionCommit
}

/*
Returns true if the scope has a non nil most recent past release commit.
*/
func (rs *ReleaseScope) HasPreviousVersionCommit() bool {
	if rs.PreviousVersionCommit == nil {
		return false
	} else {
		return true
	}
}

/*
Sets the most recent past release commit.
*/
func (rs *ReleaseScope) SetPreviousVersionCommit(previousVersionCommit *gitent.Commit) {
	rs.PreviousVersionCommit = previousVersionCommit
}

/*
Returns true if the scope has a non nil most recent past release with only core identifiers.
*/
func (rs *ReleaseScope) GetPrimeVersion() *string {
	return rs.PrimeVersion
}

/*
Returns true if the scope has a non nil most recent past release.
*/
func (rs *ReleaseScope) HasPrimeVersion() bool {
	if rs.PrimeVersion == nil {
		return false
	} else {
		return true
	}
}

/*
Sets the version identifier of the most recent past release with only core identifiers.
*/
func (rs *ReleaseScope) SetPrimeVersion(primeVersion *string) {
	rs.PrimeVersion = primeVersion
}

/*
Returns the most recent past release commit with only core identifiers.
*/
func (rs *ReleaseScope) GetPrimeVersionCommit() *gitent.Commit {
	return rs.PrimeVersionCommit
}

/*
Returns true if the scope has a non nil most recent past release commit with only core identifiers.
*/
func (rs *ReleaseScope) HasPrimeVersionCommit() bool {
	if rs.PrimeVersionCommit == nil {
		return false
	} else {
		return true
	}
}

/*
Sets the most recent past release commit with only core identifiers.
*/
func (rs *ReleaseScope) SetPrimeVersionCommit(primeVersionCommit *gitent.Commit) {
	rs.PrimeVersionCommit = primeVersionCommit
}

/*
Returns the first commit within the scope. It may be nil.
*/
func (rs *ReleaseScope) GetInitialCommit() *gitent.Commit {
	if rs.Commits == nil {
		return nil
	} else if len(rs.Commits) == 0 {
		return nil
	} else {
		return (rs.Commits)[len(rs.Commits)-1]
	}
}

/*
Returns true if the scope has a non nil first commit within the scope.
*/
func (rs *ReleaseScope) HasInitialCommit() bool {
	if rs.Commits == nil {
		return false
	} else {
		return len(rs.Commits) > 0
	}
}

/*
Returns the final commit within the scope. It may be nil.
*/
func (rs *ReleaseScope) GetFinalCommit() *gitent.Commit {
	if rs.Commits == nil {
		return nil
	} else if len(rs.Commits) == 0 {
		return nil
	} else {
		return (rs.Commits)[0]
	}
}

/*
Returns true if the scope has a non nil final commit within the scope.
*/
func (rs *ReleaseScope) HasFinalCommit() bool {
	if rs.Commits == nil {
		return false
	} else {
		return len(rs.Commits) > 0
	}
}

/*
Returns the list of significant commits (those commits causing the version number to be bumped).
Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
*/
func (rs *ReleaseScope) GetSignificantCommits() []*gitent.Commit {
	return rs.SignificantCommits
}

/*
Sets the list of significant commits (those commits causing the version number to be bumped).
Elements are in reverse order so the newest commit is at position 0 and the oldest is in the final position.
*/
func (rs *ReleaseScope) SetSignificantCommits(significantCommits []*gitent.Commit) {
	rs.SignificantCommits = significantCommits
}
