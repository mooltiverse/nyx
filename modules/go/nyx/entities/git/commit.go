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

/*
This object is a Git commit value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Commit struct {
	// The author data
	AuthorAction Action `json:"authorAction" yaml:"authorAction"`

	// The committer data
	CommitAction Action `json:"commitAction" yaml:"commitAction"`

	// The commit date
	Date int64 `json:"date,omitempty" yaml:"date,omitempty"`

	// The commit message
	Message Message `json:"message" yaml:"message"`

	// The parents SHA's.
	Parents []string `json:"parents" yaml:"parents"`

	// The tags associated to the commit.
	Tags []Tag `json:"tags" yaml:"tags"`

	// The commit SHA-1 identifier.
	Sha string `json:"sha" yaml:"sha"`
}

/*
Standard constructor.

Arguments are as follows:

- sha the commit SHA-1 identifier
- date the commit date
- parents the SHA-1 identifiers of parent commits. If the commit has no parents an empty list must be passed
- authorAction the value holder about the author
- commitAction the value holder about the committer
- message the commit message
- tags the tags applied to this commit. If the commit has no tags an empty set must be passed
*/
func NewCommitWith(sha string, date int64, parents []string, authorAction Action, commitAction Action, message Message, tags []Tag) *Commit {
	c := Commit{}

	c.AuthorAction = authorAction
	c.CommitAction = commitAction
	c.Date = date
	c.Message = message
	c.Parents = parents
	c.Tags = tags
	c.Sha = sha

	return &c
}

/*
Returns the author data.
*/
func (c Commit) GetAuthorAction() Action {
	return c.AuthorAction
}

/*
Returns the committer data.
*/
func (c Commit) GetCommitAction() Action {
	return c.CommitAction
}

/*
Returns the commit date.
*/
func (c Commit) GetDate() int64 {
	return c.Date
}

/*
Returns the commit message.
*/
func (c Commit) GetMessage() Message {
	return c.Message
}

/*
Returns the immutable list of parent commit SHA-1 identifiers.
*/
func (c Commit) GetParents() []string {
	return c.Parents
}

/*
Returns the immutable list of tags pointing to this commit.
*/
func (c Commit) GetTags() []Tag {
	return c.Tags
}

/*
Returns the SHA-1 identifier for the commit.
*/
func (c Commit) GetSHA() string {
	return c.Sha
}

/*
Returns the string representation of the commit.
*/
func (c Commit) String() string {
	return c.Sha
}
