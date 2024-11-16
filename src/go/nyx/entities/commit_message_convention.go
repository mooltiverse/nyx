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

/*
This object models the fields used to configure a generic commit message convention.

The regular expression associated with this object is important and needs to define the following named capturing groups:
- type yields to the commit message type (if any)
- scope yields to the commit message scope (if any)
- title yields to the commit message title (the short, 1 line, description of the commit)

The bump regular expressions provided with this object are used to determine if a commit message is meant to bump a
version identifier. Each entry in the map has a version identifier as the key and a regular expression as the value.
When the regular expression in the value matches the commit message then the identifier in the key has to be bumped.
These regular expressions are evaluated simply, just match or no-match, without named groups etc.
The order of the entries does not matter as in case of multiple matches only the most significant identifier is
bumped. Identifier names depend on the versioning scheme in use.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type CommitMessageConvention struct {
	// The regular expression used to parse informations from a commit message.
	Expression *string `json:"expression,omitempty" yaml:"expression,omitempty"`

	// The map where each key is a version identifier to bump and the value is a regular expression to be evaluated
	// against the commit message. When the expression matches the commit message the version identifier
	// in the key is to be bumped.
	BumpExpressions *map[string]string `json:"bumpExpressions,omitempty" yaml:"bumpExpressions,omitempty"`
}

/*
Default constructor
*/
func NewCommitMessageConvention() *CommitMessageConvention {
	return &CommitMessageConvention{}
}

/*
Standard constructor.

Arguments are as follows:

- expression the regular expression used to parse informations from a commit message. It must comply with the requirements define on top of this class documentation.
- bumpExpressions the map where each key is a version identifier to bump and the value is a regular expression to be evaluated against the commit message. When the expression matches the commit message
  the version identifier in the key is to be bumped. It must comply with the requirements define on top of this class documentation.
*/
func NewCommitMessageConventionWith(expression *string, bumpExpressions *map[string]string) *CommitMessageConvention {
	cmm := CommitMessageConvention{}

	cmm.Expression = expression
	cmm.BumpExpressions = bumpExpressions

	return &cmm
}

/*
Returns the regular expression used to parse informations from a commit message.
*/
func (cmc *CommitMessageConvention) GetExpression() *string {
	return cmc.Expression
}

/*
Sets the regular expression used to parse informations from a commit message.
*/
func (cmc *CommitMessageConvention) SetExpression(expression *string) {
	cmc.Expression = expression
}

/*
Returns the map where each key is a version identifier to bump and the value is a regular expression to be evaluated
against the commit message. When the expression matches the commit message the version identifier in the key is to be bumped.
*/
func (cmc *CommitMessageConvention) GetBumpExpressions() *map[string]string {
	return cmc.BumpExpressions
}

/*
Sets the map where each key is a version identifier to bump and the value is a regular expression to be evaluated
against the commit message. When the expression matches the commit message the version identifier in the key is to be bumped.
*/
func (cmc *CommitMessageConvention) SetBumpExpressions(bumpExpressions *map[string]string) {
	cmc.BumpExpressions = bumpExpressions
}
