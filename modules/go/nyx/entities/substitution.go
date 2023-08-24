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
This object models a rule to replace some text matched by a regular expression in arbitrary text files
with a static or dynamic value that is configured within the rule.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Substitution struct {
	// The glob expression to select the text files to replace the matched strings into.
	Files *string `json:"files,omitempty" yaml:"files,omitempty"`

	// The regular expression used to match the text to be replaced replace in files.
	Match *string `json:"match,omitempty" yaml:"match,omitempty"`

	// The template expression defining the text to use when replacing all matched tokens.
	Replace *string `json:"replace,omitempty" yaml:"replace,omitempty"`
}

/*
Default constructor
*/
func NewSubstitution() *Substitution {
	return &Substitution{}
}

/*
Standard constructor.

Arguments are as follows:

  - files the glob expression to select the text files to replace the matched strings into.
  - match the regular expression used to match the text to be replaced replace in files.
  - replace the template expression defining the text to use when replacing all matched tokens.
*/
func NewSubstitutionWith(files *string, match *string, replace *string) *Substitution {
	s := Substitution{}

	s.Files = files
	s.Match = match
	s.Replace = replace

	return &s
}

/*
Returns the glob expression to select the text files to replace the matched strings into.
*/
func (s *Substitution) GetFiles() *string {
	return s.Files
}

/*
Sets the glob expression to select the text files to replace the matched strings into.
*/
func (s *Substitution) SetFiles(files *string) {
	s.Files = files
}

/*
Returns the regular expression used to match the text to be replaced replace in files.
*/
func (s *Substitution) GetMatch() *string {
	return s.Match
}

/*
Sets the regular expression used to match the text to be replaced replace in files.
*/
func (s *Substitution) SetMatch(match *string) {
	s.Match = match
}

/*
Returns the template expression defining the text to use when replacing all matched tokens.
*/
func (s *Substitution) GetReplace() *string {
	return s.Replace
}

/*
Sets the template expression defining the text to use when replacing all matched tokens.
*/
func (s *Substitution) SetReplace(replace *string) {
	s.Replace = replace
}
