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
This object is a Git commit message value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Message struct {
	// The message footer lines, where keys are names and values are values.
	Footers map[string]string `json:"footers,omitempty" yaml:"footers,omitempty"`

	// The full message.
	FullMessage string `json:"fullMessage,omitempty" yaml:"fullMessage,omitempty"`

	// The short message.
	ShortMessage string `json:"shortMessage,omitempty" yaml:"shortMessage,omitempty"`
}

/*
Standard constructor.

Arguments are as follows:

- fullMessage the full message
- shortMessage the short message
- footers the map of message footers, where keys are names and values are values
*/
func NewMessageWith(fullMessage string, shortMessage string, footers map[string]string) *Message {
	m := Message{}

	m.FullMessage = fullMessage
	m.ShortMessage = shortMessage
	m.Footers = footers

	return &m
}

/*
Returns the immutable list of footers, where keys are names and values are values.
*/
func (m Message) GeFooters() map[string]string {
	return m.Footers
}

/*
Returns the full message.
*/
func (m Message) GetFullMessage() string {
	return m.FullMessage
}

/*
Returns the short message.
*/
func (m Message) GetShortMessage() string {
	return m.ShortMessage
}

/*
Returns the string representation of the message.
*/
func (m Message) String() string {
	if m.FullMessage == "" {
		return m.ShortMessage
	} else {
		return m.ShortMessage + " ..."
	}
}
