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
This object is a Git action value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Action struct {
	// The identity.
	Identity Identity `json:"identity" yaml:"identity"`

	// The time stamp.
	TimeStamp TimeStamp `json:"timeStamp,omitempty" yaml:"timeStamp,omitempty"`
}

/*
Standard constructor.

Arguments are as follows:

- identity the identity
- timeStamp the time stamp
*/
func NewActionWith(identity Identity, timeStamp TimeStamp) *Action {
	a := Action{}

	a.Identity = identity
	a.TimeStamp = timeStamp

	return &a
}

/*
Returns the identity.
*/
func (a Action) GetIdentity() Identity {
	return a.Identity
}

/*
Returns the time stamp.
*/
func (a Action) GetTimeStamp() TimeStamp {
	return a.TimeStamp
}

/*
Returns the string representation of the action.
*/
func (a Action) String() string {
	return a.TimeStamp.String() + " " + a.TimeStamp.String()
}
