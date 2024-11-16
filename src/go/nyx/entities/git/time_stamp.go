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

import (
	"time" // https://pkg.go.dev/time
)

/*
This object is a Git timestamp value holder independent from the underlying Git implementation.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type TimeStamp struct {
	// The time stamp.
	TimeStamp int64 `json:"timeStamp,omitempty" yaml:"timeStamp,omitempty"`

	// The time zone offset in minutes relative to UTC.
	Offset *int `json:"offset,omitempty" yaml:"offset,omitempty"`
}

/*
Standard constructor.

Arguments are as follows:

- tm the time object
*/
func NewTimeStampFrom(tm time.Time) *TimeStamp {
	t := TimeStamp{}

	t.TimeStamp = tm.UnixMilli()
	_, offset := tm.Zone()
	// the time package uses seconds for the offset, we need to convert in minutes
	offset = offset / 60
	t.Offset = &offset

	return &t
}

/*
Standard constructor.

Arguments are as follows:

- timeStamp the time stamp
*/
func NewTimeStampWith(timeStamp int64) *TimeStamp {
	t := TimeStamp{}

	t.TimeStamp = timeStamp

	return &t
}

/*
Standard constructor.

Arguments are as follows:

- timeStamp the time stamp
- offset the time zone offset in minutes relative to UTC. It may be nil.
*/
func NewTimeStampWithIn(timeStamp int64, offset *int) *TimeStamp {
	t := TimeStamp{}

	t.TimeStamp = timeStamp
	t.Offset = offset

	return &t
}

/*
Returns the time stamp.
*/
func (t TimeStamp) GetTimeStamp() int64 {
	return t.TimeStamp
}

/*
Returns the time zone. It may be nil.
*/
func (t TimeStamp) GetOffset() *int {
	return t.Offset
}

/*
Returns the string representation of the time stamp.
*/
func (t TimeStamp) String() string {
	return t.ToTime().String()
}

/*
Returns the Go Time representation of this object.
*/
func (t TimeStamp) ToTime() time.Time {
	res := time.UnixMilli(t.TimeStamp)
	if t.Offset == nil {
		return res.UTC()
	} else {
		// "UTC" here may be replaced by an empty string? It looks we have no way to create a zone with just the offset, without the name
		return res.UTC().In(time.FixedZone("UTC", *t.Offset))
	}
}
