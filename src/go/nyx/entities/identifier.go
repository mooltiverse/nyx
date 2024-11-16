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
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/src/go/errors"
)

/*
This enum maps allowed values for a custom identifier position (where the identifier has to be placed in version names).
*/
type Position string

// These are the Position values
const (
	// The identifier has to be placed in the pre-release part of the version (when using Semantic Versioning - https://semver.org/).
	PRE_RELEASE Position = "PRE_RELEASE"

	// The identifier has to be placed in the build part of the version (when using Semantic Versioning - https://semver.org/).
	BUILD Position = "BUILD"
)

/*
Returns the string representation of the position
*/
func (p Position) String() string {
	switch p {
	case PRE_RELEASE:
		return "PRE_RELEASE"
	case BUILD:
		return "BUILD"
	default:
		// this is never reached, but in case...
		panic("unknown Position. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the position corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown position is passed
*/
func ValueOfPosition(s string) (Position, error) {
	switch s {
	case "PRE_RELEASE":
		return PRE_RELEASE, nil
	case "BUILD":
		return BUILD, nil
	default:
		return BUILD, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal position '%s'", s)}
	}
}

/*
This object models a custom identifier to be used in version names. Each custom identifier is made of an optional
qualifier (which will appear as the leftmost identifier, if present), an optional value (which will appear as the
rightmost identifier, if present) and a position, indicating in which part of the version the identifier has to be
placed.
<br>
At least one among the qualifier or the value must be present. The qualifier and the value can be templates to be
rendered.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type Identifier struct {
	// The identifier qualifier.
	Qualifier *string `json:"qualifier,omitempty" yaml:"qualifier,omitempty"`

	// The identifier value.
	Value *string `json:"value,omitempty" yaml:"value,omitempty"`

	// The identifier position.
	Position *Position `json:"position,omitempty" yaml:"position,omitempty"`
}

/*
Default constructor.
*/
func NewIdentifier() *Identifier {
	return &Identifier{}
}

/*
Standard constructor.

Arguments are as follows:

- qualifier the identifier qualifier
- value the identifier value
- position the identifier position
*/
func NewIdentifierWith(qualifier *string, value *string, position *Position) *Identifier {
	i := Identifier{}

	i.Qualifier = qualifier
	i.Value = value
	i.Position = position

	return &i
}

/*
Returns the identifier qualifier
*/
func (i *Identifier) GetQualifier() *string {
	return i.Qualifier
}

/*
Sets the identifier qualifier
*/
func (i *Identifier) SetQualifier(qualifier *string) {
	i.Qualifier = qualifier
}

/*
Returns the identifier value
*/
func (i *Identifier) GetValue() *string {
	return i.Value
}

/*
Sets the identifier value
*/
func (i *Identifier) SetValue(value *string) {
	i.Value = value
}

/*
Returns the identifier position
*/
func (i *Identifier) GetPosition() *Position {
	return i.Position
}

/*
Sets the identifier position
*/
func (i *Identifier) SetPosition(position *Position) {
	i.Position = position
}
