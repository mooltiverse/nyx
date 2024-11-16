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

package version

import (
	"fmt"     // https://pkg.go.dev/fmt
	"reflect" // https://pkg.go.dev/reflect
	"strings" // https://pkg.go.dev/strings
)

const (
	// The default separator used in the string representation if no other is specified.
	DEFAULT_SEPARATOR string = "."
)

/*
An identifier holding multiple child identifiers.
*/
type compositeIdentifier struct {
	// The nested identifiers.
	children []identifier

	// Store the string representation after the first rendering to avoid repetitive formatting.
	renderedString *string

	// The separator among children values in the string representation.
	separator string
}

/*
Transforms the given slice of generic interfaces in a slice with the same elements of type
identifier.
*/
func castSliceOfAnyToSliceOfIdentifiers(ids []interface{}) ([]identifier, error) {
	res := make([]identifier, len(ids))
	for i, item := range ids {
		id, ok := item.(identifier)
		if !ok {
			return nil, fmt.Errorf("unexpected error when casting identifiers: element %v cannot be cast to identifier interface", item)
		}
		res[i] = id
	}
	return res, nil
}

/*
Returns the underlying list of identifiers, children of this composite identifier
*/
func (ci compositeIdentifier) getValue() interface{} {
	return ci.children
}

/*
Returns the list of values held by child identifiers
*/
func (ci compositeIdentifier) getValues() []interface{} {
	res := make([]interface{}, len(ci.children))
	for i, item := range ci.children {
		res[i] = item.getValue()
	}
	return res
}

/*
Tests the given object for equality against this instance.
*/
func (ci compositeIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(compositeIdentifier)
	if !ok {
		return false
	}
	if ci.separator != otherIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(ci.children, otherIdentifier.children)
}

/*
Returns a string representation of the object, coherent with the identifier scope,
suitable for generating a version string.
*/
func (ci compositeIdentifier) String() string {
	if ci.renderedString == nil {
		var sb strings.Builder
		for i, identifier := range ci.children {
			if i > 0 {
				sb.WriteString(ci.separator)
			}
			sb.WriteString(identifier.String())
		}
		rs := sb.String()
		ci.renderedString = &rs
	}
	return *ci.renderedString
}
