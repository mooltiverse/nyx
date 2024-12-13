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
)

/*
A composite identifier that only accepts StringIdentifierss as children.
*/
type compositeStringIdentifier struct {
	// Extend compositeIdentifier by composition
	compositeIdentifier

	// The nested identifiers. This is redundant with the member from the nested compositeIdentifier
	// but it's typed so we can save methods with (up/down) casting.
	children []stringIdentifier
}

/*
Builds the identifier with the given nested identifiers and separator.
*/
func newCompositeStringIdentifierWithSeparator(separator string, children ...stringIdentifier) (compositeStringIdentifier, error) {
	identifier := compositeStringIdentifier{}

	identifiers, err := castSliceOfStringIdentifiersToSliceOfIdentifiers(children)
	if err != nil {
		return compositeStringIdentifier{}, fmt.Errorf("unexpected error when downcasting identifiers: %w", err)
	}

	identifier.compositeIdentifier.separator = separator
	identifier.compositeIdentifier.children = identifiers
	identifier.children = children

	return identifier, nil
}

/*
Builds the identifier with the given nested identifiers and the default separator.
*/
func newCompositeStringIdentifier(children ...stringIdentifier) (compositeStringIdentifier, error) {
	identifier := compositeStringIdentifier{}

	identifiers, err := castSliceOfStringIdentifiersToSliceOfIdentifiers(children)
	if err != nil {
		return compositeStringIdentifier{}, fmt.Errorf("unexpected error when downcasting identifiers: %w", err)
	}

	identifier.compositeIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeIdentifier.children = identifiers
	identifier.children = children

	return identifier, nil
}

/*
Transforms the given slice of elements in a slice with the same elements of type
identifier.
*/
func castSliceOfStringIdentifiersToSliceOfIdentifiers(ids []stringIdentifier) ([]identifier, error) {
	res := make([]identifier, len(ids))
	for i, item := range ids {
		res[i] = item
	}
	return res, nil
}

/*
Returns the string at the given position (the first being at position 0)
*/
func (csi compositeStringIdentifier) get(i uint) string {
	return csi.children[i].getStringValue()
}

/*
Tests the given object for equality against this instance.
*/
func (csi compositeStringIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(compositeStringIdentifier)
	if !ok {
		return false
	}
	if csi.compositeIdentifier.separator != otherIdentifier.compositeIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(csi.children, otherIdentifier.children)
}
