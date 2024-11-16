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
	"reflect" // https://pkg.go.dev/reflect
)

/*
A composite identifier that only accepts simpleIdentifiers as children.
*/
type compositeObjectIdentifier struct {
	// Extend compositeIdentifier by composition
	compositeIdentifier

	// The nested identifiers. This is redundant with the member from the nested compositeIdentifier
	// but it's typed so we can save methods with (up/down) casting.
	children []identifier
}

/*
Builds the identifier with the given nested identifiers and separator.
*/
func newCompositeObjectIdentifierWithSeparator(separator string, children ...identifier) (compositeObjectIdentifier, error) {
	identifier := compositeObjectIdentifier{}

	identifier.compositeIdentifier.separator = separator
	identifier.compositeIdentifier.children = children
	identifier.children = children

	return identifier, nil
}

/*
Builds the identifier with the given nested identifiers and the default separator.
*/
func newCompositeObjectIdentifier(children ...identifier) (compositeObjectIdentifier, error) {
	identifier := compositeObjectIdentifier{}

	identifier.compositeIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeIdentifier.children = children
	identifier.children = children

	return identifier, nil
}

/*
Returns the object at the given position (the first being at position 0)
*/
func (coi compositeObjectIdentifier) get(i uint) interface{} {
	return coi.children[i].getValue()
}

/*
Tests the given object for equality against this instance.
*/
func (coi compositeObjectIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(compositeObjectIdentifier)
	if !ok {
		return false
	}
	if coi.compositeIdentifier.separator != otherIdentifier.compositeIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(coi.children, otherIdentifier.children)
}
