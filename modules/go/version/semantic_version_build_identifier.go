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
The specialization of a Build version number as per Semantic Versioning 2.0.0.

This identifier has a peculiar behavior as all parts are parsed as strings, even numeric ones, to preserve any
leading zeroes.
*/
type semanticVersionBuildIdentifier struct {
	// embed the compositeStringIdentifier to compose (extend) from compositeStringIdentifier
	compositeStringIdentifier
}

/*
Builds the identifier with the given nested identifiers and separator.
*/
func newSemanticVersionBuildIdentifier(children ...stringIdentifier) (semanticVersionBuildIdentifier, error) {
	identifier := semanticVersionBuildIdentifier{}

	identifier.compositeStringIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeStringIdentifier.children = children
	identifier.compositeStringIdentifier.compositeIdentifier.separator = DEFAULT_SEPARATOR
	childrenAsIdentifiers, err := castSliceOfStringIdentifiersToSliceOfIdentifiers(children)
	if err != nil {
		return semanticVersionBuildIdentifier{}, err
	}
	identifier.compositeStringIdentifier.compositeIdentifier.children = childrenAsIdentifiers

	return identifier, nil
}

/*
Returns an identifier instance representing the specified String value.

Arguments are as follows:

  - multipleIdentifiers when true the given string is parsed as it (may) contain multiple
    identifiers, separated by the default separator, so this method may yield to multiple identifiers.
    When false the given string is expected to have a single identifier so if the given
    string has multiple identifiers an error is thrown.
  - s the string to parse
*/
func valueOfSemanticVersionBuildIdentifierFromString(multipleIdentifiers bool, s string) (semanticVersionBuildIdentifier, error) {
	if multipleIdentifiers {
		identifiers, err := toStringIdentifiersWithSeparator(s, DEFAULT_SEPARATOR)
		if err != nil {
			return semanticVersionBuildIdentifier{}, err
		}
		return newSemanticVersionBuildIdentifier(identifiers...)
	} else {
		identifier, err := newStringIdentifier(s)
		if err != nil {
			return semanticVersionBuildIdentifier{}, err
		}
		return newSemanticVersionBuildIdentifier(identifier)
	}
}

/*
Returns an identifier instance representing the specified String values.

Arguments are as follows:

  - multipleIdentifiers when true the given string is parsed as it (may) contain multiple
    identifiers, separated by the default separator, so this method may yield to multiple identifiers.
    When false the given string is expected to have a single identifier so if the given
    string has multiple identifiers an error is thrown.
  - items the strings to parse
*/
func valueOfSemanticVersionBuildIdentifierFromStrings(multipleIdentifiers bool, items ...string) (semanticVersionBuildIdentifier, error) {
	if items == nil {
		return semanticVersionBuildIdentifier{}, fmt.Errorf("can't build the list of identifiers from a nil list")
	}

	if len(items) == 0 {
		return semanticVersionBuildIdentifier{}, fmt.Errorf("can't build the list of identifiers from an empty list")
	}

	var allIdentifiers []stringIdentifier

	for _, s := range items {
		if multipleIdentifiers {
			itemIdentifiers, err := toStringIdentifiersWithSeparator(s, DEFAULT_SEPARATOR)
			if err != nil {
				return semanticVersionBuildIdentifier{}, err
			}
			allIdentifiers = append(allIdentifiers, itemIdentifiers...)
		} else {
			itemIdentifier, err := newStringIdentifier(s)
			if err != nil {
				return semanticVersionBuildIdentifier{}, err
			}
			allIdentifiers = append(allIdentifiers, itemIdentifier)
		}
	}

	return newSemanticVersionBuildIdentifier(allIdentifiers...)
}

/*
Returns true if an attribute with the given name is present, false otherwise.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty false is returned
*/
func (svbi semanticVersionBuildIdentifier) hasAttribute(name string) bool {
	if "" == name {
		return false
	}

	for _, v := range svbi.getValues() {
		if name == v {
			return true
		}
	}
	return false
}

/*
If an attribute with the given name is present, return the identifier after that, otherwise return nil.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty nil is returned
*/
func (svbi semanticVersionBuildIdentifier) getAttributeValue(name string) *string {
	if "" == name {
		return nil
	}

	values := svbi.getValues()

	for i := 0; i < len(values); i++ {
		value := values[i]
		if name == fmt.Sprint(value) && len(values) > i {
			i = i + 1
			res := fmt.Sprint(values[i])
			return &res
		}
	}

	return nil
}

/*
Returns a new instance with the new attribute added or replaced. This method tries to be less intrusive as it
only works on the given attribute (and its optional value) while leaving the other attributes unchanged.

If this instance already has a build part that contains an identifier matching the given attribute name then
the identifier matching the attribute name is left unchanged and if the given value is not nil,
the next identifier is added or replaced with the given value. ATTENTION: if the value is not nil
the identifier after the name is replaced without further consideration.

Arguments are as follows:

- name the name to set for the attribute
- value the value to set for the attribute, or nil just set the attribute name, ignoring the value
*/
func (svbi semanticVersionBuildIdentifier) setAttribute(name string, value *string) (semanticVersionBuildIdentifier, error) {
	if "" == name {
		return semanticVersionBuildIdentifier{}, fmt.Errorf("can't set the attribute name to an empty identifier")
	}

	var newValues []string
	found := false
	values := svbi.getValues()

	for i := 0; i < len(values); i++ {
		previousValue := values[i]
		newValues = append(newValues, fmt.Sprint(previousValue))
		if name == fmt.Sprint(previousValue) {
			// if the identifier is found re-add it and work on the next item (the value)

			found = true
			if value != nil {
				if len(values) > i {
					// discard the previous value to replace it with the passed value
					i = i + 1
				}
				newValues = append(newValues, *value)
			}
		}
	}
	if !found {
		// if not yet found it means that no identifier with such name was found, so add it at the end, along with the value
		newValues = append(newValues, name)
		if value != nil {
			newValues = append(newValues, *value)
		}
	}

	return valueOfSemanticVersionBuildIdentifierFromStrings(false, newValues...)
}

/*
Returns a new instance with the new attribute removed, if any was present, otherwise the same version is returned.
If the attribute is found and removeValue then also the attribute value (the attribute after the
one identified by name) is removed, unless there are no more attributes after name.
If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
the return value is nil

Arguments are as follows:

- name the name of the attribute to remove, if present. If nil or empty no action is taken
- removeValue if true also the attribute after name is removed (if any)
*/
func (svbi semanticVersionBuildIdentifier) removeAttribute(name *string, removeValue bool) (*semanticVersionBuildIdentifier, error) {
	if name == nil {
		return &svbi, nil
	}
	if !svbi.hasAttribute(*name) {
		return &svbi, nil
	}

	var newValues []string
	values := svbi.getValues()

	for i := 0; i < len(values); i++ {
		previousValue := values[i]
		if *name == fmt.Sprint(previousValue) {
			// do not re-add the name to the new values. If removeValue is true then do the same with the next element too, if any
			if removeValue && len(values) > i {
				// discard the previous value to replace it with the passed value
				i = i + 1
			}
		} else {
			newValues = append(newValues, fmt.Sprint(previousValue))
		}
	}

	if len(newValues) == 0 {
		return nil, nil
	} else {
		res, err := valueOfSemanticVersionBuildIdentifierFromStrings(false, newValues...)
		return &res, err
	}
}

/*
Tests the given object for equality against this instance.
*/
func (svbi semanticVersionBuildIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(semanticVersionBuildIdentifier)
	if !ok {
		return false
	}
	if svbi.compositeStringIdentifier.separator != otherIdentifier.compositeStringIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(svbi.compositeStringIdentifier.children, otherIdentifier.compositeStringIdentifier.children)
}
