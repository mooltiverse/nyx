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
The specialization of a Prerelease version number as per Semantic Versioning 2.0.0.

This identifier has a peculiar behavior as numeric parts are all parsed as integers. This is also helpful to remove any
leading zeroes and allows bumping.

Numeric identifiers allow bumping as well as string identifiers. When bumping a string identifier it's assumed that
there is a numeric identifier after the string and that numeric identifier is the value to be bumped. If the string
identifier doesn't have a numeric identifier next to it, the numeric identifier is appended, with its number starting
with the default value.

Examples:

1.2.3-4 can bump the (anonymous) value at index 0, resulting in 1.2.3-5.

1.2.3-alpha.4 can bump the named value alpha, resulting in 1.2.3-alpha.5. This
is equivalent to bumping the value at index 1 as it was anonymous.

1.2.3-beta can bump the named value beta, resulting in 1.2.3-beta.0 (assuming
the default start number is 0). This is equivalent to bumping the value at index 1 as it
was anonymous.

1.2.3-beta can bump the named value gamma, resulting in 1.2.3-beta.gamma.0.

Similarly, 1.2.3 can bump value anonymously the value at index 0, resulting in
1.2.3-0 or the named value pre, resulting in 1.2.3-pre.0.
*/
type semanticVersionPreReleaseIdentifier struct {
	// embed the compositeObjectIdentifier to compose (extend) from compositeObjectIdentifier
	compositeObjectIdentifier
}

/*
Builds the identifier with the given nested identifiers and separator.
*/
func newSemanticVersionPreReleaseIdentifier(children ...identifier) (semanticVersionPreReleaseIdentifier, error) {
	identifier := semanticVersionPreReleaseIdentifier{}

	identifier.compositeObjectIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeObjectIdentifier.children = children
	identifier.compositeObjectIdentifier.compositeIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeObjectIdentifier.compositeIdentifier.children = children

	return identifier, nil
}

/*
Parses the given string and returns the new identifier modelling the single identifiers. Numeric parts are treated
as integers and must be positive. If a numeric part is preceded by a string part then they both have the same name.
String values produce identifiers with the same name as their value even when they aren't followed by a numeric part.

Arguments are as follows:

  - multipleIdentifiers when true the given string is parsed as it (may) contain multiple
    identifiers, separated by the default separator, so this method may yield to multiple identifiers.
    When false the given string is expected to have a single identifier so if the given
    string has multiple identifiers an error is thrown.
  - s the string to parse
*/
func valueOfSemanticVersionPreReleaseIdentifierFromString(multipleIdentifiers bool, s string) (semanticVersionPreReleaseIdentifier, error) {
	if multipleIdentifiers {
		identifiers, err := toIdentifiersWithSeparator(s, DEFAULT_SEPARATOR, WHEN_POSSIBLE)
		if err != nil {
			return semanticVersionPreReleaseIdentifier{}, err
		}
		return newSemanticVersionPreReleaseIdentifier(identifiers...)
	} else {
		identifier, err := toIdentifier(s, WHEN_POSSIBLE)
		if err != nil {
			return semanticVersionPreReleaseIdentifier{}, err
		}
		return newSemanticVersionPreReleaseIdentifier(identifier)
	}
}

/*
Creates a new identifier instance with the given identifiers. When elements of the given list are integer
instances they are treated as numeric identifiers. All other object types are read using their String()
method. If the string returned by String() can be parsed to a positive integer then it is converted
to a numeric identifier, otherwise it's used as a string. Items cannot be all nil.
String representations of objects must not be empty or contain illegal characters while integer must be positive.

Arguments are as follows:

  - multipleIdentifiers when true the given string is parsed as it (may) contain multiple
    identifiers, separated by the default separator, so this method may yield to multiple identifiers.
    When false the given string is expected to have a single identifier so if the given
    string has multiple identifiers an error is thrown.
  - items the items to build the identifier with
*/
func valueOfSemanticVersionPreReleaseIdentifierFromObjects(multipleIdentifiers bool, items ...interface{}) (semanticVersionPreReleaseIdentifier, error) {
	if items == nil {
		return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't build the list of identifiers from a nil list")
	}

	if len(items) == 0 {
		return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't build the list of identifiers from an empty list")
	}

	var identifiers []identifier
	for _, item := range items {
		if item == nil {
			return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't build the list of identifiers from a list with nil values")
		}

		if multipleIdentifiers {
			itemIdentifiers, err := toIdentifiersWithSeparator(fmt.Sprint(item), DEFAULT_SEPARATOR, WHEN_POSSIBLE)
			if err != nil {
				return semanticVersionPreReleaseIdentifier{}, err
			}
			identifiers = append(identifiers, itemIdentifiers...)
		} else {
			itemIdentifier, err := toIdentifier(fmt.Sprint(item), WHEN_POSSIBLE)
			if err != nil {
				return semanticVersionPreReleaseIdentifier{}, err
			}
			identifiers = append(identifiers, itemIdentifier)
		}
	}

	return newSemanticVersionPreReleaseIdentifier(identifiers...)
}

/*
Returns a new identifier instance with the number identified by the given value bumped.

If this identifier has no identifier that equals the given id, then the returned identifier version has all the
previous identifiers followed by the two new identifiers: the given string and the following number
defaultNumber.

If this identifier already has a string identifier equal to the given id there are two options: if the selected
identifier already has a numeric value that follows, the returned identifier will have that numeric identifier
incremented by one; if the selected identifier doesn't have a numeric identifier that follows, a new numeric
identifiers is added after the string with the initial value defaultNumber.

If this identifier already has multiple identifiers that equal to the given value then all of them will be bumped.
In case they have different numeric values (or missing) each occurrence is bumped independently according to the
above rules.

Arguments are as follows:

  - id the selector of the identifier to bump
  - defaultNumber the default number to use when the given identifier doesn't have a numeric part following
    the string. This is usually set to 0 or 1. It must be a non-negative integer.
*/
func (svpri semanticVersionPreReleaseIdentifier) bump(id string, defaultNumber int) (semanticVersionPreReleaseIdentifier, error) {
	if "" == id {
		return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't bump an empty identifier")
	}
	if defaultNumber < 0 {
		return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't use a negative number for the default number to bump. '%d' was passed", defaultNumber)
	}

	var newValues []interface{}
	bumped := false
	values := svpri.getValues()

	for i := 0; i < len(values); i++ {
		value := values[i]
		newValues = append(newValues, value)
		if id == fmt.Sprint(value) {
			// if the identifier is found see if the next identifier is a number and, if so, bump its value,
			// otherwise create one
			bumped = true
			if len(values) > i+1 {
				i = i + 1
				value = values[i]
				intValue, ok := value.(int)
				if ok {
					newValues = append(newValues, intValue+1)
				} else {
					newValues = append(newValues, defaultNumber) // insert a new default Integer value
					newValues = append(newValues, value)         // re-add the non-integer value
				}
			} else {
				newValues = append(newValues, defaultNumber)
			}
		}
	}
	if !bumped {
		// if not yet bumped it means that no identifier with such name was found, so add it at the beginning, along with the numeric identifier
		newValues = append(newValues, id)
		newValues = append(newValues, defaultNumber)
	}
	return valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, newValues...)
}

/*
Returns true if an attribute with the given name is present, false otherwise.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty false is returned
*/
func (svpri semanticVersionPreReleaseIdentifier) hasAttribute(name string) bool {
	if "" == name {
		return false
	}

	for _, v := range svpri.getValues() {
		if name == v {
			return true
		}
	}
	return false
}

/*
If an attribute with the given name is present, return the identifier after that if and only if it's a numeric
identifier, otherwise return nil.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty nil is returned
*/
func (svpri semanticVersionPreReleaseIdentifier) getAttributeValue(name string) *int {
	if "" == name {
		return nil
	}

	values := svpri.getValues()

	for i := 0; i < len(values); i++ {
		value := values[i]
		if name == fmt.Sprint(value) && len(values) > i {
			i = i + 1
			maybeTheValue := values[i]
			intValue, ok := maybeTheValue.(int)
			if ok {
				return &intValue
			} else {
				return nil
			}
		}
	}

	return nil
}

/*
Returns a new instance with the new attribute added or replaced. This method tries to be less intrusive as it
only works on the given attribute (and its optional value) while leaving the other attributes unchanged.

If this instance already has a prerelease part that contains an identifier matching the given attribute name then
the identifier matching the attribute name is left unchanged and if the given value is not nil,
the next identifier is added or replaced with the given value. ATTENTION: if the value is not nil
the identifier after the name is replaced if is a numeric identifier, otherwise it's added after the identifier name.

Arguments are as follows:

- name the name to set for the attribute
- value the value to set for the attribute, or -1 just set the attribute name, ignoring the value
*/
func (svpri semanticVersionPreReleaseIdentifier) setAttribute(name string, value *int) (semanticVersionPreReleaseIdentifier, error) {
	if "" == name {
		return semanticVersionPreReleaseIdentifier{}, fmt.Errorf("can't set the attribute name to an empty identifier")
	}

	var newValues []interface{}
	found := false
	values := svpri.getValues()

	for i := 0; i < len(values); i++ {
		previousValue := values[i]
		newValues = append(newValues, previousValue)
		if name == fmt.Sprint(previousValue) {
			// if the identifier is found re-add it and work on the next item (the value)

			found = true
			if value != nil {
				// add the new value
				newValues = append(newValues, *value)

				if len(values) > i+1 {
					// if the previous value is an integer do not re-add it (so we replace it with the new one)
					i = i + 1
					maybeTheValueToReplace := values[i]
					_, ok := maybeTheValueToReplace.(int)
					if !ok {
						newValues = append(newValues, maybeTheValueToReplace)
					}
				}
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

	return valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, newValues...)
}

/*
Returns a new instance with the new attribute removed, if any was present, otherwise the same version is returned.
If the attribute is found and removeValue then also the attribute value (the attribute after the
one identified by name) is removed (if it's a numeric value), unless there are no more attributes after name.
If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
the return value is nil.

Arguments are as follows:

- name the name of the attribute to remove, if present. If nil or empty no action is taken
- removeValue if true also the attribute after name is removed (if any and if it's numeric)
*/
func (svpri semanticVersionPreReleaseIdentifier) removeAttribute(name *string, removeValue bool) (*semanticVersionPreReleaseIdentifier, error) {
	if name == nil {
		return &svpri, nil
	}
	if !svpri.hasAttribute(*name) {
		return &svpri, nil
	}

	var newValues []interface{}
	values := svpri.getValues()

	for i := 0; i < len(values); i++ {
		previousValue := values[i]
		if *name == fmt.Sprint(previousValue) {
			// do not re-add the name to the new values. If removeValue is true and the next element
			// is an Integer then do the same with the next element too, if any
			if removeValue && len(values) > i+1 {
				i = i + 1
				maybeTheValueToRemove := values[i]
				_, ok := maybeTheValueToRemove.(int)
				if !ok {
					newValues = append(newValues, maybeTheValueToRemove)
				}
			}
		} else {
			newValues = append(newValues, fmt.Sprint(previousValue))
		}
	}

	if len(newValues) == 0 {
		return nil, nil
	} else {
		res, err := valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, newValues...)
		return &res, err
	}
}

/*
Tests the given object for equality against this instance.
*/
func (svpri semanticVersionPreReleaseIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(semanticVersionPreReleaseIdentifier)
	if !ok {
		return false
	}
	if svpri.compositeObjectIdentifier.separator != otherIdentifier.compositeObjectIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(svpri.compositeObjectIdentifier.children, otherIdentifier.compositeObjectIdentifier.children)
}
