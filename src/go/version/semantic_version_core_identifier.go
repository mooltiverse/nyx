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
The specialization of a Core version number allows exactly 3 positive integers, the major,
minor and patch as per Semantic Versioning 2.0.0.
*/
type semanticVersionCoreIdentifier struct {
	// embed the compositeIntegerIdentifier to compose (extend) from compositeIntegerIdentifier
	compositeIntegerIdentifier
}

/*
Builds the identifier with the given nested identifiers and separator.
*/
func newSemanticVersionCoreIdentifier(children ...integerIdentifier) (semanticVersionCoreIdentifier, error) {
	identifier := semanticVersionCoreIdentifier{}

	identifier.compositeIntegerIdentifier.separator = DEFAULT_SEPARATOR
	identifier.compositeIntegerIdentifier.children = children
	identifier.compositeIntegerIdentifier.compositeIdentifier.separator = DEFAULT_SEPARATOR
	childrenAsIdentifiers, err := castSliceOfIntegerIdentifiersToSliceOfIdentifiers(children)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	identifier.compositeIntegerIdentifier.compositeIdentifier.children = childrenAsIdentifiers

	return identifier, nil
}

/*
Builds the core identifier with the given values.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number
*/
func newSemanticVersionCoreIdentifierFromIntegers(major int, minor int, patch int) (semanticVersionCoreIdentifier, error) {
	majorIdentifier, err := newIntegerIdentifierFromInt(major)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	minorIdentifier, err := newIntegerIdentifierFromInt(minor)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	patchIdentifier, err := newIntegerIdentifierFromInt(patch)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	return newSemanticVersionCoreIdentifier(majorIdentifier, minorIdentifier, patchIdentifier)
}

/*
Builds the core identifier with the given values.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number
*/
func newSemanticVersionCoreIdentifierFromStrings(major string, minor string, patch string) (semanticVersionCoreIdentifier, error) {
	majorIdentifier, err := newIntegerIdentifierFromString(major)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	minorIdentifier, err := newIntegerIdentifierFromString(minor)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	patchIdentifier, err := newIntegerIdentifierFromString(patch)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	return newSemanticVersionCoreIdentifier(majorIdentifier, minorIdentifier, patchIdentifier)
}

/*
Returns an identifier instance representing the specified String value.
*/
func valueOfSemanticVersionCoreIdentifierFromString(s string) (semanticVersionCoreIdentifier, error) {
	integerIdentifiers, err := toIntegerIdentifiersWithSeparator(s, DEFAULT_SEPARATOR)
	if err != nil {
		return semanticVersionCoreIdentifier{}, err
	}
	return newSemanticVersionCoreIdentifier(integerIdentifiers...)
}

/*
Returns an identifier instance representing with the given values.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number
*/
func valueOfSemanticVersionCoreIdentifierFromIntegers(major int, minor int, patch int) (semanticVersionCoreIdentifier, error) {
	return newSemanticVersionCoreIdentifierFromIntegers(major, minor, patch)
}

/*
Returns an identifier instance representing with the given values.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number
*/
func valueOfSemanticVersionCoreIdentifierFromStrings(major string, minor string, patch string) (semanticVersionCoreIdentifier, error) {
	return newSemanticVersionCoreIdentifierFromStrings(major, minor, patch)
}

/*
Returns the major version number
*/
func (svci semanticVersionCoreIdentifier) getMajor() int {
	return svci.compositeIntegerIdentifier.get(0)
}

/*
Returns the minor version number
*/
func (svci semanticVersionCoreIdentifier) getMinor() int {
	return svci.compositeIntegerIdentifier.get(1)
}

/*
Returns the patch version number
*/
func (svci semanticVersionCoreIdentifier) getPatch() int {
	return svci.compositeIntegerIdentifier.get(2)
}

/*
Returns a new instance with the major number of this current instance incremented by one and the minor and patch
numbers reset to zero.
*/
func (svci semanticVersionCoreIdentifier) bumpMajor() semanticVersionCoreIdentifier {
	identifier, err := newSemanticVersionCoreIdentifierFromIntegers(svci.compositeIntegerIdentifier.get(0)+1, 0, 0)
	if err != nil {
		panic(fmt.Errorf("unexpected error when bumping the major identifier: %w", err))
	}
	return identifier
}

/*
Returns a new instance with the major number of this current instance, the minor number incremented by one and
the patch number reset to zero.
*/
func (svci semanticVersionCoreIdentifier) bumpMinor() semanticVersionCoreIdentifier {
	identifier, err := newSemanticVersionCoreIdentifierFromIntegers(svci.compositeIntegerIdentifier.get(0), svci.compositeIntegerIdentifier.get(1)+1, 0)
	if err != nil {
		panic(fmt.Errorf("unexpected error when bumping the minor identifier: %w", err))
	}
	return identifier
}

/*
Returns a new instance with the major and minor numbers of this current instance and the patch number
incremented by one.
*/
func (svci semanticVersionCoreIdentifier) bumpPatch() semanticVersionCoreIdentifier {
	identifier, err := newSemanticVersionCoreIdentifierFromIntegers(svci.compositeIntegerIdentifier.get(0), svci.compositeIntegerIdentifier.get(1), svci.compositeIntegerIdentifier.get(2)+1)
	if err != nil {
		panic(fmt.Errorf("unexpected error when bumping the patch identifier: %w", err))
	}
	return identifier
}

/*
Tests the given object for equality against this instance.
*/
func (svci semanticVersionCoreIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	otherIdentifier, ok := obj.(semanticVersionCoreIdentifier)
	if !ok {
		return false
	}
	if svci.compositeIntegerIdentifier.separator != otherIdentifier.compositeIntegerIdentifier.separator {
		return false
	}
	return reflect.DeepEqual(svci.compositeIntegerIdentifier.children, otherIdentifier.compositeIntegerIdentifier.children)
}
