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
	"strings" // https://pkg.go.dev/strings
)

/*
The slice of strings that allows sorting for semantic version identifiers.

Sorting of semantic version identifier strings can be done using the https://pkg.go.dev/sort package like:

	identifiers := []string{
			"alpha",
			"beta",
			"minor",
			"patch",
			"gamma",
			"gamma",
			"minor",
			"major",
			"theta",
			"patch",
			"major",
			"epsylon",
		}

	sort.Sort(semanticVersionIdentifiers(identifiers))
*/
type semanticVersionIdentifiers []string

/*
Len is the number of elements in the collection.

This method implements the sort.Interface
*/
func (svi semanticVersionIdentifiers) Len() int {
	return len(svi)
}

/*
Less reports whether the element with index i must sort before the element with index j.

If both Less(i, j) and Less(j, i) are false, then the elements at index i and j are considered equal.
Sort may place equal elements in any order in the final result, while Stable preserves
the original input order of equal elements.

Less must describe a transitive ordering:
- if both Less(i, j) and Less(j, k) are true, then Less(i, k) must be true as well.
- if both Less(i, j) and Less(j, k) are false, then Less(i, k) must be false as well.

This method implements the sort.Interface
*/
func (svi semanticVersionIdentifiers) Less(i, j int) bool {
	return compareSemanticVersionIdentifiers(svi[i], svi[j]) < 0
}

/*
Swap swaps the elements with indexes i and j.

This method implements the sort.Interface
*/
func (svi semanticVersionIdentifiers) Swap(i, j int) {
	svi[i], svi[j] = svi[j], svi[i]
}

/*
A comparator for identifier names. The method can be used to sort identifier names
by their relevance, according to Semantic Versioning 2.0.0,
provided that:
- core identifiers are always more relevant that any other identifier
- core identifiers are always sorted as major, minor, patch
- other identifiers are considered pre-release identifiers and are sorted by their natural order

Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.

The comparator does not admit nil values.
*/
func compareSemanticVersionIdentifiers(i1 string, i2 string) int {
	if i1 == i2 {
		return 0
	} else if "major" == i1 {
		return -1
	} else if "major" == i2 {
		return 1
	} else if "minor" == i1 {
		return -1
	} else if "minor" == i2 {
		return +1
	} else if "patch" == i1 {
		return -1
	} else if "patch" == i2 {
		return 1
	} else {
		return strings.Compare(i1, i2)
	}
}
