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
	"sort"    // https://pkg.go.dev/sort
	"strings" // https://pkg.go.dev/strings
)

/*
Returns a negative integer, zero, or a positive integer as the version represented by v1 is less than, equal to,
or greater than the version represented by v2, according to the given scheme. nil values are always
considered less than non nil valid version identifiers.
This method does no sanitization or prefix interpretation.

Arguments are as follows:

- scheme the scheme to check against.
- v1 the first version to compare. It may be nil. If it's not a valid version it's considered as nil.
- v2 the second version to compare. It may be nil. If it's not a valid version it's considered as nil.

Errors can be returned if:

- a given string doesn't represent a legal version, according to the selected scheme
*/
func Compare(scheme Scheme, v1 *string, v2 *string) int {
	return CompareWithSanitization(scheme, v1, v2, false)
}

/*
Returns a negative integer, zero, or a positive integer as the version represented by v1 is less than, equal to,
or greater than the version represented by v2, according to the given scheme. nil values are always
considered less than non nil valid version identifiers.
This method is different than CompareWithSanitization as it only tolerates a prefix, while
CompareWithSanitization is more lenient as it also sanitizes extra characters in the body
of the version identifier (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - v1 the first version to compare. It may be nil. If it's not a valid version it's considered as nil.
  - v2 the second version to compare. It may be nil. If it's not a valid version it's considered as nil.
  - prefix the initial string that is used for the version prefix. This will be stripped off from the given
    string representation of the versions. It can be nil or empty, in which case it's ignored. If not empty
    and the given version string doesn't start with this prefix, this prefix is ignored.

Errors can be returned if:

- a given string doesn't represent a legal version, according to the selected scheme
*/
func CompareWithPrefix(scheme Scheme, v1 *string, v2 *string, prefix *string) int {
	if v1 == nil && v2 == nil {
		return 0
	}
	var v1b *string = nil
	if v1 == nil || prefix == nil {
		v1b = v1
	} else {
		if strings.HasPrefix(*v1, *prefix) {
			t := strings.Replace(*v1, *prefix, "", 1)
			v1b = &t
		} else {
			v1b = v1
		}
	}
	var v2b *string = nil
	if v2 == nil || prefix == nil {
		v2b = v2
	} else {
		if strings.HasPrefix(*v2, *prefix) {
			t := strings.Replace(*v2, *prefix, "", 1)
			v2b = &t
		} else {
			v2b = v2
		}
	}
	return CompareWithSanitization(scheme, v1b, v2b, false)
}

/*
Returns a negative integer, zero, or a positive integer as the version represented by v1 is less than, equal to,
or greater than the version represented by v2, according to the given scheme. nil values are always
considered less than non nil valid version identifiers.
If sanitize is true this method will try to sanitize the given strings before parsing so that if there are
illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.

This method is different than CompareWithPrefix as it also sanitizes extra characters
in the body of the version identifiers instead of just an optional prefix (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - v1 the first version to compare. It may be nil. If it's not a valid version it's considered as nil.
  - v2 the second version to compare. It may be nil. If it's not a valid version it's considered as nil.
    sanitize optionally enables sanitization before parsing versions

Errors can be returned if:

- a given string doesn't represent a legal version, according to the selected scheme
*/
func CompareWithSanitization(scheme Scheme, v1 *string, v2 *string, sanitize bool) int {
	switch scheme {
	case SEMVER:
		{
			var sv1 *SemanticVersion = nil
			if v1 != nil {
				svp1, err := ValueOfSemanticVersionWithSanitization(*v1, sanitize)
				if err == nil {
					sv1 = &svp1
				}
			}
			var sv2 *SemanticVersion = nil
			if v2 != nil {
				svp2, err := ValueOfSemanticVersionWithSanitization(*v2, sanitize)
				if err == nil {
					sv2 = &svp2
				}
			}
			if sv1 == nil && sv2 == nil {
				return 0
			} else if sv1 == nil {
				return -1
			} else if sv2 == nil {
				return 1
			} else {
				return sv1.CompareTo(*sv2)
			}
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns a Version instance representing the default initial value to use for the given scheme.

Arguments are as follows:

- scheme the scheme to get the initial version for
*/
func DefaultInitial(scheme Scheme) Version {
	switch scheme {
	case SEMVER:
		{
			res, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			if err != nil {
				panic("unable to build a new semantic version from the default initial value")
			}
			return res
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns true if the given string is a legal version and it only contains core identifiers
according to the given scheme.

This method uses a strict criteria, without trying to sanitize the given string.

Arguments are as follows:

- scheme the scheme to check against.
- s the string version to check.
*/
func IsCore(scheme Scheme, s string) bool {
	return IsCoreWithLenience(scheme, s, false)
}

/*
Returns true if the given string is a legal version and it only contains core identifiers
according to the given scheme.

This method is different than IsCoreWithPrefix as it also sanitizes extra characters
in the body of the version identifier instead of just an optional prefix (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - s the string version to check.
  - lenient when true prefixes and non critical extra characters are tolerated even if they are not
    strictly legal from the version scheme specification perspective.
*/
func IsCoreWithLenience(scheme Scheme, s string, lenient bool) bool {
	switch scheme {
	case SEMVER:
		{
			if !IsLegalSemanticVersionWithLenience(s, lenient) {
				return false
			}
			res, _ := ValueOfSemanticVersionWithSanitization(s, lenient)
			if res.GetPrerelease() == nil && res.GetBuild() == nil {
				return true
			} else {
				return false
			}
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns true if the given string is a legal version and it only contains core identifiers
according to the given scheme.

This method is different than IsCoreWithLenience as it only tolerates a prefix, while
IsCoreWithLenience is more lenient as it also sanitizes extra characters in the body
of the version identifier (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - s the string version to check.
  - prefix the initial string that is used for the version prefix. This will be stripped off from the given
    string representation of the version. It can be nil or empty, in which case it's ignored. If not empty
    and the given version string doesn't start with this prefix, this prefix is ignored.
*/
func IsCoreWithPrefix(scheme Scheme, s string, prefix *string) bool {
	if prefix != nil && strings.HasPrefix(s, *prefix) {
		s = strings.Replace(s, *prefix, "", 1)
	}
	return IsCore(scheme, s)
}

/*
Returns true if the given string is a legal version which, for example, can be parsed using
ValueOf(Scheme, String) without errors using the implementation selected by the given scheme.

This method uses a strict criteria, without trying to sanitize the given string.

Arguments are as follows:

- scheme the scheme to check against.
- s the string version to check.
*/
func IsLegal(scheme Scheme, s string) bool {
	return IsLegalWithLenience(scheme, s, false)
}

/*
Returns true if the given string is a legal version which, for example, can be parsed using
ValueOf(Scheme, String, boolean) without errors using the implementation selected by the given scheme.

This method is different than IsLegalWithPrefix as it also sanitizes extra characters
in the body of the version identifier instead of just an optional prefix (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - s the string version to check.
  - lenient when true prefixes and non critical extra characters are tolerated even if they are not
    strictly legal from the version scheme specification perspective.
*/
func IsLegalWithLenience(scheme Scheme, s string, lenient bool) bool {
	switch scheme {
	case SEMVER:
		{
			return IsLegalSemanticVersionWithLenience(s, lenient)
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns true if the given string is a legal version which, for example, can be parsed using
ValueOf(Scheme, String, boolean) without errors using the implementation selected by the given scheme.

This method is different than IsLegalWithLenience as it only tolerates a prefix, while
IsLegalWithLenience is more lenient as it also sanitizes extra characters in the body
of the version identifier (when sanitize is true).

Arguments are as follows:

  - scheme the scheme to check against.
  - s the string version to check.
  - prefix the initial string that is used for the version prefix. This will be stripped off from the given
    string representation of the version. It can be nil or empty, in which case it's ignored. If not empty
    and the given version string doesn't start with this prefix, this prefix is ignored.
*/
func IsLegalWithPrefix(scheme Scheme, s string, prefix *string) bool {
	if prefix != nil && strings.HasPrefix(s, *prefix) {
		s = strings.Replace(s, *prefix, "", 1)
	}
	return IsLegal(scheme, s)
}

/*
Returns the most relevant identifier in the given collection, according to the given scheme ordering, or nil
if the given list is empty.

Arguments are as follows:

- scheme the scheme to peek the most relevand item from
- identifiers the identifiers to inspect
*/
func MostRelevantIdentifierIn(scheme Scheme, identifiers []string) *string {
	if len(identifiers) == 0 {
		return nil
	}

	switch scheme {
	case SEMVER:
		{
			orderedList := make([]string, len(identifiers))
			copy(orderedList, identifiers)
			sort.Sort(semanticVersionIdentifiers(orderedList))
			return &orderedList[0]
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the most relevant identifier between the given arguments, according to the given scheme ordering, or nil
if both arguments are nil.

Arguments are as follows:

- scheme the scheme to peek the most relevand item from
- identifier1 the first identifier to inspect
- identifier2 the secondt identifier to inspect
*/
func MostRelevantIdentifierBetween(scheme Scheme, identifier1 *string, identifier2 *string) *string {
	if identifier1 == nil {
		return identifier2
	} else if identifier2 == nil {
		return identifier1
	}

	switch scheme {
	case SEMVER:
		{
			identifiers := []string{*identifier1, *identifier2}
			sort.Sort(semanticVersionIdentifiers(identifiers))
			return &identifiers[0]
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns a Version instance representing the specified String value. No sanitization attempt is done.

Arguments are as follows:

- scheme the scheme the version belongs to
- s the string to parse

Errors can be returned if:

- the given string doesn't represent a legal version, according to the selected scheme
*/
func ValueOf(scheme Scheme, s string) (Version, error) {
	return ValueOfWithSanitization(scheme, s, false)
}

/*
Returns a Version instance representing the specified String value. No sanitization attempt is done.

If sanitize is true this method will try to sanitize the given string before parsing so that if there are
illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.

When sanitization is enabled on a string that actually needs sanitization the string representation of the
returned object will not exactly match the input value.

This method is different than ValueOfWithPrefix as it also sanitizes extra characters
in the body of the version identifier instead of just an optional prefix (when sanitize is true).

Arguments are as follows:

- scheme the scheme the version belongs to
- s the string to parse
- sanitize optionally enables sanitization before parsing

Errors can be returned if:

- the given string doesn't represent a legal version, according to the selected scheme
*/
func ValueOfWithSanitization(scheme Scheme, s string, sanitize bool) (Version, error) {
	switch scheme {
	case SEMVER:
		{
			return ValueOfSemanticVersionWithSanitization(s, sanitize)
		}
		//MAVEN: not yet supported
	default:
		// this is never reached, but in case...
		panic("unknown Scheme. This means the switch/case statement needs to be updated")
	}
}

/*
Returns a Version instance representing the specified String value.

This method is different than ValueOfWithSanitization as it only tolerates a prefix, while
ValueOfWithSanitization is more lenient as it also sanitizes extra characters in the body
of the version identifier (when sanitize is true).

Arguments are as follows:

  - scheme the scheme the version belongs to
  - s the string to parse
  - prefix the initial string that is used for the version prefix. This will be stripped off from the given
    string representation of the version. It can be nil or empty, in which case it's ignored. If not empty
    and the given version string doesn't start with this prefix, this prefix is ignored.

Errors can be returned if:

-the given string doesn't represent a legal version, according to the selected scheme
*/
func ValueOfWithPrefix(scheme Scheme, s string, prefix *string) (Version, error) {
	if prefix != nil && strings.HasPrefix(s, *prefix) {
		s = strings.Replace(s, *prefix, "", 1)
	}
	return ValueOf(scheme, s)
}
