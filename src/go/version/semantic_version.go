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
	"strconv" // https://pkg.go.dev/strconv
	"strings" // https://pkg.go.dev/strings

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
)

const (
	// The default value to start from when bumping an identifier that has no numeric value yet.
	DEFAULT_BUMP_VALUE = 1

	// The default initial version that can be used when non version is yet available.
	SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION = "0.1.0"

	// The character that marks the separation between the core and the pre-release part. Note that this character is
	// used as separator only at the first occurrence while other occurrences are considered legal characters in the
	// pre-release and the build identifiers.
	PRERELEASE_DELIMITER = "-"

	// The character that marks the separation between the core or the pre-release part and the build part.
	BUILD_DELIMITER = "+"

	// A relaxed version of the SEMANTIC_VERSION_PATTERN that works also when a prefix appears at the beginning
	// of the version string or some zeroes appear in front of numbers.
	SEMANTIC_VERSION_PATTERN_RELAXED = "([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(?:-((?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"

	//T he regexp pattern taken directly from >Semantic Versioning 2.0.0 (https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string)
	// used to parse semantic versions.
	SEMANTIC_VERSION_PATTERN = "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$"
)

/*
The implementation of a Semantic Versioning 2.0.0 compliant version.

Instances of this class are immutable so whenever you alter some values you actually receive a
new instance holding the new value, while the old one remains unchanged.
*/
type SemanticVersion struct {
	// Store the immutable string representation to avoid repetitive formatting.
	renderedString *string

	// The identifier of the core version part. It can't be nil.
	coreIdentifier *semanticVersionCoreIdentifier

	// The identifier of the pre-release part of the version. It may be nil.
	prereleaseIdentifier *semanticVersionPreReleaseIdentifier

	// The identifier of the build part of the version. It may be nil.
	buildIdentifier *semanticVersionBuildIdentifier
}

/*
The slice of semantic versions that allows sorting for semantic versions.

Sorting of semantic versions can be done using the https://pkg.go.dev/sort package like:

	sv1, _ := ValueOfSemanticVersion("3.5.1")
	sv2, _ := ValueOfSemanticVersion("1.2.3")
	sv3, _ := ValueOfSemanticVersion("2.0.1-alpha4")
	versions := []SemanticVersion{
			sv1,
			sv2,
			sv3,
		}

	sort.Sort(SemanticVersions(versions))
*/
type SemanticVersions []SemanticVersion

/*
The slice of semantic version strings that allows sorting for semantic versions in their string representations.

Sorting of semantic version strings can be done using the https://pkg.go.dev/sort package like:

	versions := []string{
			"3.5.1",
			"1.2.3",
			"2.0.1-alpha4",
		}

	sort.Sort(SemanticVersionStrings(versions))
*/
type SemanticVersionStrings []string

/*
Builds a new version object with the given values.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number

Errors can be returned if:

- one of major, minor, patch is negative
*/
func NewSemanticVersionWith(major int, minor int, patch int) (SemanticVersion, error) {
	return NewSemanticVersionWithAllIdentifiers(major, minor, patch, nil, nil)
}

/*
Builds a new version object with the given values.

Arguments are as follows:

  - major the major number
  - minor the minor number
  - patch the patch number
  - prereleaseIdentifiers the prereleaseIdentifiers the array of Integer or String
    objects to use as identifiers in the prerelease block. nil items are ignored. Integers and strings
    representing integers must not have leading zeroes or represent negative numbers. If the array is nil
    then the instance will have no prerelease block
  - buildIdentifiers the buildIdentifiers the array of String to use as identifiers in
    the build block. nil items are ignored. If the array is nil then the instance will
    have no build block

Errors can be returned if:

  - one of major, minor, patch is negative or one object in the prereleaseIdentifiers represents a negative integer (either when
    passed as an Integer or String) or have leading zeroes. This error is also raised when objects
    in the prereleaseIdentifiers are not of type Integer or String or when string
    identifiers in the prereleaseIdentifiers or buildIdentifiers contain illegal characters
*/
func NewSemanticVersionWithAllIdentifiers(major int, minor int, patch int, prereleaseIdentifiers []interface{}, buildIdentifiers []string) (SemanticVersion, error) {
	coreIdentifier, err := newSemanticVersionCoreIdentifierFromIntegers(major, minor, patch)
	if err != nil {
		return SemanticVersion{}, err
	}
	var preReleaseIdentifier *semanticVersionPreReleaseIdentifier = nil
	if hasValues(prereleaseIdentifiers) {
		pri, err := valueOfSemanticVersionPreReleaseIdentifierFromObjects(true, prereleaseIdentifiers...)
		if err != nil {
			return SemanticVersion{}, err
		}
		preReleaseIdentifier = &pri
	}
	var buildIdentifier *semanticVersionBuildIdentifier = nil
	if buildIdentifiers != nil && len(buildIdentifiers) > 0 {
		bi, err := valueOfSemanticVersionBuildIdentifierFromStrings(true, buildIdentifiers...)
		if err != nil {
			return SemanticVersion{}, err
		}
		buildIdentifier = &bi
	}
	return newSemanticVersion(&coreIdentifier, preReleaseIdentifier, buildIdentifier)
}

/*
Builds the version with the given identifier values.

Arguments are as follows:

- coreIdentifier the identifier of the core version part. It can't be nil.
- prereleaseIdentifier the identifier of the pre-release part of the version. It may be nil.
- buildIdentifier the identifier of the build part of the version. It may be nil.
*/
func newSemanticVersion(coreIdentifier *semanticVersionCoreIdentifier, prereleaseIdentifier *semanticVersionPreReleaseIdentifier, buildIdentifier *semanticVersionBuildIdentifier) (SemanticVersion, error) {
	if coreIdentifier == nil {
		return SemanticVersion{}, fmt.Errorf("can't build a valid semantic version without the core version numbers")
	}

	return SemanticVersion{coreIdentifier: coreIdentifier, prereleaseIdentifier: prereleaseIdentifier, buildIdentifier: buildIdentifier}, nil
}

/*
Indicates whether some other object is "equal to" this one. This object equals to the given one if they are the
same object or they are of the same type and hold exactly the same version.

Arguments are as follows:

- obj the reference object with which to compare.
*/
func (sv SemanticVersion) Equals(obj interface{}) bool {
	if obj == nil {
		return false
	}

	/*if &sv == &obj {
		return true
	}*/
	otherVersion, ok := obj.(SemanticVersion)
	if !ok {
		return false
	}

	if !sv.coreIdentifier.equals(*otherVersion.coreIdentifier) {
		return false
	}

	if sv.prereleaseIdentifier == nil {
		if otherVersion.prereleaseIdentifier != nil {
			return false
		}
	} else if otherVersion.prereleaseIdentifier == nil {
		return false
	} else if !sv.prereleaseIdentifier.equals(*otherVersion.prereleaseIdentifier) {
		return false
	}

	if sv.buildIdentifier == nil {
		if otherVersion.buildIdentifier != nil {
			return false
		}
	} else if otherVersion.buildIdentifier == nil {
		return false
	} else if !sv.buildIdentifier.equals(*otherVersion.buildIdentifier) {
		return false
	}

	return true
}

/*
Len is the number of elements in the collection.

This method implements the sort.Interface
*/
func (svs SemanticVersions) Len() int {
	return len(svs)
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
func (svs SemanticVersions) Less(i, j int) bool {
	if svs[i].CompareTo(svs[j]) < 0 {
		return true
	} else {
		return false
	}
}

/*
Swap swaps the elements with indexes i and j.

This method implements the sort.Interface
*/
func (svs SemanticVersions) Swap(i, j int) {
	svs[i], svs[j] = svs[j], svs[i]
}

/*
Len is the number of elements in the collection.

This method implements the sort.Interface
*/
func (svs SemanticVersionStrings) Len() int {
	return len(svs)
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
func (svs SemanticVersionStrings) Less(i, j int) bool {
	svi, erri := ValueOfSemanticVersion(svs[i])
	svj, errj := ValueOfSemanticVersion(svs[j])
	if erri == nil && errj == nil {
		if svi.CompareTo(svj) < 0 {
			return true
		} else {
			return false
		}
	} else if erri == nil && errj != nil {
		// the one that can't pares as a valid semantic version is considered less
		return false
	} else if erri != nil && errj == nil {
		// the one that can't pares as a valid semantic version is considered less
		return true
	} else {
		// if none of the two is a valid semantic version then compare them lexicomatically
		res := strings.Compare(svs[i], svs[j])
		if res < 0 {
			return true
		} else {
			return false
		}
	}
}

/*
Swap swaps the elements with indexes i and j.

This method implements the sort.Interface
*/
func (svs SemanticVersionStrings) Swap(i, j int) {
	svs[i], svs[j] = svs[j], svs[i]
}

/*
Compares this version with the specified version for order. Returns a negative integer, zero, or a positive
integer as this object is less than, equal to, or greater than the specified version.

Semantic Versioning 2.0.0 states that:
  - rule #9: Pre-release versions have a lower precedence than the associated normal version.
  - rule #10: Build metadata MUST be ignored when determining version precedence. Thus two versions that differ
    only in the build metadata, have the same precedence.
  - rule #11: Precedence refers to how versions are compared to each other when ordered. Precedence MUST be
    calculated by separating the version into major, minor, patch and pre-release identifiers in that order
    (Build metadata does not figure into precedence). Precedence is determined by the first difference when
    comparing each of these identifiers from left to right as follows: Major, minor, and patch versions are always
    compared numerically. Example: 1.0.0 &lt; 2.0.0 &lt; 2.1.0 &lt; 2.1.1.
    When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version.
    Example: 1.0.0-alpha &lt; 1.0.0.
    Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined by
    comparing each dot separated identifier from left to right until a difference is found as follows:
  - identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens are
    compared lexically in ASCII sort order.
  - numeric identifiers always have lower precedence than non-numeric identifiers
  - a larger set of pre-release fields has a higher precedence than a smaller set, if all of the preceding
    identifiers are equal.

Note that when the specification says "lower precedence" it translates to &lt;, which means the "less" part is
supposed to appear first in order. this means that the object with "lower precedence" returns a negative number.

However, to cope with total ordering, we need to amend the above rules and make them a little stricter just
because two versions with different values can't be considered equal. With more detail:

  - rule #10 is amended so that two versions that only differ in their build metadata will not return 0 (as if they
    the same) but their build metadata of two versions that are equal in their core and prerelease parts affects
    the order by their literal comparison (remember that numeric identifiers are not treated as such in build
    metadata and, instead, they are just treated as strings). In other words we are not ignoring build metadata as
    required by rule #10 but we consider it with the least priority only when the core and prerelease parts are the
    same. Two be consistent with rule #9, when comparing two versions with the same core and prerelease parts, when
    one has build metadata and the other doesn't, the one with the build metadata has lower precedence on the one
    without the build metadata. Example: 1.2.3+build.1 &lt; 1.2.3 and 1.2.3-alpha.0+build.1 &lt; 1.2.3-alpha.0.

Arguments are as follows:

- v the version to be compared.
*/
func (sv SemanticVersion) CompareTo(v SemanticVersion) int {
	// Rule #9
	if sv.GetMajor() != v.GetMajor() {
		return sv.GetMajor() - v.GetMajor()
	}
	if sv.GetMinor() != v.GetMinor() {
		return sv.GetMinor() - v.GetMinor()
	}
	if sv.GetPatch() != v.GetPatch() {
		return sv.GetPatch() - v.GetPatch()
	}

	// Rule #11
	if sv.prereleaseIdentifier != nil || v.prereleaseIdentifier != nil {
		// When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version.
		// Example: 1.0.0-alpha < 1.0.0.
		// So if only one has the prerelease block, that means it has lower precedence (comes first)
		if sv.prereleaseIdentifier != nil && v.prereleaseIdentifier == nil {
			return -1
		} else if sv.prereleaseIdentifier == nil && v.prereleaseIdentifier != nil {
			return 1
		}
	}
	if sv.prereleaseIdentifier != nil {
		// Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined
		// by comparing each dot separated identifier from left to right until a difference is found as follows:
		// identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens
		// are compared lexically in ASCII sort order.
		theseItems := sv.prereleaseIdentifier.getValues()
		otherItems := v.prereleaseIdentifier.getValues()

		for i := 0; i < len(theseItems); i++ {
			if len(otherItems) >= i+1 {
				thisItem := theseItems[i]
				otherItem := otherItems[i]

				// Identifiers consisting of only digits are compared numerically and identifiers with letters or
				// hyphens are compared lexically in ASCII sort order. Numeric identifiers always have lower
				// precedence than non-numeric identifiers.

				thisItemIntValue, ok := thisItem.(int)
				if ok {
					// This item is a number
					otherItemIntValue, ok := otherItem.(int)
					if ok {
						// Also the other item is a number so let's compare them as such
						res := thisItemIntValue - otherItemIntValue
						if res != 0 {
							return res
						}
					} else {
						// This item is a number while the other is a string, so this has lower precedence (comes first)
						return -1
					}
				} else {
					// This item is a string
					_, ok := otherItem.(int)
					if ok {
						// the other item is a number while this is a string so the other has lower precedence (comes first) than this
						return 1
					} else {
						// Also the other item is a string so let's see how they compare as strings
						res := strings.Compare(fmt.Sprint(thisItem), fmt.Sprint(otherItem))
						if res != 0 {
							return res
						}
					}
				}
			} else {
				// This set has more elements than the other so it has more precedence (comes after in order)
				//
				// A larger set of pre-release fields has a higher precedence than a smaller set, if all of the
				// preceding identifiers are equal.
				// Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
				return 1
			}
		}
		if len(otherItems) > len(theseItems) {
			// This set has less elements than the other so it has less precedence (comes first in order)
			//
			// A larger set of pre-release fields has a higher precedence than a smaller set, if all of the
			// preceding identifiers are equal.
			// Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
			return -1
		}
	}

	// Rule #10 (amended to achieve total order)
	if sv.buildIdentifier != nil || v.buildIdentifier != nil {
		// Although the build part should be ignored from SemVer specs, the compareTo method needs to differentiate
		// versions that have the same core and prerelease but different build parts.
		// We chose to be consistent with rule #10 (about prerelease) so the version that has a build part has
		// When major, minor, and patch and prerelease are equal, a build version has lower precedence than a normal
		// (or normal-prerelease) version.
		// Example: 1.0.0-alpha+build < 1.0.0-alpha, and 1.0.0+build < 1.0.0.
		if sv.buildIdentifier != nil && v.buildIdentifier == nil {
			return -1
		} else if sv.buildIdentifier == nil && v.buildIdentifier != nil {
			return 1
		}
	}
	// If no difference is found, let's consider the number of items in the build part. This is not requested by
	// SemVer but, again, we use the same behavior as for prerelease. So the set that has less elements has less
	// precedence (comes first in order).
	if sv.buildIdentifier != nil && v.buildIdentifier != nil {
		if len(sv.buildIdentifier.children) != len(v.buildIdentifier.children) {
			return len(v.buildIdentifier.children) - len(sv.buildIdentifier.children)
		}
	}

	// If no difference has been found yet, let's just compare the build parts as strings
	if sv.buildIdentifier != nil && strings.Compare(sv.buildIdentifier.String(), v.buildIdentifier.String()) != 0 {
		return strings.Compare(sv.buildIdentifier.String(), v.buildIdentifier.String())
	}

	// As a last resort, compare the string version of both entities. This is not requested (explicitly) by SemVer
	// but it's still compliant with it and with Comparable as well.
	return strings.Compare(sv.String(), v.String())
}

/*
Returns SEMVER.
*/
func (sv SemanticVersion) GetScheme() Scheme {
	return SEMVER
}

/*
Returns true if the given string is a legal semantic version which, for example, can be parsed without errors.

This method uses a strict criteria, without trying to sanitize the given string.

Arguments are as follows:

- s the string version to check.
*/
func IsLegalSemanticVersion(s string) bool {
	return IsLegalSemanticVersionWithLenience(s, false)
}

/*
Returns true if the given string is a legal semantic version which, for example, can be parsed without errors.

Arguments are as follows:

  - s the string version to check.
  - lenient when true prefixes and non critical extra characters are tolerated even if they are not
    strictly legal from the semantic version specification perspective.
*/
func IsLegalSemanticVersionWithLenience(s string, lenient bool) bool {
	isMatch := false
	if lenient {
		re := regexp2.MustCompile(SEMANTIC_VERSION_PATTERN_RELAXED, 0)
		isMatch, _ = re.MatchString(s)
	} else {
		re := regexp2.MustCompile(SEMANTIC_VERSION_PATTERN, 0)
		isMatch, _ = re.MatchString(s)
	}
	return isMatch
}

/*
Returns a string representation of the object.
*/
func (sv SemanticVersion) String() string {
	if sv.renderedString == nil {
		var sb strings.Builder
		sb.WriteString(sv.coreIdentifier.String())
		if sv.prereleaseIdentifier != nil {
			sb.WriteString(PRERELEASE_DELIMITER)
			sb.WriteString(sv.prereleaseIdentifier.String())
		}
		if sv.buildIdentifier != nil {
			sb.WriteString(BUILD_DELIMITER)
			sb.WriteString(sv.buildIdentifier.String())
		}
		rs := sb.String()
		sv.renderedString = &rs
	}
	return *sv.renderedString
}

/*
Returns a SemanticVersion instance representing the specified String value. No sanitization attempt is done.

Arguments are as follows:

- s the string to parse

Errors can be returned if:

- the given string doesn't represent a legal semantic version
*/
func ValueOfSemanticVersion(s string) (SemanticVersion, error) {
	if "" == s {
		return SemanticVersion{}, fmt.Errorf("can't parse an empty string")
	}

	re, err := regexp2.Compile(SEMANTIC_VERSION_PATTERN, 0)
	if err != nil {
		return SemanticVersion{}, fmt.Errorf("regular expression '%s' can't be compiled: %w", SEMANTIC_VERSION_PATTERN, err)
	}
	m, err := re.FindStringMatch(s)
	if err != nil {
		return SemanticVersion{}, fmt.Errorf("regular expression '%s' can't be matched: %w", SEMANTIC_VERSION_PATTERN, err)
	}

	if m != nil { // when this is nil it means no match was found, so skip this argument
		// group 0 is the entire version string
		// group 1 is the major number
		// group 2 is the minor number
		// group 3 is the patch number
		// group 4 (optional) is the prerelease
		// group 5 (optional) is the build

		if len(m.GroupByNumber(1).Captures) == 0 || len(m.GroupByNumber(2).Captures) == 0 || len(m.GroupByNumber(3).Captures) == 0 {
			return SemanticVersion{}, fmt.Errorf("the string '%s' does not contain a valid semantic number", s)
		}
		coreIdentifier, err := newSemanticVersionCoreIdentifierFromStrings(m.GroupByNumber(1).Captures[0].String(), m.GroupByNumber(2).Captures[0].String(), m.GroupByNumber(3).Captures[0].String())
		if err != nil {
			return SemanticVersion{}, err
		}
		var prereleaseIdentifier *semanticVersionPreReleaseIdentifier = nil
		if m.GroupByNumber(4) != nil && len(m.GroupByNumber(4).Captures) > 0 && "" != m.GroupByNumber(4).Captures[0].String() {
			pi, err := valueOfSemanticVersionPreReleaseIdentifierFromString(true, m.GroupByNumber(4).Captures[0].String())
			if err != nil {
				return SemanticVersion{}, err
			}
			prereleaseIdentifier = &pi
		}
		var buildIdentifier *semanticVersionBuildIdentifier = nil
		if m.GroupByNumber(5) != nil && len(m.GroupByNumber(5).Captures) > 0 && "" != m.GroupByNumber(5).Captures[0].String() {
			bi, err := valueOfSemanticVersionBuildIdentifierFromString(true, m.GroupByNumber(5).Captures[0].String())
			if err != nil {
				return SemanticVersion{}, err
			}
			buildIdentifier = &bi
		}

		return newSemanticVersion(&coreIdentifier, prereleaseIdentifier, buildIdentifier)
	} else {
		return SemanticVersion{}, fmt.Errorf("the string '%s' does not contain a valid semantic number", s)
	}
}

/*
This method is a shorthand for ValueOfSemanticVersion and SanitizeSemanticVersion.

Returns a SemanticVersion instance representing the specified String value. If sanitize is
true this method will try to sanitize the given string before parsing so that if there are
illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.

When sanitization is enabled on a string that actually needs sanitization the string representation of the
returned object will not exactly match the input value.

Arguments are as follows:

- s the string to parse
- sanitize optionally enables sanitization before parsing

Errors can be returned if:

- the given string doesn't represent a legal semantic version
*/
func ValueOfSemanticVersionWithSanitization(s string, sanitize bool) (SemanticVersion, error) {
	if sanitize {
		sanitizedVersion, err := SanitizeSemanticVersion(s)
		if err != nil {
			return SemanticVersion{}, err
		}
		return ValueOfSemanticVersion(sanitizedVersion)
	} else {
		return ValueOfSemanticVersion(s)
	}
}

/*
Performs all of the sanitizations in the given string by sanitizing, in order, the prefix and leading zeroes
in numeric identifiers. This order is the one that yields to the highest success probability in obtaining a
legal version.
Invoking this method is like invoking the sanitization methods SanitizeNumbers and SanitizePrefix.

Arguments are as follows:

- s the semantic version string to sanitize

Errors can be returned if:

- the given string doesn't represent a legal semantic version, even tolerating the aspects to sanitize
*/
func SanitizeSemanticVersion(s string) (string, error) {
	sanitizedVersion, err := SanitizeSemanticVersionPrefix(s)
	if err != nil {
		return "", err
	}
	return SanitizeSemanticVersionNumbers(sanitizedVersion)
}

/*
Takes the given string and tries to parse it as a semantic version number, even with illegal characters or prefix.
All numeric identifiers in the core version (major.minor.patch) and in the prerelease metadata are
sanitized by removing all leading zeroes to make them compliant. Numeric identifiers in the build metadata part
are left intact, even when they have leading zeroes.
If the given string contains a prefix (see SanitizePrefix) or illegal characters they are left
untouched and they are returned as they were in the input string.

Arguments are as follows:

  - s a semantic version string which may have illegal leading zeroes to be removed in the numeric identifiers
    in the core or the prerelease parts.

Errors can be returned if:

- the given string doesn't represent a legal semantic version, even tolerating the prefix
*/
func SanitizeSemanticVersionNumbers(s string) (string, error) {
	if "" == s {
		return "", fmt.Errorf("can't sanitize an empty string")
	}

	var result strings.Builder

	// if there's any prefix, leave it there in the result
	prefix, err := GetSemanticVersionPrefix(s)
	if err != nil {
		return "", err
	}
	if prefix != nil {
		result.WriteString(*prefix)
	}

	re, err := regexp2.Compile(SEMANTIC_VERSION_PATTERN_RELAXED, 0)
	if err != nil {
		return "", fmt.Errorf("regular expression '%s' can't be compiled: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}
	m, err := re.FindStringMatch(s)
	if err != nil {
		return "", fmt.Errorf("regular expression '%s' can't be matched: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}

	if m != nil { // when this is nil it means no match was found, so skip this argument
		// group 0 is the entire version string
		// group 1 is the major number
		// group 2 is the minor number
		// group 3 is the patch number
		// group 4 (optional) is the prerelease
		// group 5 (optional) is the build

		// to remove leading zeroes, just transform the numbers to Integers and back to strings
		if len(m.GroupByNumber(1).Captures) == 0 {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		integer, err := strconv.Atoi(m.GroupByNumber(1).Captures[0].String())
		if err != nil {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		if integer < 0 {
			return "", fmt.Errorf("can't sanitize negative number '%d' in '%s'", integer, s)
		}
		result.WriteString(fmt.Sprint(integer))
		result.WriteString(DEFAULT_SEPARATOR)

		if len(m.GroupByNumber(2).Captures) == 0 {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		integer, err = strconv.Atoi(m.GroupByNumber(2).Captures[0].String())
		if err != nil {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		if integer < 0 {
			return "", fmt.Errorf("can't sanitize negative number '%d' in '%s'", integer, s)
		}
		result.WriteString(fmt.Sprint(integer))
		result.WriteString(DEFAULT_SEPARATOR)

		if len(m.GroupByNumber(3).Captures) == 0 {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		integer, err = strconv.Atoi(m.GroupByNumber(3).Captures[0].String())
		if err != nil {
			return "", fmt.Errorf("numeric identifiers in string '%s' can't be converted to valid integers", s)
		}
		if integer < 0 {
			return "", fmt.Errorf("can't sanitize negative number '%d' in '%s'", integer, s)
		}
		result.WriteString(fmt.Sprint(integer))

		// Go through all identifiers in the prerelease part. If they can convert to an integer just do it and
		// append their string representation to the output, this automatically removes leading zeroes.
		// If they can't be converted to numberst just append them as they are.

		if m.GroupByNumber(4) != nil && len(m.GroupByNumber(4).Captures) > 0 && "" != m.GroupByNumber(4).Captures[0].String() {
			result.WriteString(PRERELEASE_DELIMITER)
			identifiers := strings.Split(m.GroupByNumber(4).Captures[0].String(), DEFAULT_SEPARATOR)
			for i := 0; i < len(identifiers); i++ {
				identifier := identifiers[i]
				integerIdentifier, err := strconv.Atoi(identifier)
				if err == nil {
					result.WriteString(strconv.Itoa(integerIdentifier))
				} else {
					result.WriteString(identifier)
				}
				if len(identifiers) > i+1 {
					result.WriteString(DEFAULT_SEPARATOR)
				}
			}
		}

		// append the build part just as it was
		if m.GroupByNumber(5) != nil && len(m.GroupByNumber(5).Captures) > 0 && "" != m.GroupByNumber(5).Captures[0].String() {
			result.WriteString(BUILD_DELIMITER)
			result.WriteString(m.GroupByNumber(5).Captures[0].String())
		}

	} else {
		return "", fmt.Errorf("the string '%s' does not contain a valid semantic number", s)
	}

	return result.String(), nil
}

/*
Takes the given string and tries to parse it as a semantic version with an optional prefix (which may be any
string before the core major.minor.patch numbers). The returned string is the semantic version passed
as input with the prefix removed. If no prefix is present then the returned string is the same as the one passed
as input. Prefixes are often used (i.e. the 'v' used it Git tags or release-, rel etc)
so this method helps in stripping those prefixes to get a compliant semantic version.

Arguments are as follows:

- s a semantic version string which may have an additional prefix to be removed

Errors can be returned if:

- the given string doesn't represent a legal semantic version, even tolerating the prefix
*/
func SanitizeSemanticVersionPrefix(s string) (string, error) {
	if "" == s {
		return "", fmt.Errorf("can't sanitize an empty string")
	}

	re, err := regexp2.Compile(SEMANTIC_VERSION_PATTERN_RELAXED, 0)
	if err != nil {
		return "", fmt.Errorf("regular expression '%s' can't be compiled: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}
	m, err := re.FindStringMatch(s)
	if err != nil {
		return "", fmt.Errorf("regular expression '%s' can't be matched: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}

	if m != nil && len(m.GroupByNumber(0).Captures) > 0 { // when this is nil it means no match was found, so skip this argument
		// group 0 is the entire version string
		return m.GroupByNumber(0).Captures[0].String(), nil
	} else {
		return "", fmt.Errorf("the string '%s' does not contain a valid semantic number", s)
	}
}

/*
Takes the given string and tries to parse it as a semantic version with an optional prefix (which may be any
string before the core major.minor.patch numbers). The returned string is the prefix before the core
version number, if any, or nil otherwise.

Arguments are as follows:

- s a semantic version string which may have an additional prefix to be isolated

Errors can be returned if:

- the given string doesn't represent a legal semantic version, even tolerating the prefix
*/
func GetSemanticVersionPrefix(s string) (*string, error) {
	if "" == s {
		return nil, nil
	}

	re, err := regexp2.Compile(SEMANTIC_VERSION_PATTERN_RELAXED, 0)
	if err != nil {
		return nil, fmt.Errorf("regular expression '%s' can't be compiled: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}
	m, err := re.FindStringMatch(s)
	if err != nil {
		return nil, fmt.Errorf("regular expression '%s' can't be matched: %w", SEMANTIC_VERSION_PATTERN_RELAXED, err)
	}

	if m != nil && len(m.GroupByNumber(0).Captures) > 0 { // when this is nil it means no match was found, so skip this argument
		// group 0 is the entire version string
		if s == m.GroupByNumber(0).Captures[0].String() {
			return nil, nil
		} else {
			res := s[0 : len(s)-len(m.GroupByNumber(0).Captures[0].String())]
			return &res, nil
		}
	} else {
		return nil, fmt.Errorf("the string '%s' does not contain a valid semantic number", s)
	}
}

/*
Returns the major version number
*/
func (sv SemanticVersion) GetMajor() int {
	return sv.coreIdentifier.getMajor()
}

/*
Returns the minor version number
*/
func (sv SemanticVersion) GetMinor() int {
	return sv.coreIdentifier.getMinor()
}

/*
Returns the patch version number
*/
func (sv SemanticVersion) GetPatch() int {
	return sv.coreIdentifier.getPatch()
}

/*
Returns the core part (major.minor.patch) of the version as a string.
*/
func (sv SemanticVersion) GetCore() string {
	return sv.coreIdentifier.String()
}

/*
Returns an array of the single identifiers of the core part of the version
*/
func (sv SemanticVersion) GetCoreIdentifiers() []int {
	res := make([]int, len(sv.coreIdentifier.children))
	for i := 0; i < len(sv.coreIdentifier.children); i++ {
		res[i] = sv.coreIdentifier.get(uint(i))
	}
	return res
}

/*
Returns the prerelease part of the version, if any, or nil otherwise.
*/
func (sv SemanticVersion) GetPrerelease() *string {
	if sv.prereleaseIdentifier == nil {
		return nil
	} else {
		res := sv.prereleaseIdentifier.String()
		return &res
	}
}

/*
Returns an array of the single identifiers of the prerelease part of the version, if any, or nil
otherwise.
*/
func (sv SemanticVersion) GetPrereleaseIdentifiers() *[]interface{} {
	if sv.prereleaseIdentifier == nil {
		return nil
	}
	res := make([]interface{}, len(sv.prereleaseIdentifier.children))
	for i := 0; i < len(sv.prereleaseIdentifier.children); i++ {
		res[i] = sv.prereleaseIdentifier.get(uint(i))
	}
	return &res
}

/*
Returns the build part of the version, if any, or nil otherwise.
*/
func (sv SemanticVersion) GetBuild() *string {
	if sv.buildIdentifier == nil {
		return nil
	} else {
		res := sv.buildIdentifier.String()
		return &res
	}
}

/*
Returns an array of the single identifiers of the build part of the version, if any, or nil
otherwise.
*/
func (sv SemanticVersion) GetBuildIdentifiers() *[]string {
	if sv.buildIdentifier == nil {
		return nil
	}
	res := make([]string, len(sv.buildIdentifier.children))
	for i := 0; i < len(sv.buildIdentifier.children); i++ {
		res[i] = sv.buildIdentifier.get(uint(i))
	}
	return &res
}

/*
Returns a new version object with the major, minor and patch numbers set
to the given values. This method doesn't reset any number and the prerelease and build blocks are left unchanged.

Arguments are as follows:

- major the major number
- minor the minor number
- patch the patch number

Errors can be returned if:

- any of the given values is negative
*/
func (sv SemanticVersion) SetCore(major int, minor int, patch int) (SemanticVersion, error) {
	coreIdentifier, err := newSemanticVersionCoreIdentifierFromIntegers(major, minor, patch)
	if err != nil {
		return SemanticVersion{}, err
	}
	return newSemanticVersion(&coreIdentifier, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new version object with the major number set to the given value.
This method doesn't reset any number and the prerelease and build blocks are left unchanged.

Arguments are as follows:

- major the major number to set

Errors can be returned if:

- the given value is negative
*/
func (sv SemanticVersion) SetMajor(major int) (SemanticVersion, error) {
	coreIdentifier, err := newSemanticVersionCoreIdentifierFromIntegers(major, sv.coreIdentifier.getMinor(), sv.coreIdentifier.getPatch())
	if err != nil {
		return SemanticVersion{}, err
	}
	return newSemanticVersion(&coreIdentifier, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new version object with the minor number set to the given value.
This method doesn't reset any number and the prerelease and build blocks are left unchanged.

Arguments are as follows:

- minor the minor number to set

Errors can be returned if:

- the given value is negative
*/
func (sv SemanticVersion) SetMinor(minor int) (SemanticVersion, error) {
	coreIdentifier, err := newSemanticVersionCoreIdentifierFromIntegers(sv.coreIdentifier.getMajor(), minor, sv.coreIdentifier.getPatch())
	if err != nil {
		return SemanticVersion{}, err
	}
	return newSemanticVersion(&coreIdentifier, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new version object with the patch number set to the given value.
This method doesn't reset any number and the prerelease and build blocks are left unchanged.

Arguments are as follows:

- patch the patch number to set

Errors can be returned if:

- the given value is negative
*/
func (sv SemanticVersion) SetPatch(patch int) (SemanticVersion, error) {
	coreIdentifier, err := newSemanticVersionCoreIdentifierFromIntegers(sv.coreIdentifier.getMajor(), sv.coreIdentifier.getMinor(), patch)
	if err != nil {
		return SemanticVersion{}, err
	}
	return newSemanticVersion(&coreIdentifier, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new version object with the prerelease part set to the given values. If a nil value or an array
of all nil values is passed then the returned version will have no prerelease part, otherwise it will have
all of the given non nil identifiers, with the core and build elements of this version instance.

Arguments are as follows:

  - identifiers the identifiers to use for the new version instance, or nil to remove the
    prerelease block. All non nil items must be String or Integer instances.
    String instances representing numeric values will be interpreted as Integer.
    If the current version had a pre-release part it is completely replaced by the given identifiers.

Errors can be returned if:

  - some non nil item passed contains illegal characters, if a given number is negative or contains leading zeroes
    or any item is not an instance of String or Integer
*/
func (sv SemanticVersion) SetPrerelease(identifiers ...interface{}) (SemanticVersion, error) {
	var svpri *semanticVersionPreReleaseIdentifier = nil
	if hasValues(identifiers) {
		instance, err := valueOfSemanticVersionPreReleaseIdentifierFromObjects(true, identifiers...)
		if err != nil {
			return SemanticVersion{}, err
		}
		svpri = &instance
	}
	return newSemanticVersion(sv.coreIdentifier, svpri, sv.buildIdentifier)
}

/*
Returns a new version object with the build part set to the given values. If a nil value or an array
of all nil values is passed then the returned version will have no build part, otherwise it will have
all of the given non nil identifiers, with the core and prerelease elements of this version instance.
If the current version had a build part it is completely replaced by the given identifiers.

Arguments are as follows:

  - identifiers the identifiers to use for the new version instance, or nil to remove the
    build block

Errors can be returned if:

- some non nil item passed contains illegal characters
*/
func (sv SemanticVersion) SetBuild(identifiers ...string) (SemanticVersion, error) {
	var svbi *semanticVersionBuildIdentifier = nil
	if identifiers != nil && len(identifiers) > 0 {
		instance, err := valueOfSemanticVersionBuildIdentifierFromStrings(true, identifiers...)
		if err != nil {
			return SemanticVersion{}, err
		}
		svbi = &instance
	}
	return newSemanticVersion(sv.coreIdentifier, sv.prereleaseIdentifier, svbi)
}

/*
Returns true if an attribute with the given name is present in the prerelease part, false otherwise.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty false is returned
*/
func (sv SemanticVersion) HasPrereleaseAttribute(name string) bool {
	if sv.prereleaseIdentifier == nil {
		return false
	} else {
		return sv.prereleaseIdentifier.hasAttribute(name)
	}
}

/*
Returns true if an attribute with the given name is present in the build part, false otherwise.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty false is returned
*/
func (sv SemanticVersion) HasBuildAttribute(name string) bool {
	if sv.buildIdentifier == nil {
		return false
	} else {
		return sv.buildIdentifier.hasAttribute(name)
	}
}

/*
If an attribute with the given name is present in the prerelease part, return the identifier after that, otherwise return nil.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty nil is returned
*/
func (sv SemanticVersion) GetPrereleaseAttributeValue(name string) *int {
	if sv.prereleaseIdentifier == nil {
		return nil
	} else {
		return sv.prereleaseIdentifier.getAttributeValue(name)
	}
}

/*
If an attribute with the given name is present in the build part, return the identifier after that, otherwise return nil.

Arguments are as follows:

- name the name of the attribute to look up. If nil or empty nil is returned
*/
func (sv SemanticVersion) GetBuildAttributeValue(name string) *string {
	if sv.buildIdentifier == nil {
		return nil
	} else {
		return sv.buildIdentifier.getAttributeValue(name)
	}
}

/*
Returns a new version object with the new attribute added or replaced in the prerelease part. This method tries to be
less intrusive as it only works on the given attribute (and its optional value) while leaving the other attributes
unchanged.

If this version doesn't have a prerelease part, the returned version will have one with the new attribute appended
(and its value as well, if not nil).

If this version already has a prerelease part with no identifier matching the given attribute name then the returned
version will have the same prerelease part as the current one with the new attribute appended (and its value as well,
if not nil).

If this version already has a prerelease part that contains an identifier matching the given attribute name then
the identifier matching the attribute name is left unchanged and if the given value is not nil,
the next identifier is added or replaced with the given value.
ATTENTION: if the value is not nil the current identifier after the name (if any) is replaced
if it's a numeric identifier.

Examples of invoking SetPrereleaseAttribute("build") with nil value:
- 1.2.3 = 1.2.3-build
- 1.2.3-alpha = 1.2.3-alpha.build
- 1.2.3-alpha.beta = 1.2.3-alpha.beta.build
- 1.2.3+timestamp = 1.2.3-build+timestamp
- 1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha.build+timestamp.20200101
- 1.2.3-build = 1.2.3-build (unchanged)
- 1.2.3-build.12345 = 1.2.3-build.12345 (unchanged)
- 1.2.3-build.12345.timestamp.20200101 = 1.2.3-build.12345.timestamp.20200101 (unchanged)

Examples of invoking SetPrereleaseAttribute("build") with 12345 value:
- 1.2.3 = 1.2.3-build.12345
- 1.2.3-alpha = 1.2.3-alpha.build.12345
- 1.2.3-alpha.beta = 1.2.3-alpha.beta.build.12345
- 1.2.3+timestamp = 1.2.3-build.12345+timestamp
- 1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha.build.12345+timestamp.20200101
- 1.2.3-build = 1.2.3-build.12345
- 1.2.3-build.12345 = 1.2.3-build.12345 (unchanged)
- 1.2.3-build.12345.timestamp.20200101 = 1.2.3-build.12345.timestamp.20200101 (unchanged)

Arguments are as follows:

- name the name to set for the attribute
- value the value to set for the attribute, or nil just set the attribute name, ignoring the value

Errors can be returned if:

- some non nil item passed contains illegal characters
*/
func (sv SemanticVersion) SetPrereleaseAttributeWith(name string, value *int) (SemanticVersion, error) {
	if sv.prereleaseIdentifier == nil {
		// valueOfSemanticVersionPreReleaseIdentifierFromObjects does not accept a nil for a second parameter (value) so we need to discriminate here
		if value == nil {
			svpri, err := valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, name)
			if err != nil {
				return SemanticVersion{}, err
			}
			return newSemanticVersion(sv.coreIdentifier, &svpri, sv.buildIdentifier)
		} else {
			svpri, err := valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, name, *value)
			if err != nil {
				return SemanticVersion{}, err
			}
			return newSemanticVersion(sv.coreIdentifier, &svpri, sv.buildIdentifier)
		}
	} else {
		svpri, err := sv.prereleaseIdentifier.setAttribute(name, value)
		if err != nil {
			return SemanticVersion{}, err
		}
		return newSemanticVersion(sv.coreIdentifier, &svpri, sv.buildIdentifier)
	}
}

/*
Returns a new version object with the new attribute added or replaced in the prerelease part.
This method is a shorthand for SetPrereleaseAttribute to only set a simple identifier instead of a pair.

Arguments are as follows:

- name the name to set for the attribute

Errors can be returned if:

- some non nil item passed contains illegal characters
*/
func (sv SemanticVersion) SetPrereleaseAttribute(name string) (SemanticVersion, error) {
	return sv.SetPrereleaseAttributeWith(name, nil)
}

/*
Returns a new version object with the new attribute added or replaced in the build part. This method tries to be
less intrusive as it only works on the given attribute (and its optional value) while leaving the other attributes
unchanged.

If this version doesn't have a build part, the returned version will have one with the new attribute appended
(and its value as well, if not nil).

If this version already has a build part with no identifier matching the given attribute name then the returned
version will have the same build part as the current one with the new attribute appended (and its value as well,
if not nil).

If this version already has a build part that contains an identifier matching the given attribute name then
the identifier matching the attribute name is left unchanged and if the given value is not nil,
the next identifier is added or replaced with the given value.
ATTENTION: if the value is not nil the current identifier after the name (if any) is replaced
without further consideration.

Examples of invoking SetBuildAttribute("build") with nil value:
- 1.2.3 = 1.2.3+build
- 1.2.3-alpha = 1.2.3-alpha+build
- 1.2.3-alpha.beta = 1.2.3-alpha.beta+build
- 1.2.3+timestamp = 1.2.3+timestamp.build
- 1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha+timestamp.20200101.build
- 1.2.3+build = 1.2.3+build (unchanged)
- 1.2.3+build.12345 = 1.2.3+build.12345 (unchanged)
- 1.2.3+build.12345.timestamp.20200101 = 1.2.3+build.12345.timestamp.20200101 (unchanged)

Examples of invoking SetBuildAttribute("build") with 12345 value:
- 1.2.3 = 1.2.3+build.12345
- 1.2.3-alpha = 1.2.3-alpha+build.12345
- 1.2.3-alpha.beta = 1.2.3-alpha.beta+build.12345
- 1.2.3+timestamp = 1.2.3+timestamp.build.12345
- 1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha+timestamp.20200101.build.12345
- 1.2.3+build = 1.2.3+build.12345
- 1.2.3+build.12345 = 1.2.3+build.12345 (unchanged)
- 1.2.3+build.12345.timestamp.20200101 = 1.2.3+build.12345.timestamp.20200101 (unchanged)

Arguments are as follows:

- name the name to set for the attribute
- value the value to set for the attribute, or nil just set the attribute name, ignoring the value

Errors can be returned if:

- some non nil item passed contains illegal characters
*/
func (sv SemanticVersion) SetBuildAttributeWith(name string, value *string) (SemanticVersion, error) {
	if sv.buildIdentifier == nil {
		// valueOfSemanticVersionBuildIdentifierFromStrings does not accept a nil for a second parameter (value) so we need to discriminate here
		if value == nil {
			svbi, err := valueOfSemanticVersionBuildIdentifierFromStrings(false, name)
			if err != nil {
				return SemanticVersion{}, err
			}
			return newSemanticVersion(sv.coreIdentifier, sv.prereleaseIdentifier, &svbi)
		} else {
			svbi, err := valueOfSemanticVersionBuildIdentifierFromStrings(false, name, *value)
			if err != nil {
				return SemanticVersion{}, err
			}
			return newSemanticVersion(sv.coreIdentifier, sv.prereleaseIdentifier, &svbi)
		}
	} else {
		svbi, err := sv.buildIdentifier.setAttribute(name, value)
		if err != nil {
			return SemanticVersion{}, err
		}
		return newSemanticVersion(sv.coreIdentifier, sv.prereleaseIdentifier, &svbi)
	}
}

/*
Returns a new version object with the new attribute added or replaced in the build part.
This method is a shorthand for SetBuildAttribute to only
set a simple identifier instead of a pair.

Arguments are as follows:

- name the name to set for the attribute

Errors can be returned if:

- some non nil item passed contains illegal characters
*/
func (sv SemanticVersion) SetBuildAttribute(name string) (SemanticVersion, error) {
	return sv.SetBuildAttributeWith(name, nil)
}

/*
Returns a new instance with the new attribute removed from the prerelease part, if any was present, otherwise the same version is returned.
If the attribute is found and removeValue is true then also the attribute value (the attribute after the
one identified by name) is removed, unless there are no more attributes after name or the value attribute
is not numeric.

Arguments are as follows:

- name the name of the attribute to remove from the prerelease part, if present. If nil or empty no action is taken
- removeValue if true also the attribute after name is removed (if any)
*/
func (sv SemanticVersion) RemovePrereleaseAttribute(name *string, removeValue bool) (SemanticVersion, error) {
	if sv.prereleaseIdentifier == nil {
		return sv, nil
	} else {
		svpri, err := sv.prereleaseIdentifier.removeAttribute(name, removeValue)
		if err != nil {
			return SemanticVersion{}, err
		}
		return newSemanticVersion(sv.coreIdentifier, svpri, sv.buildIdentifier)
	}
}

/*
Returns a new instance with the new attribute removed from the build part, if any was present, otherwise the same version is returned.
If the attribute is found and removeValue is true then also the attribute value (the attribute after the
one identified by name) is removed, unless there are no more attributes after name.

Arguments are as follows:

- name the name of the attribute to remove from the build part, if present. If nil or empty no action is taken
- removeValue if true also the attribute after name is removed (if any)
*/
func (sv SemanticVersion) RemoveBuildAttribute(name *string, removeValue bool) (SemanticVersion, error) {
	if sv.buildIdentifier == nil {
		return sv, nil
	} else {
		svbi, err := sv.buildIdentifier.removeAttribute(name, removeValue)
		if err != nil {
			return SemanticVersion{}, err
		}
		return newSemanticVersion(sv.coreIdentifier, sv.prereleaseIdentifier, svbi)
	}
}

/*
Returns a new instance with the major number of this current instance incremented by one and the minor and patch
numbers reset to zero. Prerelease and build parts are left intact.
*/
func (sv SemanticVersion) BumpMajor() (SemanticVersion, error) {
	ci := sv.coreIdentifier.bumpMajor()
	return newSemanticVersion(&ci, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new instance with the major number of this current instance, the minor number incremented by one and
the patch number reset to zero. Prerelease and build parts are left intact.
*/
func (sv SemanticVersion) BumpMinor() (SemanticVersion, error) {
	ci := sv.coreIdentifier.bumpMinor()
	return newSemanticVersion(&ci, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new instance with the major and minor numbers of this current instance and the patch number
incremented by one. Prerelease and build parts are left intact.
*/
func (sv SemanticVersion) BumpPatch() (SemanticVersion, error) {
	ci := sv.coreIdentifier.bumpPatch()
	return newSemanticVersion(&ci, sv.prereleaseIdentifier, sv.buildIdentifier)
}

/*
Returns a new instance with the number identified by the given value bumped.

Arguments are as follows:

- id the selector of the identifier to bump
*/
func (sv SemanticVersion) BumpIdentifier(id CoreIdentifiers) (SemanticVersion, error) {
	switch id {
	case MAJOR:
		return sv.BumpMajor()
	case MINOR:
		return sv.BumpMinor()
	case PATCH:
		return sv.BumpPatch()
	default:
		// this is never reached, but in case...
		panic("unknown CoreIdentifiers. This means the switch/case statement needs to be updated")
	}
}

/*
Returns a new instance with the number identified by the given value bumped in the prerelease part. The core and
the build blocks (if present) are left unchanged.

If this version doesn't have a prerelease block the returned version will have one, containing two identifiers:
the given string and the following number .0.

If this version already has a prerelease block without any identifier that equals the given id, then the returned
version has all the previous prerelease identifiers preceded by the two new identifiers the given string and
the following number .1.
If this version already has a prerelease block that contains a string identifier equal to the given id there are
two options: if the selected identifier already has a numeric value that follows, the returned version will have
that numeric identifier incremented by one; if the selected identifier doesn't have a numeric identifier that
follows, a new numeric identifiers is added after the string with the initial value .1.

If the version already has multiple identifiers in the prerelease block that equal to the given value then all of
them will be bumped. In case they have different numeric values (or missing) each occurrence is bumped
independently according to the above rules.

Examples of invoking BumpPrerelease("alpha") on different versions:
- 1.2.3 = 1.2.3-alpha.0
- 1.2.3-alpha = 1.2.3-alpha.0
- 1.2.3-alpha.beta = 1.2.3-alpha.0.beta
- 1.2.3-gamma = 1.2.3-alpha.0.gamma
- 1.2.3-gamma.delta = 1.2.3-alpha.0.gamma.delta
- 1.2.3+999 = 1.2.3-alpha.0+999
- 1.2.3-alpha+999 = 1.2.3-alpha.0+999
- 1.2.3-alpha.beta+999 = 1.2.3-alpha.0.beta+999
- 1.2.3-gamma+999 = 1.2.3-alpha.0.gamma+999
- 1.2.3-gamma.delta+999 = 1.2.3-alpha.0.gamma.delta+999
- 1.2.3-alpha.alpha.1.alpha.2 = 1.2.3-alpha.0.alpha.2.alpha.3

Arguments are as follows:

- id the selector of the identifier to bump

Errors can be returned if:

- the given string is empty, contains illegal characters or represents a number
*/
func (sv SemanticVersion) BumpPrerelease(id string) (SemanticVersion, error) {
	if "" == id {
		return SemanticVersion{}, fmt.Errorf("can't bump an empty identifier")
	}

	_, err := strconv.Atoi(id)
	if err == nil {
		// it's a number and can't be bumped
		return SemanticVersion{}, fmt.Errorf("the value '%s' is numeric and can't be used as a string identifier in the prerelease", id)
	}

	// ok, not a number. Proceed
	var pri semanticVersionPreReleaseIdentifier
	if sv.prereleaseIdentifier == nil {
		pri, err = valueOfSemanticVersionPreReleaseIdentifierFromObjects(false, id, DEFAULT_BUMP_VALUE)
		if err != nil {
			return SemanticVersion{}, err
		}
	} else {
		pri, err = sv.prereleaseIdentifier.bump(id, DEFAULT_BUMP_VALUE)
		if err != nil {
			return SemanticVersion{}, err
		}
	}
	return newSemanticVersion(sv.coreIdentifier, &pri, sv.buildIdentifier)
}

/*
Returns a new instance with the number identified by the given value bumped. If the given value represents a core
identifier (CoreIdentifiers}, namely major, minor, patch) then that
identifier is bumped, otherwise the given id is used to bump a prerelease identifier by invoking
BumpPrerelease.

In other words this method is a shorthand for BumpIdentifier and BumpPrerelease,
the latter being used only when the given id is not a core identifier.

If the version already has multiple identifiers in the prerelease block that equal to the given value then all of
them will be bumped. In case they have different numeric values (or missing) each occurrence is bumped
independently according to the above rules.

Arguments are as follows:

- id the name of the identifier to bump

Errors can be returned if:

- the given string is empty, contains illegal characters or represents a number
*/
func (sv SemanticVersion) Bump(id string) (SemanticVersion, error) {
	if "" == id {
		return SemanticVersion{}, fmt.Errorf("can't bump an empty identifier")
	}

	ci, err := ValueOfCoreIdentifiers(id)
	if err == nil {
		return sv.BumpIdentifier(ci)
	} else {
		return sv.BumpPrerelease(id)
	}
}

/*
Returns a new instance with the number identified by the given value bumped.
This is the same as Bump method but returns a generic object.

Arguments are as follows:

- id the name of the identifier to bump

Errors can be returned if:

- the given string is empty, contains illegal characters or does not represent
a valid identifier to be bumped
*/
func (sv SemanticVersion) BumpVersion(id string) (Version, error) {
	return sv.Bump(id)
}
