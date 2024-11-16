//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

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
	"fmt"       // https://pkg.go.dev/fmt
	"math"      // https://pkg.go.dev/math
	"math/rand" // https://pkg.go.dev/math/rand
	"reflect"   // https://pkg.go.dev/reflect
	"sort"      // https://pkg.go.dev/sort
	"strconv"   // https://pkg.go.dev/strconv
	"strings"   // https://pkg.go.dev/strings
	"testing"   // https://pkg.go.dev/testing
	"time"      // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

var (
	/*
		A fixture with valid structured data to test semantic versions.
		Each item is core, so it doesn't have pre-release and/or build identifiers.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- major: the major number
		- minor: the minor number
		- patch: the patch number
		- prerelease: nil
		- build: nil
	*/
	wellKnownValidCoreVersions = []struct {
		version    *string
		major      *int
		minor      *int
		patch      *int
		prerelease *[]interface{}
		build      *[]string
	}{
		// This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
		{version: strptr("0.0.4"), major: intptr(0), minor: intptr(0), patch: intptr(4), prerelease: nil, build: nil},
		{version: strptr("1.2.3"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: nil, build: nil},
		{version: strptr("10.20.30"), major: intptr(10), minor: intptr(20), patch: intptr(30), prerelease: nil, build: nil},
		{version: strptr("1.0.0"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: nil, build: nil},
		{version: strptr("2.0.0"), major: intptr(2), minor: intptr(0), patch: intptr(0), prerelease: nil, build: nil},
		{version: strptr("1.1.7"), major: intptr(1), minor: intptr(1), patch: intptr(7), prerelease: nil, build: nil},
		//Let's use just the biggest int here. just make it -1 to support bumps
		{version: strptr(strconv.Itoa(math.MaxInt-1) + "." + strconv.Itoa(math.MaxInt-1) + "." + strconv.Itoa(math.MaxInt-1)), major: intptr(math.MaxInt - 1), minor: intptr(math.MaxInt - 1), patch: intptr(math.MaxInt - 1), prerelease: nil, build: nil},
	}

	/*
		A fixture with valid structured data to test semantic versions.
		Each item is not core, so it has pre-release and/or build identifiers.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- major: the major number
		- minor: the minor number
		- patch: the patch number
		- prerelease: an (optional) List of strings, each representing one identifier in the prerelease part
		- build: an (optional) List of strings, each representing one identifier in the build part
	*/
	wellKnownValidNonCoreVersions = []struct {
		version    *string
		major      *int
		minor      *int
		patch      *int
		prerelease *[]interface{}
		build      *[]string
	}{
		// This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
		{version: strptr("1.1.2-prerelease+meta"), major: intptr(1), minor: intptr(1), patch: intptr(2), prerelease: &[](interface{}){"prerelease"}, build: &[]string{"meta"}},
		{version: strptr("1.1.2+meta"), major: intptr(1), minor: intptr(1), patch: intptr(2), prerelease: nil, build: &[]string{"meta"}},
		{version: strptr("1.1.2+meta-valid"), major: intptr(1), minor: intptr(1), patch: intptr(2), prerelease: nil, build: &[]string{"meta-valid"}},
		{version: strptr("1.0.0-alpha"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha"}, build: nil},
		{version: strptr("1.0.0-beta"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"beta"}, build: nil},
		{version: strptr("1.0.0-alpha.beta"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha", "beta"}, build: nil},
		{version: strptr("1.0.0-alpha.beta.1"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha", "beta", "1"}, build: nil},
		{version: strptr("1.0.0-alpha.1"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha", "1"}, build: nil},
		{version: strptr("1.0.0-alpha0.valid"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha0", "valid"}, build: nil},
		{version: strptr("1.0.0-alpha.0valid"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha", "0valid"}, build: nil},
		{version: strptr("1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha-a", "b-c-somethinglong"}, build: &[]string{"build", "1-aef", "1-its-okay"}},
		{version: strptr("1.0.0-rc.1+build.1"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"rc", "1"}, build: &[]string{"build", "1"}},
		{version: strptr("2.0.0-rc.1+build.123"), major: intptr(2), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"rc", "1"}, build: &[]string{"build", "123"}},
		{version: strptr("1.2.3-beta"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"beta"}, build: nil},
		{version: strptr("10.2.3-DEV-SNAPSHOT"), major: intptr(10), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"DEV-SNAPSHOT"}, build: nil},
		{version: strptr("1.2.3-SNAPSHOT-123"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"SNAPSHOT-123"}, build: nil},
		{version: strptr("2.0.0+build.1848"), major: intptr(2), minor: intptr(0), patch: intptr(0), prerelease: nil, build: &[]string{"build", "1848"}},
		{version: strptr("2.0.1-alpha.1227"), major: intptr(2), minor: intptr(0), patch: intptr(1), prerelease: &[](interface{}){"alpha", "1227"}, build: nil},
		{version: strptr("1.0.0-alpha+beta"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"alpha"}, build: &[]string{"beta"}},
		{version: strptr("1.2.3----RC-SNAPSHOT.12.9.1--.12+788"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"---RC-SNAPSHOT", "12", "9", "1--", "12"}, build: &[]string{"788"}},
		{version: strptr("1.2.3----R-S.12.9.1--.12+meta"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"---R-S", "12", "9", "1--", "12"}, build: &[]string{"meta"}},
		{version: strptr("1.2.3----RC-SNAPSHOT.12.9.1--.12"), major: intptr(1), minor: intptr(2), patch: intptr(3), prerelease: &[](interface{}){"---RC-SNAPSHOT", "12", "9", "1--", "12"}, build: nil},
		{version: strptr("1.0.0+0.build.1-rc.10000aaa-kk-0.1"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: nil, build: &[]string{"0", "build", "1-rc", "10000aaa-kk-0", "1"}},
		{version: strptr("1.0.0-0A.is.legal"), major: intptr(1), minor: intptr(0), patch: intptr(0), prerelease: &[](interface{}){"0A", "is", "legal"}, build: nil},
	}

	/*
		A fixture with valid structured data to test semantic versions.
		Each item is not core, so it has pre-release and/or build identifiers.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- major: the major number
		- minor: the minor number
		- patch: the patch number
		- prerelease: an (optional) List of strings, each representing one identifier in the prerelease part
		- build: an (optional) List of strings, each representing one identifier in the build part
	*/
	wellKnownValidVersions = append(wellKnownValidCoreVersions, wellKnownValidNonCoreVersions...)

	/*
		A fixture with invalid structured data to test semantic versions.
		These strings should not parse to a correct version as they are illegal by some mean.

		Each returned argument has the fields:
		- version: the entire string representation of the version
	*/
	wellKnownInvalidVersions = []struct {
		version string
	}{
		// This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
		{version: "1"},
		{version: "1.2"},
		{version: "1.2.3-0123"},
		{version: "1.2.3-0123.0123"},
		{version: "1.1.2+.123"},
		{version: "+invalid"},
		{version: "-invalid"},
		{version: "-invalid+invalid"},
		{version: "-invalid.01"},
		{version: "alpha"},
		{version: "alpha.beta"},
		{version: "alpha.beta.1"},
		{version: "alpha.1"},
		{version: "alpha+beta"},
		{version: "alpha_beta"},
		{version: "alpha."},
		{version: "alpha.."},
		{version: "beta"},
		{version: "1.0.0-alpha_beta"},
		{version: "-alpha."},
		{version: "1.0.0-alpha.."},
		{version: "1.0.0-alpha..1"},
		{version: "1.0.0-alpha...1"},
		{version: "1.0.0-alpha....1"},
		{version: "1.0.0-alpha.....1"},
		{version: "1.0.0-alpha......1"},
		{version: "1.0.0-alpha.......1"},
		{version: "01.1.1"},
		{version: "1.01.1"},
		{version: "1.1.01"},
		{version: "1.2"},
		{version: "1.2.3.DEV"},
		{version: "1.2-SNAPSHOT"},
		{version: "1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788"},
		{version: "1.2-RC-SNAPSHOT"},
		{version: "-1.0.3-gamma+b7718"},
		{version: "+justmeta"},
		{version: "9.8.7+meta+meta"},
		{version: "9.8.7-whatever+meta+meta"},
		{version: "99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12"},
	}

	/*
		A fixture with structured data to test semantic versions.
		Each item has spurious charachers but can be sanitized to a core version.
		Each returned argument has the fields:
		- version: the entire string representation of the sanitizeable original version
		- prefix: if the original version has a prefix, it's the expected outcome of the invocation of the getPrefix() method, otherwise nil means that the version has no prefix and getPrefix() is expected to return nil
		- sanitizedOutcome: the expected outcome of the entire sanitization over the original string
		- sanitizedPrefixOutcome: the expected outcome of sanitizePrefix() over the original string
		- sanitizedNumberOutcome: the expected outcome of sanitizeNumbers() over the original string
	*/
	wellKnownSanitizableCoreVersions = []struct {
		version                string
		prefix                 *string
		sanitizedOutcome       string
		sanitizedPrefixOutcome string
		sanitizedNumberOutcome string
	}{
		// common prefixes
		{version: "v1.2.3", prefix: strptr("v"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "v1.2.3"},
		{version: "ver1.2.3", prefix: strptr("ver"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "ver1.2.3"},
		{version: "version1.2.3", prefix: strptr("version"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "version1.2.3"},
		{version: "r1.2.3", prefix: strptr("r"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "r1.2.3"},
		{version: "rel1.2.3", prefix: strptr("rel"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "rel1.2.3"},
		{version: "release1.2.3", prefix: strptr("release"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "release1.2.3"},
		// spurious prefixes
		{version: "-1.2.3", prefix: strptr("-"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "-1.2.3"},
		{version: "!1.2.3", prefix: strptr("!"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "!1.2.3"},
		{version: "+1.2.3", prefix: strptr("+"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "+1.2.3"},
		{version: "!(trash)v1.2.3", prefix: strptr("!(trash)v"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "!(trash)v1.2.3"},
		// leading zeroes
		{version: "01.02.03", prefix: nil, sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "01.02.03", sanitizedNumberOutcome: "1.2.3"},
		{version: "000001.000002.000003", prefix: nil, sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "000001.000002.000003", sanitizedNumberOutcome: "1.2.3"},
		// These are taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
		// they are actually illegal numbers, bu they can be sanitized to legal ones
		{version: "01.1.1", prefix: nil, sanitizedOutcome: "1.1.1", sanitizedPrefixOutcome: "01.1.1", sanitizedNumberOutcome: "1.1.1"},
		{version: "1.01.1", prefix: nil, sanitizedOutcome: "1.1.1", sanitizedPrefixOutcome: "1.01.1", sanitizedNumberOutcome: "1.1.1"},
		{version: "1.1.01", prefix: nil, sanitizedOutcome: "1.1.1", sanitizedPrefixOutcome: "1.1.01", sanitizedNumberOutcome: "1.1.1"},
	}

	/*
		A fixture with structured data to test semantic versions.
		Each item has spurious charachers but can be sanitized to a non core version.
		Each returned argument has the fields:
		- version: the entire string representation of the sanitizeable original version
		- prefix: if the original version has a prefix, it's the expected outcome of the invocation of the getPrefix() method, otherwise nil means that the version has no prefix and getPrefix() is expected to return nil
		- sanitizedOutcome: the expected outcome of the entire sanitization over the original string
		- sanitizedPrefixOutcome: the expected outcome of sanitizePrefix() over the original string
		- sanitizedNumberOutcome: the expected outcome of sanitizeNumbers() over the original string
	*/
	wellKnownSanitizableNonCoreVersions = []struct {
		version                string
		prefix                 *string
		sanitizedOutcome       string
		sanitizedPrefixOutcome string
		sanitizedNumberOutcome string
	}{
		// common prefixes
		{version: "v1.2.3-alpha.1", prefix: strptr("v"), sanitizedOutcome: "1.2.3-alpha.1", sanitizedPrefixOutcome: "1.2.3-alpha.1", sanitizedNumberOutcome: "v1.2.3-alpha.1"},
		{version: "v1.2.3-alpha.1+build.2", prefix: strptr("v"), sanitizedOutcome: "1.2.3-alpha.1+build.2", sanitizedPrefixOutcome: "1.2.3-alpha.1+build.2", sanitizedNumberOutcome: "v1.2.3-alpha.1+build.2"},
		{version: "v1.2.3-alpha.1.delta-q.whatever+build.2.20200101", prefix: strptr("v"), sanitizedOutcome: "1.2.3-alpha.1.delta-q.whatever+build.2.20200101", sanitizedPrefixOutcome: "1.2.3-alpha.1.delta-q.whatever+build.2.20200101", sanitizedNumberOutcome: "v1.2.3-alpha.1.delta-q.whatever+build.2.20200101"},
		{version: "ver-1.2.3", prefix: strptr("ver-"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "ver-1.2.3"},
		{version: "ver.1.2.3", prefix: strptr("ver."), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "ver.1.2.3"},
		{version: "version-1.2.3", prefix: strptr("version-"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "version-1.2.3"},
		{version: "version.1.2.3", prefix: strptr("version."), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "version.1.2.3"},
		{version: "rel-1.2.3", prefix: strptr("rel-"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "rel-1.2.3"},
		{version: "rel.1.2.3", prefix: strptr("rel."), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "rel.1.2.3"},
		{version: "release-1.2.3", prefix: strptr("release-"), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "release-1.2.3"},
		{version: "release.1.2.3", prefix: strptr("release."), sanitizedOutcome: "1.2.3", sanitizedPrefixOutcome: "1.2.3", sanitizedNumberOutcome: "release.1.2.3"},
		// leading zeroes
		{version: "01.02.03-alpha", prefix: nil, sanitizedOutcome: "1.2.3-alpha", sanitizedPrefixOutcome: "01.02.03-alpha", sanitizedNumberOutcome: "1.2.3-alpha"},
		{version: "01.02.03-alpha.0", prefix: nil, sanitizedOutcome: "1.2.3-alpha.0", sanitizedPrefixOutcome: "01.02.03-alpha.0", sanitizedNumberOutcome: "1.2.3-alpha.0"},
		{version: "01.02.03-alpha+beta", prefix: nil, sanitizedOutcome: "1.2.3-alpha+beta", sanitizedPrefixOutcome: "01.02.03-alpha+beta", sanitizedNumberOutcome: "1.2.3-alpha+beta"},
		{version: "01.02.03-alpha+beta.0", prefix: nil, sanitizedOutcome: "1.2.3-alpha+beta.0", sanitizedPrefixOutcome: "01.02.03-alpha+beta.0", sanitizedNumberOutcome: "1.2.3-alpha+beta.0"},
		{version: "01.02.03+beta", prefix: nil, sanitizedOutcome: "1.2.3+beta", sanitizedPrefixOutcome: "01.02.03+beta", sanitizedNumberOutcome: "1.2.3+beta"},
		{version: "01.02.03+beta.0", prefix: nil, sanitizedOutcome: "1.2.3+beta.0", sanitizedPrefixOutcome: "01.02.03+beta.0", sanitizedNumberOutcome: "1.2.3+beta.0"},
		{version: "01.02.03-alpha.0+beta.0", prefix: nil, sanitizedOutcome: "1.2.3-alpha.0+beta.0", sanitizedPrefixOutcome: "01.02.03-alpha.0+beta.0", sanitizedNumberOutcome: "1.2.3-alpha.0+beta.0"},
		{version: "01.02.03-alpha.01+beta.02", prefix: nil, sanitizedOutcome: "1.2.3-alpha.1+beta.02", sanitizedPrefixOutcome: "01.02.03-alpha.01+beta.02", sanitizedNumberOutcome: "1.2.3-alpha.1+beta.02"},
		{version: "000001.000002.000003-alpha.00345+beta-00678", prefix: nil, sanitizedOutcome: "1.2.3-alpha.345+beta-00678", sanitizedPrefixOutcome: "000001.000002.000003-alpha.00345+beta-00678", sanitizedNumberOutcome: "1.2.3-alpha.345+beta-00678"},
		// These are taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
		// they are actually illegal numbers, bu they can be sanitized to legal ones
		{version: "1.2.3-0123", prefix: nil, sanitizedOutcome: "1.2.3-123", sanitizedPrefixOutcome: "1.2.3-0123", sanitizedNumberOutcome: "1.2.3-123"},
		{version: "1.2.3-0123.0123", prefix: nil, sanitizedOutcome: "1.2.3-123.123", sanitizedPrefixOutcome: "1.2.3-0123.0123", sanitizedNumberOutcome: "1.2.3-123.123"},
		{version: "-1.0.3-gamma+b7718", prefix: strptr("-"), sanitizedOutcome: "1.0.3-gamma+b7718", sanitizedPrefixOutcome: "1.0.3-gamma+b7718", sanitizedNumberOutcome: "-1.0.3-gamma+b7718"},
	}

	/*
		A fixture with valid structured data to test semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the sanitizeable original version
		- prefix: if the original version has a prefix, it's the expected outcome of the invocation of the getPrefix() method, otherwise nil means that the version has no prefix and getPrefix() is expected to return nil
		- sanitizedOutcome: the expected outcome of the entire sanitization over the original string
		- sanitizedPrefixOutcome: the expected outcome of sanitizePrefix() over the original string
		- sanitizedNumberOutcome: the expected outcome of sanitizeNumbers() over the original string
	*/
	wellKnownSanitizableVersions = append(wellKnownSanitizableCoreVersions, wellKnownSanitizableNonCoreVersions...)

	/*
		A fixture with structured data to test bumping of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the starting version
		- bumpId: the identifier to bump
		- expectedOutcomeFromBump: the outcome expected after invoking bump(bumpId)
		- expectedOutcomeFromBumpPrerelease: the outcome expected after invoking bumpPrerelease(bumpId)
	*/
	wellKnownValidBumpAttributes = []struct {
		version                           string
		bumpId                            string
		expectedOutcomeFromBump           string
		expectedOutcomeFromBumpPrerelease string
	}{
		{version: "1.2.3", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1"},
		{version: "1.2.3", bumpId: "beta", expectedOutcomeFromBump: "1.2.3-beta.1", expectedOutcomeFromBumpPrerelease: "1.2.3-beta.1"},
		{version: "1.2.3", bumpId: "gamma", expectedOutcomeFromBump: "1.2.3-gamma.1", expectedOutcomeFromBumpPrerelease: "1.2.3-gamma.1"},
		{version: "1.2.3", bumpId: "delta", expectedOutcomeFromBump: "1.2.3-delta.1", expectedOutcomeFromBumpPrerelease: "1.2.3-delta.1"},
		{version: "1.2.3", bumpId: "rc", expectedOutcomeFromBump: "1.2.3-rc.1", expectedOutcomeFromBumpPrerelease: "1.2.3-rc.1"},

		//the build part must be unaffected by bumps
		{version: "1.2.3+alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1+alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1+alpha"},
		{version: "1.2.3+alpha.1", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1+alpha.1", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1+alpha.1"},
		{version: "1.2.3+beta", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1+beta", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1+beta"},

		// test when the identifier appears multiple times
		{version: "1.2.3-alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1"},
		{version: "1.2.3-alpha.0", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1"},
		{version: "1.2.3-alpha.1", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.2", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.2"},
		{version: "1.2.3-alpha.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1.alpha"},
		{version: "1.2.3-alpha.0.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1.alpha.1", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1.alpha.1"},
		{version: "1.2.3-alpha+alpha.beta.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1+alpha.beta.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1+alpha.beta.alpha"},
		{version: "1.2.3-alpha.0+alpha.beta.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1+alpha.beta.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1+alpha.beta.alpha"},
		{version: "1.2.3-alpha.1+alpha.beta.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.2+alpha.beta.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.2+alpha.beta.alpha"},
		{version: "1.2.3-alpha.alpha+alpha.beta.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1.alpha+alpha.beta.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1.alpha+alpha.beta.alpha"},
		{version: "1.2.3-alpha.0.alpha+alpha.beta.alpha", bumpId: "alpha", expectedOutcomeFromBump: "1.2.3-alpha.1.alpha.1+alpha.beta.alpha", expectedOutcomeFromBumpPrerelease: "1.2.3-alpha.1.alpha.1+alpha.beta.alpha"},

		// these are edge cases, in which a core component is bumped by its name. The core() method is expected to bump that specific core component
		// while the bumpPrerelease() method is expected to bump the prerelease component, leaving the core identifiers untouched
		{version: "1.2.3", bumpId: "major", expectedOutcomeFromBump: "2.0.0", expectedOutcomeFromBumpPrerelease: "1.2.3-major.1"},
		{version: "1.2.3", bumpId: "minor", expectedOutcomeFromBump: "1.3.0", expectedOutcomeFromBumpPrerelease: "1.2.3-minor.1"},
		{version: "1.2.3", bumpId: "patch", expectedOutcomeFromBump: "1.2.4", expectedOutcomeFromBumpPrerelease: "1.2.3-patch.1"},
		{version: "1.2.3-major", bumpId: "major", expectedOutcomeFromBump: "2.0.0-major", expectedOutcomeFromBumpPrerelease: "1.2.3-major.1"},
		{version: "1.2.3-minor", bumpId: "minor", expectedOutcomeFromBump: "1.3.0-minor", expectedOutcomeFromBumpPrerelease: "1.2.3-minor.1"},
		{version: "1.2.3-patch", bumpId: "patch", expectedOutcomeFromBump: "1.2.4-patch", expectedOutcomeFromBumpPrerelease: "1.2.3-patch.1"},
		{version: "1.2.3-major.3", bumpId: "major", expectedOutcomeFromBump: "2.0.0-major.3", expectedOutcomeFromBumpPrerelease: "1.2.3-major.4"},
		{version: "1.2.3-minor.3", bumpId: "minor", expectedOutcomeFromBump: "1.3.0-minor.3", expectedOutcomeFromBumpPrerelease: "1.2.3-minor.4"},
		{version: "1.2.3-patch.3", bumpId: "patch", expectedOutcomeFromBump: "1.2.4-patch.3", expectedOutcomeFromBumpPrerelease: "1.2.3-patch.4"},
	}

	/*
		A fixture with structured data to test bumping of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- bumpId: the identifier to bump
	*/
	wellKnownInvalidBumpAttributes = []struct {
		version string
		bumpId  string
	}{
		{version: "1.2.3", bumpId: "."},
		{version: "1.2.3", bumpId: "alpha."},
		{version: "1.2.3", bumpId: "alpha.beta"},
		{version: "1.2.3", bumpId: "0"},
		{version: "1.2.3", bumpId: "1"},
		{version: "1.2.3", bumpId: "alpha.0"},
		{version: "1.2.3", bumpId: "alpha.1"},
		{version: "1.2.3", bumpId: "!"},
		{version: "1.2.3", bumpId: "+"},
	}

	/*
		A fixture with structured data to test setting a prerelease attribute of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the starting version
		- attributeName: the name identifier to set
		- attributeValue: the value identifier to set
		- expectedOutcomeFromSetPrereleaseAttributeWithoutValue: the outcome expected after invoking setPrereleaseAttribute(attributeName)
		- expectedOutcomeFromSetPrereleaseAttributeWithValue: the outcome expected after invoking setPrereleaseAttribute(attributeName, attributeValue)
	*/
	wellKnownValidPrereleaseAttributes = []struct {
		version                                               string
		attributeName                                         string
		attributeValue                                        int
		expectedOutcomeFromSetPrereleaseAttributeWithoutValue string
		expectedOutcomeFromSetPrereleaseAttributeWithValue    string
	}{
		{version: "1.2.3", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123"},

		//if the attribute is already present with that value the return unchanged
		{version: "1.2.3-build", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123"},
		{version: "1.2.3-build.123", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build.123", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123"},

		//the build part must be unaffected by setPrereleaseAttribute, only the prerelease part
		{version: "1.2.3+build", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build"},
		{version: "1.2.3+build.1", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.1", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.1"},
		{version: "1.2.3+build.123", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.123", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.123"},
		{version: "1.2.3+build.timestamp", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.timestamp", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.timestamp"},
		{version: "1.2.3+build.1.timestamp", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.1.timestamp", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.1.timestamp"},
		{version: "1.2.3+build.123.timestamp", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.123.timestamp", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.123.timestamp"},
		{version: "1.2.3+build.timestamp.20200101", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.timestamp.20200101", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.timestamp.20200101"},
		{version: "1.2.3+build.1.timestamp.20200101", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.1.timestamp.20200101", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.1.timestamp.20200101"},
		{version: "1.2.3+build.123.timestamp.20200101", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+build.123.timestamp.20200101", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.123.timestamp.20200101"},

		//other build attributes must be unaffected by setBuildAttribute, unless they come after the matched identifier (in which case they are overwritten)
		{version: "1.2.3+timestamp", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+timestamp", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+timestamp"},
		{version: "1.2.3+timestamp.1", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+timestamp.1", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+timestamp.1"},
		{version: "1.2.3+timestamp.123", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build+timestamp.123", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+timestamp.123"},

		{version: "1.2.3-build.567+build.timestamp", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build.567+build.timestamp", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.timestamp"},             // overwrites 567 if the value is not nil
		{version: "1.2.3-build.567+build.timestamp.1", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build.567+build.timestamp.1", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.timestamp.1"},       // overwrites 567 if the value is not nil
		{version: "1.2.3-build.567+build.timestamp.123", attributeName: "build", attributeValue: 123, expectedOutcomeFromSetPrereleaseAttributeWithoutValue: "1.2.3-build.567+build.timestamp.123", expectedOutcomeFromSetPrereleaseAttributeWithValue: "1.2.3-build.123+build.timestamp.123"}, // overwrites 567 if the value is not nil
	}

	/*
		A fixture with structured data to test setting a prerelease attribute of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- attributeName: the name identifier to set
		- attributeValue: the value identifier to set
		- expectedErrorWithoutValue: tells if an error is expected when invoking setPrereleaseAttribute(attributeName), if any. If false no error is expected in this case
		- expectedErrorWithValue: tells if an error is expected when invoking setPrereleaseAttribute(attributeName), if any. If false no error is expected in this case
	*/
	wellKnownInvalidPrereleaseAttributes = []struct {
		version                   string
		attributeName             string
		attributeValue            int
		expectedErrorWithoutValue bool
		expectedErrorWithValue    bool
	}{
		{version: "1.2.3", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: -123, expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: ".", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.beta", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.0", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.1", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},

		{version: "1.2.3", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "!", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "+", attributeValue: 123, expectedErrorWithoutValue: true, expectedErrorWithValue: true},
	}

	/*
		A fixture with structured data to test setting a build attribute of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the starting version
		- attributeName: the name identifier to set
		- attributeValue: the value identifier to set
		- expectedOutcomeFromSetBuildAttributeWithoutValue: the outcome expected after invoking setBuildAttribute(attributeName)
		- expectedOutcomeFromSetBuildAttributeWithValue: the outcome expected after invoking setBuildAttribute(attributeName, attributeValue)
	*/
	wellKnownValidBuildAttributes = []struct {
		version                                          string
		attributeName                                    string
		attributeValue                                   string
		expectedOutcomeFromSetBuildAttributeWithoutValue string
		expectedOutcomeFromSetBuildAttributeWithValue    string
	}{
		{version: "1.2.3", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123"},

		//if the attribute is already present with that value the return unchanged
		{version: "1.2.3+build", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123"},
		{version: "1.2.3+build.123", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build.123", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123"},

		//the pre-release part must be unaffected by setBuildAttribute, only the build part
		{version: "1.2.3-build", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build+build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build+build.123"},
		{version: "1.2.3-build.1", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.1+build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.1+build.123"},
		{version: "1.2.3-build.123", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.123+build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.123+build.123"},
		{version: "1.2.3-build+timestamp", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build+timestamp.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build+timestamp.build.123"},
		{version: "1.2.3-build.1+timestamp", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.1+timestamp.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.1+timestamp.build.123"},
		{version: "1.2.3-build.123+timestamp", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.123+timestamp.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.123+timestamp.build.123"},
		{version: "1.2.3-build+timestamp.20200101", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build+timestamp.20200101.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build+timestamp.20200101.build.123"},
		{version: "1.2.3-build.1+timestamp.20200101", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.1+timestamp.20200101.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.1+timestamp.20200101.build.123"},
		{version: "1.2.3-build.123+timestamp.20200101", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3-build.123+timestamp.20200101.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3-build.123+timestamp.20200101.build.123"},

		//other build attributes must be unaffected by setBuildAttribute, unless they come after the matched identifier (in which case they are overwritten)
		{version: "1.2.3+timestamp", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+timestamp.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+timestamp.build.123"},
		{version: "1.2.3+timestamp.1", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+timestamp.1.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+timestamp.1.build.123"},
		{version: "1.2.3+timestamp.123", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+timestamp.123.build", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+timestamp.123.build.123"},
		{version: "1.2.3+build.timestamp", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build.timestamp", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123"},             // overwrites timestamp if the value is not nil
		{version: "1.2.3+build.timestamp.1", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build.timestamp.1", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123.1"},       // overwrites timestamp if the value is not nil
		{version: "1.2.3+build.timestamp.123", attributeName: "build", attributeValue: "123", expectedOutcomeFromSetBuildAttributeWithoutValue: "1.2.3+build.timestamp.123", expectedOutcomeFromSetBuildAttributeWithValue: "1.2.3+build.123.123"}, // overwrites timestamp if the value is not nil
	}

	/*
		A fixture with structured data to test setting a build attribute of semantic versions.
		Each returned argument has the fields:
		- version: the entire string representation of the version
		- attributeName: the name identifier to set
		- attributeValue: the value identifier to set
		- expectedErrorWithoutValue: tells if an error is expected when invoking setBuildAttribute(attributeName), if any. If false no error is expected in this case
		- expectedErrorWithValue: tells if an error is expected when invoking setBuildAttribute(attributeName), if any. If false no error is expected in this case
	*/
	wellKnownInvalidBuildAttributes = []struct {
		version                   string
		attributeName             string
		attributeValue            string
		expectedErrorWithoutValue bool
		expectedErrorWithValue    bool
	}{
		{version: "1.2.3", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "123.1v", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: ".", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: ".", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "123.", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.beta", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "123.456", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.0", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "123.0", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build.1", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "123.1", expectedErrorWithoutValue: false, expectedErrorWithValue: true},

		{version: "1.2.3", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+beta", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-alpha+beta", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3+build", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "!", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "!", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "+", attributeValue: "123", expectedErrorWithoutValue: true, expectedErrorWithValue: true},
		{version: "1.2.3-build+build", attributeName: "build", attributeValue: "+", expectedErrorWithoutValue: false, expectedErrorWithValue: true},
	}

	/*
		An ordered list of valid version strings (according to SemVer). This list can be used to check ordering among semantic versions.
	*/
	wellKnownOrderedSemanticVersionsArray = []string{
		"0.0.0-alpha+build",
		"0.0.0-alpha+build-123",
		"0.0.0-alpha+build-a",
		"0.0.0-alpha+build-abc",
		"0.0.0-alpha",
		"0.0.0-alpha.0",
		"0.0.0-alpha.1",
		"0.0.0-alpha.1.1",
		"0.0.0-alpha.1.2",
		"0.0.0-alpha.1.2.1",
		"0.0.0-alpha.1.21",
		"0.0.0-alpha.A",
		"0.0.0-alpha.Aa",
		"0.0.0-alpha.a",
		"0.0.0-alpha.aa",
		"0.0.0+build.987794",
		"0.0.0+build.IUENLS",
		"0.0.0+build",
		"0.0.0",
		"0.0.1-alpha+build",
		"0.0.1-alpha+build-123",
		"0.0.1-alpha+build-a",
		"0.0.1-alpha+build-abc",
		"0.0.1-alpha",
		"0.0.1-alpha.0",
		"0.0.1-alpha.1",
		"0.0.1-alpha.1.1",
		"0.0.1-alpha.1.2",
		"0.0.1-alpha.1.2.1",
		"0.0.1-alpha.1.21",
		"0.0.1-alpha.9",
		"0.0.1-alpha.10",
		"0.0.1-alpha.A",
		"0.0.1-alpha.Aa",
		"0.0.1-alpha.a",
		"0.0.1-alpha.a10",
		"0.0.1-alpha.a9",
		"0.0.1-alpha.aa",
		"0.0.1+build.10",
		"0.0.1+build.9",
		"0.0.1+build.987794",
		"0.0.1+build.IUENLS",
		"0.0.1+build",
		"0.0.1",
		"0.2.1-alpha+build",
		"0.2.1-alpha+build-123",
		"0.2.1-alpha+build-a",
		"0.2.1-alpha+build-abc",
		"0.2.1-alpha",
		"0.2.1-alpha.0",
		"0.3.1-alpha.1",
		"0.3.1-alpha.1.1",
		"0.3.1-alpha.1.2",
		"0.3.1-alpha.1.2.1",
		"0.3.1-alpha.1.21",
		"0.3.1-alpha.9",
		"0.3.1-alpha.10",
		"0.3.1-alpha.A",
		"0.3.1-alpha.Aa",
		"0.4.1-alpha.a",
		"0.4.1-alpha.a10",
		"0.4.1-alpha.a9",
		"0.4.1-alpha.aa",
		"0.4.1+build.10",
		"0.4.1+build.9",
		"0.4.1+build.987794",
		"0.4.1+build.IUENLS",
		"0.4.1+build",
		"0.4.1",
		"1.2.1-alpha+build",
		"1.2.1-alpha+build-123",
		"1.2.1-alpha+build-a",
		"1.2.1-alpha+build-abc",
		"2.2.1-alpha",
		"2.2.1-alpha.0",
		"2.3.1-alpha.1",
		"3.3.1-alpha.1.1",
		"3.3.1-alpha.1.2",
		"3.3.1-alpha.1.2.1",
		"4.3.1-alpha.1.21",
		"4.3.1-alpha.A",
		"5.3.1-alpha.Aa",
		"6.4.1-alpha.a",
		"7.4.1-alpha.aa",
		"8.4.1+build.987794",
		"9.4.1+build.IUENLS",
		"10.4.1+build",
		"10.4.1",
		"10.4.2",
		"11.0.0",
		"12.4.1",
		"90.4.1",
		"9000000.0.0",
		"9000000.4.1",
		strconv.Itoa(math.MaxInt-1) + "." + strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt),
		strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt-1) + "." + strconv.Itoa(math.MaxInt),
		strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt-1),
		strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt) + "." + strconv.Itoa(math.MaxInt),
	}
)

// Returns a pointer to the string passed as parameter.
func strptr(s string) *string {
	return &s
}

// Returns a pointer to the int passed as parameter.
func intptr(i int) *int {
	return &i
}

/*
*
Convenience method that builds a core, prerelease or a build string by concatenating the single identifiers, separated
by a dot.
*/
func interfaceIdentifiersToString(identifiers []interface{}) string {
	var sb strings.Builder
	for i := 0; i < len(identifiers); i++ {
		if i > 0 {
			sb.WriteString(".")
		}
		sb.WriteString(fmt.Sprint(identifiers[i]))
	}
	return sb.String()
}

/*
*
Convenience method that builds a core, prerelease or a build string by concatenating the single identifiers, separated
by a dot.
*/
func stringIdentifiersToString(identifiers []string) string {
	return strings.Join(identifiers, ".")
}

/*
Generates a random string containing only alphabetic characters.
The additional seed is used to disambiguate between multiple invocations.
*/
func randomAlphabeticString(length int, seed int64) string {
	charset := "abcdefghijklmnopqrstuvwxyz"

	res := make([]byte, length)
	rand.Seed(time.Now().UnixNano() * seed)

	for i := 0; i < length; i++ {
		// Getting random character
		res[i] = charset[rand.Intn(len(charset))]
	}
	return string(res)
}

func TestSemanticVersionConstructorNewSemanticVersionWithErrorUsingNegativeNumbers(t *testing.T) {
	_, err := NewSemanticVersionWith(0, 0, -1)
	assert.Error(t, err)
	_, err = NewSemanticVersionWith(0, -1, 0)
	assert.Error(t, err)
	_, err = NewSemanticVersionWith(-1, 0, 0)
	assert.Error(t, err)
	_, err = NewSemanticVersionWithAllIdentifiers(0, 0, -1, nil, nil)
	assert.Error(t, err)
	_, err = NewSemanticVersionWithAllIdentifiers(0, -1, 0, nil, nil)
	assert.Error(t, err)
	_, err = NewSemanticVersionWithAllIdentifiers(-1, 0, 0, nil, nil)
	assert.Error(t, err)
}

func TestSemanticVersionConstructorNewSemanticVersionWithNoErrorUsingNilIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, nil, nil)

			coreString := stringIdentifiersToString([]string{strconv.Itoa(*vv.major), strconv.Itoa(*vv.minor), strconv.Itoa(*vv.patch)})

			assert.NoError(t, err)
			assert.Equal(t, coreString, sv.String())
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
		})
	}
}

func TestSemanticVersionConstructorNewSemanticVersionWithNoErrorUsingEmptyIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, [](interface{}){}, []string{})

			coreString := stringIdentifiersToString([]string{strconv.Itoa(*vv.major), strconv.Itoa(*vv.minor), strconv.Itoa(*vv.patch)})

			assert.NoError(t, err)
			assert.Equal(t, coreString, sv.String())
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
		})
	}
}

func TestSemanticVersionConstructorNewSemanticVersionWith(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := NewSemanticVersionWith(*vv.major, *vv.minor, *vv.patch)

			coreString := stringIdentifiersToString([]string{strconv.Itoa(*vv.major), strconv.Itoa(*vv.minor), strconv.Itoa(*vv.patch)})

			assert.NoError(t, err)
			assert.Equal(t, coreString, sv.String())
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
		})
	}
}

func TestSemanticVersionConstructorNewSemanticVersionWithAllIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			var sv SemanticVersion
			var err error
			if vv.prerelease == nil && vv.build == nil {
				sv, err = NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, nil, nil)
			} else if vv.prerelease == nil && vv.build != nil {
				sv, err = NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, nil, *vv.build)
			} else if vv.prerelease != nil && vv.build == nil {
				sv, err = NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, *vv.prerelease, nil)
			} else if vv.prerelease != nil && vv.build != nil {
				sv, err = NewSemanticVersionWithAllIdentifiers(*vv.major, *vv.minor, *vv.patch, *vv.prerelease, *vv.build)
			} else {
				// the case is not covered by this test, consider tweaking the conditions above
				t.Fatalf("this test needs to extend the branches")
			}
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
		})
	}
}

func TestSemanticVersionIsLegalWithEmptyString(t *testing.T) {
	assert.False(t, IsLegalSemanticVersion(""))
}

func TestSemanticVersionIsLegalInvalidVersion(t *testing.T) {
	for _, vv := range wellKnownInvalidVersions {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, IsLegalSemanticVersion(vv.version))
		})
	}
}

func TestSemanticVersionIsLegalValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			assert.True(t, IsLegalSemanticVersion(*vv.version))
		})
	}
}

func TestSemanticVersionIsLegalSanitizedString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			// test invoking isLegal with prefix tolerance
			assert.True(t, IsLegalSemanticVersionWithLenience(*vv.version, true))
		})
	}
}

func TestSemanticVersionIsLegalSanitizableString(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			// the method must fail without toleration and succeed when using toleration
			assert.False(t, IsLegalSemanticVersion(vv.version))
			assert.False(t, IsLegalSemanticVersionWithLenience(vv.version, false))
			assert.True(t, IsLegalSemanticVersionWithLenience(vv.version, true))
		})
	}
}

func TestSemanticVersionValueOfValidStringErrorUsingValueOfWithEmptyString(t *testing.T) {
	_, err := ValueOfSemanticVersion("")
	assert.Error(t, err)
}

func TestSemanticVersionValueOfValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			// test that invoking valueOf with sanitization on a valid string returns the same string
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestSemanticVersionValueOfSanitizedString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			// test that invoking valueOf with sanitization on a valid string returns the same string
			sv, err := ValueOfSemanticVersionWithSanitization(*vv.version, true)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestSemanticVersionValueOfSanitizableString(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			// the method must fail without sanitization and succeed when using sanitization
			_, err := ValueOfSemanticVersion(vv.version)
			assert.Error(t, err)
			_, err = ValueOfSemanticVersionWithSanitization(vv.version, false)
			assert.Error(t, err)
			sv, err := ValueOfSemanticVersionWithSanitization(vv.version, true)
			assert.NoError(t, err)
			assert.NotEqual(t, vv.version, sv.String())
			ss, _ := SanitizeSemanticVersion(vv.version)
			assert.Equal(t, ss, sv.String())
		})
	}
}

func TestSemanticVersionGetMajor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv.GetMajor())
		})
	}
}

func TestSemanticVersionSetMajor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 0, sv.GetMajor())

			sv, err = sv.SetMajor(*vv.major)
			assert.Equal(t, *vv.major, sv.GetMajor())

			// ancillary tests, just to make sure the operation didn't affect other fields
			assert.Equal(t, 1, sv.GetMinor())
			assert.Equal(t, 0, sv.GetPatch())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())
		})
	}
}

func TestSemanticVersionGetMinor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.minor, sv.GetMinor())
		})
	}
}

func TestSemanticVersionSetMinor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 1, sv.GetMinor())

			sv, err = sv.SetMinor(*vv.minor)
			assert.Equal(t, *vv.minor, sv.GetMinor())

			// ancillary tests, just to make sure the operation didn't affect other fields
			assert.Equal(t, 0, sv.GetMajor())
			assert.Equal(t, 0, sv.GetPatch())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())
		})
	}
}

func TestSemanticVersionGetPatch(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.patch, sv.GetPatch())
		})
	}
}

func TestSemanticVersionSetPatch(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 0, sv.GetPatch())

			sv, err = sv.SetPatch(*vv.patch)
			assert.Equal(t, *vv.patch, sv.GetPatch())

			// ancillary tests, just to make sure the operation didn't affect other fields
			assert.Equal(t, 0, sv.GetMajor())
			assert.Equal(t, 1, sv.GetMinor())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())
		})
	}
}

func TestSemanticVersionGetCore(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			coreString := stringIdentifiersToString([]string{strconv.Itoa(*vv.major), strconv.Itoa(*vv.minor), strconv.Itoa(*vv.patch)})
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, coreString, sv.GetCore())
		})
	}
}

func TestSemanticVersionGetCoreIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, 3, len(sv.GetCoreIdentifiers()))
			assert.Equal(t, *vv.major, sv.GetCoreIdentifiers()[0])
			assert.Equal(t, *vv.minor, sv.GetCoreIdentifiers()[1])
			assert.Equal(t, *vv.patch, sv.GetCoreIdentifiers()[2])
		})
	}
}

func TestSemanticVersionSetCore(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 0, sv.GetMajor())
			assert.Equal(t, 1, sv.GetMinor())
			assert.Equal(t, 0, sv.GetPatch())

			sv, err = sv.SetCore(*vv.major, *vv.minor, *vv.patch)
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())
		})
	}
}

func TestSemanticVersionGetPrerelease(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			if vv.prerelease == nil {
				assert.Nil(t, sv.GetPrerelease())
			} else {
				preString := interfaceIdentifiersToString(*vv.prerelease)
				assert.Equal(t, preString, *sv.GetPrerelease())
			}
		})
	}
}

func TestSemanticVersionGetPrereleaseIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			if vv.prerelease != nil {
				sv, err := ValueOfSemanticVersion(*vv.version)
				assert.NoError(t, err)
				assert.Equal(t, len(*vv.prerelease), len(*sv.GetPrereleaseIdentifiers()))
				for i, s := range *vv.prerelease {
					assert.Equal(t, s, fmt.Sprint((*sv.GetPrereleaseIdentifiers())[i]))
				}
			}
		})
	}
}

func TestSemanticVersionSetPrerelease(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 0, sv.GetMajor())
			assert.Equal(t, 1, sv.GetMinor())
			assert.Equal(t, 0, sv.GetPatch())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())

			sv, err = sv.SetCore(*vv.major, *vv.minor, *vv.patch)
			if vv.build != nil {
				sv, err = sv.SetBuild(*vv.build...)
			}
			if vv.prerelease != nil {
				sv, err = sv.SetPrerelease(*vv.prerelease...)
			}
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestSemanticVersionGetBuild(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			if vv.build == nil {
				assert.Nil(t, sv.GetBuild())
			} else {
				buildString := stringIdentifiersToString(*vv.build)
				assert.Equal(t, buildString, *sv.GetBuild())
			}
		})
	}
}

func TestSemanticVersionGetBuildIdentifiers(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			if vv.build != nil {
				sv, err := ValueOfSemanticVersion(*vv.version)
				assert.NoError(t, err)
				assert.Equal(t, len(*vv.build), len(*sv.GetBuildIdentifiers()))
				for i, s := range *vv.build {
					assert.Equal(t, s, fmt.Sprint((*sv.GetBuildIdentifiers())[i]))
				}
			}
		})
	}
}

func TestSemanticVersionSetBuild(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
			assert.NoError(t, err)
			assert.Equal(t, SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION, sv.String())
			assert.Equal(t, 0, sv.GetMajor())
			assert.Equal(t, 1, sv.GetMinor())
			assert.Equal(t, 0, sv.GetPatch())
			assert.Nil(t, sv.GetPrerelease())
			assert.Nil(t, sv.GetBuild())

			sv, err = sv.SetCore(*vv.major, *vv.minor, *vv.patch)
			if vv.build != nil {
				sv, err = sv.SetBuild(*vv.build...)
			}
			if vv.prerelease != nil {
				sv, err = sv.SetPrerelease(*vv.prerelease...)
			}
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv.GetMajor())
			assert.Equal(t, *vv.minor, sv.GetMinor())
			assert.Equal(t, *vv.patch, sv.GetPatch())
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestSemanticVersionString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, sv.String())
		})
	}
}

func TestSemanticVersionGetScheme(t *testing.T) {
	sv, err := ValueOfSemanticVersion("1.1.1")
	assert.NoError(t, err)
	assert.Equal(t, SEMVER, sv.GetScheme())
}

func TestSemanticVersionSanitizeWithEmptyString(t *testing.T) {
	_, err := SanitizeSemanticVersion("")
	assert.Error(t, err)
}

func TestSemanticVersionSanitizePrefixWithEmptyString(t *testing.T) {
	_, err := SanitizeSemanticVersionPrefix("")
	assert.Error(t, err)
}

func TestSemanticVersionSanitizeNumbersWithEmptyString(t *testing.T) {
	_, err := SanitizeSemanticVersionNumbers("")
	assert.Error(t, err)
}

func TestSemanticVersionSanitizeWithValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, ss)
		})
	}
}

func TestSemanticVersionSanitizePrefixWithValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersionPrefix(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, ss)
		})
	}
}

func TestSemanticVersionSanitizeNumbersWithValidString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersionNumbers(*vv.version)
			assert.NoError(t, err)
			assert.Equal(t, *vv.version, ss)
		})
	}
}

func TestSemanticVersionSanitize(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.Equal(t, vv.sanitizedOutcome, ss)
		})
	}
}

func TestSemanticVersionSanitizePrefix(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersionPrefix(vv.version)
			assert.NoError(t, err)
			assert.Equal(t, vv.sanitizedPrefixOutcome, ss)
		})
	}
}

func TestSemanticVersionSanitizeNumbers(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			ss, err := SanitizeSemanticVersionNumbers(vv.version)
			assert.NoError(t, err)
			assert.Equal(t, vv.sanitizedNumberOutcome, ss)
		})
	}
}

func TestSemanticVersionGetSemanticVersionPrefix(t *testing.T) {
	for _, vv := range wellKnownSanitizableVersions {
		t.Run(vv.version, func(t *testing.T) {
			ss, err := GetSemanticVersionPrefix(vv.version)
			assert.NoError(t, err)
			if vv.prefix == nil {
				assert.Nil(t, ss)
			} else {
				assert.Equal(t, *vv.prefix, *ss)
			}
		})
	}
}

func TestSemanticVersionEqualsToNil(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.False(t, sv.Equals(nil))
		})
	}
}

func TestSemanticVersionEqualsToEmptyString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.False(t, "" == sv.String())
		})
	}
}

func TestSemanticVersionEqualsToSameInstance(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.True(t, sv.Equals(sv))
		})
	}
}

func TestSemanticVersionEqualsToSameStringValue(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			assert.True(t, sv1.Equals(sv2))
			assert.True(t, sv2.Equals(sv1))
		})
	}
}

func TestSemanticVersionCompareTo(t *testing.T) {
	for i, v1 := range wellKnownOrderedSemanticVersionsArray {
		t.Run(v1, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(v1)
			assert.NoError(t, err)

			for j, v2 := range wellKnownOrderedSemanticVersionsArray[0:i] {
				// check for equality with the previous items
				sv2, err := ValueOfSemanticVersion(v2)
				assert.NoError(t, err)
				assert.True(t, sv1.CompareTo(sv2) > 0, "[%d,%d] %v.CompareTo(%v)=%d (must be >0)", i, j, sv1.String(), sv2.String(), sv1.CompareTo(sv2))
			}

			// check for equality with the same item
			sv2, err := ValueOfSemanticVersion(v1)
			assert.NoError(t, err)
			assert.True(t, sv1.CompareTo(sv2) == 0, "[%d,%d] %v.CompareTo(%v)=%d (must be 0)", i, i, sv1.String(), sv2.String(), sv1.CompareTo(sv2))

			if i < len(wellKnownOrderedSemanticVersionsArray) {
				for j, v2 := range wellKnownOrderedSemanticVersionsArray[i+1:] {
					// check for equality with the next items
					sv2, err := ValueOfSemanticVersion(v2)
					assert.NoError(t, err)
					assert.True(t, sv1.CompareTo(sv2) < 0, "[%d,%d] %v.CompareTo(%v)=%d (must be <0)", i, j, sv1.String(), sv2.String(), sv1.CompareTo(sv2))
				}
			}
		})
	}
}

func TestSemanticVersionSort(t *testing.T) {
	// create a slice with all the well known semantic versions
	orderedList := make([]SemanticVersion, len(wellKnownOrderedSemanticVersionsArray))
	for i, vv := range wellKnownOrderedSemanticVersionsArray {
		sv1, err := ValueOfSemanticVersion(vv)
		assert.NoError(t, err)
		orderedList[i] = sv1
	}

	// shuffle a copy of the ordered list
	shuffledList := make([]SemanticVersion, len(orderedList))
	copy(shuffledList, orderedList)

	// make sure the two lists are the same before shuffling
	assert.True(t, reflect.DeepEqual(orderedList, shuffledList))

	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(shuffledList), func(i, j int) { shuffledList[i], shuffledList[j] = shuffledList[j], shuffledList[i] })

	// make sure the two lists have the same elements but in different order
	assert.Equal(t, len(orderedList), len(shuffledList))
	assert.False(t, reflect.DeepEqual(orderedList, shuffledList))

	// now sort the shuffled list
	sort.Sort(SemanticVersions(shuffledList))

	// make sure the two lists have the same elements in the same order again
	assert.Equal(t, len(orderedList), len(shuffledList))
	assert.True(t, reflect.DeepEqual(orderedList, shuffledList))
}

func TestSemanticVersionSortAsStrings(t *testing.T) {
	// shuffle a copy of the ordered list
	shuffledList := make([]string, len(wellKnownOrderedSemanticVersionsArray))
	copy(shuffledList, wellKnownOrderedSemanticVersionsArray)

	// make sure the two lists are the same before shuffling
	assert.True(t, reflect.DeepEqual(wellKnownOrderedSemanticVersionsArray, shuffledList))

	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(shuffledList), func(i, j int) { shuffledList[i], shuffledList[j] = shuffledList[j], shuffledList[i] })

	// make sure the two lists have the same elements but in different order
	assert.Equal(t, len(wellKnownOrderedSemanticVersionsArray), len(shuffledList))
	assert.False(t, reflect.DeepEqual(wellKnownOrderedSemanticVersionsArray, shuffledList))

	// now sort the shuffled list
	sort.Sort(SemanticVersionStrings(shuffledList))

	// make sure the two lists have the same elements in the same order again
	assert.Equal(t, len(wellKnownOrderedSemanticVersionsArray), len(shuffledList))
	assert.True(t, reflect.DeepEqual(wellKnownOrderedSemanticVersionsArray, shuffledList))
}

func TestSemanticVersionBumpWithEmptyString(t *testing.T) {
	sv, err := ValueOfSemanticVersion(SEMANTIC_VERSION_DEFAULT_INITIAL_VERSION)
	assert.NoError(t, err)
	sv, err = sv.Bump("")
	assert.Error(t, err)
}

func TestSemanticVersionBumpWithValidAttribute(t *testing.T) {
	for _, vv := range wellKnownValidBumpAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.Bump(vv.bumpId)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromBump, sv.String())
		})
	}
}

func TestSemanticVersionBumpWithInvalidAttribute(t *testing.T) {
	for _, vv := range wellKnownInvalidBumpAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.Bump(vv.bumpId)
			assert.Error(t, err)
		})
	}
}

func TestSemanticVersionBumpMajor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpMajor()
			assert.NoError(t, err)
			assert.Equal(t, *vv.major+1, sv2.GetMajor())
			assert.Equal(t, 0, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpMajorWithEnum(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpIdentifier(MAJOR)
			assert.NoError(t, err)
			assert.Equal(t, *vv.major+1, sv2.GetMajor())
			assert.Equal(t, 0, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpMajorWithString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.Bump("major")
			assert.NoError(t, err)
			assert.Equal(t, *vv.major+1, sv2.GetMajor())
			assert.Equal(t, 0, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpMinor(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpMinor()
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor+1, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpMinorWithEnum(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpIdentifier(MINOR)
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor+1, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpMinorWithString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.Bump("minor")
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor+1, sv2.GetMinor())
			assert.Equal(t, 0, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpPatch(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpPatch()
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor, sv2.GetMinor())
			assert.Equal(t, *vv.patch+1, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpPatchWithEnum(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.BumpIdentifier(PATCH)
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor, sv2.GetMinor())
			assert.Equal(t, *vv.patch+1, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpPatchWithString(t *testing.T) {
	for _, vv := range wellKnownValidVersions {
		t.Run(*vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(*vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.Bump("patch")
			assert.NoError(t, err)
			assert.Equal(t, *vv.major, sv2.GetMajor())
			assert.Equal(t, *vv.minor, sv2.GetMinor())
			assert.Equal(t, *vv.patch+1, sv2.GetPatch())
		})
	}
}

func TestSemanticVersionBumpPrereleaseWithValidAttribute(t *testing.T) {
	for _, vv := range wellKnownValidBumpAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.BumpPrerelease(vv.bumpId)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromBumpPrerelease, sv.String())
		})
	}
}

func TestSemanticVersionBumpPrereleaseWithInvalidAttribute(t *testing.T) {
	for _, vv := range wellKnownInvalidBumpAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.BumpPrerelease(vv.bumpId)
			assert.Error(t, err)
		})
	}
}

func TestSemanticVersionHasPrereleaseAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.True(t, sv.HasPrereleaseAttribute(vv.attributeName))
		})
	}
}

func TestSemanticVersionHasPrereleaseAttributeWithValidCharacter(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "X"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.False(t, sv.HasPrereleaseAttribute("X"))
		})
	}
}

func TestSemanticVersionHasPrereleaseAttributeWithValidMockIdentifier(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "mockidentifier"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.False(t, sv.HasPrereleaseAttribute("mockidentifier"))
		})
	}
}

func TestSemanticVersionHasPrereleaseAttributeWithEmptyString(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.False(t, sv.HasPrereleaseAttribute(""))
			assert.False(t, sv.HasPrereleaseAttribute("   "))
		})
	}
}

func TestSemanticVersionGetPrereleaseAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttributeWith(vv.attributeName, &vv.attributeValue)
			assert.NoError(t, err)
			assert.Equal(t, vv.attributeValue, *sv.GetPrereleaseAttributeValue(vv.attributeName))
		})
	}
}

func TestSemanticVersionGetPrereleaseAttributeWithValidCharacter(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "X"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetPrereleaseAttributeValue("X"))
		})
	}
}

func TestSemanticVersionGetPrereleaseAttributeWithValidMockIdentifier(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "mockidentifier"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetPrereleaseAttributeValue("mockidentifier"))
		})
	}
}

func TestSemanticVersionGetPrereleaseAttributeWithEmptyString(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetPrereleaseAttributeValue(""))
			assert.Nil(t, sv.GetPrereleaseAttributeValue("   "))
		})
	}
}

func TestSemanticVersionSetPrereleaseAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err := sv1.SetPrereleaseAttribute(vv.attributeName) // this overloaded version is a shorthand for the next one, but test both
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetPrereleaseAttributeWithoutValue, sv.String())
			sv, err = sv1.SetPrereleaseAttributeWith(vv.attributeName, nil)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetPrereleaseAttributeWithoutValue, sv.String())
		})
	}
}

func TestSemanticVersionSetPrereleaseAttributeWithValidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetPrereleaseAttributeWith(vv.attributeName, &vv.attributeValue)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetPrereleaseAttributeWithValue, sv.String())
		})
	}
}

func TestSemanticVersionSetPrereleaseAttributeWithInvalidStringName(t *testing.T) {
	for _, vv := range wellKnownInvalidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			_, err = sv.SetPrereleaseAttribute(vv.attributeName) // this overloaded version is a shorthand for the next one, but test both
			if vv.expectedErrorWithoutValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			_, err = sv.SetPrereleaseAttributeWith(vv.attributeName, nil)
			if vv.expectedErrorWithoutValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestSemanticVersionSetPrereleaseAttributeWithInvalidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownInvalidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			_, err = sv.SetPrereleaseAttributeWith(vv.attributeName, &vv.attributeValue)
			if vv.expectedErrorWithValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestSemanticVersionRemovePrereleaseAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 23)

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName)) // check that the random string is not there
			sv2, err := sv1.SetPrereleaseAttribute(randomName)          // now add the random string as Prerelease attribute
			assert.NoError(t, err)
			assert.True(t, strings.Contains(sv2.String(), randomName)) // check that the attribute is there

			sv3, err := sv2.RemovePrereleaseAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))

			sv4, err := sv2.RemovePrereleaseAttribute(&randomName, true) // also remove the value (which is not present)
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv4.String(), randomName))

			// now check that the two version, removing the value and not removing it, are exactly the same
			assert.True(t, sv3.Equals(sv4))
			assert.True(t, sv4.Equals(sv3))
			assert.Equal(t, sv3.String(), sv4.String())

			// now check that the original version and the last one are equal
			assert.True(t, sv1.Equals(sv3))
			assert.True(t, sv3.Equals(sv1))
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}

func TestSemanticVersionRemoveMissingPrereleaseAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 29)

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName)) // check that the random string is not there

			sv2, err := sv1.RemovePrereleaseAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv2.String(), randomName))

			sv3, err := sv1.RemovePrereleaseAttribute(&randomName, true) // also remove the value (which is not present)
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))

			// now check that the three versions are the same
			assert.True(t, sv1.Equals(sv2))
			assert.True(t, sv1.Equals(sv3))
			assert.Equal(t, sv1.String(), sv2.String())
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}

func TestSemanticVersionRemovePrereleaseAttributeWithValidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 37)
			randomValue := rand.Intn(math.MaxInt)
			if randomValue < 0 { // make sure it's positive
				randomValue = randomValue * -1
			}

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName))                // check that the random string is not there
			assert.False(t, strings.Contains(sv1.String(), strconv.Itoa(randomValue))) // check that the random string is not there
			sv2, err := sv1.SetPrereleaseAttributeWith(randomName, &randomValue)       // now add the random string as Prerelease attribute
			assert.NoError(t, err)
			assert.True(t, strings.Contains(sv2.String(), randomName))                // check that the attribute is there
			assert.True(t, strings.Contains(sv2.String(), strconv.Itoa(randomValue))) // check that the attribute is there

			sv3, err := sv2.RemovePrereleaseAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))
			assert.True(t, strings.Contains(sv3.String(), strconv.Itoa(randomValue))) // the value must be still there

			sv4, err := sv2.RemovePrereleaseAttribute(&randomName, true) // also remove the value
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv4.String(), randomName))
			assert.False(t, strings.Contains(sv4.String(), strconv.Itoa(randomValue))) // the value must be gone

			// now check that the two version, removing the value and not removing it, are different
			assert.False(t, sv3.Equals(sv4))
			assert.False(t, sv4.Equals(sv3))
			assert.NotEqual(t, sv3.String(), sv4.String())

			// now check that the original version and the last one are equal
			assert.True(t, sv1.Equals(sv4))
			assert.True(t, sv4.Equals(sv1))
			assert.Equal(t, sv1.String(), sv4.String())
		})
	}
}

func TestSemanticVersionRemovePrereleaseAttributeWithEmptyStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		emptyString := ""
		voidString := "  "
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.RemovePrereleaseAttribute(&emptyString, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv2.String())
			sv3, err := sv1.RemovePrereleaseAttribute(&emptyString, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv3.String())

			sv4, err := sv1.RemovePrereleaseAttribute(&voidString, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv4.String())
			sv5, err := sv1.RemovePrereleaseAttribute(&voidString, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv5.String())
		})
	}
}

func TestSemanticVersionRemovePrereleaseAttributeWithNilStringName(t *testing.T) {
	for _, vv := range wellKnownValidPrereleaseAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.RemovePrereleaseAttribute(nil, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv2.String())
			sv3, err := sv1.RemovePrereleaseAttribute(nil, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}

func TestSemanticVersionHasBuildAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.True(t, sv.HasBuildAttribute(vv.attributeName))
		})
	}
}

func TestSemanticVersionHasBuildAttributeWithValidCharacter(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "X"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.False(t, sv.HasBuildAttribute("X"))
		})
	}
}

func TestSemanticVersionHasBuildAttributeWithValidMockIdentifier(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "mockidentifier"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.False(t, sv.HasBuildAttribute("mockidentifier"))
		})
	}
}

func TestSemanticVersionHasBuildAttributeWithEmptyString(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.False(t, sv.HasBuildAttribute(""))
			assert.False(t, sv.HasBuildAttribute("   "))
		})
	}
}

func TestSemanticVersionGetBuildAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttributeWith(vv.attributeName, &vv.attributeValue)
			assert.NoError(t, err)
			assert.Equal(t, vv.attributeValue, *sv.GetBuildAttributeValue(vv.attributeName))
		})
	}
}

func TestSemanticVersionGetBuildAttributeWithValidCharacter(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "X"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetBuildAttributeValue("X"))
		})
	}
}

func TestSemanticVersionGetBuildAttributeWithValidMockIdentifier(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			assert.False(t, strings.Contains(vv.version, "mockidentifier"))
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttribute(vv.attributeName)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetBuildAttributeValue("mockidentifier"))
		})
	}
}

func TestSemanticVersionGetBuildAttributeWithEmptyString(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			assert.Nil(t, sv.GetBuildAttributeValue(""))
			assert.Nil(t, sv.GetBuildAttributeValue("   "))
		})
	}
}

func TestSemanticVersionSetBuildAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err := sv1.SetBuildAttribute(vv.attributeName) // this overloaded version is a shorthand for the next one, but test both
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetBuildAttributeWithoutValue, sv.String())
			sv, err = sv1.SetBuildAttributeWith(vv.attributeName, nil)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetBuildAttributeWithoutValue, sv.String())
		})
	}
}

func TestSemanticVersionSetBuildAttributeWithValidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv, err = sv.SetBuildAttributeWith(vv.attributeName, &vv.attributeValue)
			assert.NoError(t, err)
			assert.Equal(t, vv.expectedOutcomeFromSetBuildAttributeWithValue, sv.String())
		})
	}
}

func TestSemanticVersionSetBuildAttributeWithInvalidStringName(t *testing.T) {
	for _, vv := range wellKnownInvalidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			_, err = sv.SetBuildAttribute(vv.attributeName) // this overloaded version is a shorthand for the next one, but test both
			if vv.expectedErrorWithoutValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			_, err = sv.SetBuildAttributeWith(vv.attributeName, nil)
			if vv.expectedErrorWithoutValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestSemanticVersionSetBuildAttributeWithInvalidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownInvalidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			_, err = sv.SetBuildAttributeWith(vv.attributeName, &vv.attributeValue)
			if vv.expectedErrorWithValue {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestSemanticVersionRemoveBuildAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 45)

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName)) // check that the random string is not there
			sv2, err := sv1.SetBuildAttribute(randomName)               // now add the random string as Build attribute
			assert.NoError(t, err)
			assert.True(t, strings.Contains(sv2.String(), randomName)) // check that the attribute is there

			sv3, err := sv2.RemoveBuildAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))

			sv4, err := sv2.RemoveBuildAttribute(&randomName, true) // also remove the value (which is not present)
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv4.String(), randomName))

			// now check that the two version, removing the value and not removing it, are exactly the same
			assert.True(t, sv3.Equals(sv4))
			assert.True(t, sv4.Equals(sv3))
			assert.Equal(t, sv3.String(), sv4.String())

			// now check that the original version and the last one are equal
			assert.True(t, sv1.Equals(sv3))
			assert.True(t, sv3.Equals(sv1))
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}

func TestSemanticVersionRemoveMissingBuildAttributeWithValidStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 51)

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName)) // check that the random string is not there

			sv2, err := sv1.RemoveBuildAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv2.String(), randomName))

			sv3, err := sv1.RemoveBuildAttribute(&randomName, true) // also remove the value (which is not present)
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))

			// now check that the three versions are the same
			assert.True(t, sv1.Equals(sv2))
			assert.True(t, sv1.Equals(sv3))
			assert.Equal(t, sv1.String(), sv2.String())
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}

func TestSemanticVersionRemoveBuildAttributeWithValidStringNameAndValue(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			// to avoid any potential conflicts with existing strings in the passed version we generate another random string
			randomName := randomAlphabeticString(5, 71)
			randomValue := randomAlphabeticString(8, 73)

			sv1, err := ValueOfSemanticVersion(vv.version) // first generate the plain version
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv1.String(), randomName))     // check that the random string is not there
			assert.False(t, strings.Contains(sv1.String(), randomValue))    // check that the random string is not there
			sv2, err := sv1.SetBuildAttributeWith(randomName, &randomValue) // now add the random string as Build attribute
			assert.NoError(t, err)
			assert.True(t, strings.Contains(sv2.String(), randomName))  // check that the attribute is there
			assert.True(t, strings.Contains(sv2.String(), randomValue)) // check that the attribute is there

			sv3, err := sv2.RemoveBuildAttribute(&randomName, false) // remove the attribute only
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv3.String(), randomName))
			assert.True(t, strings.Contains(sv3.String(), randomValue)) // the value must be still there

			sv4, err := sv2.RemoveBuildAttribute(&randomName, true) // also remove the value
			assert.NoError(t, err)
			assert.False(t, strings.Contains(sv4.String(), randomName))
			assert.False(t, strings.Contains(sv4.String(), randomValue)) // the value must be gone

			// now check that the two version, removing the value and not removing it, are different
			assert.False(t, sv3.Equals(sv4))
			assert.False(t, sv4.Equals(sv3))
			assert.NotEqual(t, sv3.String(), sv4.String())

			// now check that the original version and the last one are equal
			assert.True(t, sv1.Equals(sv4))
			assert.True(t, sv4.Equals(sv1))
			assert.Equal(t, sv1.String(), sv4.String())
		})
	}
}

func TestSemanticVersionRemoveBuildAttributeWithEmptyStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		emptyString := ""
		voidString := "  "
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.RemoveBuildAttribute(&emptyString, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv2.String())
			sv3, err := sv1.RemoveBuildAttribute(&emptyString, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv3.String())

			sv4, err := sv1.RemoveBuildAttribute(&voidString, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv4.String())
			sv5, err := sv1.RemoveBuildAttribute(&voidString, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv5.String())
		})
	}
}

func TestSemanticVersionRemoveBuildAttributeWithNilStringName(t *testing.T) {
	for _, vv := range wellKnownValidBuildAttributes {
		t.Run(vv.version, func(t *testing.T) {
			sv1, err := ValueOfSemanticVersion(vv.version)
			assert.NoError(t, err)
			sv2, err := sv1.RemoveBuildAttribute(nil, false)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv2.String())
			sv3, err := sv1.RemoveBuildAttribute(nil, true)
			assert.NoError(t, err)
			assert.Equal(t, sv1.String(), sv3.String())
		})
	}
}
