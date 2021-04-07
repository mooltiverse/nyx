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
package com.mooltiverse.oss.nyx.version;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Random;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

@DisplayName("SemanticVersion")
public class SemanticVersionTests {
    /**
     * A {@link MethodSource} method that returns valid structured data to test semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version<br>
     * - major: the major number<br>
     * - minor: the minor number<br>
     * - patch: the patch number<br>
     * - prerelease: an (optional) List of strings, each representing one identifier in the prerelease part<br>
     * - build: an (optional) List of strings, each representing one identifier in the build part<br>
     *
     * @return a stream of arguments representing correct versions
     */
    static Stream<Arguments> wellKnownValidVersions() {
        return Stream.of(
            // This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
            arguments("0.0.4", 0, 0, 4, null, null),
            arguments("1.2.3", 1, 2, 3, null, null),
            arguments("10.20.30", 10, 20, 30, null, null),
            arguments("1.1.2-prerelease+meta", 1, 1, 2, Arrays.asList("prerelease"), Arrays.asList("meta")),
            arguments("1.1.2+meta", 1, 1, 2, null, Arrays.asList("meta")),
            arguments("1.1.2+meta-valid", 1, 1, 2, null, Arrays.asList("meta-valid")),
            arguments("1.0.0-alpha", 1, 0, 0, Arrays.asList("alpha"), null),
            arguments("1.0.0-beta", 1, 0, 0, Arrays.asList("beta"), null),
            arguments("1.0.0-alpha.beta", 1, 0, 0, Arrays.asList("alpha", "beta"), null),
            arguments("1.0.0-alpha.beta.1", 1, 0, 0, Arrays.asList("alpha", "beta", "1"), null),
            arguments("1.0.0-alpha.1", 1, 0, 0, Arrays.asList("alpha", "1"), null),
            arguments("1.0.0-alpha0.valid", 1, 0, 0, Arrays.asList("alpha0", "valid"), null),
            arguments("1.0.0-alpha.0valid", 1, 0, 0, Arrays.asList("alpha", "0valid"), null),
            arguments("1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay", 1, 0, 0, Arrays.asList("alpha-a", "b-c-somethinglong"), Arrays.asList("build", "1-aef", "1-its-okay")),
            arguments("1.0.0-rc.1+build.1", 1, 0, 0, Arrays.asList("rc", "1"), Arrays.asList("build", "1")),
            arguments("2.0.0-rc.1+build.123", 2, 0, 0, Arrays.asList("rc", "1"), Arrays.asList("build", "123")),
            arguments("1.2.3-beta", 1, 2, 3, Arrays.asList("beta"), null),
            arguments("10.2.3-DEV-SNAPSHOT", 10, 2, 3, Arrays.asList("DEV-SNAPSHOT"), null),
            arguments("1.2.3-SNAPSHOT-123", 1, 2, 3, Arrays.asList("SNAPSHOT-123"), null),
            arguments("1.0.0", 1, 0, 0, null, null),
            arguments("2.0.0", 2, 0, 0, null, null),
            arguments("1.1.7", 1, 1, 7, null, null),
            arguments("2.0.0+build.1848", 2, 0, 0, null, Arrays.asList("build", "1848")),
            arguments("2.0.1-alpha.1227", 2, 0, 1, Arrays.asList("alpha", "1227"), null),
            arguments("1.0.0-alpha+beta", 1, 0, 0, Arrays.asList("alpha"), Arrays.asList("beta")),
            arguments("1.2.3----RC-SNAPSHOT.12.9.1--.12+788", 1, 2, 3, Arrays.asList("---RC-SNAPSHOT", "12", "9", "1--", "12"), Arrays.asList("788")),
            arguments("1.2.3----R-S.12.9.1--.12+meta", 1, 2, 3, Arrays.asList("---R-S", "12", "9", "1--", "12"), Arrays.asList("meta")),
            arguments("1.2.3----RC-SNAPSHOT.12.9.1--.12", 1, 2, 3, Arrays.asList("---RC-SNAPSHOT", "12", "9", "1--", "12"), null),
            arguments("1.0.0+0.build.1-rc.10000aaa-kk-0.1", 1, 0, 0, null, Arrays.asList("0", "build", "1-rc", "10000aaa-kk-0", "1")),
            //Let's use just the biggest int here. just make it -1 to support bumps
            arguments(String.valueOf(Integer.MAX_VALUE-1)+"."+String.valueOf(Integer.MAX_VALUE-1)+"."+String.valueOf(Integer.MAX_VALUE-1), Integer.MAX_VALUE-1, Integer.MAX_VALUE-1, Integer.MAX_VALUE-1, null, null),
            arguments("1.0.0-0A.is.legal", 1, 0, 0, Arrays.asList("0A", "is", "legal"), null)
        );
    }

    /**
     * A {@link MethodSource} method that returns invalid structured data to test semantic versions.
     * These strings should not parse to a correct version as they are illegal by some mean.
     * 
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version<br>
     * - expectedException: the class of the expected expection when parsing the string<br>
     *
     * @return a stream of arguments representing incorrect versions
     */
    static Stream<Arguments> wellKnownInvalidVersions() {
        return Stream.of(
            // This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
            arguments("1", IllegalArgumentException.class),
            arguments("1.2", IllegalArgumentException.class),
            arguments("1.2.3-0123", IllegalArgumentException.class),
            arguments("1.2.3-0123.0123", IllegalArgumentException.class),
            arguments("1.1.2+.123", IllegalArgumentException.class),
            arguments("+invalid", IllegalArgumentException.class),
            arguments("-invalid", IllegalArgumentException.class),
            arguments("-invalid+invalid", IllegalArgumentException.class),
            arguments("-invalid.01", IllegalArgumentException.class),
            arguments("alpha", IllegalArgumentException.class),
            arguments("alpha.beta", IllegalArgumentException.class),
            arguments("alpha.beta.1", IllegalArgumentException.class),
            arguments("alpha.1", IllegalArgumentException.class),
            arguments("alpha+beta", IllegalArgumentException.class),
            arguments("alpha_beta", IllegalArgumentException.class),
            arguments("alpha.", IllegalArgumentException.class),
            arguments("alpha..", IllegalArgumentException.class),
            arguments("beta", IllegalArgumentException.class),
            arguments("1.0.0-alpha_beta", IllegalArgumentException.class),
            arguments("-alpha.", IllegalArgumentException.class),
            arguments("1.0.0-alpha..", IllegalArgumentException.class),
            arguments("1.0.0-alpha..1", IllegalArgumentException.class),
            arguments("1.0.0-alpha...1", IllegalArgumentException.class),
            arguments("1.0.0-alpha....1", IllegalArgumentException.class),
            arguments("1.0.0-alpha.....1", IllegalArgumentException.class),
            arguments("1.0.0-alpha......1", IllegalArgumentException.class),
            arguments("1.0.0-alpha.......1", IllegalArgumentException.class),
            arguments("01.1.1", IllegalArgumentException.class),
            arguments("1.01.1", IllegalArgumentException.class),
            arguments("1.1.01", IllegalArgumentException.class),
            arguments("1.2", IllegalArgumentException.class),
            arguments("1.2.3.DEV", IllegalArgumentException.class),
            arguments("1.2-SNAPSHOT", IllegalArgumentException.class),
            arguments("1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788", IllegalArgumentException.class),
            arguments("1.2-RC-SNAPSHOT", IllegalArgumentException.class),
            arguments("-1.0.3-gamma+b7718", IllegalArgumentException.class),
            arguments("+justmeta", IllegalArgumentException.class),
            arguments("9.8.7+meta+meta", IllegalArgumentException.class),
            arguments("9.8.7-whatever+meta+meta", IllegalArgumentException.class),
            arguments("99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12", IllegalArgumentException.class)
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the sanitizeable original version<br>
     * - prefix: if the original version has a prefix, it's the expected outcome of the invocation of the getPrefix() method, otherwise null means that the version has no prefix and getPrefix() is expected to return null<br>
     * - sanitizedOutcome: the expected outcome of the entire sanitization over the original string<br>
     * - sanitizedPrefixOutcome: the expected outcome of sanitizePrefix() over the original string<br>
     * - sanitizedNumberOutcome: the expected outcome of sanitizeNumbers() over the original string<br>
     *
     * @return a stream of arguments representing incorrect versions that can be sanitized
     */
    static Stream<Arguments> wellKnownSanitizableVersions() {
        return Stream.of(
            // common prefixes
            arguments("v1.2.3", "v", "1.2.3", "1.2.3", "v1.2.3"),
            arguments("v1.2.3-alpha.1", "v", "1.2.3-alpha.1", "1.2.3-alpha.1", "v1.2.3-alpha.1"),
            arguments("v1.2.3-alpha.1+build.2", "v", "1.2.3-alpha.1+build.2", "1.2.3-alpha.1+build.2", "v1.2.3-alpha.1+build.2"),
            arguments("v1.2.3-alpha.1.delta-q.whatever+build.2.20200101", "v", "1.2.3-alpha.1.delta-q.whatever+build.2.20200101", "1.2.3-alpha.1.delta-q.whatever+build.2.20200101", "v1.2.3-alpha.1.delta-q.whatever+build.2.20200101"),
            arguments("ver1.2.3", "ver", "1.2.3", "1.2.3", "ver1.2.3"),
            arguments("ver-1.2.3", "ver-", "1.2.3", "1.2.3", "ver-1.2.3"),
            arguments("ver.1.2.3", "ver.", "1.2.3", "1.2.3", "ver.1.2.3"),
            arguments("version1.2.3", "version", "1.2.3", "1.2.3", "version1.2.3"),
            arguments("version-1.2.3", "version-", "1.2.3", "1.2.3", "version-1.2.3"),
            arguments("version.1.2.3", "version.", "1.2.3", "1.2.3", "version.1.2.3"),
            arguments("r1.2.3", "r", "1.2.3", "1.2.3", "r1.2.3"),
            arguments("rel1.2.3", "rel", "1.2.3", "1.2.3", "rel1.2.3"),
            arguments("rel-1.2.3", "rel-", "1.2.3", "1.2.3", "rel-1.2.3"),
            arguments("rel.1.2.3", "rel.", "1.2.3", "1.2.3", "rel.1.2.3"),
            arguments("release1.2.3", "release", "1.2.3", "1.2.3", "release1.2.3"),
            arguments("release-1.2.3", "release-", "1.2.3", "1.2.3", "release-1.2.3"),
            arguments("release.1.2.3", "release.", "1.2.3", "1.2.3", "release.1.2.3"),
            // spurious prefixes
            arguments("-1.2.3", "-", "1.2.3", "1.2.3", "-1.2.3"),
            arguments("!1.2.3", "!", "1.2.3", "1.2.3", "!1.2.3"),
            arguments("+1.2.3", "+", "1.2.3", "1.2.3", "+1.2.3"),
            arguments("!(trash)v1.2.3", "!(trash)v", "1.2.3", "1.2.3", "!(trash)v1.2.3"),
            // leading zeroes
            arguments("01.02.03", null, "1.2.3", "01.02.03", "1.2.3"),
            arguments("01.02.03-alpha", null, "1.2.3-alpha", "01.02.03-alpha", "1.2.3-alpha"),
            arguments("01.02.03-alpha.0", null, "1.2.3-alpha.0", "01.02.03-alpha.0", "1.2.3-alpha.0"),
            arguments("01.02.03-alpha+beta", null, "1.2.3-alpha+beta", "01.02.03-alpha+beta", "1.2.3-alpha+beta"),
            arguments("01.02.03-alpha+beta.0", null, "1.2.3-alpha+beta.0", "01.02.03-alpha+beta.0", "1.2.3-alpha+beta.0"),
            arguments("01.02.03+beta", null, "1.2.3+beta", "01.02.03+beta", "1.2.3+beta"),
            arguments("01.02.03+beta.0", null, "1.2.3+beta.0", "01.02.03+beta.0", "1.2.3+beta.0"),
            arguments("01.02.03-alpha.0+beta.0", null, "1.2.3-alpha.0+beta.0", "01.02.03-alpha.0+beta.0", "1.2.3-alpha.0+beta.0"),
            arguments("01.02.03-alpha.01+beta.02", null, "1.2.3-alpha.1+beta.02", "01.02.03-alpha.01+beta.02", "1.2.3-alpha.1+beta.02"),
            arguments("000001.000002.000003", null, "1.2.3", "000001.000002.000003", "1.2.3"),
            arguments("000001.000002.000003-alpha.00345+beta-00678", null, "1.2.3-alpha.345+beta-00678", "000001.000002.000003-alpha.00345+beta-00678", "1.2.3-alpha.345+beta-00678"),
            // These are taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
            // they are actually illegal numbers, bu they can be sanitized to legal ones
            arguments("1.2.3-0123", null, "1.2.3-123", "1.2.3-0123", "1.2.3-123"),
            arguments("1.2.3-0123.0123", null, "1.2.3-123.123", "1.2.3-0123.0123", "1.2.3-123.123"),
            arguments("01.1.1", null, "1.1.1", "01.1.1", "1.1.1"),
            arguments("1.01.1", null, "1.1.1", "1.01.1", "1.1.1"),
            arguments("1.1.01", null, "1.1.1", "1.1.01", "1.1.1"),
            arguments("-1.0.3-gamma+b7718", "-", "1.0.3-gamma+b7718", "1.0.3-gamma+b7718", "-1.0.3-gamma+b7718")
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test bumping of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the starting version<br>
     * - bumpId: the identifier to bump<br>
     * - expectedOutcomeFromBump: the outcome expected after invoking bump(bumpId)<br>
     * - expectedOutcomeFromBumpPrerelease: the outcome expected after invoking bumpPrerelease(bumpId)<br>
     *
     * @return a stream of arguments representing versions and their expected values after bumping
     */
    static Stream<Arguments> wellKnownValidBumpAttributes() {
        return Stream.of(
            arguments("1.2.3", "alpha", "1.2.3-alpha.1", "1.2.3-alpha.1"),
            arguments("1.2.3", "beta", "1.2.3-beta.1", "1.2.3-beta.1"),
            arguments("1.2.3", "gamma", "1.2.3-gamma.1", "1.2.3-gamma.1"),
            arguments("1.2.3", "delta", "1.2.3-delta.1", "1.2.3-delta.1"),
            arguments("1.2.3", "rc", "1.2.3-rc.1", "1.2.3-rc.1"),

            //the build part must be unaffected by bumps
            arguments("1.2.3+alpha", "alpha", "1.2.3-alpha.1+alpha", "1.2.3-alpha.1+alpha"),
            arguments("1.2.3+alpha.1", "alpha", "1.2.3-alpha.1+alpha.1", "1.2.3-alpha.1+alpha.1"),
            arguments("1.2.3+beta", "alpha", "1.2.3-alpha.1+beta", "1.2.3-alpha.1+beta"),

            // test when the identifier appears multiple times
            arguments("1.2.3-alpha", "alpha", "1.2.3-alpha.1", "1.2.3-alpha.1"),
            arguments("1.2.3-alpha.0", "alpha", "1.2.3-alpha.1", "1.2.3-alpha.1"),
            arguments("1.2.3-alpha.1", "alpha", "1.2.3-alpha.2", "1.2.3-alpha.2"),
            arguments("1.2.3-alpha.alpha", "alpha", "1.2.3-alpha.1.alpha", "1.2.3-alpha.1.alpha"),
            arguments("1.2.3-alpha.0.alpha", "alpha", "1.2.3-alpha.1.alpha.1", "1.2.3-alpha.1.alpha.1"),
            arguments("1.2.3-alpha+alpha.beta.alpha", "alpha", "1.2.3-alpha.1+alpha.beta.alpha", "1.2.3-alpha.1+alpha.beta.alpha"),
            arguments("1.2.3-alpha.0+alpha.beta.alpha", "alpha", "1.2.3-alpha.1+alpha.beta.alpha", "1.2.3-alpha.1+alpha.beta.alpha"),
            arguments("1.2.3-alpha.1+alpha.beta.alpha", "alpha", "1.2.3-alpha.2+alpha.beta.alpha", "1.2.3-alpha.2+alpha.beta.alpha"),
            arguments("1.2.3-alpha.alpha+alpha.beta.alpha", "alpha", "1.2.3-alpha.1.alpha+alpha.beta.alpha", "1.2.3-alpha.1.alpha+alpha.beta.alpha"),
            arguments("1.2.3-alpha.0.alpha+alpha.beta.alpha", "alpha", "1.2.3-alpha.1.alpha.1+alpha.beta.alpha", "1.2.3-alpha.1.alpha.1+alpha.beta.alpha"),

            // these are edge cases, in which a core component is bumped by its name. The core() method is expected to bump that specific core component
            // while the bumpPrerelease() method is expected to bump the prerelease component, leaving the core identifiers untouched
            arguments("1.2.3", "major", "2.0.0", "1.2.3-major.1"),
            arguments("1.2.3", "minor", "1.3.0", "1.2.3-minor.1"),
            arguments("1.2.3", "patch", "1.2.4", "1.2.3-patch.1"),
            arguments("1.2.3-major", "major", "2.0.0-major", "1.2.3-major.1"),
            arguments("1.2.3-minor", "minor", "1.3.0-minor", "1.2.3-minor.1"),
            arguments("1.2.3-patch", "patch", "1.2.4-patch", "1.2.3-patch.1"),
            arguments("1.2.3-major.3", "major", "2.0.0-major.3", "1.2.3-major.4"),
            arguments("1.2.3-minor.3", "minor", "1.3.0-minor.3", "1.2.3-minor.4"),
            arguments("1.2.3-patch.3", "patch", "1.2.4-patch.3", "1.2.3-patch.4")
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test bumping of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version<br>
     * - bumpId: the identifier to bump<br>
     * - expectedException: the class of the expected expection<br>
     *
     * @return a stream of arguments representing versions and their expected exceptions after bumping
     */
    static Stream<Arguments> wellKnownInvalidBumpAttributes() {
        return Stream.of(
            arguments("1.2.3", ".", IllegalArgumentException.class),
            arguments("1.2.3", "alpha.", IllegalArgumentException.class),
            arguments("1.2.3", "alpha.beta", IllegalArgumentException.class),
            arguments("1.2.3", "0", IllegalArgumentException.class),
            arguments("1.2.3", "1", IllegalArgumentException.class),
            arguments("1.2.3", "alpha.0", IllegalArgumentException.class),
            arguments("1.2.3", "alpha.1", IllegalArgumentException.class),
            arguments("1.2.3", "!", IllegalArgumentException.class),
            arguments("1.2.3", "+", IllegalArgumentException.class)
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test setting a prerelease attribute of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the starting version<br>
     * - attributeName: the name identifier to set<br>
     * - attributeValue: the value identifier to set<br>
     * - expectedOutcomeFromSetPrereleaseAttributeWithoutValue: the outcome expected after invoking setPrereleaseAttribute(attributeName)<br>
     * - expectedOutcomeFromSetPrereleaseAttributeWithValue: the outcome expected after invoking setPrereleaseAttribute(attributeName, attributeValue)<br>
     *
     * @return a stream of arguments representing versions and their expected values after bumping
     */
    static Stream<Arguments> wellKnownValidPrereleaseAttributes() {
        return Stream.of(
            arguments("1.2.3", "build", 123, "1.2.3-build", "1.2.3-build.123"),

            //if the attribute is already present with that value the return unchanged
            arguments("1.2.3-build", "build", 123, "1.2.3-build", "1.2.3-build.123"),
            arguments("1.2.3-build.123", "build", 123, "1.2.3-build.123", "1.2.3-build.123"),

            //the build part must be unaffected by setPrereleaseAttribute, only the prerelease part
            arguments("1.2.3+build", "build", 123, "1.2.3-build+build", "1.2.3-build.123+build"),
            arguments("1.2.3+build.1", "build", 123, "1.2.3-build+build.1", "1.2.3-build.123+build.1"),
            arguments("1.2.3+build.123", "build", 123, "1.2.3-build+build.123", "1.2.3-build.123+build.123"),
            arguments("1.2.3+build.timestamp", "build", 123, "1.2.3-build+build.timestamp", "1.2.3-build.123+build.timestamp"),
            arguments("1.2.3+build.1.timestamp", "build", 123, "1.2.3-build+build.1.timestamp", "1.2.3-build.123+build.1.timestamp"),
            arguments("1.2.3+build.123.timestamp", "build", 123, "1.2.3-build+build.123.timestamp", "1.2.3-build.123+build.123.timestamp"),
            arguments("1.2.3+build.timestamp.20200101", "build", 123, "1.2.3-build+build.timestamp.20200101", "1.2.3-build.123+build.timestamp.20200101"),
            arguments("1.2.3+build.1.timestamp.20200101", "build", 123, "1.2.3-build+build.1.timestamp.20200101", "1.2.3-build.123+build.1.timestamp.20200101"),
            arguments("1.2.3+build.123.timestamp.20200101", "build", 123, "1.2.3-build+build.123.timestamp.20200101", "1.2.3-build.123+build.123.timestamp.20200101"),

            //other build attributes must be unaffected by setBuildAttribute, unless they come after the matched identifier (in which case they are overwritten)
            arguments("1.2.3+timestamp", "build", 123, "1.2.3-build+timestamp", "1.2.3-build.123+timestamp"),
            arguments("1.2.3+timestamp.1", "build", 123, "1.2.3-build+timestamp.1", "1.2.3-build.123+timestamp.1"),
            arguments("1.2.3+timestamp.123", "build", 123, "1.2.3-build+timestamp.123", "1.2.3-build.123+timestamp.123"),

            arguments("1.2.3-build.567+build.timestamp", "build", 123, "1.2.3-build.567+build.timestamp", "1.2.3-build.123+build.timestamp"),                           // overwrites 567 if the value is not null
            arguments("1.2.3-build.567+build.timestamp.1", "build", 123, "1.2.3-build.567+build.timestamp.1", "1.2.3-build.123+build.timestamp.1"),                     // overwrites 567 if the value is not null
            arguments("1.2.3-build.567+build.timestamp.123", "build", 123, "1.2.3-build.567+build.timestamp.123", "1.2.3-build.123+build.timestamp.123")                // overwrites 567 if the value is not null
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test setting a prerelease attribute of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version<br>
     * - attributeName: the name identifier to set<br>
     * - attributeValue: the value identifier to set<br>
     * - expectedExceptionWithoutValue: the class of the expected expection when invoking setPrereleaseAttribute(attributeName), if any. If {@code null} no exception is expected in this case<br>
     * - expectedExceptionWithValue: the class of the expected expection when invoking setPrereleaseAttribute(attributeName), if any. If {@code null} no exception is expected in this case<br>
     *
     * @return a stream of arguments representing versions and their expected values after bumping
     */
    static Stream<Arguments> wellKnownInvalidPrereleaseAttributes() {
        return Stream.of(
            arguments("1.2.3", null, null, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3", null, 123, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3-alpha", null, null, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3+beta", null, 123, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3-alpha+beta", null, 123, NullPointerException.class, NullPointerException.class),
            
            arguments("1.2.3", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3+beta", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3-build", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3+build", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", -123, null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", ".", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.beta", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.0", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.1", 123, IllegalArgumentException.class, IllegalArgumentException.class),

            arguments("1.2.3", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "!", 123, IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "+", 123, IllegalArgumentException.class, IllegalArgumentException.class)
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test setting a build attribute of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the starting version<br>
     * - attributeName: the name identifier to set<br>
     * - attributeValue: the value identifier to set<br>
     * - expectedOutcomeFromSetBuildAttributeWithoutValue: the outcome expected after invoking setBuildAttribute(attributeName)<br>
     * - expectedOutcomeFromSetBuildAttributeWithValue: the outcome expected after invoking setBuildAttribute(attributeName, attributeValue)<br>
     *
     * @return a stream of arguments representing versions and their expected values after bumping
     */
    static Stream<Arguments> wellKnownValidBuildAttributes() {
        return Stream.of(
            arguments("1.2.3", "build", "123", "1.2.3+build", "1.2.3+build.123"),

            //if the attribute is already present with that value the return unchanged
            arguments("1.2.3+build", "build", "123", "1.2.3+build", "1.2.3+build.123"),
            arguments("1.2.3+build.123", "build", "123", "1.2.3+build.123", "1.2.3+build.123"),

            //the pre-release part must be unaffected by setBuildAttribute, only the build part
            arguments("1.2.3-build", "build", "123", "1.2.3-build+build", "1.2.3-build+build.123"),
            arguments("1.2.3-build.1", "build", "123", "1.2.3-build.1+build", "1.2.3-build.1+build.123"),
            arguments("1.2.3-build.123", "build", "123", "1.2.3-build.123+build", "1.2.3-build.123+build.123"),
            arguments("1.2.3-build+timestamp", "build", "123", "1.2.3-build+timestamp.build", "1.2.3-build+timestamp.build.123"),
            arguments("1.2.3-build.1+timestamp", "build", "123", "1.2.3-build.1+timestamp.build", "1.2.3-build.1+timestamp.build.123"),
            arguments("1.2.3-build.123+timestamp", "build", "123", "1.2.3-build.123+timestamp.build", "1.2.3-build.123+timestamp.build.123"),
            arguments("1.2.3-build+timestamp.20200101", "build", "123", "1.2.3-build+timestamp.20200101.build", "1.2.3-build+timestamp.20200101.build.123"),
            arguments("1.2.3-build.1+timestamp.20200101", "build", "123", "1.2.3-build.1+timestamp.20200101.build", "1.2.3-build.1+timestamp.20200101.build.123"),
            arguments("1.2.3-build.123+timestamp.20200101", "build", "123", "1.2.3-build.123+timestamp.20200101.build", "1.2.3-build.123+timestamp.20200101.build.123"),

            //other build attributes must be unaffected by setBuildAttribute, unless they come after the matched identifier (in which case they are overwritten)
            arguments("1.2.3+timestamp", "build", "123", "1.2.3+timestamp.build", "1.2.3+timestamp.build.123"),
            arguments("1.2.3+timestamp.1", "build", "123", "1.2.3+timestamp.1.build", "1.2.3+timestamp.1.build.123"),
            arguments("1.2.3+timestamp.123", "build", "123", "1.2.3+timestamp.123.build", "1.2.3+timestamp.123.build.123"),
            arguments("1.2.3+build.timestamp", "build", "123", "1.2.3+build.timestamp", "1.2.3+build.123"),                           // overwrites timestamp if the value is not null
            arguments("1.2.3+build.timestamp.1", "build", "123", "1.2.3+build.timestamp.1", "1.2.3+build.123.1"),                     // overwrites timestamp if the value is not null
            arguments("1.2.3+build.timestamp.123", "build", "123", "1.2.3+build.timestamp.123", "1.2.3+build.123.123")                // overwrites timestamp if the value is not null
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test setting a build attribute of semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version<br>
     * - attributeName: the name identifier to set<br>
     * - attributeValue: the value identifier to set<br>
     * - expectedExceptionWithoutValue: the class of the expected expection when invoking setBuildAttribute(attributeName), if any. If {@code null} no exception is expected in this case<br>
     * - expectedExceptionWithValue: the class of the expected expection when invoking setBuildAttribute(attributeName), if any. If {@code null} no exception is expected in this case<br>
     *
     * @return a stream of arguments representing versions and their expected values after bumping
     */
    static Stream<Arguments> wellKnownInvalidBuildAttributes() {
        return Stream.of(
            arguments("1.2.3", null, null, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3", null, "123", NullPointerException.class, NullPointerException.class),
            arguments("1.2.3-alpha", null, null, NullPointerException.class, NullPointerException.class),
            arguments("1.2.3+beta", null, "123", NullPointerException.class, NullPointerException.class),
            arguments("1.2.3-alpha+beta", null, "123", NullPointerException.class, NullPointerException.class),
            
            arguments("1.2.3", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3-build", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3+build", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "123.1", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", ".", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", ".", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "123.", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.beta", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "123.456", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.0", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "123.0", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build.1", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "123.1", null, IllegalArgumentException.class),

            arguments("1.2.3", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3+beta", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+beta", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-alpha+beta", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3-build", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3+build", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3+build", "build", "+", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "!", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "!", null, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "+", "123", IllegalArgumentException.class, IllegalArgumentException.class),
            arguments("1.2.3-build+build", "build", "+", null, IllegalArgumentException.class)
        );
    }

    /**
     * An ordered list of valid version strings (according to SemVer). This list can be used to check ordering among semantic versions.
     */
    static String[] wellKnownOrderedSemanticVersionsArray = new String[] {
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
        "0.0.1-alpha.A",
        "0.0.1-alpha.Aa",
        "0.0.1-alpha.a",
        "0.0.1-alpha.aa",
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
        "0.3.1-alpha.A",
        "0.3.1-alpha.Aa",
        "0.4.1-alpha.a",
        "0.4.1-alpha.aa",
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
        String.valueOf(Integer.MAX_VALUE-1)+"."+String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE),
        String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE-1)+"."+String.valueOf(Integer.MAX_VALUE),
        String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE-1),
        String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE)+"."+String.valueOf(Integer.MAX_VALUE)
    };

    /**
     * A {@link MethodSource} method that returns the stream of well known ordered semantic versions.
     *
     * @return a stream of ordered versions.
     *
     * @see #wellKnownOrderedSemanticVersionsArray
     */
    static Stream<String> wellKnownOrderedSemanticVersions() {
        return Stream.of(wellKnownOrderedSemanticVersionsArray);
    }

    /**
     * Convenience method that builds a core, prerelease or a build string by concatenating the single identifiers, separated
     * by a dot. If the provided list is null then the returned string is null.
     *
     * @param identifiers the identifiers to concatenate.
     *
     * @return the concatenated string.
     */
    static String identifiersToString(List<String> identifiers) {
        if (identifiers == null)
            return null;

        StringBuilder sb = new StringBuilder();
        Iterator<String> i = identifiers.iterator();
        while (i.hasNext()) {
            sb.append(i.next());
            if (i.hasNext())
                sb.append(".");
        }
        return sb.toString();
    }

    /**
     * Generates a random string containing only alphabetic characters
     * 
     * @param length the length of the string to generate
     * 
     * @return the generated string
     */
    static String randomAlphabeticString(int length) {
        int leftLimit = 97; // letter 'a'
        int rightLimit = 122; // letter 'z'
     
        return new Random().ints(leftLimit, rightLimit + 1).limit(length).collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
    }

    @Nested
    @DisplayName("SemanticVersion constructors")
    class ConstructorsTests {
        @Test
        @DisplayName("new SemanticVersion() with negative numbers throws IllegalArgumentException")
        void exceptionUsingNegativeNumbers() {
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(0, 0, -1));
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(0, -1, 0));
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(-1, 0, 0));
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(0, 0, -1, null, null));
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(0, -1, 0, null, null));
            assertThrows(IllegalArgumentException.class, () -> new SemanticVersion(-1, 0, 0, null, null));
        }

        @ParameterizedTest(name = "new SemanticVersion(''{1}'',''{2}'',''{3}'',''null'',''null'') == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void noExceptionUsingNullIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = new SemanticVersion(major, minor, patch, null, null);
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));

            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
            assertEquals(coreString, sv.getCore());
        }

        @ParameterizedTest(name = "new SemanticVersion(''{1}'',''{2}'',''{3}'',''[]'',''[]'') == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void noExceptionUsingEmptyIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = new SemanticVersion(major, minor, patch, new Object[0], new String[0]);
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));

            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
            assertEquals(coreString, sv.getCore());
        }

        @ParameterizedTest(name = "new SemanticVersion(''{1}'',''{2}'',''{3}'') == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void constructor1(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = new SemanticVersion(major, minor, patch);
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));

            assertEquals(coreString, sv.toString());
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
        }

        @ParameterizedTest(name = "new SemanticVersion(''{1}'',''{2}'',''{3}'',''null'',''null'') == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void constructor2(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = new SemanticVersion(major, minor, patch, pre == null ? null : pre.toArray(new Object[0]), build == null ? null : build.toArray(new String[0]));

            assertEquals(version, sv.toString());
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.isLegal")
    class IsLegalTests {
        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'') == false")
        @EmptySource
        void isLegalWithEmptyString(String version) {
            assertFalse(SemanticVersion.isLegal(version));
        }

        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingIsLegalWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.isLegal(version));
        }

        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void isLegalWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertFalse(SemanticVersion.isLegal(version));
        }

        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(SemanticVersion.isLegal(version));
        }

        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test invoking isLegal with prefix tolerance
            assertTrue(SemanticVersion.isLegal(version, true));
        }

        @ParameterizedTest(name = "Semanticversion.isLegal(''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void isLegalSanitizableString(String version) {
            // the method must fail without toleration and succeed when using toleration
            assertFalse(SemanticVersion.isLegal(version));
            assertFalse(SemanticVersion.isLegal(version, false));
            assertTrue(SemanticVersion.isLegal(version, true));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String version) {
            assertThrows(RuntimeException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'') throws ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void exceptionUsingValueOfWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.valueOf(version).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'', true).toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test that invoking valueOf with sanitization on a valid string returns the same string
            assertEquals(version, SemanticVersion.valueOf(version, true).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'', true).toString() == SemanticVersion.sanitize(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void valueOfSanitizableString(String version) {
            // the method must fail without sanitization and succeed when using sanitization
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(version));
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(version, false).toString());
            assertNotEquals(version, SemanticVersion.valueOf(version, true).toString());
            assertEquals(SemanticVersion.sanitize(version), SemanticVersion.valueOf(version, true).toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getMajor")
    class GetMajorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getMajor() == ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getMajor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(major, SemanticVersion.valueOf(version).getMajor());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setMajor")
    class SetMajorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).setMajor(''{1}'') == ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setMajor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(0 == sv.getMajor());

            sv = sv.setMajor(major);
            assertEquals(major, sv.getMajor());

            // ancillary tests, just to make sure the operation didn't affect other fields
            assertEquals(1, sv.getMinor());
            assertEquals(0, sv.getPatch());
            assertNull(sv.getPrerelease());
            assertNull(sv.getBuild());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getMinor")
    class GetMinorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getMinor() == ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getMinor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(minor, SemanticVersion.valueOf(version).getMinor());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setMinor")
    class SetMinorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).setMinor(''{2}'') == ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setMinor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(1 == sv.getMinor());

            sv = sv.setMinor(minor);
            assertEquals(minor, sv.getMinor());

            // ancillary tests, just to make sure the operation didn't affect other fields
            assertEquals(0, sv.getMajor());
            assertEquals(0, sv.getPatch());
            assertNull(sv.getPrerelease());
            assertNull(sv.getBuild());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPatch")
    class GetPatchTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getPatch() == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getPatch(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(patch, SemanticVersion.valueOf(version).getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setPatch")
    class SetPatchTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).setPatch(''{3}'') == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setPatch(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(0 == sv.getPatch());

            sv = sv.setPatch(patch);
            assertEquals(patch, sv.getPatch());

            // ancillary tests, just to make sure the operation didn't affect other fields
            assertEquals(0, sv.getMajor());
            assertEquals(1, sv.getMinor());
            assertNull(sv.getPrerelease());
            assertNull(sv.getBuild());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getCore")
    class GetCoreTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getCore() == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getCore(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));
            assertEquals(coreString, SemanticVersion.valueOf(version).getCore());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getCoreIdentifiers(''{0}'') == ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getCoreIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(3, SemanticVersion.valueOf(version).getCoreIdentifiers().length);
            assertEquals(Integer.valueOf(major), SemanticVersion.valueOf(version).getCoreIdentifiers()[0]);
            assertEquals(Integer.valueOf(minor), SemanticVersion.valueOf(version).getCoreIdentifiers()[1]);
            assertEquals(Integer.valueOf(patch), SemanticVersion.valueOf(version).getCoreIdentifiers()[2]);
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setCore")
    class SetCoreTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setCore(''{1}'', ''{2}'', ''{3}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setCore(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(0 == sv.getMajor());
            assumeTrue(1 == sv.getMinor());
            assumeTrue(0 == sv.getPatch());

            sv = sv.setCore(major, minor, patch);
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
            assertNull(sv.getPrerelease());
            assertNull(sv.getBuild());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPrerelease")
    class GetPrereleaseTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getPrerelease() == ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getPrerelease(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String preString = identifiersToString(pre);
            assertEquals(preString, SemanticVersion.valueOf(version).getPrerelease());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getPrereleaseIdentifiers() ==> ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getPrereleaseIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            if (pre != null) {
                assertEquals(pre.size(), SemanticVersion.valueOf(version).getPrereleaseIdentifiers().length);
                for (String s: pre) {
                    assertEquals(s.toString(), SemanticVersion.valueOf(version).getPrereleaseIdentifiers()[pre.indexOf(s)].toString());
                }
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setPrerelease")
    class SetPrereleaseTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrerelease(''{4}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setPrerelease(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(0 == sv.getMajor());
            assumeTrue(1 == sv.getMinor());
            assumeTrue(0 == sv.getPatch());
            assumeTrue(null == sv.getPrerelease());
            assumeTrue(null == sv.getBuild());

            sv = sv.setCore(major, minor, patch);
            sv = sv.setBuild(build == null ? null : build.toArray(new String[0]));
            sv = sv.setPrerelease(pre == null ? null : pre.toArray());
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
            assertEquals(version, sv.toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getBuild")
    class GetBuildTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getBuild() == ''{5}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getBuild(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String buildString = identifiersToString(build);
            assertEquals(buildString, SemanticVersion.valueOf(version).getBuild());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getBuildIdentifiers(''{0}'') ==> ''{5}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getBuildIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            if (build != null) {
                assertEquals(build.size(), SemanticVersion.valueOf(version).getBuildIdentifiers().length);
                for (String s: build) {
                    assertEquals(s.toString(), SemanticVersion.valueOf(version).getBuildIdentifiers()[build.indexOf(s)].toString());
                }
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setBuild")
    class SetBuildTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuild(''{5}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void setPrerelease(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            assumeTrue(SemanticVersion.DEFAULT_INITIAL_VERSION.equals(sv.toString()));
            assumeTrue(0 == sv.getMajor());
            assumeTrue(1 == sv.getMinor());
            assumeTrue(0 == sv.getPatch());
            assumeTrue(null == sv.getPrerelease());
            assumeTrue(null == sv.getBuild());

            sv = sv.setCore(major, minor, patch);
            sv = sv.setPrerelease(pre == null ? null : pre.toArray());
            sv = sv.setBuild(build == null ? null : build.toArray(new String[0]));
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());
            assertEquals(version, sv.toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.toString")
    class ToStringTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').toString(''{0}'') ==> ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void toString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.valueOf(version).toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getScheme")
    class GetSchemeTests {
        @Test
        @DisplayName("Semanticversion.getScheme() ==> Scheme.SEMVER")
        void getScheme() {
            assertEquals(Scheme.SEMVER, SemanticVersion.valueOf("1.1.1").getScheme());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.sanitize")
    class SanitizeTests {
        @ParameterizedTest(name = "Semanticversion.sanitize(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizeWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizePrefix(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizePrefixWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizeNumbers(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizeNumbersWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitize(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingSanitizeWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizePrefix(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingSanitizePrefixWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizeNumbers(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingSanitizeNumbersWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitize(''{0}'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizeWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizeNumbers(''{0}'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizeNumbersWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizePrefix(''{0}'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizePrefixWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitize(''{0}'') == ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void sanitize(String version, String expectedPrefix, String expectedSanitizeOutcome, String expectedSanitizePrefixOutcome, String expectedSanitizeNumberOutcome) {
            assertEquals(expectedSanitizeOutcome, SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizePrefix(''{0}'') == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void sanitizePrefix(String version, String expectedPrefix, String expectedSanitizeOutcome, String expectedSanitizePrefixOutcome, String expectedSanitizeNumberOutcome) {
            assertEquals(expectedSanitizePrefixOutcome, SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "Semanticversion.sanitizeNumbers(''{0}'') == ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void sanitizeNumbers(String version, String expectedPrefix, String expectedSanitizeOutcome, String expectedSanitizePrefixOutcome, String expectedSanitizeNumberOutcome) {
            assertEquals(expectedSanitizeNumberOutcome, SemanticVersion.sanitizeNumbers(version));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPrefix")
    class GetPrefixTests {
        @ParameterizedTest(name = "Semanticversion.getPrefix(''{0}'') == ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void getPrefix(String version, String expectedPrefix, String expectedSanitizeOutcomeOrException, String expectedSanitizePrefixOutcomeOrException, String expectedSanitizeNumberOutcomeOrException) {
            assertEquals(expectedPrefix, SemanticVersion.getPrefix(version));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.equals")
    class EqualsTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').equals(null) == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToNull(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertFalse(sv.equals(null));
            assertNotEquals(sv, null);
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').equals('''') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToEmptyString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertFalse(sv.toString().equals(""));
            assertNotEquals(sv, "");
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').equals(''0'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToSameInstance(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertTrue(sv.equals(sv));
            assertEquals(sv, sv);
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').equals(Semanticversion.valueOf(''{0}'')) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToOtherInstanceWithSameStringvalue(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = SemanticVersion.valueOf(version);
            assertFalse(sv1 == sv2);
            assertTrue(sv1.equals(sv2));
            assertEquals(sv1, sv2);
        }
    }

    @Nested
    @DisplayName("SemanticVersion.compareTo")
    class CompareToTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').compareTo([...])")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownOrderedSemanticVersions")
        void compareTo(String version) {
            SemanticVersion sv = SemanticVersion.valueOf(version);

            // The version parameter is just one value picked from the following array so we can test the order of that
            // element throughout the entire array.
            List<String> versionsList = Arrays.<String>asList(SemanticVersionTests.wellKnownOrderedSemanticVersionsArray);
            int itemPosition = versionsList.indexOf(version);
            // double check the given item is in the list
            assumeTrue(versionsList.contains(version));
            assumeTrue(itemPosition >= 0);

            ListIterator<String> iterator = versionsList.listIterator();
            while (iterator.hasNext()) {
                if (iterator.nextIndex() < itemPosition)
                    assertTrue(sv.compareTo(SemanticVersion.valueOf(iterator.next())) > 0);
                else if (iterator.nextIndex() == itemPosition)
                    assertTrue(sv.compareTo(SemanticVersion.valueOf(iterator.next())) == 0);
                else if (iterator.nextIndex() > itemPosition)
                    assertTrue(sv.compareTo(SemanticVersion.valueOf(iterator.next())) < 0);
            }
        }

        @ParameterizedTest(name = "Comparator.naturalOrder([...])")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownOrderedSemanticVersions")
        void comparatorNaturalOrder(String version) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            Comparator<SemanticVersion> comparator = Comparator.naturalOrder();

            // The version parameter is just one value picked from the following array so we can test the order of that
            // element throughout the entire array.
            List<String> versionsList = Arrays.<String>asList(SemanticVersionTests.wellKnownOrderedSemanticVersionsArray);
            int itemPosition = versionsList.indexOf(version);
            // double check the given item is in the list
            assumeTrue(versionsList.contains(version));
            assumeTrue(itemPosition >= 0);

            ListIterator<String> iterator = versionsList.listIterator();
            while (iterator.hasNext()) {
                if (iterator.nextIndex() < itemPosition)
                    assertTrue(comparator.compare(sv, SemanticVersion.valueOf(iterator.next())) > 0);
                else if (iterator.nextIndex() == itemPosition)
                    assertTrue(comparator.compare(sv, SemanticVersion.valueOf(iterator.next())) == 0);
                else if (iterator.nextIndex() > itemPosition)
                    assertTrue(comparator.compare(sv, SemanticVersion.valueOf(iterator.next())) < 0);
            }
        }

        @Test
        @DisplayName("Collections.sort()")
        void collectionsSort() {
            List<SemanticVersion> orderedlist = new ArrayList<SemanticVersion>();
            for (String s: SemanticVersionTests.wellKnownOrderedSemanticVersionsArray)
                orderedlist.add(SemanticVersion.valueOf(s));

            List<SemanticVersion> shuffledlist = new ArrayList<SemanticVersion>();
            for (String s: SemanticVersionTests.wellKnownOrderedSemanticVersionsArray)
                shuffledlist.add(SemanticVersion.valueOf(s));
            Collections.shuffle(shuffledlist);

            // double check that the two lists have the same elements but in different order
            assumeTrue(shuffledlist.containsAll(orderedlist));
            assumeTrue(orderedlist.containsAll(shuffledlist));
            assumeTrue(orderedlist.size() == shuffledlist.size());
            assumeFalse(orderedlist == shuffledlist);
            assumeFalse(orderedlist.equals(shuffledlist));

            // now sort the shuffled list
            Collections.sort(shuffledlist);

            assertTrue(shuffledlist.containsAll(orderedlist));
            assertTrue(orderedlist.containsAll(shuffledlist));
            assertEquals(orderedlist.size(), shuffledlist.size());
            assertFalse(orderedlist == shuffledlist);
            assertTrue(orderedlist.equals(shuffledlist));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bump")
    class BumpErrorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingBumpWithEmptyString(String bump) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingBumpWithNullString(String bump) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingBumpWithNullOrEmptyString(String bump) {
            assertThrows(RuntimeException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(''{1}'') == ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBumpAttributes")
        void bumpWithValidAttribute(String version, String bumpId, String expectedOutcomeFromBump, String expectedOutcomeFromBumpPrerelease) {
            assertEquals(expectedOutcomeFromBump, SemanticVersion.valueOf(version).bump(bumpId).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(''{1}'') throws ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidBumpAttributes")
        void exceptionUsingBumpWithInvalidAttribute(String version, String bumpId, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> SemanticVersion.valueOf(version).bump(bumpId));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bumpMajor")
    class BumpMajorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bumpMajor() == ''{1}+1.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMajor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpMajor();

            assertEquals(major+1, sv2.getMajor());
            assertEquals(0, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(CoreIdentifiers.MAJOR) == ''{1}+1.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMajorWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.MAJOR);

            assertEquals(major+1, sv2.getMajor());
            assertEquals(0, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(''major'') == ''{1}+1.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMajorWithString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump("major");

            assertEquals(major+1, sv2.getMajor());
            assertEquals(0, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bumpMinor")
    class BumpMinorTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bumpMinor() == ''{1}.{2}+1.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMinor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpMinor();

            assertEquals(major, sv2.getMajor());
            assertEquals(minor+1, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(CoreIdentifiers.MINOR) == ''{1}.{2}+1.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMinorWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.MINOR);

            assertEquals(major, sv2.getMajor());
            assertEquals(minor+1, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(''minor'') == ''{1}.{2}+1.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMinorWithString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump("minor");

            assertEquals(major, sv2.getMajor());
            assertEquals(minor+1, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bumpPatch")
    class BumpPatchTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bumpPatch() == ''{1}.{2}.{3}+1''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatch(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpPatch();

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(CoreIdentifiers.PATCH) == ''{1}.{2}.{3}+1''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatchWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.PATCH);

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bump(''patch'') == ''{1}.{2}.{3}+1''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatchWithString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump("patch");

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bumpPrerelease")
    class BumpPrereleaseTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bumpPrerelease(''{1}'') == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBumpAttributes")
        void bumpPrereleaseWithValidAttribute(String version, String bumpId, String expectedOutcomeFromBump, String expectedOutcomeFromBumpPrerelease) {
            assertEquals(expectedOutcomeFromBumpPrerelease, SemanticVersion.valueOf(version).bumpPrerelease(bumpId).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').bumpPrerelease(''{1}'') throws ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidBumpAttributes")
        void exceptionUsingBumpPrereleaseWithInvalidAttribute(String version, String bumpId, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> SemanticVersion.valueOf(version).bumpPrerelease(bumpId));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.hasPrereleaseAttribute")
    class HasPrereleaseAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'').hasPrereleaseAttribute(''{1}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void hasPrereleaseAttributeWithValidStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertTrue(SemanticVersion.valueOf(version).setPrereleaseAttribute(name).hasPrereleaseAttribute(name));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'').hasPrereleaseAttribute(''X'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void hasPrereleaseAttributesWithValidCharacter(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            // try with a short (1 char) string that is surely missing from the original string
            assumeFalse(version.contains("X"));
            assertFalse(SemanticVersion.valueOf(version).setPrereleaseAttribute(name).hasPrereleaseAttribute("X"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'').hasPrereleaseAttribute(''mockidentifier'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void hasPrereleaseAttributeWithValidMockIdentifier(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assumeFalse(version.contains("mockidentifier"));
            assertFalse(SemanticVersion.valueOf(version).setPrereleaseAttribute(name).hasPrereleaseAttribute("mockidentifier"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').hasPrereleaseAttribute(''{1}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void falseUsingHasPrereleaseAttributeWithNullString(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertFalse(SemanticVersion.valueOf(version).hasPrereleaseAttribute(null));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').hasPrereleaseAttribute(''{1}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void falseUsingHasPrereleaseAttributeWithEmptyString(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertFalse(SemanticVersion.valueOf(version).hasPrereleaseAttribute(""));
            assertFalse(SemanticVersion.valueOf(version).hasPrereleaseAttribute("   "));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPrereleaseAttribute")
    class GetPrereleaseAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'', , ''{2}'').getPrereleaseAttributeValue(''{1}'') == , ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void getPrereleaseAttributeValueWithValidStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertEquals(value, SemanticVersion.valueOf(version).setPrereleaseAttribute(name, value).getPrereleaseAttributeValue(name));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'').getPrereleaseAttributeValue(''X'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void getPrereleaseAttributeValueWithValidCharacter(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            // try with a short (1 char) string that is surely missing from the original string
            assumeFalse(version.contains("X"));
            assertNull(SemanticVersion.valueOf(version).setPrereleaseAttribute(name).getPrereleaseAttributeValue("X"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'').getPrereleaseAttributeValue(''mockidentifier'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void getPrereleaseAttributeValueWithValidMockIdentifier(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assumeFalse(version.contains("mockidentifier"));
            assertNull(SemanticVersion.valueOf(version).setPrereleaseAttribute(name).getPrereleaseAttributeValue("mockidentifier"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getPrereleaseAttributeValue(''{1}'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void nullUsingGetPrereleaseAttributeValueWithNullString(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertNull(SemanticVersion.valueOf(version).getPrereleaseAttributeValue(null));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getPrereleaseAttributeValue(''{1}'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void nullUsingGetPrereleaseAttributeValueWithEmptyString(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertNull(SemanticVersion.valueOf(version).getPrereleaseAttributeValue(""));
            assertNull(SemanticVersion.valueOf(version).getPrereleaseAttributeValue("   "));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setPrereleaseAttribute")
    class SetPrereleaseAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'') == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void setPrereleaseAttributeWithValidStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertEquals(expectedOutcomeFromSetPrereleaseAttributeWithoutValue, SemanticVersion.valueOf(version).setPrereleaseAttribute(name).toString()); // this overloaded version is a shorthand for the next one, but test both
            assertEquals(expectedOutcomeFromSetPrereleaseAttributeWithoutValue, SemanticVersion.valueOf(version).setPrereleaseAttribute(name, null).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'', ''{2}'') == ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void setPrereleaseAttributeWithValidStringNameAndValue(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertEquals(expectedOutcomeFromSetPrereleaseAttributeWithValue, SemanticVersion.valueOf(version).setPrereleaseAttribute(name, value).toString());
        }
        
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'') throws ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidPrereleaseAttributes")
        void exceptionUsingSetPrereleaseAttributeWithInvalidStringName(String version, String name, Integer value, Class<? extends Exception> expectedExceptionWithoutValue, Class<? extends Exception> expectedExceptionWithValue) {
            if (expectedExceptionWithoutValue == null) {
                // If no exception is expected just test it anyway, just expecting no exception to be thrown
                SemanticVersion.valueOf(version).setPrereleaseAttribute(name); // this overloaded version is a shorthand for the next one, but test both
                SemanticVersion.valueOf(version).setPrereleaseAttribute(name, null);
            }
            else {
                assertThrows(expectedExceptionWithoutValue, () -> SemanticVersion.valueOf(version).setPrereleaseAttribute(name)); // this overloaded version is a shorthand for the next one, but test both
                assertThrows(expectedExceptionWithoutValue, () -> SemanticVersion.valueOf(version).setPrereleaseAttribute(name, null));
            }
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''{1}'', ''{2}'') throws ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidPrereleaseAttributes")
        void exceptionUsingSetPrereleaseAttributeWithInvalidStringNameAndValue(String version, String name, Integer value, Class<? extends Exception> expectedExceptionWithoutValue, Class<? extends Exception> expectedExceptionWithValue) {
            if (expectedExceptionWithValue == null) {
                // If no exception is expected just test it anyway, just expecting no exception to be thrown
                SemanticVersion.valueOf(version).setPrereleaseAttribute(name, value);
            }
            else {
                assertThrows(expectedExceptionWithValue, () -> SemanticVersion.valueOf(version).setPrereleaseAttribute(name, value));
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.removePrereleaseAttribute")
    class RemovePrereleaseAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''random'').removePrereleaseAttribute(''random'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void removePrereleaseAttributeWithValidStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            SemanticVersion sv2 = sv1.setPrereleaseAttribute(randomName); // now add the random string as Prerelease attribute
            assumeTrue(sv2.toString().contains(randomName)); // check that the attribute is there

            SemanticVersion sv3 = sv2.removePrereleaseAttribute(randomName, false); // remove the attribute only
            assertFalse(sv3.toString().contains(randomName));

            SemanticVersion sv4 = sv2.removePrereleaseAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv4.toString().contains(randomName));

            // now check that the two version, removing the value and not removing it, are exactly the same
            assertEquals(sv3, sv4);
            assertEquals(sv3.toString(), sv4.toString());

            // now check that the original version and the last one are equal
            assertEquals(sv1, sv3);
            assertEquals(sv1.toString(), sv3.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removePrereleaseAttribute(''random'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void removeMissingPrereleaseAttributeWithValidStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            
            SemanticVersion sv2 = sv1.removePrereleaseAttribute(randomName, false); // remove the attribute only
            assertFalse(sv2.toString().contains(randomName));

            SemanticVersion sv3 = sv1.removePrereleaseAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv3.toString().contains(randomName));

            // now check that the three versions are the same
            assertEquals(sv1, sv2);
            assertEquals(sv1, sv3);
            assertEquals(sv1.toString(), sv2.toString());
            assertEquals(sv1.toString(), sv3.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setPrereleaseAttribute(''random'', ''random'').removePrereleaseAttribute(''random'', true) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void removePrereleaseAttributeWithValidStringNameAndValue(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);
            Integer randomValue = new Random().nextInt();
            if (randomValue.intValue()<0) // make sure it's positive
                randomValue = randomValue*-1;

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            assumeFalse(sv1.toString().contains(randomValue.toString())); // check that the random string is not there
            SemanticVersion sv2 = sv1.setPrereleaseAttribute(randomName, randomValue); // now add the random string as Prerelease attribute and its value

            assumeTrue(sv2.toString().contains(randomName)); // check that the attribute is there
            assumeTrue(sv2.toString().contains(randomValue.toString())); // check that the attribute is there

            SemanticVersion sv3 = sv2.removePrereleaseAttribute(randomName, false); // remove the attribute only
            assertFalse(sv3.toString().contains(randomName));
            assertTrue(sv3.toString().contains(randomValue.toString())); // the value must be still there

            SemanticVersion sv4 = sv2.removePrereleaseAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv4.toString().contains(randomName));
            assertFalse(sv4.toString().contains(randomValue.toString())); // the value must be gone

            // now check that the two version, removing the value and not removing it, are different
            assertNotEquals(sv3, sv4);
            assertNotEquals(sv3.toString(), sv4.toString());

            // now check that the original version and the last one are equal
            assertEquals(sv1, sv4);
            assertEquals(sv1.toString(), sv4.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removePrereleaseAttribute('''', true/false) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void removePrereleaseAttributeWithEmptyStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute("", false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute("", true).toString());

            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute("  ", false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute("  ", true).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removePrereleaseAttribute(null, true/false) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidPrereleaseAttributes")
        void removePrereleaseAttributeWithNullStringName(String version, String name, Integer value, String expectedOutcomeFromSetPrereleaseAttributeWithoutValue, String expectedOutcomeFromSetPrereleaseAttributeWithValue) {
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute(null, false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removePrereleaseAttribute(null, true).toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.hasBuildAttribute")
    class HasBuildAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'').hasBuildAttribute(''{1}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void hasBuildAttributeWithValidStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertTrue(SemanticVersion.valueOf(version).setBuildAttribute(name).hasBuildAttribute(name));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'').hasBuildAttribute(''X'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void hasBuildAttributesWithValidCharacter(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            // try with a short (1 char) string that is surely missing from the original string
            assumeFalse(version.contains("X"));
            assertFalse(SemanticVersion.valueOf(version).setBuildAttribute(name).hasBuildAttribute("X"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'').hasBuildAttribute(''mockidentifier'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void hasBuildAttributeWithValidMockIdentifier(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assumeFalse(version.contains("mockidentifier"));
            assertFalse(SemanticVersion.valueOf(version).setBuildAttribute(name).hasBuildAttribute("mockidentifier"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').hasBuildAttribute(''{1}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void falseUsingHasBuildAttributeWithNullString(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertFalse(SemanticVersion.valueOf(version).hasBuildAttribute(null));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').hasBuildAttribute(''{1}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void falseUsingHasBuildAttributeWithEmptyString(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertFalse(SemanticVersion.valueOf(version).hasBuildAttribute(""));
            assertFalse(SemanticVersion.valueOf(version).hasBuildAttribute("   "));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getBuildAttribute")
    class GetBuildAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'', , ''{2}'').getBuildAttributeValue(''{1}'') == , ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void getBuildAttributeValueWithValidStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertEquals(value, SemanticVersion.valueOf(version).setBuildAttribute(name, value).getBuildAttributeValue(name));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'').getBuildAttributeValue(''X'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void getBuildAttributeValueWithValidCharacter(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            // try with a short (1 char) string that is surely missing from the original string
            assumeFalse(version.contains("X"));
            assertNull(SemanticVersion.valueOf(version).setBuildAttribute(name).getBuildAttributeValue("X"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'').getBuildAttributeValue(''mockidentifier'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void getBuildAttributeValueWithValidMockIdentifier(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assumeFalse(version.contains("mockidentifier"));
            assertNull(SemanticVersion.valueOf(version).setBuildAttribute(name).getBuildAttributeValue("mockidentifier"));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getBuildAttributeValue(''{1}'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void nullUsingGetBuildAttributeValueWithNullString(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertNull(SemanticVersion.valueOf(version).getBuildAttributeValue(null));
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').getBuildAttributeValue(''{1}'') == null")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void nullUsingGetBuildAttributeValueWithEmptyString(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertNull(SemanticVersion.valueOf(version).getBuildAttributeValue(""));
            assertNull(SemanticVersion.valueOf(version).getBuildAttributeValue("   "));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.setBuildAttribute")
    class SetBuildAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'') == ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void setBuildAttributeWithValidStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertEquals(expectedOutcomeFromSetBuildAttributeWithoutValue, SemanticVersion.valueOf(version).setBuildAttribute(name).toString()); // this overloaded version is a shorthand for the next one, but test both
            assertEquals(expectedOutcomeFromSetBuildAttributeWithoutValue, SemanticVersion.valueOf(version).setBuildAttribute(name, null).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'', ''{2}'') == ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void setBuildAttributeWithValidStringNameAndValue(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertEquals(expectedOutcomeFromSetBuildAttributeWithValue, SemanticVersion.valueOf(version).setBuildAttribute(name, value).toString());
        }
        
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'') throws ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidBuildAttributes")
        void exceptionUsingSetBuildAttributeWithInvalidStringName(String version, String name, String value, Class<? extends Exception> expectedExceptionWithoutValue, Class<? extends Exception> expectedExceptionWithValue) {
            if (expectedExceptionWithoutValue == null) {
                // If no exception is expected just test it anyway, just expecting no exception to be thrown
                SemanticVersion.valueOf(version).setBuildAttribute(name); // this overloaded version is a shorthand for the next one, but test both
                SemanticVersion.valueOf(version).setBuildAttribute(name, null);
            }
            else {
                assertThrows(expectedExceptionWithoutValue, () -> SemanticVersion.valueOf(version).setBuildAttribute(name)); // this overloaded version is a shorthand for the next one, but test both
                assertThrows(expectedExceptionWithoutValue, () -> SemanticVersion.valueOf(version).setBuildAttribute(name, null));
            }
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''{1}'', ''{2}'') throws ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidBuildAttributes")
        void exceptionUsingSetBuildAttributeWithInvalidStringNameAndValue(String version, String name, String value, Class<? extends Exception> expectedExceptionWithoutValue, Class<? extends Exception> expectedExceptionWithValue) {
            if (expectedExceptionWithValue == null) {
                // If no exception is expected just test it anyway, just expecting no exception to be thrown
                SemanticVersion.valueOf(version).setBuildAttribute(name, value);
            }
            else {
                assertThrows(expectedExceptionWithValue, () -> SemanticVersion.valueOf(version).setBuildAttribute(name, value));
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.removeBuildAttribute")
    class RemoveBuildAttributeTests {
        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''random'').removeBuildAttribute(''random'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void removeBuildAttributeWithValidStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            SemanticVersion sv2 = sv1.setBuildAttribute(randomName); // now add the random string as build attribute
            assumeTrue(sv2.toString().contains(randomName)); // check that the attribute is there

            SemanticVersion sv3 = sv2.removeBuildAttribute(randomName, false); // remove the attribute only
            assertFalse(sv3.toString().contains(randomName));

            SemanticVersion sv4 = sv2.removeBuildAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv4.toString().contains(randomName));

            // now check that the two version, removing the value and not removing it, are exactly the same
            assertEquals(sv3, sv4);
            assertEquals(sv3.toString(), sv4.toString());

            // now check that the original version and the last one are equal
            assertEquals(sv1, sv3);
            assertEquals(sv1.toString(), sv3.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removeBuildAttribute(''random'') == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void removeMissingBuildAttributeWithValidStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            
            SemanticVersion sv2 = sv1.removeBuildAttribute(randomName, false); // remove the attribute only
            assertFalse(sv2.toString().contains(randomName));

            SemanticVersion sv3 = sv1.removeBuildAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv3.toString().contains(randomName));

            // now check that the three versions are the same
            assertEquals(sv1, sv2);
            assertEquals(sv1, sv3);
            assertEquals(sv1.toString(), sv2.toString());
            assertEquals(sv1.toString(), sv3.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').setBuildAttribute(''random'', ''random'').removeBuildAttribute(''random'', true) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void removeBuildAttributeWithValidStringNameAndValue(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            // to avoid any potential conflicts with existing strings in the passed version we generate another random string
            String randomName = randomAlphabeticString(5);
            String randomValue = randomAlphabeticString(8);

            SemanticVersion sv1 = SemanticVersion.valueOf(version); // first generate the plain version
            assumeFalse(sv1.toString().contains(randomName)); // check that the random string is not there
            assumeFalse(sv1.toString().contains(randomValue)); // check that the random string is not there
            SemanticVersion sv2 = sv1.setBuildAttribute(randomName, randomValue); // now add the random string as build attribute and its value
            assumeTrue(sv2.toString().contains(randomName)); // check that the attribute is there
            assumeTrue(sv2.toString().contains(randomValue)); // check that the attribute is there

            SemanticVersion sv3 = sv2.removeBuildAttribute(randomName, false); // remove the attribute only
            assertFalse(sv3.toString().contains(randomName));
            assertTrue(sv3.toString().contains(randomValue)); // the value must be still there

            SemanticVersion sv4 = sv2.removeBuildAttribute(randomName, true); // also remove the value (which is not present)
            assertFalse(sv4.toString().contains(randomName));
            assertFalse(sv4.toString().contains(randomValue)); // the value must be gone

            // now check that the two version, removing the value and not removing it, are different
            assertNotEquals(sv3, sv4);
            assertNotEquals(sv3.toString(), sv4.toString());

            // now check that the original version and the last one are equal
            assertEquals(sv1, sv4);
            assertEquals(sv1.toString(), sv4.toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removeBuildAttribute('''', true/false) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void removeBuildAttributeWithEmptyStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute("", false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute("", true).toString());

            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute("  ", false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute("  ", true).toString());
        }

        @ParameterizedTest(name = "Semanticversion.valueOf(''{0}'').removeBuildAttribute(null, true/false) == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidBuildAttributes")
        void removeBuildAttributeWithNullStringName(String version, String name, String value, String expectedOutcomeFromSetBuildAttributeWithoutValue, String expectedOutcomeFromSetBuildAttributeWithValue) {
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute(null, false).toString());
            assertEquals(SemanticVersion.valueOf(version).toString(), SemanticVersion.valueOf(version).removeBuildAttribute(null, true).toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getIdentifierComparator")
    class GetIdentifierComparatorTests {
        @Test
        @DisplayName("Semanticversion.getIdentifierComparator()")
        void getIdentifierComparator() {
            List<String> identifiers = new ArrayList<String>() {
                private static final long serialVersionUID = 1L;
                {
                    add("alpha");
                    add("beta");
                    add("minor");
                    add("patch");
                    add("gamma");
                    add("gamma");
                    add("minor");
                    add("major");
                    add("theta");
                    add("patch");
                    add("major");
                    add("epsylon");
                }
            };
            Collections.sort(identifiers, SemanticVersion.getIdentifierComparator());
            assertEquals("major",   identifiers.get(0));
            assertEquals("major",   identifiers.get(1));
            assertEquals("minor",   identifiers.get(2));
            assertEquals("minor",   identifiers.get(3));
            assertEquals("patch",   identifiers.get(4));
            assertEquals("patch",   identifiers.get(5));
            assertEquals("alpha",   identifiers.get(6));
            assertEquals("beta",    identifiers.get(7));
            assertEquals("epsylon", identifiers.get(8));
            assertEquals("gamma",   identifiers.get(9));
            assertEquals("gamma",   identifiers.get(10));
            assertEquals("theta",   identifiers.get(11));
        }
    }
}
