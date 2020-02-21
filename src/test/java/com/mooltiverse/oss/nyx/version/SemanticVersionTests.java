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
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("SemanticVersion")
public class SemanticVersionTests {
    /**
     * A {@link MethodSource} method that returns structured data to test semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version
     * - major: the major number
     * - minor: the minor number
     * - patch: the patch number
     * - prerelease: an (optional) List of strings, each representing one identifier in the prerelease part
     * - build: an (optional) List of strings, each representing one identifier in the build part
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
     * A {@link MethodSource} method that returns structured data to test semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the version
     *
     * @return a stream of arguments representing incorrect versions
     */
    static Stream<Arguments> wellKnownInvalidVersions() {
        return Stream.of(
            // This list is taken from https://regex101.com/r/vkijKf/1/, linked from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
            arguments("1"),
            arguments("1.2"),
            arguments("1.2.3-0123"),
            arguments("1.2.3-0123.0123"),
            arguments("1.1.2+.123"),
            arguments("+invalid"),
            arguments("-invalid"),
            arguments("-invalid+invalid"),
            arguments("-invalid.01"),
            arguments("alpha"),
            arguments("alpha.beta"),
            arguments("alpha.beta.1"),
            arguments("alpha.1"),
            arguments("alpha+beta"),
            arguments("alpha_beta"),
            arguments("alpha."),
            arguments("alpha.."),
            arguments("beta"),
            arguments("1.0.0-alpha_beta"),
            arguments("-alpha."),
            arguments("1.0.0-alpha.."),
            arguments("1.0.0-alpha..1"),
            arguments("1.0.0-alpha...1"),
            arguments("1.0.0-alpha....1"),
            arguments("1.0.0-alpha.....1"),
            arguments("1.0.0-alpha......1"),
            arguments("1.0.0-alpha.......1"),
            arguments("01.1.1"),
            arguments("1.01.1"),
            arguments("1.1.01"),
            arguments("1.2"),
            arguments("1.2.3.DEV"),
            arguments("1.2-SNAPSHOT"),
            arguments("1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788"),
            arguments("1.2-RC-SNAPSHOT"),
            arguments("-1.0.3-gamma+b7718"),
            arguments("+justmeta"),
            arguments("9.8.7+meta+meta"),
            arguments("9.8.7-whatever+meta+meta"),
            arguments("99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12")
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test semantic versions.
     * Each returned argument has the fields:<br>
     * - version: the entire string representation of the original version
     * - prefix: if the original version has a prefix, it's the expected outcome of the invocation of the getPrefix() method, otherwise null means that the version has no prefix and getPrefix() is expected to return null
     * - sanitizedOutcome: the expected outcome of the entire sanitization over the original string or the name of the class of the expected exception, if the method is expected to throw one
     * - sanitizedPrefixOutcome: the expected outcome of sanitizePrefix() over the original string or the name of the class of the expected exception, if the method is expected to throw one
     * - sanitizedNumberOutcome: the expected outcome of sanitizeNumbers() over the original string or the name of the class of the expected exception, if the method is expected to throw one
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
     * An ordered list of valid version strings (according to SemVer).
     *
     * @return an ordered list of valid version strings (according to SemVer)
     *
     * @see SemanticVersion#compareTo(SemanticVersion)
     * @see SemanticVersion#equals(Object)
     */
    static String[] wellOrderedVersionsArray = new String[] {
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
     * A {@link MethodSource} method that returns the stream of ordered versions.
     *
     * @return a stream of ordered versions.
     *
     * @see #wellOrderedVersionsArray
     */
    static Stream<String> wellOrderedVersions() {
        return Stream.of(wellOrderedVersionsArray);
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

    @Nested
    @DisplayName("SemanticVersion.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "#{index} valueOf(''{arguments}'') ==> IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "#{index} valueOf(''{arguments}'') ==> NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "#{index} valueOf(''{arguments}'') ==> RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String version) {
            assertThrows(RuntimeException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "#{index} valueOf(''{arguments}'') ==> IllegalArgumentException")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void exceptionUsingValueOfWithInvalidVersion(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(version));
        }

        @ParameterizedTest(name = "#{index} valueOf(''{0}'') ==> ''{arguments}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));
            String preString = identifiersToString(pre);
            String buildString = identifiersToString(build);

            SemanticVersion sv = SemanticVersion.valueOf(version);

            assertEquals(version, sv.toString());
            assertEquals(major, sv.getMajor());
            assertEquals(minor, sv.getMinor());
            assertEquals(patch, sv.getPatch());

            assertEquals(coreString, sv.getCore());
            assertEquals(preString, sv.getPrerelease());
            assertEquals(buildString, sv.getBuild());
        }

        @ParameterizedTest(name = "#{index} valueOf(''{0}'', true) ==> ''{arguments}''")
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
        @ParameterizedTest(name = "#{index} getMajor(''{0}'') ==> ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getMajor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(major, SemanticVersion.valueOf(version).getMajor());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getMinor")
    class GetMinorTests {
        @ParameterizedTest(name = "#{index} getMinor(''{0}'') ==> ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getMinor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(minor, SemanticVersion.valueOf(version).getMinor());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPatch")
    class GetPatchTests {
        @ParameterizedTest(name = "#{index} getPatch(''{0}'') ==> ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getPatch(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(patch, SemanticVersion.valueOf(version).getPatch());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getCore")
    class GetCoreTests {
        @ParameterizedTest(name = "#{index} getCore(''{0}'') ==> ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getCore(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String coreString = identifiersToString(List.of(String.valueOf(major), String.valueOf(minor), String.valueOf(patch)));
            assertEquals(coreString, SemanticVersion.valueOf(version).getCore());
        }

        @ParameterizedTest(name = "#{index} getCoreIdentifiers(''{0}'') ==> ''{1}.{2}.{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getCoreIdentifiers(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(3, SemanticVersion.valueOf(version).getCoreIdentifiers().length);
            assertEquals(Integer.valueOf(major), SemanticVersion.valueOf(version).getCoreIdentifiers()[0]);
            assertEquals(Integer.valueOf(minor), SemanticVersion.valueOf(version).getCoreIdentifiers()[1]);
            assertEquals(Integer.valueOf(patch), SemanticVersion.valueOf(version).getCoreIdentifiers()[2]);
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPrerelease")
    class GetgetPrereleaseTests {
        @ParameterizedTest(name = "#{index} getPrerelease(''{0}'') ==> ''{4}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getPrerelease(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String preString = identifiersToString(pre);
            assertEquals(preString, SemanticVersion.valueOf(version).getPrerelease());
        }

        @ParameterizedTest(name = "#{index} getPrereleaseIdentifiers(''{0}'') ==> ''{4}''")
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
    @DisplayName("SemanticVersion.getBuild")
    class GetBuildTests {
        @ParameterizedTest(name = "#{index} getBuild(''{0}'') ==> ''{5}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void getBuild(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            String buildString = identifiersToString(build);
            assertEquals(buildString, SemanticVersion.valueOf(version).getBuild());
        }

        @ParameterizedTest(name = "#{index} getBuildIdentifiers(''{0}'') ==> ''{4}''")
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
    @DisplayName("SemanticVersion.toString")
    class ToStringTests {
        @ParameterizedTest(name = "#{index} toString(''{0}'') ==> ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void toString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.valueOf(version).toString());
        }
    }

    @Nested
    @DisplayName("SemanticVersion.sanitize")
    class SanitizeTests {
        @ParameterizedTest(name = "#{index} sanitize(''{arguments}'') ==> IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizeWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "#{index} sanitizePrefix(''{arguments}'') ==> IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizePrefixWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "#{index} sanitizeNumbers(''{arguments}'') ==> IllegalArgumentException")
        @EmptySource
        void exceptionUsingSanitizeNumbersWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "#{index} sanitize(''{arguments}'') ==> NullPointerException")
        @NullSource
        void exceptionUsingSanitizeWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "#{index} sanitizePrefix(''{arguments}'') ==> NullPointerException")
        @NullSource
        void exceptionUsingSanitizePrefixWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "#{index} sanitizeNumbers(''{arguments}'') ==> NullPointerException")
        @NullSource
        void exceptionUsingSanitizeNumbersWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "#{index} sanitize(''{0}'') ==> ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizeWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitize(version));
        }

        @ParameterizedTest(name = "#{index} sanitizeNumbers(''{0}'') ==> ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizeNumbersWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitizeNumbers(version));
        }

        @ParameterizedTest(name = "#{index} sanitizePrefix(''{0}'') ==> ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void sanitizePrefixWithValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, SemanticVersion.sanitizePrefix(version));
        }

        @ParameterizedTest(name = "#{index} sanitize(''{0}'') ==> ''{2}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        @SuppressWarnings("unchecked")
        void sanitize(String version, String expectedPrefix, String expectedSanitizeOutcomeOrException, String expectedSanitizePrefixOutcomeOrException, String expectedSanitizeNumberOutcomeOrException) {
            // see if the excepted outcome is an exception
            Class expectedException = null;
            try {
                expectedException = Class.forName(expectedSanitizeOutcomeOrException);
            }
            catch (ClassNotFoundException cnfe) {
                // the expected outcome is a string value, not an exception
            }

            if (expectedException == null) {
                assertEquals(expectedSanitizeOutcomeOrException, SemanticVersion.sanitize(version));
            }
            else {
                assertThrows(expectedException, () -> SemanticVersion.sanitize(version));
            }
        }

        @ParameterizedTest(name = "#{index} sanitizeNumbers(''{0}'') ==> ''{5}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        @SuppressWarnings("unchecked")
        void sanitizeNumbers(String version, String expectedPrefix, String expectedSanitizeOutcomeOrException, String expectedSanitizePrefixOutcomeOrException, String expectedSanitizeNumberOutcomeOrException) {
            // see if the excepted outcome is an exception
            Class expectedException = null;
            try {
                expectedException = Class.forName(expectedSanitizeNumberOutcomeOrException);
            }
            catch (ClassNotFoundException cnfe) {
                // the expected outcome is a string value, not an exception
            }

            if (expectedException == null) {
                assertEquals(expectedSanitizeNumberOutcomeOrException, SemanticVersion.sanitizeNumbers(version));
            }
            else {
                assertThrows(expectedException, () -> SemanticVersion.sanitizeNumbers(version));
            }
        }

        @ParameterizedTest(name = "#{index} sanitizePrefix(''{0}'') ==> ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        @SuppressWarnings("unchecked")
        void sanitizePrefix(String version, String expectedPrefix, String expectedSanitizeOutcomeOrException, String expectedSanitizePrefixOutcomeOrException, String expectedSanitizeNumberOutcomeOrException) {
            // see if the excepted outcome is an exception
            Class expectedException = null;
            try {
                expectedException = Class.forName(expectedSanitizePrefixOutcomeOrException);
            }
            catch (ClassNotFoundException cnfe) {
                // the expected outcome is a string value, not an exception
            }

            if (expectedException == null) {
                assertEquals(expectedSanitizePrefixOutcomeOrException, SemanticVersion.sanitizePrefix(version));
            }
            else {
                assertThrows(expectedException, () -> SemanticVersion.sanitizePrefix(version));
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.getPrefix")
    class GetPrefixTests {
        @ParameterizedTest(name = "#{index} getPrefix(''{0}'') ==> ''{1}'' or ''{3}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        @SuppressWarnings("unchecked")
        void getPrefix(String version, String expectedPrefix, String expectedSanitizeOutcomeOrException, String expectedSanitizePrefixOutcomeOrException, String expectedSanitizeNumberOutcomeOrException) {
            // see if the excepted outcome is an exception
            Class expectedException = null;
            try {
                expectedException = Class.forName(expectedSanitizePrefixOutcomeOrException);
            }
            catch (ClassNotFoundException cnfe) {
                // the expected outcome is a string value, not an exception
            }

            if (expectedException == null) {
                assertEquals(expectedPrefix, SemanticVersion.getPrefix(version));
            }
            else {
                assertThrows(expectedException, () -> SemanticVersion.getPrefix(version));
            }
        }
    }

    @Nested
    @DisplayName("SemanticVersion.equals")
    class EqualsTests {
        @ParameterizedTest(name = "#{index} equals(''{0}'', null) ==> false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToNull(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertFalse(sv.equals(null));
            assertNotEquals(sv, null);
        }

        @ParameterizedTest(name = "#{index} equals(''{0}'', '''') ==> false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToEmptyString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertFalse(sv.equals(""));
            assertNotEquals(sv, "");
        }

        @ParameterizedTest(name = "#{index} equals(''{0}'', ''{0}'') ==> true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void equalsToSameInstance(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            assertTrue(sv.equals(sv));
            assertEquals(sv, sv);
        }

        @ParameterizedTest(name = "#{index} equals(''{0}'', ''{0}'') ==> true")
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
        @ParameterizedTest(name = "#{index} compareTo(''{0}'', [...])")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellOrderedVersions")
        void compareTo(String version) {
            SemanticVersion sv = SemanticVersion.valueOf(version);

            // The version parameter is just one value picked from the following array so we can test the order of that
            // element throughout the entire array.
            List<String> versionsList = Arrays.<String>asList(SemanticVersionTests.wellOrderedVersionsArray);
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

        @ParameterizedTest(name = "#{index} ''{0}'' Comparator.naturalOrder()")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellOrderedVersions")
        void comparatorNaturalOrder(String version) {
            SemanticVersion sv = SemanticVersion.valueOf(version);
            Comparator<SemanticVersion> comparator = Comparator.naturalOrder();

            // The version parameter is just one value picked from the following array so we can test the order of that
            // element throughout the entire array.
            List<String> versionsList = Arrays.<String>asList(SemanticVersionTests.wellOrderedVersionsArray);
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
            for (String s: SemanticVersionTests.wellOrderedVersionsArray)
                orderedlist.add(SemanticVersion.valueOf(s));

            List<SemanticVersion> shuffledlist = new ArrayList<SemanticVersion>();
            for (String s: SemanticVersionTests.wellOrderedVersionsArray)
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
        @ParameterizedTest(name = "#{index} bump(''{arguments}'') ==> IllegalArgumentException")
        @EmptySource
        void exceptionUsingBumpWithEmptyString(String bump) {
            assertThrows(IllegalArgumentException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }

        @ParameterizedTest(name = "#{index} bump(''{arguments}'') ==> IllegalArgumentException")
        @NullSource
        void exceptionUsingBumpWithNullString(String bump) {
            assertThrows(NullPointerException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }

        @ParameterizedTest(name = "#{index} bump(''{arguments}'') ==> RuntimeException")
        @NullAndEmptySource
        void exceptionUsingBumpWithNullOrEmptyString(String bump) {
            assertThrows(RuntimeException.class, () -> SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION).bump(bump));
        }
    }

    @Nested
    @DisplayName("SemanticVersion.bumpMajor")
    class BumpMajorTests {
        @ParameterizedTest(name = "#{index} bumpMajor(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMajor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpMajor();

            assertEquals(major+1, sv2.getMajor());
            assertEquals(0, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''MAJOR'', ''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMajorWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.MAJOR);

            assertEquals(major+1, sv2.getMajor());
            assertEquals(0, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''major'', ''{0}'')")
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
        @ParameterizedTest(name = "#{index} bumpMinor(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMinor(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpMinor();

            assertEquals(major, sv2.getMajor());
            assertEquals(minor+1, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''MINOR'', ''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpMinorWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.MINOR);

            assertEquals(major, sv2.getMajor());
            assertEquals(minor+1, sv2.getMinor());
            assertEquals(0, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''minor'', ''{0}'')")
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
        @ParameterizedTest(name = "#{index} bumpPatch(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatch(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bumpPatch();

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''PATCH'', ''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatchWithEnum(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump(CoreIdentifiers.PATCH);

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }

        @ParameterizedTest(name = "#{index} bump(''patch'', ''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void bumpPatchWithString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            SemanticVersion sv1 = SemanticVersion.valueOf(version);
            SemanticVersion sv2 = sv1.bump("patch");

            assertEquals(major, sv2.getMajor());
            assertEquals(minor, sv2.getMinor());
            assertEquals(patch+1, sv2.getPatch());
        }
    }
}
