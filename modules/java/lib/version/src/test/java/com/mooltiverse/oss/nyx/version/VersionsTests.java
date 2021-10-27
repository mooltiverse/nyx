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

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

@DisplayName("Versions")
public class VersionsTests {
    @Nested
    @DisplayName("Versions.defaultInitial")
    class DefaultInitialTests {
        @Test
        @DisplayName("Versions.defaultInitial(null) throws NullPointerException")
        void exceptionUsingDefaultInitialWithNullString() {
            assertThrows(NullPointerException.class, () -> Versions.defaultInitial(null));
        }

        @Test
        @DisplayName("Versions.defaultInitial(Scheme.SEMVER).toString() == "+SemanticVersion.DEFAULT_INITIAL_VERSION)
        void defaultInitial() {
            assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, Versions.defaultInitial(Scheme.SEMVER).toString());
        }
    }

    @Nested
    @DisplayName("Versions.isCore")
    class IsCoreTests {
        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') == false")
        @EmptySource
        void isCoreWithEmptyString(String version) {
            assertFalse(Versions.isCore(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingIsCoreWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> Versions.isCore(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void isCoreWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertFalse(Versions.isCore(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidCoreVersions")
        void isCoreValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(Versions.isCore(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidNonCoreVersions")
        void isNotCoreValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertFalse(Versions.isCore(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidCoreVersions")
        void isCoreValidStringWithPrefix(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(Versions.isCore(Scheme.SEMVER, "".concat(version), null));
            assertTrue(Versions.isCore(Scheme.SEMVER, "".concat(version), ""));
            assertTrue(Versions.isCore(Scheme.SEMVER, "v".concat(version), "v"));
            assertTrue(Versions.isCore(Scheme.SEMVER, "prefix".concat(version), "prefix"));

            assertFalse(Versions.isCore(Scheme.SEMVER, "v".concat(version), null));
            assertFalse(Versions.isCore(Scheme.SEMVER, "v".concat(version), ""));
            assertFalse(Versions.isCore(Scheme.SEMVER, "prefix".concat(version), null));
            assertFalse(Versions.isCore(Scheme.SEMVER, "prefix".concat(version), ""));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidCoreVersions")
        void isCoreSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test invoking isCore with prefix tolerance
            assertTrue(Versions.isCore(Scheme.SEMVER, version, true));
        }

        @ParameterizedTest(name = "Versions.isCore(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableCoreVersions")
        void isCoreSanitizableString(String version) {
            // the method must fail without toleration and succeed when using toleration
            assertFalse(Versions.isCore(Scheme.SEMVER, version));
            assertFalse(Versions.isCore(Scheme.SEMVER, version, false));
            assertTrue(Versions.isCore(Scheme.SEMVER, version, true));
        }
    }

    @Nested
    @DisplayName("Versions.isLegal")
    class IsLegalTests {
        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'') == false")
        @EmptySource
        void isLegalWithEmptyString(String version) {
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingIsLegalWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void isLegalWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalValidStringWithPrefix(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(Versions.isLegal(Scheme.SEMVER, "".concat(version), null));
            assertTrue(Versions.isLegal(Scheme.SEMVER, "".concat(version), ""));
            assertTrue(Versions.isLegal(Scheme.SEMVER, "v".concat(version), "v"));
            assertTrue(Versions.isLegal(Scheme.SEMVER, "prefix".concat(version), "prefix"));

            assertFalse(Versions.isLegal(Scheme.SEMVER, "v".concat(version), null));
            assertFalse(Versions.isLegal(Scheme.SEMVER, "v".concat(version), ""));
            assertFalse(Versions.isLegal(Scheme.SEMVER, "prefix".concat(version), null));
            assertFalse(Versions.isLegal(Scheme.SEMVER, "prefix".concat(version), ""));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test invoking isLegal with prefix tolerance
            assertTrue(Versions.isLegal(Scheme.SEMVER, version, true));
        }

        @ParameterizedTest(name = "Versions.isLegal(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void isLegalSanitizableString(String version) {
            // the method must fail without toleration and succeed when using toleration
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
            assertFalse(Versions.isLegal(Scheme.SEMVER, version, false));
            assertTrue(Versions.isLegal(Scheme.SEMVER, version, true));
        }
    }

    @Nested
    @DisplayName("Versions.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String version) {
            assertThrows(RuntimeException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'') throws ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void exceptionUsingValueOfWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, version).toString());
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'', prefix).toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidStringWithPrefix(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, "".concat(version), null).toString());
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, "".concat(version), "").toString());
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, "v".concat(version), "v").toString());
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, "prefix".concat(version), "prefix").toString());

            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, "v".concat(version), null));
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, "v".concat(version), ""));
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, "prefix".concat(version), null));
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, "prefix".concat(version), ""));
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test that invoking valueOf with sanitization on a valid string returns the same string
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, version, true).toString());
        }

        @ParameterizedTest(name = "Versions.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == SemanticVersion.sanitize(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void valueOfSanitizableString(String version) {
            // the method must fail without sanitization and succeed when using sanitization
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, version, false).toString());
            assertNotEquals(version, Versions.valueOf(Scheme.SEMVER, version, true).toString());
            assertEquals(SemanticVersion.sanitize(version), Versions.valueOf(Scheme.SEMVER, version, true).toString());
        }
    }

    @Nested
    @DisplayName("Versions.mostRelevantIdentifier")
    class SortIdentifiersTests {
        @Test
        @DisplayName("Versions.mostRelevantIdentifier(Scheme.SEMVER, [...])")
        void mostRelevantIdentifier1() {
            List<String> identifiers = new ArrayList<String>();
            assertNull(Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("alpha");
            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("beta");
            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("patch");
            assertEquals("patch", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("minor");
            assertEquals("minor", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("major");
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("gamma");
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("theta");
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));

            identifiers.add("epsylon");
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, identifiers));
        }

        @Test
        @DisplayName("Versions.mostRelevantIdentifier(Scheme.SEMVER, String, String)")
        void mostRelevantIdentifier2() {
            assertNull(Versions.mostRelevantIdentifier(Scheme.SEMVER, null, null));

            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, "alpha", null));
            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, null, "alpha"));
            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, "alpha", "alpha"));

            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, "alpha", "beta"));
            assertEquals("alpha", Versions.mostRelevantIdentifier(Scheme.SEMVER, "beta", "alpha"));

            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, "major", "major"));
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, "major", "minor"));
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, "major", "patch"));
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, "minor", "major"));
            assertEquals("major", Versions.mostRelevantIdentifier(Scheme.SEMVER, "patch", "major"));

            assertEquals("minor", Versions.mostRelevantIdentifier(Scheme.SEMVER, "minor", "minor"));
            assertEquals("minor", Versions.mostRelevantIdentifier(Scheme.SEMVER, "minor", "patch"));
            assertEquals("minor", Versions.mostRelevantIdentifier(Scheme.SEMVER, "patch", "minor"));

            assertEquals("patch", Versions.mostRelevantIdentifier(Scheme.SEMVER, "patch", "patch"));
            assertEquals("patch", Versions.mostRelevantIdentifier(Scheme.SEMVER, "patch", "alpha"));
            assertEquals("patch", Versions.mostRelevantIdentifier(Scheme.SEMVER, "alpha", "patch"));
        }
    }

    @Nested
    @DisplayName("Versions.compare")
    class CompareTests {
        @Test
        @DisplayName("Versions.compare(Scheme.SEMVER, [...])")
        void compareCoreVersions() {
            assertEquals(0, Versions.compare(Scheme.SEMVER, null, null));
            assertEquals(0, Versions.compare(Scheme.SEMVER, null, null, null));
            assertEquals(0, Versions.compare(Scheme.SEMVER, null, null, false));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0", null));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0", false));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.2.3-alpha.1", "1.2.3-alpha.1"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.2.3-alpha.1", "1.2.3-alpha.1", null));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.2.3-alpha.1", "1.2.3-alpha.1", false));

            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", null) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", null, null) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", null, false) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "0.1.0") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "0.1.0", null) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "0.1.0", false) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0-alpha.1") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0-alpha.1", null) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0-alpha.1", false) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.2", "1.0.0-alpha.1") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.2", "1.0.0-alpha.1", null) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.2", "1.0.0-alpha.1", false) > 0);

            assertTrue(Versions.compare(Scheme.SEMVER, null, "1.0.0") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, null, "1.0.0", null) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, null, "1.0.0", false) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.0", "1.0.0") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.0", "1.0.0", null) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.0", "1.0.0", false) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0", null) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0", false) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0-alpha.2") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0-alpha.2", null) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0-alpha.2", false) < 0);
        }

        @Test
        @DisplayName("Versions.compare(Scheme.SEMVER, [...])")
        void compareAndSanitizeCoreVersions() {
            assertEquals(0, Versions.compare(Scheme.SEMVER, null, null, true));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.00.0", "01.0.0", true));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "01.2.3-alpha.1", "1.2.3-alpha.1", true));

            assertTrue(Versions.compare(Scheme.SEMVER, "01.0.0", null, true) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "01.0.0", "0.1.00", true) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "01.0.0", "1.0.00-alpha.1", true) > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.00-alpha.2", "1.0.00-alpha.1", true) > 0);

            assertTrue(Versions.compare(Scheme.SEMVER, null, "01.0.0", true) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.00", "01.0.000", true) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.00.0-alpha.1", "01.0.0", true) < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "01.0.0-alpha.1", "1.0.00-alpha.2", true) < 0);
        }

        @Test
        @DisplayName("Versions.compare(Scheme.SEMVER, [...])")
        void compareCoreVersionsWithPrefix() {
            assertEquals(0, Versions.compare(Scheme.SEMVER, null, null, "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0", "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "rel-1.0.0", "1.0.0", "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.0.0", "rel-1.0.0", "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.2.3-alpha.1", "1.2.3-alpha.1", "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "rel-1.2.3-alpha.1", "1.2.3-alpha.1", "rel-"));
            assertEquals(0, Versions.compare(Scheme.SEMVER, "1.2.3-alpha.1", "rel-1.2.3-alpha.1", "rel-"));

            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", null, "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0", null, "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "0.1.0", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0", "0.1.0", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "rel-0.1.0", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "1.0.0-alpha.1", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0", "1.0.0-alpha.1", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0", "rel-1.0.0-alpha.1", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.2", "1.0.0-alpha.1", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0-alpha.2", "1.0.0-alpha.1", "rel-") > 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.2", "rel-1.0.0-alpha.1", "rel-") > 0);

            assertTrue(Versions.compare(Scheme.SEMVER, null, "1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, null, "rel-1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.0", "1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-0.1.0", "1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "0.1.0", "rel-1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0-alpha.1", "1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "rel-1.0.0", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "1.0.0-alpha.2", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "rel-1.0.0-alpha.1", "1.0.0-alpha.2", "rel-") < 0);
            assertTrue(Versions.compare(Scheme.SEMVER, "1.0.0-alpha.1", "rel-1.0.0-alpha.2", "rel-") < 0);
        }
    }
}
