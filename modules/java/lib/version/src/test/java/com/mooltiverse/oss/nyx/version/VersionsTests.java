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

@DisplayName("VersionFactory")
public class VersionsTests {
    @Nested
    @DisplayName("VersionFactory.defaultInitial")
    class DefaultInitialTests {
        @Test
        @DisplayName("VersionFactory.defaultInitial(null) throws NullPointerException")
        void exceptionUsingDefaultInitialWithNullString() {
            assertThrows(NullPointerException.class, () -> Versions.defaultInitial(null));
        }

        @Test
        @DisplayName("VersionFactory.defaultInitial(Scheme.SEMVER).toString() == "+SemanticVersion.DEFAULT_INITIAL_VERSION)
        void defaultInitial() {
            assertEquals(SemanticVersion.DEFAULT_INITIAL_VERSION, Versions.defaultInitial(Scheme.SEMVER).toString());
        }
    }

    @Nested
    @DisplayName("VersionFactory.isLegal")
    class IsLegalTests {
        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'') == false")
        @EmptySource
        void isLegalWithEmptyString(String version) {
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingIsLegalWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'') == false")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void isLegalWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'') == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertTrue(Versions.isLegal(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'') == true")
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

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void isLegalSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test invoking isLegal with prefix tolerance
            assertTrue(Versions.isLegal(Scheme.SEMVER, version, true));
        }

        @ParameterizedTest(name = "VersionFactory.isLegal(Scheme.SEMVER, ''{0}'', true) == true")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void isLegalSanitizableString(String version) {
            // the method must fail without toleration and succeed when using toleration
            assertFalse(Versions.isLegal(Scheme.SEMVER, version));
            assertFalse(Versions.isLegal(Scheme.SEMVER, version, false));
            assertTrue(Versions.isLegal(Scheme.SEMVER, version, true));
        }
    }

    @Nested
    @DisplayName("VersionFactory.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String version) {
            assertThrows(RuntimeException.class, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void exceptionUsingValueOfWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> Versions.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, version).toString());
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'', prefix).toString() == ''{0}''")
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

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test that invoking valueOf with sanitization on a valid string returns the same string
            assertEquals(version, Versions.valueOf(Scheme.SEMVER, version, true).toString());
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == SemanticVersion.sanitize(''{0}'')")
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
    @DisplayName("VersionFactory.sortIdentifiers")
    class SortIdentifiersTests {
        @Test
        @DisplayName("VersionFactory.sortIdentifiers(Scheme.SEMVER, [...])")
        void sortIdentifiers() {
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
    }
}
