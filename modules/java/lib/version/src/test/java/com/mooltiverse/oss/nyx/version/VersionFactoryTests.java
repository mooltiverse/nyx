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

import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

@DisplayName("VersionFactory")
public class VersionFactoryTests {
    @Nested
    @DisplayName("VersionFactory.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String version) {
            assertThrows(IllegalArgumentException.class, () -> VersionFactory.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String version) {
            assertThrows(NullPointerException.class, () -> VersionFactory.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String version) {
            assertThrows(RuntimeException.class, () -> VersionFactory.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'') throws ''{1}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownInvalidVersions")
        void exceptionUsingValueOfWithInvalidVersion(String version, Class<? extends Exception> expectedException) {
            assertThrows(expectedException, () -> VersionFactory.valueOf(Scheme.SEMVER, version));
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfValidString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            assertEquals(version, VersionFactory.valueOf(Scheme.SEMVER, version).toString());
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownValidVersions")
        void valueOfSanitizedString(String version, int major, int minor, int patch, List<String> pre, List<String> build) {
            // test that invoking valueOf with sanitization on a valid string returns the same string
            assertEquals(version, VersionFactory.valueOf(Scheme.SEMVER, version, true).toString());
        }

        @ParameterizedTest(name = "VersionFactory.valueOf(Scheme.SEMVER, ''{0}'', true).toString() == SemanticVersion.sanitize(''{0}'')")
        @MethodSource("com.mooltiverse.oss.nyx.version.SemanticVersionTests#wellKnownSanitizableVersions")
        void valueOfSanitizableString(String version) {
            // the method must fail without sanitization and succeed when using sanitization
            assertThrows(IllegalArgumentException.class, () -> VersionFactory.valueOf(Scheme.SEMVER, version));
            assertThrows(IllegalArgumentException.class, () -> VersionFactory.valueOf(Scheme.SEMVER, version, false).toString());
            assertNotEquals(version, VersionFactory.valueOf(Scheme.SEMVER, version, true).toString());
            assertEquals(SemanticVersion.sanitize(version), VersionFactory.valueOf(Scheme.SEMVER, version, true).toString());
        }
    }
}
