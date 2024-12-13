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
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EmptySource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;

@DisplayName("StringIdentifier")
public class StringIdentifierTest {
    /**
     * A {@link MethodSource} method that returns structured data to test string identifiers.
     * Each returned argument is a string that must yield to a valid instance of a string identifier.
     *
     * @return a stream of arguments representing correct string identifiers
     */
    static Stream<Arguments> wellKnownValidStringIdentifiers() {
        return Stream.of(
            // Strings with digits only
            arguments("0"),
            arguments("1"),
            arguments("123"),
            arguments("123456789"),

            // Strings with alphanumeric characters only
            arguments("a"),
            arguments("A"),
            arguments("aB"),
            arguments("bA"),
            arguments("alpha"),
            arguments("build"),
            arguments("snapshot"),
            arguments("BETA"),
            arguments("gAMma"),
            arguments("Delta"),

            // Strings with digits and alphanumeric characters
            arguments("a1"),
            arguments("2A"),
            arguments("a3B"),
            arguments("b45A"),
            arguments("alpha012"),
            arguments("34build"),

            // Strings with dashes only are allowed by spec
            arguments("-"),
            arguments("--"),
            arguments("---"),

            // Strings with mixture of characters, ingluding dash
            arguments("a-"),
            arguments("-A"),
            arguments("a-B"),
            arguments("1bA"),
            arguments("alpha2"),
            arguments("build-2"),
            arguments("3-snapshot"),
            arguments("BETA-GAMMA"),
            arguments("gAMma-3-alpha"),
            arguments("-Delta")
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test string identifiers.
     * Each returned argument is a string that must yield to an exception
     *
     * @return a stream of arguments representing invalid string identifiers
     */
    static Stream<Arguments> wellKnownInvalidStringIdentifiers() {
        return Stream.of(
            // Strings with whitespaces
            arguments(" aB"),
            arguments("bA "),
            arguments("a lp ha"),
            arguments("bui ld"),
            arguments("sn apsh ot"),
            arguments("BET A"),
            arguments("gA Mma"),
            arguments("Del ta"),

            // Strings with dots
            arguments("."),
            arguments(".a"),
            arguments("a."),
            arguments("a.a"),

            // Strings with illegal characters
            arguments("|"),
            arguments("\\"),
            arguments("!"),
            arguments("\""),
            arguments("£"),
            arguments("$"),
            arguments("%"),
            arguments("&"),
            arguments("/"),
            arguments("("),
            arguments(")"),
            arguments("="),
            arguments("?"),
            arguments("'"),
            arguments("^"),
            arguments("["),
            arguments("]"),
            arguments(","),
            arguments(";"),
            arguments(":"),
            arguments("_"),
            arguments("*"),
            arguments("+"),
            arguments("@"),
            arguments("#"),
            arguments("°"),
            arguments("§"),

            // Strings with accents
            arguments("ì"),
            arguments("è"),
            arguments("é"),
            arguments("ò"),

            // And mixtures
            arguments("òlpha"),
            arguments("béta"),
            arguments("status:"),
            arguments("good?"),
            arguments("!%/)?=(&)(£?")
        );
    }

    @Nested
    @DisplayName("StringIdentifier.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "StringIdentifier.valueOf(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String identifier) {
            assertThrows(IllegalArgumentException.class, () -> StringIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "StringIdentifier.valueOf(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String identifier) {
            assertThrows(NullPointerException.class, () -> StringIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "StringIdentifier.valueOf(''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String identifier) {
            assertThrows(RuntimeException.class, () -> StringIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "StringIdentifier.valueOf(''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.StringIdentifierTest#wellKnownValidStringIdentifiers")
        void valueOfWithValidStringIdentifier(String identifier) {
            assertEquals(identifier, StringIdentifier.valueOf(identifier).toString());
        }

        @ParameterizedTest(name = "StringIdentifier.valueOf(''{0}'') throws IllegalArgumentException")
        @MethodSource("com.mooltiverse.oss.nyx.version.StringIdentifierTest#wellKnownInvalidStringIdentifiers")
        void exceptionUsingValueOfWithInvalidStringIdentifier(String identifier) {
            assertThrows(IllegalArgumentException.class, () -> StringIdentifier.valueOf(identifier));
        }
    }
}