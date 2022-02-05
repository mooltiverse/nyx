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

@DisplayName("IntegerIdentifier")
public class IntegerIdentifierTest {
    /**
     * A {@link MethodSource} method that returns structured data to test integer identifiers.
     * Each returned argument is a string that must yield to a valid instance of an integer identifier.
     *
     * @return a stream of arguments representing correct integer identifiers
     */
    static Stream<Arguments> wellKnownValidStringIdentifiers() {
        return Stream.of(
            // Strings with digits only
            arguments("0"),
            arguments("1"),
            arguments("123"),
            arguments("123456789"),
            arguments(Integer.valueOf(Integer.MAX_VALUE).toString())
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test integer identifiers.
     * Each returned argument is an integer that must yield to a valid instance of an integer identifier.
     *
     * @return a stream of arguments representing correct integer identifiers
     */
    static Stream<Arguments> wellKnownValidIntegerIdentifiers() {
        return Stream.of(
            arguments(0),
            arguments(1),
            arguments(123),
            arguments(123456789),
            arguments(Integer.MAX_VALUE)
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test integer identifiers.
     * Each returned argument is a string that must yield to an exception
     *
     * @return a stream of arguments representing invalid integer identifiers
     */
    static Stream<Arguments> wellKnownInvalidStringIdentifiers() {
        return Stream.of(
            // Numbers with whitespaces
            arguments(" 12"),
            arguments("34 "),
            arguments("5 6"),

            // Numbers with dots
            arguments(".12"),
            arguments("34."),
            arguments("5.6"),

            // Numbers with alphanumeric characters
            arguments("a12"),
            arguments("34b"),
            arguments("5c6"),

            // Strings with illegal characters
            arguments("|1"),
            arguments("2\\"),
            arguments("!3"),
            arguments("4\""),
            arguments("£5"),
            arguments("6$"),
            arguments("%7"),
            arguments("8&"),
            arguments("/9"),
            arguments("1("),
            arguments(")2"),
            arguments("3="),
            arguments("?4"),
            arguments("5'"),
            arguments("^6"),
            arguments("7["),
            arguments("]8"),
            arguments("9,"),
            arguments(";1"),
            arguments("2:"),
            arguments("_3"),
            arguments("4*"),
            arguments("+5"),
            arguments("6@"),
            arguments("#7"),
            arguments("8°"),
            arguments("§9"),

            // Negative numbers
            arguments("-12"),
            arguments("-34"),
            arguments("-56"),

            // Numbers with leading zeroes
            arguments("012"),
            arguments("034"),
            arguments("056"),

            // Real numbers,
            arguments("1.2"),
            arguments("123.345"),
            arguments("1,2"),
            arguments("123,345")
        );
    }

    /**
     * A {@link MethodSource} method that returns structured data to test integer identifiers.
     * Each returned argument is a number that must yield to an exception
     *
     * @return a stream of arguments representing invalid integer identifiers
     */
    static Stream<Arguments> wellKnownInvalidIntegerIdentifiers() {
        return Stream.of(
            // Negative numbers
            arguments(-12),
            arguments(-34),
            arguments(-56)
        );
    }

    @Nested
    @DisplayName("IntegerIdentifier.valueOf")
    class ValueOfTests {
        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'') throws IllegalArgumentException")
        @EmptySource
        void exceptionUsingValueOfWithEmptyString(String identifier) {
            assertThrows(IllegalArgumentException.class, () -> IntegerIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'') throws NullPointerException")
        @NullSource
        void exceptionUsingValueOfWithNullString(String identifier) {
            assertThrows(NullPointerException.class, () -> IntegerIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'') throws RuntimeException")
        @NullAndEmptySource
        void exceptionUsingValueOfWithNullOrEmptyString(String identifier) {
            assertThrows(RuntimeException.class, () -> IntegerIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'').toString() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.IntegerIdentifierTest#wellKnownValidStringIdentifiers")
        void valueOfWithValidStringsIdentifier(String identifier) {
            assertEquals(identifier, IntegerIdentifier.valueOf(identifier).toString());
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'').getValue() == ''{0}''")
        @MethodSource("com.mooltiverse.oss.nyx.version.IntegerIdentifierTest#wellKnownValidIntegerIdentifiers")
        void valueOfWithValidIntegerIdentifier(Integer identifier) {
            assertEquals(identifier, IntegerIdentifier.valueOf(identifier).getValue());
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'') throws IllegalArgumentException")
        @MethodSource("com.mooltiverse.oss.nyx.version.IntegerIdentifierTest#wellKnownInvalidStringIdentifiers")
        void exceptionUsingValueOfWithInvalidStringIdentifier(String identifier) {
            assertThrows(IllegalArgumentException.class, () -> IntegerIdentifier.valueOf(identifier));
        }

        @ParameterizedTest(name = "IntegerIdentifier.valueOf(''{0}'') throws IllegalArgumentException")
        @MethodSource("com.mooltiverse.oss.nyx.version.IntegerIdentifierTest#wellKnownInvalidIntegerIdentifiers")
        void exceptionUsingValueOfWithInvalidIntegerIdentifier(Integer identifier) {
            assertThrows(IllegalArgumentException.class, () -> IntegerIdentifier.valueOf(identifier));
        }
    }
}