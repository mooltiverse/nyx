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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Utility class with static methods used to parse and validate values.
 */
final class Parser {
    /**
     * The range of allowed characters in string identifiers.
     *
     * {@value}
     */
    static final String ALLOWED_ALPHANUMERIC_CHARACTERS = "[0-9a-zA-Z-]";

    /**
     * The regexp pattern that can be used to validate characters in string identifiers.
     *
     * {@value}
     */
    static final String ALLOWED_ALPHANUMERIC_CHARACTERS_REGEXP_PATTERN = "^[0-9a-zA-Z-]+$";

    /**
     * Default constructor mate private on purpose
     */
    private Parser() {
        super();
    }

    /**
     * Validates the given value to make sure it's legal for an alphanumeric identifier.
     * This method returns the same value received as input if validation passes, otherwise an exception is thrown.
     *
     * @param value the same value passed in input, if no exceptions are thrown.
     *
     * @return the same value passed as input
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters
     */
    static String validateStringIdentifier(String value) {
        Objects.requireNonNull(value, "Identifier value cannot be null");
        if (value.isBlank())
            throw new IllegalArgumentException("Identifier value cannot be empty");

        if (!value.matches(ALLOWED_ALPHANUMERIC_CHARACTERS_REGEXP_PATTERN))
            throw new IllegalArgumentException(String.format("Illegal value %s for string identifier. Illegal characters have been found. Allowed characters are limited to "+ALLOWED_ALPHANUMERIC_CHARACTERS+".", value));
        return value;
    }

    /**
     * Validates the given value to make sure it's legal for a positive numeric identifier.
     * This method returns the same value received as input if validation passes, otherwise an exception is thrown.
     *
     * @param value the value to validate
     * @param allowNegative if {@code false} negative integers are not allowed so if the given value represents
     * a negative integer an {@link IllegalArgumentException} is thrown. If {@code true} this check is not performed.
     *
     * @return the same value passed as input, converted to Integer
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters
     */
    static Integer validateIntegerIdentifier(Integer value, boolean allowNegative) {
        Objects.requireNonNull(value, "Identifier value cannot be null");
        if (!allowNegative && (value.intValue() < 0))
            throw new IllegalArgumentException(String.format("Illegal value %d for integer identifier. Value cannot be negative.", value));

        return value;
    }

    /**
     * Validates the given value to make sure it's legal for a positive numeric identifier.
     * This method returns the same value received as input if validation passes, otherwise an exception is thrown.
     * 
     * @param value the value to validate
     * @param allowNegative if {@code false} negative integers are not allowed so if the given value represents
     * a negative integer an {@link IllegalArgumentException} is thrown. If {@code true} this check is not performed.
     * @param allowExtraCharacters if {@code false} this method also makes sure that no extra characters not affecting
     * the acual value are present. Those may be a leading sign or leading zeroes. If {@code true} this check is not performed.
     * When this flag is enabled this method makes sure that the string representation of converted integer returns the
     * same as the input value, otherwise it means that some extra characters were present.
     *
     * @return the same value passed as input, converted to Integer
     *
     * @throws NullPointerException if the given value is {@code null}
     * @throws IllegalArgumentException if the given value is illegal for some reason, like including forbidden characters.
     * When the string cannot be converted to an integer because it's not an integer representation the exception will
     * also carry a {@link NumberFormatException} as its {@link Throwable#getCause() root cause}, otherwise the root
     * cause may me {@code null}.
     */
    static Integer validateIntegerIdentifier(String value, boolean allowNegative, boolean allowExtraCharacters) {
        Objects.requireNonNull(value, "Identifier value cannot be null");
        if (value.isBlank())
            throw new IllegalArgumentException("Identifier value cannot be empty");

        Integer intValue = null;
        try {
            intValue = Integer.valueOf(value);
        }
        catch (NumberFormatException nfe) {
            throw new IllegalArgumentException(String.format("Illegal value %s for integer identifier. Not a valid Integer.", value), nfe);
        }

        // Check that the string representation of the parsed value is the same as the input value
        // they may differ if the input value had leading zeroes or signs
        if (!value.equals(intValue.toString()))
            throw new IllegalArgumentException(String.format("Illegal value %s for integer identifier. Value should be %d. Consider sanitizing the string before parsing.", value, intValue));

        // Let the other overloaded version of the validate() method check if it's positive
        return validateIntegerIdentifier(intValue, allowNegative);
    }

    /**
     * Validates the given list of elements to make sure it's legal, mening it's not {@code null}, it's not
     * empty and does not contain {@code null} values.
     *
     * @param elements the list to validate
     *
     * @return the same list passed as input if the validation succeeds
     *
     * @throws NullPointerException if the given list is {@code null}
     * @throws IllegalArgumentException if the given list contains illegal values or contains {@code null} items
     * 
     * @param <T> the type of items in the list to validate
     */
    static <T> List<T> validateElements(List<T> elements) {
        Objects.requireNonNull(elements, "Elements list cannot be null");
        if (elements.isEmpty())
            throw new IllegalArgumentException("Elements list cannot be empty");

        return elements;
    }

    /**
     * Splits the given string into its components strings, using the given separator to separate components.
     *
     * @param s the string to split
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the array of sub strings
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string is empty or contains illegal characters
     */
    static String[] split(String s, char separator) {
        Objects.requireNonNull(s, "Can't split null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't split an empty string");
        //if (s.startsWith(Character.toString(separator)) || s.endsWith(Character.toString(separator)))
        //    throw new IllegalArgumentException(String.format("String %s has separator occurrences at the beginning or the end", s, Character.toString(separator)));
        return s.split("["+separator+"]");
    }

    /**
     * Utility method that returns {@code true} if the given array is not {@code null}, it's not empty
     * and all the elements in the given array are not {@code null}, {@code false} otherwise.
     *
     * @param items the array to check
     *
     * @return {@code true} if the given array contains valid elements
     */
    static boolean hasValues(Object[] items) {
        if ((items == null) || (items.length == 0))
            return false;
        
        for (Object item: items)
            if (item == null)
                return false;
        return true;
    }

    /**
     * Returns a list of string identifiers representing the specified String value.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the list of identifiers representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static List<StringIdentifier> toStringIdentifiers(String s, char separator) {
        Objects.requireNonNull(s, "Can't parse null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        return toStringIdentifiers(split(s, separator));
    }

    /**
     * Returns a list of string identifiers representing the specified String values.
     *
     * @param s the strings to parse
     *
     * @return the list of identifiers representing the given strings.
     *
     * @throws NullPointerException if the given string array is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static List<StringIdentifier> toStringIdentifiers(String[] s) {
        Objects.requireNonNull(s, "Can't parse null string array");
        if (s.length == 0)
            throw new IllegalArgumentException("Can't parse an empty string array");

        List<StringIdentifier> identifiers = new ArrayList<StringIdentifier>();
        for (String part: s) {
            identifiers.add(StringIdentifier.valueOf(part));
        }
        return identifiers;
    }

    /**
     * Returns a list of integer identifiers representing the specified String value.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the list of identifiers representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static List<IntegerIdentifier> toIntegerIdentifiers(String s, char separator) {
        Objects.requireNonNull(s, "Can't parse null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        return toIntegerIdentifiers(Parser.split(s, separator));
    }

    /**
     * Returns a list of integer identifiers representing the specified String values.
     *
     * @param s the strings to parse
     *
     * @return the list of identifiers representing the given strings.
     *
     * @throws NullPointerException if the given string array is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given string contains illegal identifiers.
     * When the string cannot be converted to an integer because it's not an integer representation the exception will
     * also carry a {@link NumberFormatException} as its {@link Throwable#getCause() root cause}, otherwise the root
     * cause may me {@code null}.
     */
    static List<IntegerIdentifier> toIntegerIdentifiers(String[] s) {
        Objects.requireNonNull(s, "Can't parse null string array");
        if (s.length == 0)
            throw new IllegalArgumentException("Can't parse an empty string array");

        List<IntegerIdentifier> identifiers = new ArrayList<IntegerIdentifier>();
        for (String part: s) {
            identifiers.add(IntegerIdentifier.valueOf(part));
        }
        return identifiers;
    }

    /**
     * Returns a list of identifiers representing the specified String value.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     * @param integersPolicy the flag specifying how to parse integers
     *
     * @return the list of identifiers representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static List<SimpleIdentifier> toIdentifiers(String s, char separator, UseIntegerIdentifiers integersPolicy) {
        Objects.requireNonNull(s, "Can't parse null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        return toIdentifiers(split(s, separator), integersPolicy);
    }

    /**
     * Returns a list of identifiers representing the specified String values.
     *
     * @param s the strings to parse
     * @param integersPolicy the flag specifying how to parse integers
     *
     * @return the list of identifiers representing the given strings.
     *
     * @throws NullPointerException if the given string array is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static List<SimpleIdentifier> toIdentifiers(String[] s, UseIntegerIdentifiers integersPolicy) {
        Objects.requireNonNull(s, "Can't parse null string array");
        if (s.length == 0)
            throw new IllegalArgumentException("Can't parse an empty string array");

        List<SimpleIdentifier> identifiers = new ArrayList<SimpleIdentifier>();
        for (String part: s) {
            switch (integersPolicy)
            {
                case ALWAYS: {
                    identifiers.add(IntegerIdentifier.valueOf(part));
                }
                case NEVER: {
                    identifiers.add(StringIdentifier.valueOf(part));
                }
                case WHEN_POSSIBLE: {
                    try {
                        identifiers.add(IntegerIdentifier.valueOf(part));
                    }
                    catch (IllegalArgumentException iae) {
                        // If the integer was not validated because it's a negative number it has the NumberFormatException
                        // as the root cause. In this case we can't just use a string but we need to let the exception go.
                        // If, instead, the string was not an integer representation, let's use a string
                        if ((iae.getCause() != null) && NumberFormatException.class.isInstance(iae.getCause()))
                            identifiers.add(StringIdentifier.valueOf(part));
                        else throw iae;
                    }
                }
            }
        }
        return identifiers;
    }

    /**
     * Returns an identifier representing the specified String value. If the {@code integersPolicy}
     * flag is {@code ALWAYS} or the given string represents an integer and the {@code integersPolicy}
     * flag is {@code WHEN_POSSIBLE} then the returned identifier is an {@link IntegerIdentifier},
     * otherwise it's a {@link StringIdentifier}.
     *
     * @param s the string to parse
     * @param integersPolicy the flag specifying how to parse integers
     *
     * @return the parsed identifier. The type depends on the {@code integersPolicy} flag and the
     * content of the given string
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal identifiers
     */
    static SimpleIdentifier toIdentifier(String s, UseIntegerIdentifiers integersPolicy) {
        Objects.requireNonNull(s, "Can't parse null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        switch (integersPolicy)
        {
            case ALWAYS: {
                return IntegerIdentifier.valueOf(s);
            }
            case NEVER: {
                return StringIdentifier.valueOf(s);
            }
            case WHEN_POSSIBLE: {
                try {
                    return IntegerIdentifier.valueOf(s);
                }
                catch (IllegalArgumentException iae) {
                    // If the integer was not validated because it's a negative number it has the NumberFormatException
                    // as the root cause. In this case we can't just use a string but we need to let the exception go.
                    // If, instead, the string was not an integer representation, let's use a string
                    if ((iae.getCause() != null) && NumberFormatException.class.isInstance(iae.getCause()))
                        return StringIdentifier.valueOf(s);
                    else throw iae;
                }
            }
        }

        // last resort, actually never reached
        return StringIdentifier.valueOf(s);
    }
}