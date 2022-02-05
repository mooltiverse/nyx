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

import java.util.List;

/**
 * The specialization of a Core version number allows exactly 3 positive integers, the {@code major},
 * {@code minor} and {@code patch} as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 */
class SemanticVersionCoreIdentifier extends CompositeIntegerIdentifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * Builds the core identifier with the given values.
     *
     * @param children the children of this identifier. It must have exactly 3 elements
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    private SemanticVersionCoreIdentifier(List<IntegerIdentifier> children) {
        super(children);
    }
    
    /**
     * Builds the core identifier with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative)
     */
    private SemanticVersionCoreIdentifier(Integer major, Integer minor, Integer patch) {
        super(IntegerIdentifier.valueOf(major), IntegerIdentifier.valueOf(minor), IntegerIdentifier.valueOf(patch));
    }

    /**
     * Builds the core identifier with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative).
     * When the string cannot be converted to an integer because it's not an integer representation the exception will
     * also carry a {@link NumberFormatException} as its {@link Throwable#getCause() root cause}, otherwise the root
     * cause may me {@code null}.
     */
    private SemanticVersionCoreIdentifier(String major, String minor, String patch) {
        super(IntegerIdentifier.valueOf(major), IntegerIdentifier.valueOf(minor), IntegerIdentifier.valueOf(patch));
    }

    /**
     * Returns an identifier instance representing the specified String value.
     *
     * @param s the string to parse (in the {@code x.y.x} format)
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic core version
     */
    static SemanticVersionCoreIdentifier valueOf(String s) {
        return new SemanticVersionCoreIdentifier(Parser.toIntegerIdentifiers(s, DEFAULT_SEPARATOR));
    }

    /**
     * Returns an identifier instance representing with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @return the new identifier instance representing the given values.
     *
     * @throws IllegalArgumentException if a given value is {@code null}
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative)
     */
    static SemanticVersionCoreIdentifier valueOf(Integer major, Integer minor, Integer patch) {
        return new SemanticVersionCoreIdentifier(major, minor, patch);
    }

    /**
     * Returns an identifier instance representing with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @return the new identifier instance representing the given values.
     *
     * @throws IllegalArgumentException if a given value is {@code null}
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative)
     */
    static SemanticVersionCoreIdentifier valueOf(String major, String minor, String patch) {
        return new SemanticVersionCoreIdentifier(major, minor, patch);
    }

    /**
     * Returns the major version number
     *
     * @return the major version number
     */
    public Integer getMajor() {
        return super.get(0);
    }

    /**
     * Returns the minor version number
     *
     * @return the minor version number
     */
    public Integer getMinor() {
        return super.get(1);
    }

    /**
     * Returns the patch version number
     *
     * @return the patch version number
     */
    public Integer getPatch() {
        return super.get(2);
    }

    /**
     * Returns a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero.
     *
     * @return a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero.
     */
    SemanticVersionCoreIdentifier bumpMajor() {
       return new SemanticVersionCoreIdentifier(super.get(0)+1, 0, 0);
    }

    /**
     * Returns a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero.
     *
     * @return a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero.
     */
    SemanticVersionCoreIdentifier bumpMinor() {
        return new SemanticVersionCoreIdentifier(super.get(0), super.get(1)+1, 0);
    }

    /**
     * Returns a new instance with the major and minor numbers of this current instance and the patch number
     * incremented by one.
     *
     * @return a new instance with the major and minor numbers of this current instance and the patch number
     * incremented by one.
     */
    SemanticVersionCoreIdentifier bumpPatch() {
        return new SemanticVersionCoreIdentifier(super.get(0), super.get(1), super.get(2)+1);
    }
}
