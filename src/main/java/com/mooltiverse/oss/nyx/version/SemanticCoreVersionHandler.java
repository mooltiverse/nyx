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

/**
 * The specialization of a Core version number allows exactly 3 positive integers, the <code>major</code>,
 * <code>minor</code> and <code>patch</code> as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 */
class SemanticCoreVersionHandler extends CompositeIntegerValueHandler {
    /**
     * Builds the value handler with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative)
     */
    private SemanticCoreVersionHandler(int major, int minor, int patch) {
        super(new IntegerValueHandler(major), new IntegerValueHandler(minor), new IntegerValueHandler(patch));
    }

    /**
     * Builds the value handler with the given values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @throws IllegalArgumentException if a given value is illegal (i.e. negative)
     */
    private SemanticCoreVersionHandler(String major, String minor, String patch) {
        super(new IntegerValueHandler(major), new IntegerValueHandler(minor), new IntegerValueHandler(patch));
    }

    /**
     * Returns the major version number
     *
     * @return the major version number
     */
    public int getMajor() {
        return super.get(0);
    }

    /**
     * Returns the minor version number
     *
     * @return the minor version number
     */
    public int getMinor() {
        return super.get(1);
    }

    /**
     * Returns the patch version number
     *
     * @return the patch version number
     */
    public int getPatch() {
        return super.get(2);
    }

    /**
     * Returns a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero.
     *
     * @return a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero.
     */
    SemanticCoreVersionHandler bumpMajor() {
       return new SemanticCoreVersionHandler(super.get(0)+1, 0, 0);
    }

    /**
     * Returns a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero.
     *
     * @return a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero.
     */
    SemanticCoreVersionHandler bumpMinor() {
        return new SemanticCoreVersionHandler(super.get(0), super.get(1)+1, 0);
    }

    /**
     * Returns a new instance with the major and minor numbers of this current instance and the patch number
     * incremented by one.
     *
     * @return a new instance with the major and minor numbers of this current instance and the patch number
     *      * incremented by one.
     */
    SemanticCoreVersionHandler bumpPatch() {
        return new SemanticCoreVersionHandler(super.get(0), super.get(1), super.get(2)+1);
    }

    /**
     * Returns an handler instance representing the specified String value.
     *
     * @param s the string to parse (in the <code>x.y.x</code> format)
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic core version
     */
    static SemanticCoreVersionHandler valueOf(String s) {
        String[] parts = split(s, DEFAULT_SEPARATOR);
        if (parts.length != 3)
            throw new IllegalArgumentException(String.format("The string %s is not composed of 3 integers", s));
        return new SemanticCoreVersionHandler(parts[0], parts[1], parts[2]);
    }

    /**
     * Returns an handler instance representing the specified values.
     *
     * @param major the major number
     * @param minor the minor number
     * @param patch the patch number
     *
     * @return the new handler instance representing the given values.
     *
     * @throws NullPointerException if any of the given strings is <code>null</code>
     * @throws IllegalArgumentException if the given strings don't represent a legal semantic core version number
     */
    static SemanticCoreVersionHandler valueOf(String major, String minor, String patch) {
        try {
            return new SemanticCoreVersionHandler(major, minor, patch);
        }
        catch (IllegalArgumentException iae) {
            throw new IllegalArgumentException(String.format("The strings %s, %s, %s are not valid integers", major, minor, patch), iae);
        }
    }
}
