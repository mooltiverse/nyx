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

import java.util.Objects;

/**
 * The utility class used to create version instances.
 */
public class VersionFactory {
    /**
     * Default constructor is private on purpose
     */
    private VersionFactory() {
        super();
    }

    /**
     * Returns a Version instance representing the default initial value to use for the given scheme.
     *
     * @param scheme the scheme to get the initial version for
     *
     * @return the new Version instance representing the default initial value. The concrete class depends on the given {@code scheme}
     *
     * @throws NullPointerException if the given arguument is {@code null}
     */
    public static final Version defaultInitial(Scheme scheme) {
        Objects.requireNonNull(scheme, "Scheme is required");
        switch (scheme) {
            case SEMVER:  return SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupporte scheme %s", scheme));
        }
    }

    /**
     * Returns {@code true} if the given string is a legal version which, for example, can be parsed using
     * {@link #valueOf(Scheme, String)} without exceptions using the implementation selected by the given scheme.
     * <br>
     * This method uses a strict criteria, without trying to sanitize the given string.
     * 
     * @param scheme the scheme the check against.
     * @param s the string version to check.
     * 
     * @return {@code true} if the given string represents a legal version sing the implementation selected
     * by the given scheme, {@code false} otherwise.
     * 
     * @see #valueOf(Scheme, String)
     */
    public static boolean isLegal(Scheme scheme, String s) {
        return isLegal(scheme, s, false);
    }

    /**
     * Returns {@code true} if the given string is a legal version which, for example, can be parsed using
     * {@link #valueOf(Scheme, String, boolean)} without exceptions using the implementation selected by the given scheme.
     * 
     * @param scheme the scheme the check against.
     * @param s the string version to check.
     * @param prefixLenient when {@code true} prefixes are tolerated even if they are not strictly legal from the
     * version scheme specification perspective.
     * 
     * @return {@code true} if the given string represents a legal version sing the implementation selected
     * by the given scheme, {@code false} otherwise.
     * 
     * @see #valueOf(Scheme, String, boolean)
     */
    public static boolean isLegal(Scheme scheme, String s, boolean prefixLenient) {
        Objects.requireNonNull(s, "Can't parse a null string");
        switch (scheme) {
            case SEMVER:  return SemanticVersion.isLegal(s, prefixLenient);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupporte scheme %s", scheme));
        }
    }

    /**
     * Returns a Version instance representing the specified String value. No sanitization attempt is done.
     *
     * @param scheme the scheme the version belongs to
     * @param s the string to parse
     *
     * @return the new Version instance representing the given string. The concrete class depends on the given {@code scheme}
     *
     * @throws NullPointerException if a given arguument is {@code null}
     * @throws IllegalArgumentException if the given string doesn't represent a legal version, according to the selected scheme
     */
    public static final Version valueOf(Scheme scheme, String s) {
        return valueOf(scheme, s, false);
    }

    /**
     * Returns a Version instance representing the specified String value. No sanitization attempt is done.
     * <br>
     * If {@code sanitize} is {@code true} this method will try to sanitize the given string before parsing so that if there are
     * illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.
     * <br>
     * When sanitization is enabled on a string that actually needs sanitization the string representation of the
     * returned object will not exactly match the input value.
     *
     * @param scheme the scheme the version belongs to
     * @param s the string to parse
     * @param sanitize optionally enables sanitization before parsing
     *
     * @return the new Version instance representing the given string. The concrete class depends on the given {@code scheme}
     *
     * @throws NullPointerException if a given arguument is {@code null}
     * @throws IllegalArgumentException if the given string doesn't represent a legal version, according to the selected scheme
     */
    public static final Version valueOf(Scheme scheme, String s, boolean sanitize) {
        Objects.requireNonNull(scheme, "Scheme is required");
        Objects.requireNonNull(s, "Can't parse a null string");
        switch (scheme) {
            case SEMVER:  return SemanticVersion.valueOf(s, sanitize);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupporte scheme %s", scheme));
        }
    }
}
