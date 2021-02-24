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
     * If <code>sanitize</code> is <code>true</code> this method will try to sanitize the given string before parsing so that if there are
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
