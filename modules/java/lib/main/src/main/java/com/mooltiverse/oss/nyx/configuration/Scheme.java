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
package com.mooltiverse.oss.nyx.configuration;

import java.util.Objects;

/**
 * This class maps version {@link com.mooltiverse.oss.nyx.version.Scheme}s and the corresponding configuration options, whose string representations
 * may not always match the {@link com.mooltiverse.oss.nyx.version.Scheme} values.
 * 
 * Although there is a one to one mapping with these values and those in the version {@link com.mooltiverse.oss.nyx.version.Scheme}
 * the two must not be confused. This class is meant to map configuration values (lowercase strings) to version {@link com.mooltiverse.oss.nyx.version.Scheme}.
 * 
 * In other words, here is the mapping between version schemes and the string values users can use in configuration to ask for those schemes.
 * 
 * @see com.mooltiverse.oss.nyx.version.Scheme
 */
public enum Scheme {
    /**
     * The <a href="https://semver.org/">Semantic Versioning</a> scheme.
     * 
     * @see com.mooltiverse.oss.nyx.version.Scheme#SEMVER
     */
    SEMVER(com.mooltiverse.oss.nyx.version.Scheme.SEMVER, com.mooltiverse.oss.nyx.version.Scheme.SEMVER.toString().toLowerCase());

    /**
     * The backing scheme enum value.
     */
    private final com.mooltiverse.oss.nyx.version.Scheme scheme;

    /**
     * The scheme value used in configuration (the lowercase string).
     */
    private final String value;

    /**
     * Builds the enumeration item.
     * 
     * @param scheme the version scheme
     * @param value the scheme value used in configuration
     */
    private Scheme(com.mooltiverse.oss.nyx.version.Scheme scheme, String value) {
        this.scheme = scheme;
        this.value = value;
    }

    /**
     * Returns the version scheme
     * 
     * @return the version scheme
     */
    public com.mooltiverse.oss.nyx.version.Scheme getScheme() {
        return scheme;
    }

    /**
     * Returns the string representation of this scheme. This string is also used in configuration options.
     * 
     * @return the string representation of this scheme
     */
    public String getValue() {
        return value;
    }

    /**
     * Returns the proper scheme mapped from the given version scheme.
     * 
     * @param scheme the version scheme to parse and return the scheme for
     * 
     * @return the proper scheme mapped from the given version scheme.
     * 
     * @throws IllegalArgumentException if the given value cannot be mapped to any existing scheme
     * @throws NullPointerException if the given value is {@code null}
     */
    public static Scheme from(com.mooltiverse.oss.nyx.version.Scheme scheme)
        throws IllegalArgumentException, NullPointerException {
        if (Objects.isNull(scheme))
            throw new NullPointerException();

        switch (scheme) {
            case SEMVER: return Scheme.SEMVER;
            default: throw new IllegalArgumentException(scheme.toString());
        }
    }

    /**
     * Returns the proper version scheme mapped from the given value.
     * 
     * @param value string value scheme to parse and return the scheme for
     * 
     * @return the proper scheme mapped from the given value.
     * 
     * @throws IllegalArgumentException if the given value cannot be mapped to any existing scheme
     * @throws NullPointerException if the given value is {@code null}
     */
    public static Scheme from(String value)
        throws IllegalArgumentException, NullPointerException {
        if (Objects.isNull(value))
            throw new NullPointerException();

        switch (value) {
            case "semver":  return Scheme.SEMVER;
            default: throw new IllegalArgumentException(value);
        }
    }
}
