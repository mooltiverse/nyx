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

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Objects;

/**
 * The utility class used to create version instances.
 */
public class Versions {
    /**
     * Default constructor is private on purpose
     */
    private Versions() {
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
            case SEMVER: return SemanticVersion.valueOf(SemanticVersion.DEFAULT_INITIAL_VERSION);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
    }

    /**
     * Returns {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme.
     * <br>
     * This method uses a strict criteria, without trying to sanitize the given string.
     * 
     * @param scheme the scheme to check against.
     * @param s the string version to check.
     * 
     * @return {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme, {@code false} otherwise.
     * 
     * @see #isLegal(Scheme, String)
     */
    public static boolean isCore(Scheme scheme, String s) {
        return isCore(scheme, s, false);
    }

    /**
     * Returns {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme.
     * <br>
     * This method is different than {@link #isCore(Scheme, String, String)} as it also sanitizes extra characters
     * in the body of the version identifier instead of just an optional prefix (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param s the string version to check.
     * @param lenient when {@code true} prefixes and non critical extra characters are tolerated even if they are not
     * strictly legal from the version scheme specification perspective.
     * 
     * @return {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme, {@code false} otherwise.
     * 
     * @see #isLegal(Scheme, String, boolean)
     */
    public static boolean isCore(Scheme scheme, String s, boolean lenient) {
        Objects.requireNonNull(s, "Can't parse a null string");
        switch (scheme) {
            case SEMVER: {
                    if (!SemanticVersion.isLegal(s, lenient))
                        return false;
                    
                    SemanticVersion v = SemanticVersion.valueOf(s, lenient);
                    return Objects.isNull(v.getPrerelease()) && Objects.isNull(v.getBuild());
                }
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
    }

    /**
     * Returns {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme.
     * <br>
     * This method is different than {@link #isCore(Scheme, String, boolean)} as it only tolerates a prefix, while
     * {@link #isCore(Scheme, String, boolean)} is more lenient as it also sanitizes extra characters in the body
     * of the version identifier (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param s the string version to check.
     * @param prefix the initial string that is used for the version prefix. This will be stripped off from the given
     * string representation of the version. It can be {@code null} or empty, in which case it's ignored. If not empty
     * and the given version string doesn't start with this prefix, this prefix is ignored.
     * 
     * @return {@code true} if the given string is a legal version and it only contains core identifiers
     * according to the given scheme, {@code false} otherwise.
     * 
     * @see #isLegal(Scheme, String, boolean)
     */
    public static boolean isCore(Scheme scheme, String s, String prefix) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (!Objects.isNull(prefix) && s.startsWith(prefix))
            s = s.replaceFirst(prefix, "");
        return isCore(scheme, s);
    }

    /**
     * Returns {@code true} if the given string is a legal version which, for example, can be parsed using
     * {@link #valueOf(Scheme, String)} without exceptions using the implementation selected by the given scheme.
     * <br>
     * This method uses a strict criteria, without trying to sanitize the given string.
     * 
     * @param scheme the scheme to check against.
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
     * <br>
     * This method is different than {@link #isLegal(Scheme, String, String)} as it also sanitizes extra characters
     * in the body of the version identifier instead of just an optional prefix (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param s the string version to check.
     * @param lenient when {@code true} prefixes and non critical extra characters are tolerated even if they are not
     * strictly legal from the version scheme specification perspective.
     * 
     * @return {@code true} if the given string represents a legal version sing the implementation selected
     * by the given scheme, {@code false} otherwise.
     * 
     * @see #valueOf(Scheme, String, boolean)
     */
    public static boolean isLegal(Scheme scheme, String s, boolean lenient) {
        Objects.requireNonNull(s, "Can't parse a null string");
        switch (scheme) {
            case SEMVER: return SemanticVersion.isLegal(s, lenient);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
    }

    /**
     * Returns {@code true} if the given string is a legal version which, for example, can be parsed using
     * {@link #valueOf(Scheme, String, boolean)} without exceptions using the implementation selected by the given scheme.
     * <br>
     * This method is different than {@link #isLegal(Scheme, String, boolean)} as it only tolerates a prefix, while
     * {@link #isLegal(Scheme, String, boolean)} is more lenient as it also sanitizes extra characters in the body
     * of the version identifier (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param s the string version to check.
     * @param prefix the initial string that is used for the version prefix. This will be stripped off from the given
     * string representation of the version. It can be {@code null} or empty, in which case it's ignored. If not empty
     * and the given version string doesn't start with this prefix, this prefix is ignored.
     * 
     * @return {@code true} if the given string represents a legal version sing the implementation selected
     * by the given scheme, {@code false} otherwise.
     * 
     * @see #valueOf(Scheme, String, boolean)
     */
    public static boolean isLegal(Scheme scheme, String s, String prefix) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (!Objects.isNull(prefix) && s.startsWith(prefix))
            s = s.replaceFirst(prefix, "");
        return isLegal(scheme, s);
    }

    /**
     * Returns the most relevant identifier in the given collection, according to the given scheme ordering, or {@code null}
     * if the given list is empty.
     * 
     * @param scheme the scheme to peek the most relevand item from
     * @param identifiers the identifiers to inspect
     * 
     * @return the most relevant identifier in the given collection, according to the given scheme ordering, or {@code null}
     * if the given list is empty.
     * 
     * @see SemanticVersion#getIdentifierComparator()
     */
    public static String mostRelevantIdentifier(Scheme scheme, Collection<String> identifiers) {
        if (identifiers.isEmpty())
            return null;

        Comparator<String> comparator = null;
        switch (scheme) {
            case SEMVER: {
                comparator = SemanticVersion.getIdentifierComparator();
                break;
            }
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }

        Iterator<String> iterator = identifiers.iterator();
        String candidate = iterator.next();
        while (iterator.hasNext()) {
            String next = iterator.next();
            if (comparator.compare(next, candidate) < 0)
                candidate = next;
        }
        return candidate;
    }

    /**
     * Returns the most relevant identifier between the given arguments, according to the given scheme ordering, or {@code null}
     * if both arguments are {@code null}.
     * 
     * @param scheme the scheme to peek the most relevand item from
     * @param identifier1 the first identifier to inspect
     * @param identifier2 the secondt identifier to inspect
     * 
     * @return the most relevant identifier between the given arguments, according to the given scheme ordering, or {@code null}
     * if both arguments are {@code null}.
     * 
     * @see SemanticVersion#getIdentifierComparator()
     */
    public static String mostRelevantIdentifier(Scheme scheme, String identifier1, String identifier2) {
        if (Objects.isNull(identifier1))
            return identifier2;
        else if (Objects.isNull(identifier2))
        return identifier1;

        Comparator<String> comparator = null;
        switch (scheme) {
            case SEMVER: {
                comparator = SemanticVersion.getIdentifierComparator();
                break;
            }
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
        return (comparator.compare(identifier1, identifier2) < 0) ? identifier1 : identifier2;
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
     * <br>
     * This method is different than {@link #valueOf(Scheme, String, String)} as it also sanitizes extra characters
     * in the body of the version identifier instead of just an optional prefix (when {@code sanitize} is {@code true}).
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
            case SEMVER: return SemanticVersion.valueOf(s, sanitize);
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
    }

    /**
     * Returns a Version instance representing the specified String value.
     * <br>
     * This method is different than {@link #valueOf(Scheme, String, boolean)} as it only tolerates a prefix, while
     * {@link #valueOf(Scheme, String, boolean)} is more lenient as it also sanitizes extra characters in the body
     * of the version identifier (when {@code sanitize} is {@code true}).
     *
     * @param scheme the scheme the version belongs to
     * @param s the string to parse
     * @param prefix the initial string that is used for the version prefix. This will be stripped off from the given
     * string representation of the version. It can be {@code null} or empty, in which case it's ignored. If not empty
     * and the given version string doesn't start with this prefix, this prefix is ignored.
     *
     * @return the new Version instance representing the given string. The concrete class depends on the given {@code scheme}
     *
     * @throws NullPointerException if a given arguument is {@code null}
     * @throws IllegalArgumentException if the given string doesn't represent a legal version, according to the selected scheme
     */
    public static final Version valueOf(Scheme scheme, String s, String prefix) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (!Objects.isNull(prefix) && s.startsWith(prefix))
            s = s.replaceFirst(prefix, "");
        return valueOf(scheme, s);
    }

    /**
     * Returns a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}, according to the given scheme. {@code null} values are always
     * considered less than non null valid version identifiers.
     * This method does no sanitization or prefix interpretation.
     * 
     * @param scheme the scheme to check against.
     * @param v1 the first version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * @param v2 the second version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * 
     * @return a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}
     * 
     * @throws IllegalArgumentException if a given string doesn't represent a legal version, according to the selected scheme
     */
    public static int compare(Scheme scheme, String v1, String v2) {
        return compare(scheme, v1, v2, false);
    }

    /**
     * Returns a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}, according to the given scheme. {@code null} values are always
     * considered less than non null valid version identifiers.
     * This method is different than {@link #compare(Scheme, String, String, boolean)} as it only tolerates a prefix, while
     * {@link #compare(Scheme, String, String, boolean)} is more lenient as it also sanitizes extra characters in the body
     * of the version identifier (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param v1 the first version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * @param v2 the second version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * @param prefix the initial string that is used for the version prefix. This will be stripped off from the given
     * string representation of the versions. It can be {@code null} or empty, in which case it's ignored. If not empty
     * and the given version string doesn't start with this prefix, this prefix is ignored.
     * 
     * @return a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}
     * 
     * @throws IllegalArgumentException if a given string doesn't represent a legal version, according to the selected scheme
     */
    public static int compare(Scheme scheme, String v1, String v2, String prefix) {
        if (Objects.isNull(v1) && Objects.isNull(v2))
            return 0;
        
        String v1b = Objects.isNull(v1) || Objects.isNull(prefix) ? v1 : (v1.startsWith(prefix) ? v1.replaceFirst(prefix, "") : v1);
        String v2b = Objects.isNull(v2) || Objects.isNull(prefix) ? v2 : (v2.startsWith(prefix) ? v2.replaceFirst(prefix, "") : v2);
        return compare(scheme, v1b, v2b, false);
    }

    /**
     * Returns a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}, according to the given scheme. {@code null} values are always
     * considered less than non null valid version identifiers.
     * If {@code sanitize} is {@code true} this method will try to sanitize the given strings before parsing so that if there are
     * illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.
     * <br>
     * This method is different than {@link #compare(Scheme, String, String, String)} as it also sanitizes extra characters
     * in the body of the version identifiers instead of just an optional prefix (when {@code sanitize} is {@code true}).
     * 
     * @param scheme the scheme to check against.
     * @param v1 the first version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * @param v2 the second version to compare. It may be {@code null}. If it's not a valid version it's considered as {@code null}.
     * @param sanitize optionally enables sanitization before parsing versions
     * 
     * @return a negative integer, zero, or a positive integer as the version represented by {@code v1} is less than, equal to,
     * or greater than the version represented by {@code v2}
     * 
     * @throws IllegalArgumentException if a given string doesn't represent a legal version, according to the selected scheme
     */
    public static int compare(Scheme scheme, String v1, String v2, boolean sanitize) {
        switch (scheme) {
            case SEMVER: {
                SemanticVersion sv1 = Objects.isNull(v1) ? null : SemanticVersion.valueOf(v1, sanitize);
                SemanticVersion sv2 = Objects.isNull(v2) ? null : SemanticVersion.valueOf(v2, sanitize);
                if (Objects.isNull(sv1) && Objects.isNull(sv2))
                    return 0;
                else if (Objects.isNull(sv1))
                    return -1;
                else if (Objects.isNull(sv2))
                    return 1;
                else return sv1.compareTo(sv2);
            }
            //MAVEN: not yet supported
            default: throw new IllegalArgumentException(String.format("Illegal or unsupported scheme '%s'", scheme));
        }
    }
}
