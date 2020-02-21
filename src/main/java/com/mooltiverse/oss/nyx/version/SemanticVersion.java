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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The implementation of a <a href="https://semver.org/">Semantic Versioning 2.0.0</a> compliant version.
 */
public class SemanticVersion extends AbstractVersion implements Comparable<SemanticVersion> {
    /**
     * The default initial version that can be used when non version is yet available.
     */
    public static final String DEFAULT_INITIAL_VERSION = "0.1.0";

    /**
     * The character that marks the separation between the core and the pre-release part. Note that this character is
     * used as separator only at the first occurrence while other occurrences are considered legal characters in the
     * pre-release and the build identifiers.
     */
    public static final char PRERELEASE_MARKER = '-';

    /**
     * The character that marks the separation between the core or the pre-release part and the build part.
     */
    public static final char BUILD_MARKER = '+';

    /**
     * A relaxed version of the {@link #SEMANTIC_VERSION_PATTERN} that works also when a prefix appears at the beginning
     * of the version string or some zeroes appear in front of numbers.
     * Value is: {@value}
     *
     * @see #SEMANTIC_VERSION_PATTERN
     */
    public static final String SEMANTIC_VERSION_RELAXED_PATTERN = "([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(?:-((?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$";

    /**
     * The regexp pattern taken directly from <a href="https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string">Semantic Versioning 2.0.0</a>
     * used to parse semantic versions. Value is: {@value}
     */
    public static final String SEMANTIC_VERSION_PATTERN = "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$";

    /**
     * Store the immutable string representation to avoid repetitive formatting.
     */
    private transient String renderedString = null;

    /**
     * The handler of the core version part. It can't be <code>null</code>.
     */
    private final SemanticCoreVersionHandler coreHandler;

    /**
     * The handler of the pre-release part of the version. It may be <code>null</code>.
     */
    private final SemanticPreReleaseVersionHandler prereleaseHandler;

    /**
     * The handler of the build part of the version. It may be <code>null</code>.
     */
    private final SemanticBuildVersionHandler buildHandler;

    /**
     * Builds the version with the given handlers values.
     *
     * @param coreHandler the handler of the core version part. It can't be <code>null</code>.
     * @param prereleaseHandler the handler of the pre-release part of the version. It may be <code>null</code>.
     * @param buildHandler the handler of the build part of the version. It may be <code>null</code>.
     *
     * @throws NullPointerException if the core handler is <code>null</code>
     */
    @SuppressWarnings("unchecked")
    private SemanticVersion(SemanticCoreVersionHandler coreHandler, SemanticPreReleaseVersionHandler prereleaseHandler, SemanticBuildVersionHandler buildHandler) {
        super(new CompositeValueHandler(coreHandler, prereleaseHandler, buildHandler));
        Objects.requireNonNull(coreHandler, "Can't build a valid semantic version without the core version numbers");

        this.coreHandler = coreHandler;
        this.prereleaseHandler = prereleaseHandler;
        this.buildHandler = buildHandler;
    }

    /**
     * Indicates whether some other object is "equal to" this one. This object equals to the given one if they are the
     * same object or they are of the same type and hold exactly the same version.
     *
     * @param obj the reference object with which to compare.
     * 
     * @return <code>true</code> if this object is the same as the <code>obj</code> argument; <code>false</code> otherwise.
     * 
     * @see Object#equals(Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (this == obj)
            return true;
        if (!this.getClass().isInstance(obj))
            return false;

        SemanticVersion otherVersion = SemanticVersion.class.cast(obj);
        if (!coreHandler.equals(otherVersion.coreHandler))
            return false;

        if (prereleaseHandler == null) {
            if (otherVersion.prereleaseHandler != null)
                return false;
        }
        else if (!prereleaseHandler.equals(otherVersion.prereleaseHandler))
            return false;

        if (buildHandler == null) {
            if (otherVersion.buildHandler != null)
                return false;
        }
        else if (!buildHandler.equals(otherVersion.buildHandler))
            return false;

        return true;
    }

    /**
     * Compares this version with the specified version for order. Returns a negative integer, zero, or a positive
     * integer as this object is less than, equal to, or greater than the specified version.
     * <br><br>
     * <a href="https://semver.org/">Semantic Versioning 2.0.0</a> states that:<br>
     * - rule #9: Pre-release versions have a lower precedence than the associated normal version.<br>
     * - rule #10: Build metadata MUST be ignored when determining version precedence. Thus two versions that differ
     *   only in the build metadata, have the same precedence.<br>
     * - rule #11: Precedence refers to how versions are compared to each other when ordered. Precedence MUST be
     *   calculated by separating the version into major, minor, patch and pre-release identifiers in that order
     *   (Build metadata does not figure into precedence). Precedence is determined by the first difference when
     *   comparing each of these identifiers from left to right as follows: Major, minor, and patch versions are always
     *   compared numerically. Example: 1.0.0 &lt; 2.0.0 &lt; 2.1.0 &lt; 2.1.1.<br>
     *   When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version.
     *   Example: 1.0.0-alpha &lt; 1.0.0.<br>
     *   Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined by
     *   comparing each dot separated identifier from left to right until a difference is found as follows:<br>
     *   - identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens are
     *     compared lexically in ASCII sort order.<br>
     *   - numeric identifiers always have lower precedence than non-numeric identifiers<br>
     *   - a larger set of pre-release fields has a higher precedence than a smaller set, if all of the preceding
     *     identifiers are equal.
     * <br><br>
     * Note that when the specification says "lower precedence" it translates to &lt;, which means the "less" part is
     * supposed to appear first in order. Translating this to the {@link Comparable#compareTo(Object)} method, it means
     * that the object with "lower precedence" returns a negative number.
     * <br><br>
     * However, to cope with the {@link Comparable} contract, which imposes a total ordering, we need to amend the above
     * rules and make them s little stricter just because two versions with different values can't be considered equal
     * (also see {@link #equals(Object)}). With more detail:
     * <br>
     * - rule #10 is amended so that two versions that only differ in their build metadata will not return 0 (as if they
     *   the same) but their build metadata of two versions that are equal in their core and prerelease parts affects
     *   the order by their literal comparison (remember that numeric identifiers are not treated as such in build
     *   metadata and, instead, they are just treated as strings). In other words we are not ignoring build metadata as
     *   required by rule #10 but we consider it with the least priority only when the core and prerelease parts are the
     *   same. Two be consistent with rule #9, when comparing two versions with the same core and prerelease parts, when
     *   one has build metadata and the other doesn't, the one with the build metadata has lower precedence on the one
     *   without the build metadata. Example: 1.2.3+build.1 &lt; 1.2.3 and 1.2.3-alpha.0+build.1 &lt; 1.2.3-alpha.0.
     *
     * @param v the version to be compared.
     *
     * @return a negative integer, zero, or a positive integer as this version is less than, equal to, or greater than
     * the specified version.
     *
     * @see Comparable#compareTo(Object)
     */
    public int compareTo(SemanticVersion v) {
        if (v == null)
            return 1;

        // Rule #9
        if (getMajor() != v.getMajor())
            return getMajor() - v.getMajor();
        if (getMinor() != v.getMinor())
            return getMinor() - v.getMinor();
        if (getPatch() != v.getPatch())
            return getPatch() - v.getPatch();

        // Rule #11
        if ((prereleaseHandler != null) || (v.prereleaseHandler != null)) {
            // When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version.
            // Example: 1.0.0-alpha < 1.0.0.
            // So if only one has the prerelease block, that means it has lower precedence (comes first)
            if ((prereleaseHandler != null) && (v.prereleaseHandler == null))
                return -1;
            else if ((prereleaseHandler == null) && (v.prereleaseHandler != null))
                return 1;
        }
        if (prereleaseHandler != null) {
            // Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined
            // by comparing each dot separated identifier from left to right until a difference is found as follows:
            // identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens
            // are compared lexically in ASCII sort order.
            Iterator thisIterator = prereleaseHandler.getChildren().iterator();
            Iterator otherIterator = v.prereleaseHandler.getChildren().iterator();

            while (thisIterator.hasNext()) {
                if (otherIterator.hasNext()) {
                    Object thisItem = thisIterator.next();
                    Object otherItem = otherIterator.next();

                    // Identifiers consisting of only digits are compared numerically and identifiers with letters or
                    // hyphens are compared lexically in ASCII sort order. Numeric identifiers always have lower
                    // precedence than non-numeric identifiers.

                    if (Integer.class.isInstance(thisItem)) {
                        // This item is a number
                        if (Integer.class.isInstance(otherItem)) {
                            // Also the other item is a number so let's compare them as such
                            int res = Integer.class.cast(thisItem).compareTo(Integer.class.cast(otherItem));
                            if (res != 0)
                                return res;
                        }
                        else {
                            // This item is a number while the other is a string, so this has lower precedence (comes first)
                            return -1;
                        }
                    }
                    else {
                        // This item is a string
                        if (Integer.class.isInstance(otherItem)) {
                            // the other item is a number while this is a string so the other has lower precedence (comes first) than this
                            return 1;
                        }
                        else {
                            // Also the other item is a string so let's see how they compare as strings
                            int res = thisItem.toString().compareTo(otherItem.toString());
                            if (res != 0)
                                return res;
                        }
                    }
                }
                else {
                    // This set has more elements than the other so it has more precedence (comes after in order)
                    //
                    // A larger set of pre-release fields has a higher precedence than a smaller set, if all of the
                    // preceding identifiers are equal.
                    // Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
                    return 1;
                }
            }
            if (otherIterator.hasNext()) {
                // This set has less elements than the other so it has less precedence (comes first in order)
                //
                // A larger set of pre-release fields has a higher precedence than a smaller set, if all of the
                // preceding identifiers are equal.
                // Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
                return -1;
            }
        }

        // Rule #10 (amended to achieve total order)
        if ((buildHandler != null) || (v.buildHandler != null)) {
            // Although the build part should be ignored from SemVer specs, the compareTo method needs to differentiate
            // versions that have the same core and prerelease but different build parts.
            // We chose to be consistent with rule #10 (about prerelease) so the version that has a build part has
            // When major, minor, and patch and prerelease are equal, a build version has lower precedence than a normal
            // (or normal-prerelease) version.
            // Example: 1.0.0-alpha+build < 1.0.0-alpha, and 1.0.0+build < 1.0.0.
            if ((buildHandler != null) && (v.buildHandler == null))
                return -1;
            else if ((buildHandler == null) && (v.buildHandler != null))
                return 1;
        }
        // If no difference is found, let's consider the number of items in the build part. This is not requested by
        // SemVer but, again, we use the same behavior as for prerelease. So the set that has less elements has less
        // precedence (comes first in order).
        if ((buildHandler != null) && (v.buildHandler != null)) {
            if (buildHandler.children.size() != v.buildHandler.children.size())
                return v.buildHandler.children.size() - buildHandler.children.size();
        }

        // If no difference has been found yet, let's just compare the build parts as strings
        if ((buildHandler != null) && (buildHandler.toString().compareTo(v.buildHandler.toString()) != 0))
            return buildHandler.toString().compareTo(v.buildHandler.toString());

        // As a last resort, compare the string version of both entities. This is not requested (explicitly) by SemVer
        // but it's still compliant with it and with Comparable as well.
        return toString().compareTo(v.toString());
    }

    /**
     * Returns a string representation of the object.
     *
     * @return a string representation of the object.
     *
     * @see Object#toString()
     */
    @Override
    public String toString() {
        if (renderedString == null) {
            StringBuilder sb = new StringBuilder(coreHandler.toString());
            if (prereleaseHandler != null) {
                sb.append(PRERELEASE_MARKER);
                sb.append(prereleaseHandler.toString());
            }
            if (buildHandler != null) {
                sb.append(BUILD_MARKER);
                sb.append(buildHandler.toString());
            }
            renderedString = sb.toString();
        }
        return renderedString;
    }

    /**
     * Returns a SemanticVersion instance representing the specified String value.
     *
     * @param s the string to parse
     *
     * @return the new SemanticVersion instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic version
     *
     * @see #sanitizePrefix(String)
     */
    public static SemanticVersion valueOf(String s) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (s.isEmpty())
            throw new IllegalArgumentException("Can't parse an empty string");

        Matcher m = Pattern.compile(SEMANTIC_VERSION_PATTERN).matcher(s);
        if (m.find()) {
            // group 0 is the entire version string
            // group 1 is the major number
            // group 2 is the minor number
            // group 3 is the patch number
            // group 4 (optional) is the prerelease
            // group 5 (optional) is the build

            SemanticCoreVersionHandler coreHandler = SemanticCoreVersionHandler.valueOf(m.group(1), m.group(2), m.group(3));
            SemanticPreReleaseVersionHandler preReleaseHandler = null;
            if ((m.group(4) != null) && (!m.group(4).isEmpty()))
                preReleaseHandler = SemanticPreReleaseVersionHandler.valueOf(m.group(4));
            SemanticBuildVersionHandler buildHandler = null;
            if ((m.group(5) != null) && (!m.group(5).isEmpty()))
                buildHandler = SemanticBuildVersionHandler.valueOf(m.group(5));

            return new SemanticVersion(coreHandler, preReleaseHandler, buildHandler);
        }
        else throw new IllegalArgumentException(String.format("The string %s does not contain a valid semantic number", s));
    }

    /**
     * Performs all of the sanitizations in the given string by sanitizing, in order, the prefix and leading zeroes
     * in numeric identifiers. This order is the one that yields to the highest success probability in obtaining a
     * legal version.
     * Invoking this method is like invoking the sanitization methods {@link #sanitizeNumbers(String)}
     * and {@link #sanitizePrefix(String)}.
     *
     * @param s the semantic version string to sanitize
     *
     * @return the sanitized semantic string version
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a semantic version, even tolerating
     * the aspects to sanitize
     *
     * @see #sanitizePrefix(String)
     * @see #sanitizeNumbers(String)
     */
    public static String sanitize(String s) {
        return sanitizeNumbers(sanitizePrefix(s));
    }

    /**
     * Takes the given string and tries to parse it as a semantic version number, even with illegal characters or prefix.
     * All numeric identifiers in the core version (<code>major.minor.patch</code>) and in the prerelease metadata are
     * sanitized by removing all leading zeroes to make them compliant. Numeric identifiers in the build metadata part
     * are left intact, even when they have leading zeroes.
     * If the given string contains a prefix (see {@link #sanitizePrefix(String)}) or illegal characters  they are left
     * untouched and they are returned as they were in the input string.
     *
     * @param s a semantic version string which may have illegal leading zeroes to be removed in the numeric identifiers
     * in the core or the prerelease parts.
     *
     * @return the string given as input with the illegal leading zeroes removed.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic version, even tolerating
     * the prefix
     *
     * @see #sanitize(String)
     */
    public static String sanitizeNumbers(String s) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (s.isEmpty())
            throw new IllegalArgumentException("Can't parse an empty string");

        StringBuilder result = new StringBuilder();

        // if there's any prefix, leave it there in the result
        String prefix = getPrefix(s);
        if (prefix != null)
            result.append(prefix);

        Matcher m = Pattern.compile(SEMANTIC_VERSION_RELAXED_PATTERN).matcher(s);
        if (m.find()) {
            // group 0 is the entire version string
            // group 1 is the major number
            // group 2 is the minor number
            // group 3 is the patch number
            // group 4 (optional) is the prerelease
            // group 5 (optional) is the build

            try {
                // to remove leading zeroes, just transform the numbers to Integers and back to strings
                Integer integer = Integer.valueOf(m.group(1));
                if (integer.intValue()<0)
                    throw new IllegalArgumentException(String.format("Can't sanitize negative number %d in %s", integer, s));
                result.append(integer.toString());
                result.append(CompositeValueHandler.DEFAULT_SEPARATOR);
                integer = Integer.valueOf(m.group(2));
                if (integer.intValue()<0)
                    throw new IllegalArgumentException(String.format("Can't sanitize negative number %d in %s", integer, s));
                result.append(integer.toString());
                result.append(CompositeValueHandler.DEFAULT_SEPARATOR);
                integer = Integer.valueOf(m.group(3));
                if (integer.intValue()<0)
                    throw new IllegalArgumentException(String.format("Can't sanitize negative number %d in %s", integer, s));
                result.append(integer.toString());
            }
            catch (NumberFormatException nfe) {
                throw new IllegalArgumentException(String.format("Numeric identifiers in string %s can't be converted to valid Integers", s), nfe);
            }

            // Go through all identifiers in the prerelease part. If they can convert to an integer just do it and
            // append their string representation to the output, this automatically removes leading zeroes.
            // If they can't be converted to numberst just append them as they are.
            if ((m.group(4) != null) && (!m.group(4).isEmpty())) {
                result.append(PRERELEASE_MARKER);
                //String[] identifiers = m.group(4).split("["+CompositeValueHandler.DEFAULT_SEPARATOR+"]");
                List<String> identifiers = Arrays.asList(m.group(4).split("["+CompositeValueHandler.DEFAULT_SEPARATOR+"]"));
                Iterator<String> idIterator = identifiers.iterator();
                //for (String identifier: identifiers) {
                while (idIterator.hasNext()) {
                    String identifier = idIterator.next();
                    try {
                        result.append(Integer.valueOf(identifier).toString());
                    }
                    catch (NumberFormatException nfe) {
                        // it wasn't an integer, so append it as a string
                        result.append(identifier);
                    }
                    if (idIterator.hasNext())
                        result.append(CompositeValueHandler.DEFAULT_SEPARATOR);
                }

            }

            // append the build part just as it was
            if ((m.group(5) != null) && (!m.group(5).isEmpty())) {
                result.append(BUILD_MARKER);
                result.append(m.group(5));
            }
        }
        else throw new IllegalArgumentException(String.format("The string %s does not contain a valid semantic number", s));

        return result.toString();
    }

    /**
     * Takes the given string and tries to parse it as a semantic version with an optional prefix (which may be any
     * string before the core <code>major.minor.patch</code> numbers). The returned string is the semantic version passed
     * as input with the prefix removed. If no prefix is present then the returned string is the same as the one passed
     * as input. Prefixes are often used (i.e. the 'v' used it Git tags or <code>release-</code>, <code>rel</code> etc)
     * so this method helps in stripping those prefixes to get a compliant semantic version.
     *
     * @param s a semantic version string which may have an additional prefix to be removed
     *
     * @return the string given as input with the prefix removed, if any, or the same string passed as input if no prefix
     * was found.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic version, even tolerating
     * the prefix
     *
     * @see #sanitize(String)
     * @see #getPrefix(String)
     */
    public static String sanitizePrefix(String s) {
        Objects.requireNonNull(s, "Can't sanitize a null string");
        if (s.isEmpty())
            throw new IllegalArgumentException("Can't sanitize an empty string");

        Matcher m = Pattern.compile(SEMANTIC_VERSION_RELAXED_PATTERN).matcher(s);
        if (m.find()) {
            // group 0 is the entire version string
            return m.group(0);
        }
        else throw new IllegalArgumentException(String.format("The string %s does not contain a valid semantic number", s));
    }

    /**
     * Takes the given string and tries to parse it as a semantic version with an optional prefix (which may be any
     * string before the core <code>major.minor.patch</code> numbers). The returned string is the prefix before the core
     * version number, if any, or <code>null</code> otherwise.
     *
     * @param s a semantic version string which may have an additional prefix to be isolated
     *
     * @return the prefix in the given semantic version string, if any, or <code>null</code> otherwise. <code>null</code>
     * is also returned when the given string is empty.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string is not empty but doesn't represent a legal semantic version,
     * even tolerating the prefix
     *
     * @see #sanitizePrefix(String)
     */
    public static String getPrefix(String s) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (s.isEmpty())
            return null;

        Matcher m = Pattern.compile(SEMANTIC_VERSION_RELAXED_PATTERN).matcher(s);
        if (m.find()) {
            // group 0 is the entire version string
            if (s.equals(m.group(0)))
                return null;
            else return s.substring(0, s.length()-m.group(0).length());
        }
        else throw new IllegalArgumentException(String.format("The string %s does not contain a valid semantic number", s));
    }

    /**
     * Returns the major version number
     *
     * @return the major version number
     */
    public int getMajor() {
        return coreHandler.getMajor();
    }

    /**
     * Returns the minor version number
     *
     * @return the minor version number
     */
    public int getMinor() {
        return coreHandler.getMinor();
    }

    /**
     * Returns the patch version number
     *
     * @return the patch version number
     */
    public int getPatch() {
        return coreHandler.getPatch();
    }

    /**
     * Returns the core part (<code>major.minor.patch</code>) of the version as a string.
     *
     * @return the core part of the version as a string.
     */
    public String getCore() {
        return coreHandler.toString();
    }

    /**
     * Returns the prerelease part of the version, if any, or <code>null</code> otherwise.
     *
     * @return the prerelease part of the version.
     */
    public String getPrerelease() {
        return prereleaseHandler == null ? null : prereleaseHandler.toString();
    }

    /**
     * Returns the build part of the version, if any, or <code>null</code> otherwise.
     *
     * @return the build part of the version.
     */
    public String getBuild() {
        return buildHandler == null ? null : buildHandler.toString();
    }
}
