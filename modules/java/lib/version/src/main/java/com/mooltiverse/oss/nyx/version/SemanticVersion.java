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
 * <br>
 * Instances of this class are <b>immutable</b> so whenever you alter some values you actually receive a
 * new instance holding the new value, while the old one remains unchanged.
 * <br>
 * To get new instances you can also use one of the {@link #valueOf(String)} methods.
 */
public class SemanticVersion extends Version implements Comparable<SemanticVersion> {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The default value to start from when bumping an identifier that has no numeric value yet. {@value}
     */
    private static final int DEFAULT_BUMP_VALUE = 1;
    
    /**
     * The default initial version that can be used when non version is yet available.
     */
    public static final String DEFAULT_INITIAL_VERSION = "0.1.0";

    /**
     * The character that marks the separation between the core and the pre-release part. Note that this character is
     * used as separator only at the first occurrence while other occurrences are considered legal characters in the
     * pre-release and the build identifiers.
     */
    public static final char PRERELEASE_DELIMITER = '-';

    /**
     * The character that marks the separation between the core or the pre-release part and the build part.
     */
    public static final char BUILD_DELIMITER = '+';

    /**
     * A relaxed version of the {@link #SEMANTIC_VERSION_PATTERN} that works also when a prefix appears at the beginning
     * of the version string or some zeroes appear in front of numbers.
     * Value is: {@value}
     *
     * @see #SEMANTIC_VERSION_PATTERN
     */
    public static final String SEMANTIC_VERSION_PATTERN_RELAXED = "([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(?:-((?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:[0-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$";

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
     * The identifier of the core version part. It can't be <code>null</code>.
     */
    private final SemanticVersionCoreIdentifier coreIdentifier;

    /**
     * The identifier of the pre-release part of the version. It may be <code>null</code>.
     */
    private final SemanticVersionPreReleaseIdentifier prereleaseIdentifier;

    /**
     * The identifier of the build part of the version. It may be <code>null</code>.
     */
    private final SemanticVersionBuildIdentifier buildIdentifier;

    /**
     * Builds a new version object with the given values.
     *
     * @param major the <code>major</code> number
     * @param minor the <code>minor</code> number
     * @param patch the <code>patch</code> number
     *
     * @throws IllegalArgumentException if one of <code>major</code>, <code>minor</code>, <code>patch</code> is
     * negative
     */
    public SemanticVersion(int major, int minor, int patch) {
        this(major, minor, patch, null, null);
    }

    /**
     * Builds a new version object with the given values.
     *
     * @param major the <code>major</code> number
     * @param minor the <code>minor</code> number
     * @param patch the <code>patch</code> number
     * @param prereleaseIdentifiers the <code>prereleaseIdentifiers</code> the array of {@link Integer} or {@link String}
     * objects to use as identifiers in the prerelease block. <code>null</code> items are ignored. Integers and strings
     * representing integers must not have leading zeroes or represent negative numbers. If the array is <code>null</code>
     * then the instance will have no prerelease block
     * @param buildIdentifiers the <code>buildIdentifiers</code> the array of  {@link String} to use as identifiers in
     * the build block. <code>null</code> items are ignored. If the array is <code>null</code> then the instance will
     * have no build block
     *
     * @throws IllegalArgumentException if one of <code>major</code>, <code>minor</code>, <code>patch</code> is
     * negative or one object in the <code>prereleaseIdentifiers</code> represents a negative integer (either when
     * passed as an {@link Integer} or {@link String}) or have leading zeroes. This exception is also raised when objects
     * in the <code>prereleaseIdentifiers</code> are not of type {@link Integer} or {@link String} or when string
     * identifiers in the <code>prereleaseIdentifiers</code> or <code>buildIdentifiers</code> contain illegal characters
     */
    public SemanticVersion(int major, int minor, int patch, Object[] prereleaseIdentifiers, String[] buildIdentifiers) {
        this(SemanticVersionCoreIdentifier.valueOf(major, minor, patch), Parser.hasValues(prereleaseIdentifiers) ? SemanticVersionPreReleaseIdentifier.valueOf(true, prereleaseIdentifiers) : null, Parser.hasValues(buildIdentifiers) ? SemanticVersionBuildIdentifier.valueOf(true, buildIdentifiers) : null);
    }

    /**
     * Builds the version with the given identifier values.
     *
     * @param coreIdentifier the identifier of the core version part. It can't be <code>null</code>.
     * @param prereleaseIdentifier the identifier of the pre-release part of the version. It may be <code>null</code>.
     * @param buildIdentifier the identifier of the build part of the version. It may be <code>null</code>.
     *
     * @throws NullPointerException if the core identifier is <code>null</code>
     */
    private SemanticVersion(SemanticVersionCoreIdentifier coreIdentifier, SemanticVersionPreReleaseIdentifier prereleaseIdentifier, SemanticVersionBuildIdentifier buildIdentifier) {
        super();
        
        Objects.requireNonNull(coreIdentifier, "Can't build a valid semantic version without the core version numbers");

        this.coreIdentifier = coreIdentifier;
        this.prereleaseIdentifier = prereleaseIdentifier;
        this.buildIdentifier = buildIdentifier;
    }

    /**
     * Returns a hash code value for the object.
     *
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        return 19 * coreIdentifier.hashCode() * (prereleaseIdentifier == null ? 1 : 23 * prereleaseIdentifier.hashCode()) * (buildIdentifier == null ? 1 : 29 * buildIdentifier.hashCode());
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
        if (!coreIdentifier.equals(otherVersion.coreIdentifier))
            return false;

        if (prereleaseIdentifier == null) {
            if (otherVersion.prereleaseIdentifier != null)
                return false;
        }
        else if (!prereleaseIdentifier.equals(otherVersion.prereleaseIdentifier))
            return false;

        if (buildIdentifier == null) {
            if (otherVersion.buildIdentifier != null)
                return false;
        }
        else if (!buildIdentifier.equals(otherVersion.buildIdentifier))
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
    @Override
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
        if ((prereleaseIdentifier != null) || (v.prereleaseIdentifier != null)) {
            // When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version.
            // Example: 1.0.0-alpha < 1.0.0.
            // So if only one has the prerelease block, that means it has lower precedence (comes first)
            if ((prereleaseIdentifier != null) && (v.prereleaseIdentifier == null))
                return -1;
            else if ((prereleaseIdentifier == null) && (v.prereleaseIdentifier != null))
                return 1;
        }
        if (prereleaseIdentifier != null) {
            // Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined
            // by comparing each dot separated identifier from left to right until a difference is found as follows:
            // identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens
            // are compared lexically in ASCII sort order.
            Iterator<? extends Identifier> thisIterator = prereleaseIdentifier.getValue().iterator();
            Iterator<? extends Identifier> otherIterator = v.prereleaseIdentifier.getValue().iterator();

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
        if ((buildIdentifier != null) || (v.buildIdentifier != null)) {
            // Although the build part should be ignored from SemVer specs, the compareTo method needs to differentiate
            // versions that have the same core and prerelease but different build parts.
            // We chose to be consistent with rule #10 (about prerelease) so the version that has a build part has
            // When major, minor, and patch and prerelease are equal, a build version has lower precedence than a normal
            // (or normal-prerelease) version.
            // Example: 1.0.0-alpha+build < 1.0.0-alpha, and 1.0.0+build < 1.0.0.
            if ((buildIdentifier != null) && (v.buildIdentifier == null))
                return -1;
            else if ((buildIdentifier == null) && (v.buildIdentifier != null))
                return 1;
        }
        // If no difference is found, let's consider the number of items in the build part. This is not requested by
        // SemVer but, again, we use the same behavior as for prerelease. So the set that has less elements has less
        // precedence (comes first in order).
        if ((buildIdentifier != null) && (v.buildIdentifier != null)) {
            if (buildIdentifier.children.size() != v.buildIdentifier.children.size())
                return v.buildIdentifier.children.size() - buildIdentifier.children.size();
        }

        // If no difference has been found yet, let's just compare the build parts as strings
        if ((buildIdentifier != null) && (buildIdentifier.toString().compareTo(v.buildIdentifier.toString()) != 0))
            return buildIdentifier.toString().compareTo(v.buildIdentifier.toString());

        // As a last resort, compare the string version of both entities. This is not requested (explicitly) by SemVer
        // but it's still compliant with it and with Comparable as well.
        return toString().compareTo(v.toString());
    }

    /**
     * Returns {@link Scheme#SEMVER}.
     * 
     * @return {@link Scheme#SEMVER}
     */
    public final Scheme getScheme() {
        return Scheme.SEMVER;
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
            StringBuilder sb = new StringBuilder(coreIdentifier.toString());
            if (prereleaseIdentifier != null) {
                sb.append(PRERELEASE_DELIMITER);
                sb.append(prereleaseIdentifier.toString());
            }
            if (buildIdentifier != null) {
                sb.append(BUILD_DELIMITER);
                sb.append(buildIdentifier.toString());
            }
            renderedString = sb.toString();
        }
        return renderedString;
    }

    /**
     * Returns a SemanticVersion instance representing the specified String value. No sanitization attempt is done.
     *
     * @param s the string to parse
     *
     * @return the new SemanticVersion instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic version
     *
     * @see #valueOf(String, boolean)
     * @see #sanitizePrefix(String)
     */
    public static SemanticVersion valueOf(String s) {
        Objects.requireNonNull(s, "Can't parse a null string");
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        Matcher m = Pattern.compile(SEMANTIC_VERSION_PATTERN).matcher(s);
        if (m.find()) {
            // group 0 is the entire version string
            // group 1 is the major number
            // group 2 is the minor number
            // group 3 is the patch number
            // group 4 (optional) is the prerelease
            // group 5 (optional) is the build

            SemanticVersionCoreIdentifier coreIdentifier = SemanticVersionCoreIdentifier.valueOf(m.group(1), m.group(2), m.group(3));
            SemanticVersionPreReleaseIdentifier preReleaseIdentifier = null;
            if ((m.group(4) != null) && (!m.group(4).isBlank()))
                preReleaseIdentifier = SemanticVersionPreReleaseIdentifier.valueOf(true, m.group(4));
            SemanticVersionBuildIdentifier buildIdentifier = null;
            if ((m.group(5) != null) && (!m.group(5).isBlank()))
                buildIdentifier = SemanticVersionBuildIdentifier.valueOf(true, m.group(5));

            return new SemanticVersion(coreIdentifier, preReleaseIdentifier, buildIdentifier);
        }
        else throw new IllegalArgumentException(String.format("The string %s does not contain a valid semantic number", s));
    }

    /**
     * This method is a shorthand for {@link #valueOf(String)} and {@link #sanitize(String)}.
     * <br>
     * Returns a SemanticVersion instance representing the specified String value. If <code>sanitize</code> is
     * <code>true</code> this method will try to sanitize the given string before parsing so that if there are
     * illegal characters like a prefix or leading zeroes in numeric identifiers they are removed.
     * <br>
     * When sanitization is enabled on a string that actually needs sanitization the string representation of the
     * returned object will not exactly match the input value.
     *
     * @param s the string to parse
     * @param sanitize optionally enables sanitization before parsing
     *
     * @return the new SemanticVersion instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string doesn't represent a legal semantic version
     *
     * @see #valueOf(String)
     * @see #sanitizePrefix(String)
     */
    public static SemanticVersion valueOf(String s, boolean sanitize) {
        return valueOf(sanitize ? sanitize(s) : s);
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
        if (s.isBlank())
            throw new IllegalArgumentException("Can't parse an empty string");

        StringBuilder result = new StringBuilder();

        // if there's any prefix, leave it there in the result
        String prefix = getPrefix(s);
        if (prefix != null)
            result.append(prefix);

        Matcher m = Pattern.compile(SEMANTIC_VERSION_PATTERN_RELAXED).matcher(s);
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
                result.append(CompositeIdentifier.DEFAULT_SEPARATOR);
                integer = Integer.valueOf(m.group(2));
                if (integer.intValue()<0)
                    throw new IllegalArgumentException(String.format("Can't sanitize negative number %d in %s", integer, s));
                result.append(integer.toString());
                result.append(CompositeIdentifier.DEFAULT_SEPARATOR);
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
            if ((m.group(4) != null) && (!m.group(4).isBlank())) {
                result.append(PRERELEASE_DELIMITER);
                List<String> identifiers = Arrays.asList(m.group(4).split("["+CompositeIdentifier.DEFAULT_SEPARATOR+"]"));
                Iterator<String> idIterator = identifiers.iterator();
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
                        result.append(CompositeIdentifier.DEFAULT_SEPARATOR);
                }

            }

            // append the build part just as it was
            if ((m.group(5) != null) && (!m.group(5).isBlank())) {
                result.append(BUILD_DELIMITER);
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
        if (s.isBlank())
            throw new IllegalArgumentException("Can't sanitize an empty string");

        Matcher m = Pattern.compile(SEMANTIC_VERSION_PATTERN_RELAXED).matcher(s);
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
        if (s.isBlank())
            return null;

        Matcher m = Pattern.compile(SEMANTIC_VERSION_PATTERN_RELAXED).matcher(s);
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
        return coreIdentifier.getMajor();
    }

    /**
     * Returns the minor version number
     *
     * @return the minor version number
     */
    public int getMinor() {
        return coreIdentifier.getMinor();
    }

    /**
     * Returns the patch version number
     *
     * @return the patch version number
     */
    public int getPatch() {
        return coreIdentifier.getPatch();
    }

    /**
     * Returns the core part (<code>major.minor.patch</code>) of the version as a string.
     *
     * @return the core part of the version as a string.
     */
    public String getCore() {
        return coreIdentifier.toString();
    }

    /**
     * Returns an array of the single identifiers of the core part of the version
     *
     * @return the identifiers of the core part of the version.
     */
    public Integer[] getCoreIdentifiers() {
        Integer[] res = new Integer[coreIdentifier.children.size()];
        for (int i=0; i<coreIdentifier.children.size(); i++)
            res[i] = coreIdentifier.get(i);
        return res;
    }

    /**
     * Returns the prerelease part of the version, if any, or <code>null</code> otherwise.
     *
     * @return the prerelease part of the version.
     */
    public String getPrerelease() {
        return prereleaseIdentifier == null ? null : prereleaseIdentifier.toString();
    }

    /**
     * Returns an array of the single identifiers of the prerelease part of the version, if any, or <code>null</code>
     * otherwise.
     *
     * @return the identifiers of the prerelease part of the version. The objects in the array can be either {@link Integer}
     * or {@link String}.
     */
    public Object[] getPrereleaseIdentifiers() {
        if (prereleaseIdentifier == null)
            return null;
        Object[] res = new Object[prereleaseIdentifier.children.size()];
        for (int i=0; i<prereleaseIdentifier.children.size(); i++)
            res[i] = prereleaseIdentifier.children.get(i).getValue();
        return res;
    }

    /**
     * Returns the build part of the version, if any, or <code>null</code> otherwise.
     *
     * @return the build part of the version.
     */
    public String getBuild() {
        return buildIdentifier == null ? null : buildIdentifier.toString();
    }

    /**
     * Returns an array of the single identifiers of the build part of the version, if any, or <code>null</code>
     * otherwise.
     *
     * @return the identifiers of the build part of the version.
     */
    public String[] getBuildIdentifiers() {
        if (buildIdentifier == null)
            return null;
        String[] res = new String[buildIdentifier.children.size()];
        for (int i=0; i<buildIdentifier.children.size(); i++)
            res[i] = buildIdentifier.get(i);
        return res;
    }

    /**
     * Returns a new version object with the <code>major</code>, <code>minor</code> and <code>patch</code> numbers set
     * to the given values. This method doesn't reset any number and the prerelease and build blocks are left unchanged.
     *
     * @param major the major number to set
     * @param minor the minor number to set
     * @param patch the patch number to set
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if any of the given values is negative
     */
    public SemanticVersion setCore(int major, int minor, int patch) {
        return new SemanticVersion(SemanticVersionCoreIdentifier.valueOf(major, minor, patch), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new version object with the <code>major</code> number set to the given value.
     * This method doesn't reset any number and the prerelease and build blocks are left unchanged.
     *
     * @param major the major number to set
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given values is negative
     */
    public SemanticVersion setMajor(int major) {
        return new SemanticVersion(SemanticVersionCoreIdentifier.valueOf(major, coreIdentifier.getMinor(), coreIdentifier.getPatch()), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new version object with the <code>minor</code> number set to the given value.
     * This method doesn't reset any number and the prerelease and build blocks are left unchanged.
     *
     * @param minor the minor number to set
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given values is negative
     */
    public SemanticVersion setMinor(int minor) {
        return new SemanticVersion(SemanticVersionCoreIdentifier.valueOf(coreIdentifier.getMajor(), minor, coreIdentifier.getPatch()), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new version object with the <code>patch</code> number set to the given value.
     * This method doesn't reset any number and the prerelease and build blocks are left unchanged.
     *
     * @param patch the patch number to set
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given values is negative
     */
    public SemanticVersion setPatch(int patch) {
        return new SemanticVersion(SemanticVersionCoreIdentifier.valueOf(coreIdentifier.getMajor(), coreIdentifier.getMinor(), patch), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new version object with the prerelease part set to the given values. If a <code>null</code> value or an array
     * of all <code>null</code> values is passed then the returned version will have no prerelease part, otherwise it will have
     * all of the given non <code>null</code> identifiers, with the core and build elements of this version instance.
     *
     * @param identifiers the identifiers to use for the new version instance, or <code>null</code> to remove the
     * prerelease block. All non <code>null</code> items must be {@link String} or {@link Integer} instances.
     * {@link String} instances representing numeric values will be interpreted as {@link Integer}.
     * If the current version had a pre-release part it is completely replaced by the given identifiers.
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if some non <code>null</code> item passed contains illegal characters, if a
     * given number is negative or contains leading zeroes or any item is not an instance of {@link String} or
     * {@link Integer}
     */
    public SemanticVersion setPrerelease(Object... identifiers) {
        SemanticVersionPreReleaseIdentifier svpri = null;
        if (Parser.hasValues(identifiers)) {
            svpri = SemanticVersionPreReleaseIdentifier.valueOf(true, identifiers);
        }
        return new SemanticVersion(coreIdentifier, svpri, buildIdentifier);
    }

    /**
     * Returns a new version object with the build part set to the given values. If a <code>null</code> value or an array
     * of all <code>null</code> values is passed then the returned version will have no build part, otherwise it will have
     * all of the given non <code>null</code> identifiers, with the core and prerelease elements of this version instance.
     * If the current version had a build part it is completely replaced by the given identifiers.
     *
     * @param identifiers the identifiers to use for the new version instance, or <code>null</code> to remove the
     * build block
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if some non <code>null</code> item passed contains illegal characters
     */
    public SemanticVersion setBuild(String... identifiers) {
        SemanticVersionBuildIdentifier svbi = null;
        if (Parser.hasValues(identifiers)) {
            svbi = SemanticVersionBuildIdentifier.valueOf(true, identifiers);
        }
        return new SemanticVersion(coreIdentifier, prereleaseIdentifier, svbi);
    }

    /**
     * Returns <code>true</code> if an attribute with the given name is present in the prerelease part, <code>false</code> otherwise.
     *
     * @param name the name of the attribute to look up. If <code>null</code> or empty <code>false</code> is returned
     *
     * @return <code>true</code> if an attribute with the given name is present in the prerelease part, <code>false</code> otherwise.
     */
    public boolean hasPrereleaseAttribute(String name) {
        return prereleaseIdentifier == null ? false : prereleaseIdentifier.hasAttribute(name);
    }

    /**
     * Returns <code>true</code> if an attribute with the given name is present in the build part, <code>false</code> otherwise.
     *
     * @param name the name of the attribute to look up. If <code>null</code> or empty <code>false</code> is returned
     *
     * @return <code>true</code> if an attribute with the given name is present in the build part, <code>false</code> otherwise.
     */
    public boolean hasBuildAttribute(String name) {
        return buildIdentifier == null ? false : buildIdentifier.hasAttribute(name);
    }

    /**
     * If an attribute with the given name is present in the prerelease part, return the identifier after that, otherwise return <code>null</code>.
     *
     * @param name the name of the attribute to look up. If <code>null</code> or empty <code>null</code> is returned
     *
     * @return the attribute after the given name if such attribute is found in the prerelease part and there is another attribute after it,
     * otherwise <code>null</code>
     */
    public Integer getPrereleaseAttributeValue(String name) {
        return prereleaseIdentifier == null ? null : prereleaseIdentifier.getAttributeValue(name);
    }

    /**
     * If an attribute with the given name is present in the build part, return the identifier after that, otherwise return <code>null</code>.
     *
     * @param name the name of the attribute to look up. If <code>null</code> or empty <code>null</code> is returned
     *
     * @return the attribute after the given name if such attribute is found in the build part and there is another attribute after it,
     * otherwise <code>null</code>
     */
    public String getBuildAttributeValue(String name) {
        return buildIdentifier == null ? null : buildIdentifier.getAttributeValue(name);
    }

    /**
     * Returns a new version object with the new attribute added or replaced in the prerelease part. This method tries to be
     * less intrusive as it only works on the given attribute (and its optional value) while leaving the other attributes
     * unchanged.
     * <br>
     * If this version doesn't have a prerelease part, the returned version will have one with the new attribute appended
     * (and its value as well, if not <code>null</code>).
     * <br>
     * If this version already has a prerelease part with no identifier matching the given attribute name then the returned
     * version will have the same prerelease part as the current one with the new attribute appended (and its value as well,
     * if not <code>null</code>).
     * <br>
     * If this version already has a prerelease part that contains an identifier matching the given attribute name then
     * the identifier matching the attribute name is left unchanged and if the given value is not <code>null</code>,
     * the next identifier is added or replaced with the given value.<br>
     * <b>ATTENTION: if the value is not <code>null</code> the current identifier after the name (if any) is replaced
     * if it's a numeric identifier.</b>
     * <br>
     * Examples of invoking <code>setPrereleaseAttribute("build")</code> with <code>null</code> value:<br>
     * - <code>1.2.3 = 1.2.3-build</code><br>
     * - <code>1.2.3-alpha = 1.2.3-alpha.build</code><br>
     * - <code>1.2.3-alpha.beta = 1.2.3-alpha.beta.build</code><br>
     * - <code>1.2.3+timestamp = 1.2.3-build+timestamp</code><br>
     * - <code>1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha.build+timestamp.20200101</code><br>
     * - <code>1.2.3-build = 1.2.3-build</code> (unchanged)<br>
     * - <code>1.2.3-build.12345 = 1.2.3-build.12345</code> (unchanged)<br>
     * - <code>1.2.3-build.12345.timestamp.20200101 = 1.2.3-build.12345.timestamp.20200101</code> (unchanged)<br>
     * <br>
     * Examples of invoking <code>setPrereleaseAttribute("build")</code> with <code>12345</code> value:<br>
     * - <code>1.2.3 = 1.2.3-build.12345</code><br>
     * - <code>1.2.3-alpha = 1.2.3-alpha.build.12345</code><br>
     * - <code>1.2.3-alpha.beta = 1.2.3-alpha.beta.build.12345</code><br>
     * - <code>1.2.3+timestamp = 1.2.3-build.12345+timestamp</code><br>
     * - <code>1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha.build.12345+timestamp.20200101</code><br>
     * - <code>1.2.3-build = 1.2.3-build.12345</code><br>
     * - <code>1.2.3-build.12345 = 1.2.3-build.12345</code> (unchanged)<br>
     * - <code>1.2.3-build.12345.timestamp.20200101 = 1.2.3-build.12345.timestamp.20200101</code> (unchanged)<br>
     *
     * @param name the name to set for the attribute
     * @param value the value to set for the attribute, or <code>null</code> just set the attribute name, ignoring the value
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given name or value contains illegal characters
     * @throws NullPointerException if the attribute name is <code>null</code>
     */
    public SemanticVersion setPrereleaseAttribute(String name, Integer value) {
        Objects.requireNonNull(name, "Can't set a null attribute name");

        // SemanticVersionBuildIdentifier.valueOf does not accept a null for a second parameter (value) so we need to discriminate here
        if (prereleaseIdentifier == null)
            return new SemanticVersion(coreIdentifier, value == null ? SemanticVersionPreReleaseIdentifier.valueOf(false, name) : SemanticVersionPreReleaseIdentifier.valueOf(false, name, value), buildIdentifier);
        else return new SemanticVersion(coreIdentifier, prereleaseIdentifier.setAttribute(name, value), buildIdentifier);
    }

    /**
     * Returns a new version object with the new attribute added or replaced in the prerelease part. 
     * This method is a shorthand for {@link #setPrereleaseAttribute(String, Integer) setPrereleaseAttribute(value, null)} to only
     * set a simple identifier instead of a pair.
     *
     * @param name the name to set for the attribute
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given name contains illegal characters
     * @throws NullPointerException if the attribute name is <code>null</code>
     * 
     * @see #setPrereleaseAttribute(String, Integer)
     */
    public SemanticVersion setPrereleaseAttribute(String name) {
        return setPrereleaseAttribute(name, null);
    }

    /**
     * Returns a new version object with the new attribute added or replaced in the build part. This method tries to be
     * less intrusive as it only works on the given attribute (and its optional value) while leaving the other attributes
     * unchanged.
     * <br>
     * If this version doesn't have a build part, the returned version will have one with the new attribute appended
     * (and its value as well, if not <code>null</code>).
     * <br>
     * If this version already has a build part with no identifier matching the given attribute name then the returned
     * version will have the same build part as the current one with the new attribute appended (and its value as well,
     * if not <code>null</code>).
     * <br>
     * If this version already has a build part that contains an identifier matching the given attribute name then
     * the identifier matching the attribute name is left unchanged and if the given value is not <code>null</code>,
     * the next identifier is added or replaced with the given value.<br>
     * <b>ATTENTION: if the value is not <code>null</code> the current identifier after the name (if any) is replaced
     * without further consideration.</b>
     * <br>
     * Examples of invoking <code>setBuildAttribute("build")</code> with <code>null</code> value:<br>
     * - <code>1.2.3 = 1.2.3+build</code><br>
     * - <code>1.2.3-alpha = 1.2.3-alpha+build</code><br>
     * - <code>1.2.3-alpha.beta = 1.2.3-alpha.beta+build</code><br>
     * - <code>1.2.3+timestamp = 1.2.3+timestamp.build</code><br>
     * - <code>1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha+timestamp.20200101.build</code><br>
     * - <code>1.2.3+build = 1.2.3+build</code> (unchanged)<br>
     * - <code>1.2.3+build.12345 = 1.2.3+build.12345</code> (unchanged)<br>
     * - <code>1.2.3+build.12345.timestamp.20200101 = 1.2.3+build.12345.timestamp.20200101</code> (unchanged)<br>
     * <br>
     * Examples of invoking <code>setBuildAttribute("build")</code> with <code>12345</code> value:<br>
     * - <code>1.2.3 = 1.2.3+build.12345</code><br>
     * - <code>1.2.3-alpha = 1.2.3-alpha+build.12345</code><br>
     * - <code>1.2.3-alpha.beta = 1.2.3-alpha.beta+build.12345</code><br>
     * - <code>1.2.3+timestamp = 1.2.3+timestamp.build.12345</code><br>
     * - <code>1.2.3-alpha+timestamp.20200101 = 1.2.3-alpha+timestamp.20200101.build.12345</code><br>
     * - <code>1.2.3+build = 1.2.3+build.12345</code><br>
     * - <code>1.2.3+build.12345 = 1.2.3+build.12345</code> (unchanged)<br>
     * - <code>1.2.3+build.12345.timestamp.20200101 = 1.2.3+build.12345.timestamp.20200101</code> (unchanged)<br>
     *
     * @param name the name to set for the attribute
     * @param value the value to set for the attribute, or <code>null</code> just set the attribute name, ignoring the value
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given name or value contains illegal characters
     * @throws NullPointerException if the attribute name is <code>null</code>
     */
    public SemanticVersion setBuildAttribute(String name, String value) {
        Objects.requireNonNull(name, "Can't set a null attribute name");

        // SemanticVersionBuildIdentifier.valueOf does not accept a null for a second parameter (value) so we need to discriminate here
        if (buildIdentifier == null)
            return new SemanticVersion(coreIdentifier, prereleaseIdentifier, value == null ? SemanticVersionBuildIdentifier.valueOf(false, name) : SemanticVersionBuildIdentifier.valueOf(false, name, value));
        else return new SemanticVersion(coreIdentifier, prereleaseIdentifier, buildIdentifier.setAttribute(name, value));
    }

    /**
     * Returns a new version object with the new attribute added or replaced in the build part. 
     * This method is a shorthand for {@link #setBuildAttribute(String, String) setBuildAttribute(value, null)} to only
     * set a simple identifier instead of a pair.
     *
     * @param name the name to set for the attribute
     *
     * @return the new version instance
     *
     * @throws IllegalArgumentException if the given name contains illegal characters
     * @throws NullPointerException if the attribute name is <code>null</code>
     * 
     * @see #setBuildAttribute(String, String)
     */
    public SemanticVersion setBuildAttribute(String name) {
        return setBuildAttribute(name, null);
    }

    /**
     * Returns a new instance with the new attribute removed from the prerelease part, if any was present, otherwise the same version is returned.
     * If the attribute is found and <code>removeValue</code> is <code>true</code> then also the attribute value (the attribute after the
     * one identified by <code>name</code>) is removed, unless there are no more attributes after <code>name</code> or the value attribute
     * is not numeric.
     *
     * @param name the name of the attribute to remove from the prerelease part, if present. If <code>null</code> or empty no action is taken
     * @param removeValue if <code>true</code> also the attribute after <code>name</code> is removed (if any)
     *
     * @return the new instance, which might be the same of the current object if no attribute with the given <code>name</code>
     * is present
     */
    public SemanticVersion removePrereleaseAttribute(String name, boolean removeValue) {
        return prereleaseIdentifier == null ? this : new SemanticVersion(coreIdentifier, prereleaseIdentifier.removeAttribute(name, removeValue), buildIdentifier);
    }

    /**
     * Returns a new instance with the new attribute removed from the build part, if any was present, otherwise the same version is returned.
     * If the attribute is found and <code>removeValue</code> is <code>true</code> then also the attribute value (the attribute after the
     * one identified by <code>name</code>) is removed, unless there are no more attributes after <code>name</code>.
     *
     * @param name the name of the attribute to remove from the build part, if present. If <code>null</code> or empty no action is taken
     * @param removeValue if <code>true</code> also the attribute after <code>name</code> is removed (if any)
     *
     * @return the new instance, which might be the same of the current object if no attribute with the given <code>name</code>
     * is present
     */
    public SemanticVersion removeBuildAttribute(String name, boolean removeValue) {
        return buildIdentifier == null ? this : new SemanticVersion(coreIdentifier, prereleaseIdentifier, buildIdentifier.removeAttribute(name, removeValue));
    }

    /**
     * Returns a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero. Prerelease and build parts are left intact.
     *
     * @return a new instance with the major number of this current instance incremented by one and the minor and patch
     * numbers reset to zero.
     */
    public SemanticVersion bumpMajor() {
        return new SemanticVersion(coreIdentifier.bumpMajor(), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero. Prerelease and build parts are left intact.
     *
     * @return a new instance with the major number of this current instance, the minor number incremented by one and
     * the patch number reset to zero.
     */
    public SemanticVersion bumpMinor() {
        return new SemanticVersion(coreIdentifier.bumpMinor(), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new instance with the major and minor numbers of this current instance and the patch number
     * incremented by one. Prerelease and build parts are left intact.
     *
     * @return a new instance with the major and minor numbers of this current instance and the patch number
     *      * incremented by one.
     */
    public SemanticVersion bumpPatch() {
        return new SemanticVersion(coreIdentifier.bumpPatch(), prereleaseIdentifier, buildIdentifier);
    }

    /**
     * Returns a new instance with the number identified by the given value bumped.
     *
     * @param id the selector of the identifier to bump
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if <code>null</code> is passed
     */
    public SemanticVersion bump(CoreIdentifiers id) {
        Objects.requireNonNull(id);
        switch (id)
        {
            case MAJOR: return bumpMajor();
            case MINOR: return bumpMinor();
            case PATCH: return bumpPatch();
            default: throw new IllegalArgumentException(id.toString());
        }
    }

    /**
     * Returns a new instance with the number identified by the given value bumped in the prerelease part. The core and
     * the build blocks (if present) are left unchanged.
     * <br>
     * If this version doesn't have a prerelease block the returned version will have one, containing two identifiers:
     * the given string and the following number <code>.0</code>.
     * <br>
     * If this version already has a prerelease block without any identifier that equals the given id, then the returned
     * version has all the previous prerelease identifiers preceded by the two new identifiers the given string and
     * the following number <code>.1</code>.
     * If this version already has a prerelease block that contains a string identifier equal to the given id there are
     * two options: if the selected identifier already has a numeric value that follows, the returned version will have
     * that numeric identifier incremented by one; if the selected identifier doesn't have a numeric identifier that
     * follows, a new numeric identifiers is added after the string with the initial value <code>.1</code>.
     * <br>
     * If the version already has multiple identifiers in the prerelease block that equal to the given value then all of
     * them will be bumped. In case they have different numeric values (or missing) each occurrence is bumped
     * independently according to the above rules.
     * <br>
     * Examples of invoking <code>bumpPrerelease("alpha")</code> on different versions:<br>
     * - <code>1.2.3 = 1.2.3-alpha.0</code><br>
     * - <code>1.2.3-alpha = 1.2.3-alpha.0</code><br>
     * - <code>1.2.3-alpha.beta = 1.2.3-alpha.0.beta</code><br>
     * - <code>1.2.3-gamma = 1.2.3-alpha.0.gamma</code><br>
     * - <code>1.2.3-gamma.delta = 1.2.3-alpha.0.gamma.delta</code><br>
     * - <code>1.2.3+999 = 1.2.3-alpha.0+999</code><br>
     * - <code>1.2.3-alpha+999 = 1.2.3-alpha.0+999</code><br>
     * - <code>1.2.3-alpha.beta+999 = 1.2.3-alpha.0.beta+999</code><br>
     * - <code>1.2.3-gamma+999 = 1.2.3-alpha.0.gamma+999</code><br>
     * - <code>1.2.3-gamma.delta+999 = 1.2.3-alpha.0.gamma.delta+999</code><br>
     * - <code>1.2.3-alpha.alpha.1.alpha.2 = 1.2.3-alpha.0.alpha.2.alpha.3</code><br>
     *
     * @param id the selector of the identifier to bump
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if <code>null</code> is passed
     * @throws IllegalArgumentException if the given string is empty, contains illegal characters or represents a number
     */
    public SemanticVersion bumpPrerelease(String id) {
        Objects.requireNonNull(id, "Can't bump a null identifier");
        if (id.isBlank())
            throw new IllegalArgumentException("Can't bump an empty identifier");

        try {
            Integer.valueOf(id);
            // it's a number and can't be bumped
            throw new IllegalArgumentException(String.format("The value %s is numeric ant can't be used as a string identifier in the prerelease", id));
        }
        catch (NumberFormatException nfe) {
            // ok, not a number. Proceed
        }

        return new SemanticVersion(coreIdentifier, prereleaseIdentifier == null ? SemanticVersionPreReleaseIdentifier.valueOf(false, id, Integer.valueOf(DEFAULT_BUMP_VALUE)) : prereleaseIdentifier.bump(id, DEFAULT_BUMP_VALUE), buildIdentifier);
    }

    /**
     * Returns a new instance with the number identified by the given value bumped. If the given value represents a core
     * identifier ({@link CoreIdentifiers}, namely <code>major</code>, <code>minor</code>, <code>patch</code>) then that
     * identifier is bumped, otherwise the given id is used to bump a prerelease identifier by invoking
     * {@link #bumpPrerelease(String)}.
     * <br>
     * In other words this method is a shorthand for {@link #bump(CoreIdentifiers)} and {@link #bumpPrerelease(String)},
     * the latter being used only when the given id is not a core identifier.
     * <br>
     * If the version already has multiple identifiers in the prerelease block that equal to the given value then all of
     * them will be bumped. In case they have different numeric values (or missing) each occurrence is bumped
     * independently according to the above rules.
     *
     * @param id the name of the identifier to bump
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if <code>null</code> is passed
     * @throws IllegalArgumentException if the given string is empty, contains illegal characters or represents a number
     *
     * @see CoreIdentifiers
     * @see #bump(CoreIdentifiers)
     * @see #bumpPrerelease(String)
     */
    public SemanticVersion bump(String id) {
        Objects.requireNonNull(id, "Can't bump a null identifier");
        if (id.isBlank())
            throw new IllegalArgumentException("Can't bump an empty identifier");

        if (CoreIdentifiers.hasName(id))
            return bump(CoreIdentifiers.byName(id));
        else return bumpPrerelease(id);
    }
}
