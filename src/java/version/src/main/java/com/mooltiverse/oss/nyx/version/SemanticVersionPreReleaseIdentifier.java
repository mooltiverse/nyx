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
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * The specialization of a Prerelease version number as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 *
 * This identifier has a peculiar behavior as numeric parts are all parsed as integers. This is also helpful to remove any
 * leading zeroes and allows bumping.
 *
 * Numeric identifiers allow bumping as well as string identifiers. When bumping a string identifier it's assumed that
 * there is a numeric identifier after the string and that numeric identifier is the value to be bumped. If the string
 * identifier doesn't have a numeric identifier next to it, the numeric identifier is appended, with its number starting
 * with the default value.
 * <br><br>
 * Examples:
 * <br><br>
 * {@code 1.2.3-4} can bump the (anonymous) value at index {@code 0}, resulting in {@code 1.2.3-5}.
 * <br><br>
 * {@code 1.2.3-alpha.4} can bump the named value {@code alpha}, resulting in {@code 1.2.3-alpha.5}. This
 * is equivalent to bumping the value at index {@code 1} as it was anonymous.
 * <br><br>
 * {@code 1.2.3-beta} can bump the named value {@code beta}, resulting in {@code 1.2.3-beta.0} (assuming
 * the default start number is {@code 0}). This is equivalent to bumping the value at index {@code 1} as it
 * was anonymous.
 * <br>
 * {@code 1.2.3-beta} can bump the named value {@code gamma}, resulting in {@code 1.2.3-beta.gamma.0}.
 * <br><br>
 * Similarly, {@code 1.2.3} can bump value anonymously the value at index {@code 0}, resulting in
 * {@code 1.2.3-0} or the named value {@code pre}, resulting in {@code 1.2.3-pre.0}.
 */
class SemanticVersionPreReleaseIdentifier extends CompositeObjectIdentifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Builds the pre-release identifier with the given values.
     *
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    private SemanticVersionPreReleaseIdentifier(List<SimpleIdentifier> children) {
        super(DEFAULT_SEPARATOR, children);
    }

    /**
     * Parses the given string and returns the new identifier modelling the single identifiers. Numeric parts are treated
     * as integers and must be positive. If a numeric part is preceded by a string part then they both have the same name.
     * String values produce identifiers with the same name as their value even when they aren't followed by a numeric part.
     *
     * @param multipleIdentifiers when {@code true} the given string is parsed as it (may) contain multiple
     * identifiers, separated by the default separator, so this method may yield to multiple identifiers.
     * When {@code false} the given string is expected to have a single identifier so if the given
     * string has multiple identifiers an exception is thrown.
     * @param s the string to parse
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the given string is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static SemanticVersionPreReleaseIdentifier valueOf(boolean multipleIdentifiers, String s) {
        if (multipleIdentifiers)
            return new SemanticVersionPreReleaseIdentifier(Parser.toIdentifiers(s, DEFAULT_SEPARATOR, UseIntegerIdentifiers.WHEN_POSSIBLE));
        else return new SemanticVersionPreReleaseIdentifier(List.<SimpleIdentifier>of(Parser.toIdentifier(s, UseIntegerIdentifiers.WHEN_POSSIBLE)));
    }

    /**
     * Creates a new identifier instance with the given identifiers. When elements of the given list are {@link Integer}
     * instances they are treated as numeric identifiers. All other object types are read using their {@link Object#toString()}
     * method. If the string returned by {@link Object#toString()} can be parsed to a positive integer then it is converted
     * to a numeric identifier, otherwise it's used as a {@link String}. Items cannot be all {@code null}.
     * String representations of objects must not be empty or contain illegal characters while {@link Integer} must be positive.
     *
     * @param multipleIdentifiers when {@code true} the given string is parsed as it (may) contain multiple
     * identifiers, separated by the default separator, so this method may yield to multiple identifiers.
     * When {@code false} the given string is expected to have a single identifier so if the given
     * string has multiple identifiers an exception is thrown.
     * @param items the items to build the identifier with
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the the isn't at least one parameter that is not {@code null}
     * @throws IllegalArgumentException if the given identifiers are not legal instances, if
     * numeric values are not positive integers or string values contain illegal characters or are empty.
     */
    static SemanticVersionPreReleaseIdentifier valueOf(boolean multipleIdentifiers, Object... items) {
        Objects.requireNonNull(items, "Can't build the list of identifiers from a null list");
        if (items.length == 0)
            throw new IllegalArgumentException("Can't build the list of identifiers from an empty list");

        List<SimpleIdentifier> identifiers = new ArrayList<SimpleIdentifier>();
        for (Object item: items) {
            Objects.requireNonNull(item, "Can't build the list of identifiers from a list with null values");

            if (multipleIdentifiers) 
                identifiers.addAll(Parser.toIdentifiers(item.toString(), DEFAULT_SEPARATOR, UseIntegerIdentifiers.WHEN_POSSIBLE));
            else identifiers.add(Parser.toIdentifier(item.toString(), UseIntegerIdentifiers.WHEN_POSSIBLE)); 
        }

        return new SemanticVersionPreReleaseIdentifier(identifiers);
    }

    /**
     * Returns a new identifier instance with the number identified by the given value bumped.
     * <br>
     * If this identifier has no identifier that equals the given id, then the returned identifier version has all the
     * previous identifiers followed by the two new identifiers: the given string and the following number
     * {@code defaultNumber}.
     * <br>
     * If this identifier already has a string identifier equal to the given id there are two options: if the selected
     * identifier already has a numeric value that follows, the returned identifier will have that numeric identifier
     * incremented by one; if the selected identifier doesn't have a numeric identifier that follows, a new numeric
     * identifiers is added after the string with the initial value {@code defaultNumber}.
     * <br>
     * If this identifier already has multiple identifiers that equal to the given value then all of them will be bumped.
     * In case they have different numeric values (or missing) each occurrence is bumped independently according to the
     * above rules.
     *
     * @param id the selector of the identifier to bump
     * @param defaultNumber the default number to use when the given identifier doesn't have a numeric part following
     * the string. This is usually set to {@code 0} or {@code 1}. It must be a non-negative integer.
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if {@code null} is passed
     * @throws IllegalArgumentException if the given string is empty, contains illegal characters or represents a number
     * or if {@code defaultNumber} is negative.
     */
    SemanticVersionPreReleaseIdentifier bump(String id, int defaultNumber) {
        Objects.requireNonNull(id, "Can't bump a null identifier");
        if (id.isBlank())
            throw new IllegalArgumentException("Can't bump an empty identifier");
        if (defaultNumber < 0)
            throw new IllegalArgumentException(String.format("Can't use a negative number for the default number to bump. '%d' was passed", defaultNumber));

        List<Object> newValues = new ArrayList<Object>();
        boolean bumped = false;
        
        Iterator<Object> previousValuesIterator = getValues().iterator();
        while (previousValuesIterator.hasNext()) {
            Object value = previousValuesIterator.next();
            newValues.add(value);
            if (id.equals(value)) {
                // if the identifier is found see if the next identifier is a number and, if so, bump its value,
                // otherwise create one
                bumped = true;
                if (previousValuesIterator.hasNext()) {
                    value = previousValuesIterator.next();
                    if (Integer.class.isInstance(value))
                        newValues.add(Integer.valueOf(Integer.class.cast(value).intValue()+1));
                    else {
                        newValues.add(Integer.valueOf(defaultNumber)); // insert a new default Integer value
                        newValues.add(value); // re-add the non-integer value
                    }
                }
                else {
                    newValues.add(Integer.valueOf(defaultNumber)); // insert a new default Integer value
                }
            }
        }
        if (!bumped) {
            // if not yet bumped it means that no identifier with such name was found, so add it at the beginning, along with the numeric identifier
            newValues.add(id);
            newValues.add(Integer.valueOf(defaultNumber));
        }
        return valueOf(false, newValues.toArray());
    }

    /**
     * Returns {@code true} if an attribute with the given name is present, {@code false} otherwise.
     *
     * @param name the name of the attribute to look up. If {@code null} or empty {@code false} is returned
     *
     * @return {@code true} if an attribute with the given name is present, {@code false} otherwise.
     */
    boolean hasAttribute(String name) {
        if ((name == null) || (name.isBlank()))
            return false;

        return getValues().contains(name);
    }

    /**
     * If an attribute with the given name is present, return the identifier after that if and only if it's a numeric
     * identifier, otherwise return {@code null}.
     *
     * @param name the name of the attribute to look up. If {@code null} or empty {@code null} is returned
     *
     * @return the attribute after the given name if such attribute is found and there is another numeric attribute after it,
     * otherwise {@code null}
     */
    Integer getAttributeValue(String name) {
        if ((name == null) || (name.isBlank()))
            return null;
        
        Iterator<Object> valuesIterator = getValues().iterator();
        while (valuesIterator.hasNext()) {
            Object value = valuesIterator.next();
            if (name.equals(value) && valuesIterator.hasNext()) {
                Object maybeTheValue = valuesIterator.next();
                return Integer.class.isInstance(maybeTheValue) ? Integer.class.cast(maybeTheValue) : null;
            }
        }

        return null;
    }

    /**
     * Returns a new instance with the new attribute added or replaced. This method tries to be less intrusive as it 
     * only works on the given attribute (and its optional value) while leaving the other attributes unchanged.
     * <br>
     * If this instance already has a prerelease part that contains an identifier matching the given attribute name then
     * the identifier matching the attribute name is left unchanged and if the given value is not {@code null},
     * the next identifier is added or replaced with the given value. <b>ATTENTION: if the value is not {@code null}
     * the identifier after the name is replaced if is a numeric identifier, otherwise it's added after the identifier name.</b>
     * <br>
     *
     * @param name the name to set for the attribute
     * @param value the value to set for the attribute, or {@code null} just set the attribute name, ignoring the value
     *
     * @return the new instance
     *
     * @throws IllegalArgumentException if the given name or value contains illegal characters
     * @throws NullPointerException if the attribute name is {@code null}
     */
    SemanticVersionPreReleaseIdentifier setAttribute(String name, Integer value) {
        Objects.requireNonNull(name, "Can't set the attribute name to a null identifier");
        if (name.isBlank())
            throw new IllegalArgumentException("Can't set the attribute name to an empty identifier");

        List<Object> newValues = new ArrayList<Object>();
        boolean found = false;

        Iterator<Object> previousValuesIterator = getValues().iterator();
        while (previousValuesIterator.hasNext()) {
            Object previousValue = previousValuesIterator.next();
            newValues.add(previousValue.toString());
            if (name.equals(previousValue.toString())) {
                // if the identifier is found re-add it and work on the next item (the value)
                
                found = true;
                if (value != null) {
                    // add the new value
                    newValues.add(value);
                    if (previousValuesIterator.hasNext()) {
                        // if the previous value is an integer do not re-add it (so we replace it with the new one)
                        Object maybeTheValueToReplace = previousValuesIterator.next();
                        if (!Integer.class.isInstance(maybeTheValueToReplace)) 
                            newValues.add(maybeTheValueToReplace);
                    }
                }
            }
        }
       
        if (!found) {
            // if not yet found it means that no identifier with such name was found, so add it at the end, along with the value
            newValues.add(name);
            if (value != null)
                newValues.add(value);
        }
        return valueOf(false, newValues.toArray());
    }

    /**
     * Returns a new instance with the new attribute removed, if any was present, otherwise the same version is returned.
     * If the attribute is found and {@code removeValue} then also the attribute value (the attribute after the
     * one identified by {@code name}) is removed (if it's a numeric value), unless there are no more attributes after {@code name}.
     * If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
     * the return value is {@code null}
     *
     * @param name the name of the attribute to remove, if present. If {@code null} or empty no action is taken
     * @param removeValue if {@code true} also the attribute after {@code name} is removed (if any and if it's numeric)
     *
     * @return the new instance, which might be the same of the current object if no attribute with the given {@code name}
     * is present. If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
     * the return value is {@code null}
     */
    SemanticVersionPreReleaseIdentifier removeAttribute(String name, boolean removeValue) {
        if (Objects.isNull(name))
            return this;

        if (!hasAttribute(name))
            return this;

        List<Object> newValues = new ArrayList<Object>();

        Iterator<Object> previousValuesIterator = getValues().iterator();
        while (previousValuesIterator.hasNext()) {
            Object previousValue = previousValuesIterator.next();
            if (name.equals(previousValue.toString())) {
                // do not re-add the name to the new values. If removeValue is true and the next element
                // is an Integer then do the same with the next element too, if any
                if (removeValue && previousValuesIterator.hasNext()) {
                    Object maybeTheValueToRemove = previousValuesIterator.next();
                    if (!Integer.class.isInstance(maybeTheValueToRemove)) 
                        newValues.add(maybeTheValueToRemove);
                }
            }
            else newValues.add(previousValue.toString());
        }
        
        return newValues.isEmpty() ? null : valueOf(false, newValues.toArray());
    }
}
