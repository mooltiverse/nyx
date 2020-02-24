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

import java.util.*;

/**
 * The specialization of a Prerelease version number as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 *
 * This handler has a peculiar behavior as numeric parts are all parsed as integers. This is also helpful to remove any
 * leading zeroes and allows bumping.
 *
 * Numeric identifiers allow bumping as well as string identifiers. When bumping a string identifier it's assumed that
 * there is a numeric identifier after the string and that numeric identifier is the value to be bumped. If the string
 * identifier doesn't have a numeric identifier next to it, the numeric identifier is appended, with its number starting
 * with the default value.
 * <br><br>
 * Examples:
 * <br><br>
 * <code>1.2.3-4</code> can bump the (anonymous) value at index <code>0</code>, resulting in <code>1.2.3-5</code>.
 * <br><br>
 * <code>1.2.3-alpha.4</code> can bump the named value <code>alpha</code>, resulting in <code>1.2.3-alpha.5</code>. This
 * is equivalent to bumping the value at index <code>1</code> as it was anonymous.
 * <br><br>
 * <code>1.2.3-beta</code> can bump the named value <code>beta</code>, resulting in <code>1.2.3-beta.0</code> (assuming
 * the default start number is <code>0</code>). This is equivalent to bumping the value at index <code>1</code> as it
 * was anonymous.
 * <br>
 * <code>1.2.3-beta</code> can bump the named value <code>gamma</code>, resulting in <code>1.2.3-beta.gamma.0</code>.
 * <br><br>
 * Similarly, <code>1.2.3</code> can bump value anonymously the value at index <code>0</code>, resulting in
 * <code>1.2.3-0</code> or the named value <code>pre</code>, resulting in <code>1.2.3-pre.0</code>.
 */
class SemanticPreReleaseVersionHandler extends CompositeObjectValueHandler {
    /**
     * Builds the value handler with the given children values.
     *
     * @param children the children of this composite handler. There must be at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given values are illegal or there isn't any non <code>null</code> item
     */
    @SuppressWarnings("unchecked")
    private SemanticPreReleaseVersionHandler(List<ObjectValueHandler> children) {
        super(DEFAULT_SEPARATOR, children);
    }

    /**
     * Creates a new handler instance with the given identifiers. Each element bust be either a {@link String} or
     * {@link Integer}. <code>null</<code> values are ignored, but items cannot be all <code>null</<code>. {@link String}
     * items must not be empty or contain illegal characters while {@link Integer} must be positive.
     *
     * @param identifiers the items to build the handler with
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the the isn't at least one parameter that is not <code>null</code>
     * @throws IllegalArgumentException if the given identifiers are not {@link String} or {@link Integer} instances, if
     * numeric values are not positive integers or string values contain illegal characters or are empty.
     */
    static SemanticPreReleaseVersionHandler of(Object... identifiers) {
        Objects.requireNonNull(identifiers);
        if (identifiers.length == 0)
            throw new IllegalArgumentException("Can't build the list of identifiers from an empty list");

        List<ObjectValueHandler> handlers = new ArrayList<ObjectValueHandler>();
        for (Object id: identifiers) {
            AbstractSimpleValueHandler handler = null;

            if (id == null) {
                // just skip it
            }
            else if (Integer.class.isInstance(id))
                handlers.add(new IntegerValueHandler(Integer.class.cast(id)));
            else if (String.class.isInstance(id)) {
                // see if it's a string representing an integer
                try {
                    handlers.add(new IntegerValueHandler(Integer.valueOf(String.class.cast(id))));
                }
                catch (NumberFormatException nfe) {
                    handlers.add(new StringValueHandler(String.class.cast(id)));
                }
            }
            else throw new IllegalArgumentException(String.format("Objects of type %s not allowed as identifier. Only String and Integer instances are supported.", id.getClass().getName()));
        }
        if (handlers.isEmpty())
            throw new NullPointerException(String.format("Identifiers must contain at leas one non null value"));
        return new SemanticPreReleaseVersionHandler(handlers);
    }

    /**
     * Parses the given string and returns the new handler modelling the single identifiers. Numeric parts are treated
     * as integers and must be positive. If a numeric part is preceded by a string part then they both have the same name.
     * String values produce handlers with the same name as their value even when they aren't followed by a numeric part.
     *
     * @param s the string to parse
     *
     * @return the new handler instance representing the given string.
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string contains illegal characters
     */
    static SemanticPreReleaseVersionHandler valueOf(String s) {
        return of((Object[]) split(s, DEFAULT_SEPARATOR));
    }

    /**
     * Returns a new instance with the number identified by the given value bumped.
     * <br>
     * If this handler has no identifier that equals the given id, then the returned handler version has all the
     * previous identifiers preceded by the two new identifiers the given string and the following number
     * <code>defaultNumber</code>.
     * <br>
     * If this handler already has a string identifier equal to the given id there are two options: if the selected
     * identifier already has a numeric value that follows, the returned handler will have that numeric identifier
     * incremented by one; if the selected identifier doesn't have a numeric identifier that follows, a new numeric
     * identifiers is added after the string with the initial value <code>defaultNumber</code>.
     * <br>
     * If this handler already has multiple identifiers that equal to the given value then all of them will be bumped.
     * In case they have different numeric values (or missing) each occurrence is bumped independently according to the
     * above rules.
     *
     * @param id the selector of the identifier to bump
     * @param defaultNumber the default number to use when the given identifier doesn't have a numeric part following
     * the string. This is usually set to <code>0</code> or <code>1</code>. It must be a non-negative integer.
     *
     * @return a new instance with the number identified by the given value bumped.
     *
     * @throws NullPointerException if <code>null</code> is passed
     * @throws IllegalArgumentException if the given string is empty, contains illegal characters or represents a number
     * or if <code>defaultNumber</code> is negative.
     */
    SemanticPreReleaseVersionHandler bump(String id, int defaultNumber) {
        Objects.requireNonNull(id);
        if (id.isEmpty())
            throw new IllegalArgumentException("Can't bump an empty identifier");
        if (defaultNumber < 0)
            throw new IllegalArgumentException(String.format("Can't use a negative number for the default number to bump. %d was passed", defaultNumber));

        List<Object> identifiers = new ArrayList<Object>();
        boolean bumped = false;
        Iterator<ObjectValueHandler> childrenIterator = children.iterator();
        while (childrenIterator.hasNext()) {
            ObjectValueHandler ovh = childrenIterator.next();
            identifiers.add(ovh.value);
            if (String.class.isInstance(ovh.value) && String.class.cast(ovh.value).equals(id)) {
                // if the identifier is found see if the next identifier is a number and, if so, bump its value,
                // otherwise create one
                bumped = true;
                if (childrenIterator.hasNext()) {
                    ovh = childrenIterator.next();
                    if (Integer.class.isInstance(ovh.value))
                        identifiers.add(Integer.valueOf(Integer.class.cast(ovh.value).intValue()+1));
                    else {
                        identifiers.add(Integer.valueOf(defaultNumber)); // insert a new Integer value handler with the default value
                        identifiers.add(ovh.value); // re-add the non-integer value handler
                    }
                }
                else {
                    identifiers.add(Integer.valueOf(defaultNumber)); // insert a new Integer value handler with value 0
                }
            }
        }
        if (!bumped) {
            // if not yet bumped it means that no identifier with such name was found, so add it at the beginning, along with the numeric identifier
            identifiers.add(0, id);
            identifiers.add(1, Integer.valueOf(defaultNumber));
        }
        return of(identifiers.toArray());
    }
}
