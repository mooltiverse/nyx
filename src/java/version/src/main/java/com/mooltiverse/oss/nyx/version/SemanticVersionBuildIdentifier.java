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
 * The specialization of a Build version number as per <a href="https://semver.org/">Semantic Versioning 2.0.0</a>.
 * 
 * This identifier has a peculiar behavior as all parts are parsed as strings, even numeric ones, to preserve any
 * leading zeroes.
 */
class SemanticVersionBuildIdentifier extends CompositeStringIdentifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;
    
    /**
     * Builds the value identifier with the given values.
     *
     * @param children the children of this composite identifier. It can't be {@code null} or contain {@code null} values
     *
     * @throws NullPointerException if the given list of children is {@code null} or contains {@code null} values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    private SemanticVersionBuildIdentifier(List<StringIdentifier> children) {
        super(DEFAULT_SEPARATOR, children);
    }

    /**
     * Returns an identifier instance representing the specified String value.
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
     * @throws IllegalArgumentException if the given string contains illegal characters or there isn't any non
     * {@code null} item
     */
    static SemanticVersionBuildIdentifier valueOf(boolean multipleIdentifiers, String s) {
        if (multipleIdentifiers)
            return new SemanticVersionBuildIdentifier(Parser.toStringIdentifiers(s, DEFAULT_SEPARATOR));
        else return new SemanticVersionBuildIdentifier(List.<StringIdentifier>of(StringIdentifier.valueOf(s)));
    }

    /**
     * Returns an identifier instance representing the specified String values.
     *
     * @param multipleIdentifiers when {@code true} the given string is parsed as it (may) contain multiple
     * identifiers, separated by the default separator, so this method may yield to multiple identifiers.
     * When {@code false} the given string is expected to have a single identifier so if the given
     * string has multiple identifiers an exception is thrown.
     * @param items the strings to parse
     *
     * @return the new identifier instance representing the given string.
     *
     * @throws NullPointerException if the given strings is {@code null}
     * @throws IllegalArgumentException if the given string contains illegal characters or there isn't any non
     * {@code null} item
     */
    static SemanticVersionBuildIdentifier valueOf(boolean multipleIdentifiers, String... items) {
        Objects.requireNonNull(items, "Can't build the list of identifiers from a null list");
        if (items.length == 0)
            throw new IllegalArgumentException("Can't build the list of identifiers from an empty list");

        List<StringIdentifier> allIdentifiers = new ArrayList<StringIdentifier>();
        for (String s: items) 
            if (multipleIdentifiers)
                allIdentifiers.addAll(Parser.toStringIdentifiers(s, DEFAULT_SEPARATOR));
            else allIdentifiers.add(StringIdentifier.valueOf(s));

        return new SemanticVersionBuildIdentifier(allIdentifiers);
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
     * If an attribute with the given name is present, return the identifier after that, otherwise return {@code null}.
     *
     * @param name the name of the attribute to look up. If {@code null} or empty {@code null} is returned
     *
     * @return the attribute after the given name if such attribute is found and there is another attribute after it,
     * otherwise {@code null}
     */
    String getAttributeValue(String name) {
        if ((name == null) || (name.isBlank()))
            return null;
        
        Iterator<Object> valuesIterator = getValues().iterator();
        while (valuesIterator.hasNext()) {
            Object value = valuesIterator.next();
            if (name.equals(value) && valuesIterator.hasNext()) 
                return valuesIterator.next().toString();
        }

        return null;
    }

    /**
     * Returns a new instance with the new attribute added or replaced. This method tries to be less intrusive as it 
     * only works on the given attribute (and its optional value) while leaving the other attributes unchanged.
     * <br>
     * If this instance already has a build part that contains an identifier matching the given attribute name then
     * the identifier matching the attribute name is left unchanged and if the given value is not {@code null},
     * the next identifier is added or replaced with the given value. <b>ATTENTION: if the value is not {@code null}
     * the identifier after the name is replaced without further consideration.</b>
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
    SemanticVersionBuildIdentifier setAttribute(String name, String value) {
        Objects.requireNonNull(name, "Can't set the attribute name to a null identifier");
        if (name.isBlank())
            throw new IllegalArgumentException("Can't set the attribute name to an empty identifier");

        List<String> newValues = new ArrayList<String>();
        boolean found = false;

        Iterator<Object> previousValuesIterator = getValues().iterator();
        while (previousValuesIterator.hasNext()) {
            Object previousValue = previousValuesIterator.next();
            newValues.add(previousValue.toString());
            if (name.equals(previousValue.toString())) {
                // if the identifier is found re-add it and work on the next item (the value)
                
                found = true;
                if (value != null) {
                    if (previousValuesIterator.hasNext())
                        previousValuesIterator.next(); // discard the previous value to replace it with the passed value
                    
                    newValues.add(value);
                }
            }
        }
       
        if (!found) {
            // if not yet found it means that no identifier with such name was found, so add it at the end, along with the value
            newValues.add(name);
            if (value != null)
                newValues.add(value);
        }
        return valueOf(false, newValues.toArray(new String[0]));
    }

    /**
     * Returns a new instance with the new attribute removed, if any was present, otherwise the same version is returned.
     * If the attribute is found and {@code removeValue} then also the attribute value (the attribute after the
     * one identified by {@code name}) is removed, unless there are no more attributes after {@code name}.
     * If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
     * the return value is {@code null}
     *
     * @param name the name of the attribute to remove, if present. If {@code null} or empty no action is taken
     * @param removeValue if {@code true} also the attribute after {@code name} is removed (if any)
     *
     * @return the new instance, which might be the same of the current object if no attribute with the given {@code name}
     * is present. If, after the removal of the attribute (and optionally its value, if any) there are no attributes left,
     * the return value is {@code null}
     */
    SemanticVersionBuildIdentifier removeAttribute(String name, boolean removeValue) {
        if (Objects.isNull(name))
            return this;
            
        if (!hasAttribute(name))
            return this;

        List<String> newValues = new ArrayList<String>();

        Iterator<Object> previousValuesIterator = getValues().iterator();
        while (previousValuesIterator.hasNext()) {
            Object previousValue = previousValuesIterator.next();
            if (name.equals(previousValue.toString())) {
                // do not re-add the name to the new values. If removeValue is true then do the same with the next element too, if any
                if (removeValue && previousValuesIterator.hasNext()) {
                    previousValuesIterator.next();
                }
            }
            else newValues.add(previousValue.toString());
        }
        
        return newValues.isEmpty() ? null : valueOf(false, newValues.toArray(new String[0]));
    }
}
