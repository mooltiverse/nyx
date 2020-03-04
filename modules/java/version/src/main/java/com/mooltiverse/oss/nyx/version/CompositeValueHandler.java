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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * A value handler that holds nested handlers and can form a hierarchy of values, where leaves are simple
 * values like {@link String} or {@link Integer}.
 * This implementation manages the nested handlers of the composite handler and adds the string representation of the
 * composition.
 *
 * @param <H> the type of child handlers that the composite handler accepts
 */
class CompositeValueHandler<H extends ValueHandler> extends AbstractValueHandler {
    /**
     * The nested handlers.
     */
    protected final List<H> children;

    /**
     * Store the immutable string representation to avoid repetitive formatting.
     */
    private transient String renderedString = null;

    /**
     * The separator among children values in the string representation.
     *
     * @see #DEFAULT_SEPARATOR
     */
    protected final char separator;

    /**
     * The default separator used in the string representation if no other is specified.
     *
     * @see #separator
     */
    public static final char DEFAULT_SEPARATOR = '.';

    /**
     * Builds the handler with the given nested handlers.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite handler. It must contain at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal or doesn't contain any non <code>null</code> item
     */
    @SafeVarargs
    protected CompositeValueHandler(char separator, H... children) {
        super();
        this.separator = separator;
        this.children = validate(Arrays.<H>asList(children));
    }

    /**
     * Builds the handler with the given nested handlers.
     *
     * @param children the children of this composite handler. It must contain at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal or doesn't contain any non <code>null</code> item
     */
    @SafeVarargs
    protected CompositeValueHandler(H... children) {
        this(DEFAULT_SEPARATOR, children);
    }

    /**
     * Builds the handler with the given nested handlers.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite handler. It must contain at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal or doesn't contain any non <code>null</code> item
     */
    protected CompositeValueHandler(char separator, List<H> children) {
        super();
        this.separator = separator;
        this.children = validate(children);
    }

    /**
     * Builds the handler with the given nested handlers.
     *
     * @param children the children of this composite handler. It must contain at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @throws NullPointerException if the given list of children is <code>null</code>
     * @throws IllegalArgumentException if the given value is illegal or doesn't contain any non <code>null</code> item
     */
    protected CompositeValueHandler(List<H> children) {
        this(DEFAULT_SEPARATOR, children);
    }

    /**
     * Validates the given list of handlers to make sure it's legal. If there are <code>null</code> values they are
     * ignored and removed from the returned list.
     *
     * @param children the children of this composite handler. It must contain at least one non <code>null</code> value.
     * All <code>null</code> values are ignored and stripped off.
     *
     * @return an immutable list with the same value passed as input except for <code>null</code> values, if any
     *
     * @throws NullPointerException if the given value is <code>null</code>
     * @throws IllegalArgumentException if the given list contains illegal values or does not contain any non
     * <code>null</code> item
     *
     * @param <H> the type of child handlers that the composite handler accepts
     */
    static <H extends ValueHandler> List<H> validate(List<H> children) {
        List<H> result = new ArrayList<H>();

        Objects.requireNonNull(children, "The list of nested value handlers cannot be null");
        for (H o: children) {
            if (o != null)
                result.add(o);
        }
        if (result.isEmpty())
            throw new IllegalArgumentException("The list of nested value handlers doesn't contain any non-null value");

        return Collections.<H>unmodifiableList(result);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        int hash = 31 * Character.valueOf(separator).hashCode();
        for (H c : children)
            hash = hash * 37 * c.hashCode();
        return hash;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (this == obj)
            return true;
        if (!this.getClass().isInstance(obj))
            return false;
        if (!children.equals(CompositeValueHandler.class.cast(obj).children))
            return false;
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public String toString() {
        if (renderedString == null) {
            StringBuilder sb = new StringBuilder();
            Iterator<H> i = children.iterator();
            while (i.hasNext()) {
                sb.append(i.next().toString());
                if (i.hasNext())
                    sb.append(separator);
            }
            renderedString = sb.toString();
        }
        return renderedString;
    }

    /**
     * Splits the given string into sub strings, each separated by the given separator.
     *
     * @param s the string to parse
     * @param separator the separator used for the string representation to separate children values
     *
     * @return the array of sub strings
     *
     * @throws NullPointerException if the given string is <code>null</code>
     * @throws IllegalArgumentException if the given string is empty or contains illegal characters
     */
    static String[] split(String s, char separator) {
        Objects.requireNonNull(s);
        if (s.isEmpty())
            throw new IllegalArgumentException("Can't parse an empty string");
        return s.split("["+separator+"]");
    }

    /**
     * Returns the list direct children of this composite.
     *
     * @return the list direct children of this composite.
     */
    public List<H> getChildren() {
        return children;
    }

    /**
     * Utility method that returns <code>true</code> if all the elements in the given array are <code>null</code>,
     * <code>false</code> otherwise.
     *
     * @param items the array to check
     *
     * @return <code>true</code> if all the elements in the given array are <code>null</code>
     *
     * @throws NullPointerException if the given array is <code>null</code>
     */
    public static boolean allNulls(Object[] items) {
        Objects.requireNonNull(items);
        for (Object item: items)
            if (item != null)
                return false;
        return true;
    }
}