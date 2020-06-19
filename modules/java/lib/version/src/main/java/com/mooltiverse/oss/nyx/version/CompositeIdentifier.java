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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * An identifier holding multiple child identifiers.
 */
abstract class CompositeIdentifier extends Identifier {
    /**
     * Serial version UID to comply with {@link java.io.Serializable}
     */
    private static final long serialVersionUID = 1L;

    /**
     * The nested identifiers.
     */
    protected final List<? extends Identifier> children;

    /**
     * Store the immutable string representation after the first rendering to avoid repetitive formatting.
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
     * Builds the identifier with the given nested identifiers and separator.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite identifier. It can't be <code>null</code> or contain <code>null</code> values
     *
     * @throws NullPointerException if the given list of children is <code>null</code> or contains <code>null</code> values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    @SafeVarargs
    protected CompositeIdentifier(char separator, Identifier... children) {
        super();
        this.separator = separator;
        this.children = Parser.validateElements(Arrays.<Identifier>asList(children));
    }

    /**
     * Builds the identifier with the given nested identifiers and the default separator.
     *
     * @param children the children of this composite identifier. It can't be <code>null</code> or contain <code>null</code> values
     *
     * @throws NullPointerException if the given list of children is <code>null</code> or contains <code>null</code> values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    @SafeVarargs
    protected CompositeIdentifier(Identifier... children) {
        this(DEFAULT_SEPARATOR, children);
    }

    /**
     * Builds the identifier with the given nested identifiers and separator.
     *
     * @param separator the separator used for the string representation to separate children values
     * @param children the children of this composite identifier. It can't be <code>null</code> or contain <code>null</code> values
     *
     * @throws NullPointerException if the given list of children is <code>null</code> or contains <code>null</code> values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeIdentifier(char separator, List<? extends Identifier> children) {
        super();
        this.separator = separator;
        this.children = Parser.validateElements(children);
    }

    /**
     * Builds the identifier with the given nested identifiers and the default separator.
     *
     * @param children the children of this composite identifier. It can't be <code>null</code> or contain <code>null</code> values
     *
     * @throws NullPointerException if the given list of children is <code>null</code> or contains <code>null</code> values
     * @throws IllegalArgumentException if the given list of children contains illegal values
     */
    protected CompositeIdentifier(List<? extends Identifier> children) {
        this(DEFAULT_SEPARATOR, children);
    }

    /**
     * Returns the underlying list of identifiers, children of this composite identifier
     * 
     * @return the underlying list of identifiers
     * 
     * @see #getValues()
     */
    public List<? extends Identifier> getValue() {
        return children;
    }

    /**
     * Returns the list of values held by child identifiers
     * 
     * @return the underlying list of values
     * 
     * @see #getValue()
     */
    public List<Object> getValues() {
        List<Object> values = new ArrayList<Object>(children.size());
        for (Identifier child: children) {
            values.add(child.getValue());
        }

        return values;
    }

    /**
     * {@inheritDoc}
     */
    public int hashCode() {
        return 31 * Character.valueOf(separator).hashCode() + 37 * children.hashCode();
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
        if (!children.equals(this.getClass().cast(obj).children))
            return false;
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public String toString() {
        if (renderedString == null) {
            StringBuilder sb = new StringBuilder();
            Iterator<? extends Identifier> i = children.iterator();
            while (i.hasNext()) {
                sb.append(i.next().toString());
                if (i.hasNext())
                    sb.append(separator);
            }
            renderedString = sb.toString();
        }
        return renderedString;
    }
}