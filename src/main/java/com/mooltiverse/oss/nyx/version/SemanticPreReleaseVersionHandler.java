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
import java.util.List;

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
class SemanticPreReleaseVersionHandler extends CompositeValueHandler {
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
    private SemanticPreReleaseVersionHandler(List<ValueHandler> children) {
        super(DEFAULT_SEPARATOR, children);
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
        List<ValueHandler> handlers = new ArrayList<ValueHandler>();
        for (String part: split(s, DEFAULT_SEPARATOR)) {
            AbstractSimpleValueHandler handler = null;
            boolean isInteger = false;
            // see if it's a number
            try {
                Integer integerValue = Integer.valueOf(part);
                isInteger = true;
            }
            catch (NumberFormatException nfe) {
                //do nothing, it's just not a valid Integer
            }
            handlers.add(isInteger ? new IntegerValueHandler(part) : new StringValueHandler(part));
        }
        return new SemanticPreReleaseVersionHandler(handlers);
    }
}
