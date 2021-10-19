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
package com.mooltiverse.oss.nyx.data;

/**
 * This object models a custom identifier to be used in version names. Each custom identifier is made of an optional
 * qualifier (which will appear as the leftmost identifier, if present), an optional value (which will appear as the
 * rightmost identifier, if present) and a position, indicating in which part of the version the identifier has to be
 * placed.
 * <br>
 * At least one among the qualifier or the value must be present. The qualifier and the value can be templates to be
 * rendered.
 */
public class Identifier {
    /**
     * The identifier qualifier.
     */
    private String qualifier;

    /**
     * The identifier value.
     */
    private String value;

    /**
     * The identifier position.
     */
    private IdentifierPosition position;

    /**
     * Default constructor.
     */
    public Identifier() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param qualifier the identifier qualifier
     * @param value the identifier value
     * @param position the identifier position
     */
    public Identifier(String qualifier, String value, IdentifierPosition position) {
        super();
        this.qualifier = qualifier;
        this.value = value;
        this.position = position;
    }

    /**
     * Returns the identifier qualifier
     * 
     * @return the identifier qualifier
     */
    public String getQualifier() {
        return qualifier;
    }

    /**
     * Sets the identifier qualifier
     * 
     * @param qualifier the identifier qualifier
     */
    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    /**
     * Returns the identifier value
     * 
     * @return the identifier value
     */
    public String getValue() {
        return value;
    }

    /**
     * Sets the identifier value
     * 
     * @param value the identifier value
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Returns the identifier position
     * 
     * @return the identifier position
     */
    public IdentifierPosition getPosition() {
        return position;
    }

    /**
     * Sets the identifier position
     * 
     * @param position the identifier position
     */
    public void setPosition(IdentifierPosition position) {
        this.position = position;
    }
}
