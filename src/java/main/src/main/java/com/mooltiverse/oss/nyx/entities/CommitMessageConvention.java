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
package com.mooltiverse.oss.nyx.entities;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.mooltiverse.oss.nyx.version.Scheme;

/**
 * This object models the fields used to configure a generic commit message convention.
 * <br>
 * The regular expression associated with this object is important and needs to define the following named capturing groups:<br>
 * - {@code type} yields to the commit message type (if any)<br>
 * - {@code scope} yields to the commit message scope (if any)<br>
 * - {@code title} yields to the commit message title (the short, 1 line, description of the commit)<br>
 * <br>
 * The bump regular expressions provided with this object are used to determine if a commit message is meant to bump a
 * version identifier. Each entry in the map has a version identifier as the key and a regular expression as the value.
 * When the regular expression in the value matches the commit message then the identifier in the key has to be bumped.
 * These regular expressions are evaluated simply, just match or no-match, without named groups etc.
 * The order of the entries does not matter as in case of multiple matches only the most significant identifier is
 * bumped. Identifier names depend on the versioning {@link Scheme} in use.
 */
public class CommitMessageConvention {
    /**
     * The regular expression used to parse informations from a commit message.
     */
    private String expression;

    /**
     * The map where each key is a version identifier to bump and the value is a regular expression to be evaluated
     * against the commit message. When the expression matches the commit message the version identifier
     * in the key is to be bumped.
     */
    private final Map<String,String> bumpExpressions = new HashMap<String,String>();

    /**
     * Default constructor.
     */
    public CommitMessageConvention() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param expression the regular expression used to parse informations from a commit message.
     * It must comply with the requirements define on top of this class documentation.
     * @param bumpExpressions the map where each key is a version identifier to bump and the value is a regular
     * expression to be evaluated against the commit message. When the expression matches the commit message
     * the version identifier in the key is to be bumped. It must comply with the requirements define on top
     * of this class documentation.
     */
    public CommitMessageConvention(String expression, Map<String,String> bumpExpressions) {
        super();
        this.expression = expression;
        if (!Objects.isNull(bumpExpressions))
            this.bumpExpressions.putAll(bumpExpressions);
    }

    /**
     * Returns the regular expression used to parse informations from a commit message.
     * 
     * @return the regular expression used to parse informations from a commit message.
     */
    public String getExpression() {
        return expression;
    }

    /**
     * Sets the regular expression used to parse informations from a commit message.
     * 
     * @param expression the regular expression used to parse informations from a commit message.
     */
    public void setExpression(String expression) {
        this.expression = expression;
    }

    /**
     * Returns the map where each key is a version identifier to bump and the value is a regular expression to be evaluated
     * against the commit message. When the expression matches the commit message the version identifier in the key is to be bumped.
     * 
     * @return the map where each key is a version identifier to bump and the value is a regular expression to be evaluated
     * against the commit message. Never {@code null} but may be empty.
     */
    public Map<String,String> getBumpExpressions() {
        return bumpExpressions;
    }
}
