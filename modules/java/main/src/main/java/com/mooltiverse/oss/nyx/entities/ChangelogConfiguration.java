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

/**
 * This object models the fields used to configure the changelog generation.
 */
public class ChangelogConfiguration {
    /**
     * The path to the destination file.
     */
    private String path = null;

    /**
     * The map of sections and commit types.
     */
    private Map<String,String> sections = null;

    /**
     * The map of substitution strings.
     */
    private Map<String,String> substitutions = null;

    /**
     * The path to the optional template file.
     */
    private String template = null;

    /**
     * Default constructor.
     */
    public ChangelogConfiguration() {
        super();
        this.sections = new HashMap<String,String>();
        this.substitutions = new HashMap<String,String>();
    }

    /**
     * Standard constructor.
     * 
     * @param path the path to the destination file. It may be {@code null}.
     * @param sections the map of sections and commit types.
     * @param template the path to the optional template file. It may be {@code null}.
     * @param substitutions the map of substitution strings.
     * 
     * @throws NullPointerException if some mandatory argument is {@code null}
     */
    public ChangelogConfiguration(String path, Map<String,String> sections, String template, Map<String,String> substitutions) {
        super();
        Objects.requireNonNull(sections);
        this.path = path;
        this.sections = sections;
        this.template = template;
        this.substitutions = substitutions;
    }

    /**
     * Returns the path to the destination file. It may be {@code null}.
     * 
     * @return the path to the destination file. It may be {@code null}.
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the path to the destination file. It may be {@code null}.
     * 
     * @param path the path to the destination file. It may be {@code null}.
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Returns the map of sections and commit types.
     * 
     * @return the map of sections and commit types.
     */
    public Map<String,String> getSections() {
        return sections;
    }

    /**
     * Sets the map of sections and commit types.
     * 
     * @param sections the map of sections and commit types.
     */
    public void setSections(Map<String,String> sections) {
        this.sections = sections;
    }

    /**
     * Returns the map of substitution strings.
     * 
     * @return the map of substitution strings.
     */
    public Map<String,String> getSubstitutions() {
        return substitutions;
    }

    /**
     * Sets the map of substitution strings.
     * 
     * @param substitutions the map of substitution strings.
     */
    public void setSubstitutions(Map<String,String> substitutions) {
        this.substitutions = substitutions;
    }

    /**
     * Returns the path to the optional template file. It may be {@code null}.
     * 
     * @return the path to the optional template file. It may be {@code null}.
     */
    public String getTemplate() {
        return template;
    }

    /**
     * Sets the path to the optional template file. It may be {@code null}.
     * 
     * @param template the path to the optional template file. It may be {@code null}.
     */
    public void setTemplate(String template) {
        this.template = template;
    }
}
