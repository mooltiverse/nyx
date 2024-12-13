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

/**
 * This object models a rule to replace some text matched by a regular expression in arbitrary text files
 * with a static or dynamic value that is configured within the rule.
 */
public class Substitution {
    /**
     * The glob expression to select the text files to replace the matched strings into.
     */
    private String files;

    /**
     * The regular expression used to match the text to be replaced replace in files.
     */
    private String match;

    /**
     * The template expression defining the text to use when replacing all matched tokens.
     */
    private String replace;

    /**
     * Default constructor.
     */
    public Substitution() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param files the glob expression to select the text files to replace the matched strings into.
     * @param match the regular expression used to match the text to be replaced replace in files.
     * @param replace the template expression defining the text to use when replacing all matched tokens.
     */
    public Substitution(String files, String match, String replace) {
        super();
        this.files = files;
        this.match = match;
        this.replace = replace;
    }

    /**
     * Returns the glob expression to select the text files to replace the matched strings into.
     * 
     * @return the glob expression to select the text files to replace the matched strings into.
     */
    public String getFiles() {
        return files;
    }

    /**
     * Sets the glob expression to select the text files to replace the matched strings into.
     * 
     * @param files glob expression to select the text files to replace the matched strings into.
     */
    public void setFiles(String files) {
        this.files = files;
    }

    /**
     * Returns the regular expression used to match the text to be replaced replace in files.
     * 
     * @return the regular expression used to match the text to be replaced replace in files.
     */
    public String getMatch() {
        return match;
    }

    /**
     * Sets the regular expression used to match the text to be replaced replace in files.
     * 
     * @param match the regular expression used to match the text to be replaced replace in files.
     */
    public void setMatch(String match) {
        this.match = match;
    }

    /**
     * Returns the template expression defining the text to use when replacing all matched tokens.
     * 
     * @return the template expression defining the text to use when replacing all matched tokens.
     */
    public String getReplace() {
        return replace;
    }

    /**
     * Sets the template expression defining the text to use when replacing all matched tokens.
     * 
     * @param replace the template expression defining the text to use when replacing all matched tokens.
     */
    public void setReplace(String replace) {
        this.replace = replace;
    }
}
