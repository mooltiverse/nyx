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

import java.util.Objects;

/**
 * This object models a MIME attachment.
 * 
 * It can be used as a reference to local or remote files, links etc.
 */
public class Attachment {
    /**
     * The attachment name (usually the file name).
     */
    private String name = null;

    /**
     * The attachment (short) description or label.
     */
    private String description = null;

    /**
     * The attachment MIME type.
     */
    private String type = null;

    /**
     * The attachment path (local file or URL).
     */
    private String path = null;

    /**
     * Default constructor.
     */
    public Attachment() {
        super();
    }

    /**
     * Standard constructor.
     * 
     * @param name the attachment name (usually the file name).
     * @param description the attachment (short) description or label.
     * @param type the attachment MIME type.
     * @param path the attachment path (local file or URL).
     * 
     * @throws NullPointerException if some argument is {@code null}
     */
    public Attachment(String name, String description, String type, String path) {
        super();
        Objects.requireNonNull(name);
        Objects.requireNonNull(description);
        Objects.requireNonNull(type);
        Objects.requireNonNull(path);
        this.name = name;
        this.description = description;
        this.type = type;
        this.path = path;
    }

    /**
     * Returns the attachment name (usually the file name).
     * 
     * @return the attachment name (usually the file name).
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the attachment name (usually the file name).
     * 
     * @param name the attachment name (usually the file name).
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Returns the attachment (short) description or label.
     * 
     * @return the attachment (short) description or label.
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the attachment (short) description or label.
     * 
     * @param description the attachment (short) description or label.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Returns the attachment MIME type.
     * 
     * @return the attachment MIME type.
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the attachment MIME type.
     * 
     * @param type the attachment MIME type.
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Returns the attachment path (local file or URL).
     * 
     * @return the attachment path (local file or URL).
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets the attachment path (local file or URL).
     * 
     * @param path the attachment path (local file or URL).
     */
    public void setPath(String path) {
        this.path = path;
    }
}
