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
package com.mooltiverse.oss.nyx.services.gitlab;

import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * A common superclass for GitLab entities providing basic features for marshalling and unmarshalling objects
 * to/from JSON.
 */
abstract class GitLabEntity {
    /**
     * The private instance of the API reference.
     */
    private final API api;

    /**
     * The private instance of the user attributes map.
     */
    private final Map<String, Object> attributes;

    /**
     * Creates the entity modelled by the given attributes.
     * 
     * @param api the reference to the API used to communicate with the remote end. Can't be {@code null}
     * @param attributes the map of attributes for this object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given attributes map is {@code null}
     * @throws IllegalArgumentException if the map of attributes is empty
     */
    protected GitLabEntity(API api, Map<String, Object> attributes) {
        super();
        Objects.requireNonNull(api, "The API reference cannot be null");
        Objects.requireNonNull(attributes, "The map of attributes cannot be null");
        if (attributes.isEmpty())
            throw new IllegalArgumentException("Attributes can't be empty");
        this.api = api;
        this.attributes = attributes;
    }

    /**
     * Reads all the attributes of the given object and puts them in the resulting map.
     * 
     * @param node the node to read the attributes from
     * 
     * @return the map with all the attributes from the given node
     * 
     * @throws NullPointerException if the given node is {@code null}
     */
    protected static Map<String, Object> toAttributeMap(JsonNode node) {
        Objects.requireNonNull(node, "Can't parse attributes from a null node");
        Map<String, Object> res = new Hashtable<String, Object>();

        Iterator<Map.Entry<String,JsonNode>> fieldsIterator = node.fields();
        while (fieldsIterator.hasNext()) {
            Map.Entry<String,JsonNode> field = fieldsIterator.next();
            // There is no way to retrieve a generic Object value so let's get them all as text
            res.put(field.getKey(), field.getValue().asText());
        }

        return res;
    }

    /**
     * Returns the internal API reference.
     * 
     * @return the internal API reference.
     */
    protected API getAPI() {
        return api;
    }

    /**
     * Returns the map of attributes this object is built on
     * 
     * @return the map of attributes this object is built on
     */
    public Map<String, Object> getAttributes() {
        return Collections.unmodifiableMap(attributes);
    }
}