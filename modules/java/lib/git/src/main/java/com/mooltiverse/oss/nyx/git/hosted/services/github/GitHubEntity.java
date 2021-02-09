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
package com.mooltiverse.oss.nyx.git.hosted.services.github;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * A common superclass for GitLab entities
 */
abstract class GitHubEntity {
    /**
     * The private instance of the session object
     */
    private final GitHubSession session;

    /**
     * Default constructor is hidden on purpose
     */
    private GitHubEntity() {
        super();
        session = null;
    }

    /**
     * Creates the object associated with the given session
     * 
     * @param session the session associated with this entity
     * 
     * @throws NullPointerException if the given session is <code>null</code>
     */
    protected GitHubEntity(GitHubSession session) {
        super();
        Objects.requireNonNull(session, "The session can't be null");
        this.session = session;
    }

    /**
     * Returns the service associated with this entity.
     * 
     * @return the service associated with this entity.
     */
    public GitHub getService() {
        return session.getService();
    }

    /**
     * Returns the session associated with this entity.
     * 
     * @return the session associated with this entity.
     */
    public GitHubSession getSession() {
        return session;
    }

    /**
     * Reads all the attributes of the given object and puts them in the resulting map.
     * 
     * @param node the node to read the attributes from
     * 
     * @return the map with all the attributes from the given node
     * 
     * @throws NullPointerException if the given node is <code>null</code>
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
}