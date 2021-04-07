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

import java.util.List;
import java.util.Map;

/**
 * Models the commit message conventions configuration block.
 */
public interface CommitMessageConventions extends Block {
    /**
     * Returns the list of enabled commit message convention names.
     * 
     * @return the list of enabled commit message convention names. It may be {@code null} if not configured.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public List<String> getEnabled()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the map of the commit message conventions configured in this block, where keys are convention names
     * and values are actual convention objects.
     * 
     * @return the map of the commit message conventions configured in this block, where keys are convention names
     * and values are actual convention objects. It may be {@code null} if not configured.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public Map<String,CommitMessageConvention> getItems()
        throws DataAccessException, IllegalPropertyException;

    /**
     * Returns the commit message convention with the given name configured in this block, if any.
     * 
     * @param name the name of the item to retrieve. Cannot be {@code null}.
     * 
     * @return the commit message convention with the given name configured in this block, if any.
     * It may be {@code null} if this block has no such item.
     * 
     * @throws DataAccessException in case the option cannot be read or accessed.
     * @throws IllegalPropertyException in case the option has been defined but has incorrect values or it can't be resolved.
     */
    public CommitMessageConvention getItem(String name)
        throws DataAccessException, IllegalPropertyException;
}
