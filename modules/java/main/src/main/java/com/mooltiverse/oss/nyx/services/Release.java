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
package com.mooltiverse.oss.nyx.services;

import java.util.Set;

import com.mooltiverse.oss.nyx.entities.Attachment;

/**
 * A published release. These entities are managed through services implementing the
 * {@link ReleaseService} interface and {@link Service#supports(Feature) supporting} the
 * {@link Service.Feature#RELEASES} feature.
 */
public interface Release {
    /**
     * Returns the assets attached to the relese, otherwise returns {@code null}.
     * 
     * @return the assets attached to the relese, if any, otherwise {@code null}.
     */
    public Set<Attachment> getAssets();

    /**
     * Returns the tag the release refers to.
     * 
     * @return the tag the release refers to.
     */
    public String getTag();

    /**
     * Returns the release title.
     * 
     * @return the release title.
     */
    public String getTitle();
}