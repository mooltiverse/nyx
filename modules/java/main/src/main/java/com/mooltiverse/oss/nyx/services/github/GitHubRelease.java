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
package com.mooltiverse.oss.nyx.services.github;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.services.Release;

/**
 * A GitHub release.
 */
public class GitHubRelease extends GitHubEntity implements Release {
    /**
     * The internal set of release assets
     */
    private Set<Attachment> assets = null;

    /**
     * Creates the user object modelled by the given attributes.
     * 
     * @param api the reference to the API used to communicate with the remote end. Can't be {@code null}
     * @param attributes the map of attributes for this object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given attributes map is {@code null}
     * @throws IllegalArgumentException if the map of attributes is empty
     */
    GitHubRelease(API api, Map<String, Object> attributes) {
        super(api, attributes);
    }

    /**
     * Creates the user object modelled by the given attributes.
     * 
     * @param api the reference to the API used to communicate with the remote end. Can't be {@code null}
     * @param attributes the map of attributes for this object. Can't be {@code null}
     * @param assets the set of assets attached to this release. It may be {@code null}
     * 
     * @throws NullPointerException if the given attributes map is {@code null}
     * @throws IllegalArgumentException if the map of attributes is empty
     */
    GitHubRelease(API api, Map<String, Object> attributes, Set<Attachment> assets) {
        super(api, attributes);
        this.assets = assets;
    }

    /**
     * Adds the given asset to the internal set of assets. The internal set of assets is initialized
     * in case it was still {@code null}.
     * 
     * @param asset the asset to add
     * 
     * @return this same object reference
     */
    public GitHubRelease addAsset(Attachment asset) {
        if (!Objects.isNull(asset)) {
            if (Objects.isNull(assets)) {
                assets = new HashSet<Attachment>();
            }
            assets.add(asset);
        }
            
        return this;
    }

    /**
     * Adds the given assets to the internal set of assets. The internal set of assets is initialized
     * in case it was still {@code null}.
     * 
     * @param assets the assets to add
     * 
     * @return this same object reference
     */
    public GitHubRelease addAssets(Set<Attachment> assets) {
        if (!Objects.isNull(assets)) {
            if (Objects.isNull(this.assets)) {
                this.assets = new HashSet<Attachment>();
            }
            this.assets.addAll(assets);
        }
            
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Attachment> getAssets() {
        return assets;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTitle() {
        return getAttributes().get("name").toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTag() {
        return getAttributes().get("tag_name").toString();
    }
}