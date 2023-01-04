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

import java.io.IOException;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import org.kohsuke.github.GHAsset;
import org.kohsuke.github.GHRelease;

import com.mooltiverse.oss.nyx.entities.Attachment;
import com.mooltiverse.oss.nyx.io.TransportException;
import com.mooltiverse.oss.nyx.services.Release;

/**
 * A GitHub release.
 */
public class GitHubRelease extends GitHubEntity  implements Release {
    /**
     * The release title
     */
    private final String title;

    /**
     * The release tag
     */
    private final String tag;

    /**
     * The internal set of release assets
     */
    private final Set<Attachment> assets;

    /**
     * Creates the user object modelled by the given reference.
     * 
     * @param release the reference to the backing object. Can't be {@code null}
     * 
     * @throws NullPointerException if the given reference is {@code null}
     * @throws TransportException if the given reference can't be read for some reasons
     */
    GitHubRelease(GHRelease release)
        throws TransportException {
        super(release);
        Objects.requireNonNull(release, "The release reference cannot be null");
        this.title = release.getName();
        this.tag = release.getTagName();
        this.assets = parseAssets(release);
    }

    /**
     * Parse the GitHub release assets and returns them as a set of attachments.
     * 
     * @param release the reference to the backing object. Can't be {@code null}
     * 
     * @return the set of attachments parsed from the release assets.
     * 
     * @throws TransportException if the given reference can't be read for some reasons
     */
    private static Set<Attachment> parseAssets(GHRelease release)
        throws TransportException {
        Set<Attachment> assets = new HashSet<Attachment>();

        try {
            for (GHAsset ghAsset: release.listAssets().toList()) {
                assets.add(new Attachment(ghAsset.getName(), ghAsset.getLabel(), ghAsset.getContentType(), ghAsset.getBrowserDownloadUrl()));
            }
        }
        catch (IOException ioe) {
            throw new TransportException(String.format("Unable to retrieve the release asset"), ioe);
        }

        return assets;
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
        return title;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTag() {
        return tag;
    }
}