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

import com.mooltiverse.oss.nyx.io.TransportException;

/**
 * A service that {@link Service#supports(Feature) supports} the {@link Service.Feature#RELEASES} feature
 * to publish releases.
 */
public interface ReleaseService extends Service {
    /**
     * Finds the release in the given repository by the release tag.
     * 
     * @param owner the name of the repository owner to get the release for. It may be {@code null}, in which case,
     * the repository owner must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * @param repository the name of the repository to get the release for. It may be {@code null}, in which case,
     * the repository name must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * @param tag the tag the release refers to (i.e. {@code 1.2.3}, {@code v4.5.6}). It can't be {@code null}
     * 
     * @return the release for the given tag, or {@code null} if there is no such release
     * 
     * @throws SecurityException if authentication or authorization fails
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#RELEASES} feature.
     */
    public Release getReleaseByTag(String owner, String repository, String tag)
        throws SecurityException, TransportException;

    /**
     * Publishes a new release.
     * 
     * @param owner the name of the repository owner to create the release for. It may be {@code null}, in which case,
     * the repository owner must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * @param repository the name of the repository to create the release for. It may be {@code null}, in which case,
     * the repository name must be passed as a service option (see services implementing this interface for more
     * details on the options they accept). If not {@code null} this value overrides the option passed to the service.
     * @param title the release title, it may be the same of {@code tag} but not necessarily. It may be {@code null}
     * @param tag tag to publish the release for (i.e. {@code 1.2.3}, {@code v4.5.6}). It can't be {@code null}
     * @param description the release description. This is usually a Markdown text containing release notes or a changelog
     * or something like that giving an overall description of the release
     * 
     * @return the newly created release
     * 
     * @throws SecurityException if authentication or authorization fails
     * @throws TransportException if communication to the remote endpoint fails
     * @throws UnsupportedOperationException if the underlying implementation does not
     * {@link #supports(Service.Feature) support} the {@link Service.Feature#RELEASES} feature.
     */
    public Release publishRelease(String owner, String repository, String title, String tag, String description)
        throws SecurityException, TransportException;
}