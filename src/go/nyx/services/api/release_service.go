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

package api

import (
	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
)

/*
The name of the release option used to define a release as draft.
Use this option in the 'options' map passed to publishRelease(...).
This option, when defined, must have a boolean value.
*/
const RELEASE_OPTION_DRAFT = "draft"

/*
The name of the release option used to define a release as a pre-release.
Use this option in the 'options' map passed to publishRelease(...).
This option, when defined, must have a boolean value.
*/
const RELEASE_OPTION_PRE_RELEASE = "pre-release"

/*
A service that supports the RELEASES feature to publish releases.
*/
type ReleaseService interface {
	/*
		Finds the release in the given repository by the release tag.

		Arguments are as follows:

		- owner the name of the repository owner to get the release for. It may be nil, in which case,
		  the repository owner must be passed as a service option (see services implementing this interface for more
		  details on the options they accept). If not nil this value overrides the option passed to the service.
		- repository the name of the repository to get the release for. It may be nil, in which case,
		  the repository name must be passed as a service option (see services implementing this interface for more
		  details on the options they accept). If not nil this value overrides the option passed to the service.
		- tag the tag the release refers to (i.e. 1.2.3, v4.5.6). It can't be nil

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
	*/
	GetReleaseByTag(owner *string, repository *string, tag string) (*Release, error)

	/*
		Publishes a new release.

		Arguments are as follows:

		- owner the name of the repository owner to create the release for. It may be nil, in which case,
			the repository owner must be passed as a service option (see services implementing this interface for more
			details on the options they accept). If not nil this value overrides the option passed to the service.
		- repository the name of the repository to create the release for. It may be nil, in which case,
			the repository name must be passed as a service option (see services implementing this interface for more
			details on the options they accept). If not nil this value overrides the option passed to the service.
		- title the release title, it may be the same of tag but not necessarily. It may be nil
		- tag tag to publish the release for (i.e. 1.2.3, v4.5.6). It can't be nil
		- description the release description. This is usually a Markdown text containing release notes or a changelog
			or something like that giving an overall description of the release
		- options the optional map of release options (RELEASE_OPTION_DRAFT, RELEASE_OPTION_PRE_RELEASE).
			When nil no options are evaluated.

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
	*/
	PublishRelease(owner *string, repository *string, title *string, tag string, description *string, options *map[string]interface{}) (*Release, error)

	/*
		Publishes a set of assets for a release. Even when the service supports the RELEASE_ASSETS
		feature not all types of assets may be supported. Please check the implementation class for any restrictions
		on the supported assets.

		Returns the given release with also the links to the uploaded assets. The returned object represents the same
		release provided as parameter but may be a different instance. Only the assets that were actually published
		are returned while those not supported by the implementing service are not within the list of assets referred
		by the returned object

		Arguments are as follows:

		- owner the name of the repository owner to create the assets for. It may be nil, in which case,
		  the repository owner must be passed as a service option (see services implementing this interface for more
		  details on the options they accept). If not nil this value overrides the option passed to the service.
		- repository the name of the repository to create the assets for. It may be nil, in which case,
		  the repository name must be passed as a service option (see services implementing this interface for more
		  details on the options they accept). If not nil this value overrides the option passed to the service.
		- release the release to publish the assets for. It must be an object created by the same service
		  implementation
		- assets the set of assets to publish. Assets may be interpreted differently depending on their
		  path and type. Please check the implementation class for restrictions on the supported assets

		Errors can be:

		- SecurityError if authentication or authorization fails or there is no currently authenticated user
		- TransportError if communication to the remote endpoint fails
		- UnsupportedOperationError if the underlying implementation does not support the RELEASES feature.
	*/
	PublishReleaseAssets(owner *string, repository *string, release *Release, assets []ent.Attachment) (*Release, error)
}
