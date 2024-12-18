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

package gitlab

import (
	gl "gitlab.com/gitlab-org/api/client-go" // https://pkg.go.dev/gitlab.com/gitlab-org/api/client-go

	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
)

/*
A user for a remote GitLab release.
*/
type GitLabRelease struct {
	// The assets attached to the relese, or nil.
	assets []ent.Attachment

	// The tag the release refers to.
	tag string

	// The release title.
	title string
}

/*
Creates the user object modelled by the attributed from the given reference.

Arguments are as follows:

  - release the object to read the attributes from
*/
func newGitLabRelease(release gl.Release) *GitLabRelease {
	res := &GitLabRelease{}
	res.tag = release.TagName
	res.title = release.Name
	return res
}

/*
Adds the given asset to the internal set of assets. The internal set of assets is initialized
in case it was still nil.

Arguments are as follows:

- asset the asset to add
*/
func (r *GitLabRelease) addAsset(asset ent.Attachment) *GitLabRelease {
	r.assets = append(r.assets, asset)

	return r
}

/*
Adds the given assets to the internal set of assets. The internal set of assets is initialized
in case it was still nil.

Arguments are as follows:

- assets the assets to add
*/
func (r *GitLabRelease) addAssets(assets []ent.Attachment) *GitLabRelease {
	r.assets = append(r.assets, assets...)

	return r
}

/*
Returns the assets attached to the relese, otherwise returns nil.
*/
func (r *GitLabRelease) GetAssets() []ent.Attachment {
	return r.assets
}

/*
Returns the tag the release refers to.
*/
func (r *GitLabRelease) GetTag() string {
	return r.tag
}

/*
Returns the release title.
*/
func (r *GitLabRelease) GetTitle() string {
	return r.title
}
