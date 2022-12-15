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
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
)

/*
A published release. These entities are managed through services implementing the
ReleaseService interface and supporting the RELEASES feature.
*/
type Release interface {
	/*
		Returns the assets attached to the relese, otherwise returns nil.
	*/
	GetAssets() []ent.Attachment

	/*
		Returns the tag the release refers to.
	*/
	GetTag() string

	/*
		Returns the release title.
	*/
	GetTitle() string
}
