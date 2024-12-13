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

package configuration

import (
	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

const (
	// The name identifier for the simple preset.
	SIMPLE_NAME = "simple"
)

/*
Returns the simple configuration preset.
*/
func NewSimplePreset() *SimpleConfigurationLayer {
	scl := NewSimpleConfigurationLayer()

	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits")}, &map[string]*ent.CommitMessageConvention{"conventionalCommits": COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS})
	scl.SetCommitMessageConventions(commitMessageConventions)

	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("mainline"), utl.PointerToString("internal")}, &[]*string{}, &[]*string{}, &map[string]*ent.ReleaseType{"mainline": RELEASE_TYPES_MAINLINE, "internal": RELEASE_TYPES_INTERNAL})
	scl.SetReleaseTypes(releaseTypes)

	return scl
}
