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
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

const (
	// The name identifier for the extended preset.
	EXTENDED_NAME = "extended"
)

/*
Returns the extended configuration preset.
*/
func NewExtendedPreset() *SimpleConfigurationLayer {
	scl := NewSimpleConfigurationLayer()

	commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("conventionalCommits"), utl.PointerToString("gitmoji")}, &map[string]*ent.CommitMessageConvention{"conventionalCommits": COMMIT_MESSAGE_CONVENTIONS_CONVENTIONAL_COMMITS, "gitmoji": COMMIT_MESSAGE_CONVENTIONS_GITMOJI})
	scl.SetCommitMessageConventions(commitMessageConventions)

	releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("mainline"), utl.PointerToString("integration"), utl.PointerToString("maturity"), utl.PointerToString("feature"), utl.PointerToString("fix"), utl.PointerToString("hotfix"), utl.PointerToString("release"), utl.PointerToString("maintenance"), utl.PointerToString("internal")}, &[]*string{}, &[]*string{}, &map[string]*ent.ReleaseType{"mainline": RELEASE_TYPES_MAINLINE, "integration": RELEASE_TYPES_INTEGRATION, "maturity": RELEASE_TYPES_MATURITY, "feature": RELEASE_TYPES_FEATURE, "fix": RELEASE_TYPES_FIX, "hotfix": RELEASE_TYPES_HOTFIX, "release": RELEASE_TYPES_RELEASE, "maintenance": RELEASE_TYPES_MAINTENANCE, "internal": RELEASE_TYPES_INTERNAL})
	scl.SetReleaseTypes(releaseTypes)

	substitutions, _ := ent.NewSubstitutionsWith(&[]*string{}, &map[string]*ent.Substitution{"cargo_version": CARGO_VERSION, "composer_version": COMPOSER_VERSION, "dart_version": DART_VERSION, "elixir_version": ELIXIR_VERSION, "expo_version": EXPO_VERSION, "helm_version": HELM_VERSION, "node_version": NODE_VERSION, "text_version": TEXT_VERSION})
	scl.SetSubstitutions(substitutions)

	return scl
}
