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

var (
	// The release type used for feature branches.
	RELEASE_TYPES_FEATURE = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$"), utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, nil, utl.PointerToString("^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$"), nil, nil, utl.PointerToString("false"), nil, utl.PointerToBoolean(false))

	// The release type used for fix branches.
	RELEASE_TYPES_FIX = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-fix(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$"), utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, nil, utl.PointerToString("^fix((-|\\/)[0-9a-zA-Z-_]+)?$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("false"), nil, utl.PointerToBoolean(false))

	// The release type used for hotfix branches.
	RELEASE_TYPES_HOTFIX = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-hotfix(([0-9a-zA-Z]*)(\\.([0-9]\\d*))?)?)$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^hotfix((-|\\/)[0-9a-zA-Z-_]+)?$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("true"), nil, utl.PointerToBoolean(false))

	// The release type used for integration branches.
	RELEASE_TYPES_INTEGRATION = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(develop|development|integration|latest)(\\.([0-9]\\d*))?)$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^(develop|development|integration|latest)$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("true"), nil, utl.PointerToBoolean(false))

	// The fallback release type used for releases not fitting other, more specific, types.
	RELEASE_TYPES_INTERNAL = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("internal"), nil, nil, utl.PointerToString("false"), nil, utl.PointerToString("false"), utl.PointerToString("false"), nil, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("timestamp"), utl.PointerToString("{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}"), ent.PointerToPosition(ent.BUILD))}, nil, nil, nil, utl.PointerToString("false"), nil, utl.PointerToBoolean(false))

	// The release type used to issue official releases from the main branch.
	RELEASE_TYPES_MAINLINE = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(false), nil, nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^(master|main)$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("true"), nil, utl.PointerToBoolean(false))

	// The release type used for maintenance branches.
	RELEASE_TYPES_MAINTENANCE = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(false), nil, nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^[a-zA-Z]*([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("true"), nil, utl.PointerToBoolean(true))

	// The release type used for maturity branches.
	RELEASE_TYPES_MATURITY = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("true"), nil, utl.PointerToBoolean(false))

	// The release type used for release branches.
	RELEASE_TYPES_RELEASE = ent.NewReleaseTypeWith(nil, utl.PointerToBoolean(true), utl.PointerToString("{{#firstLower}}{{branch}}{{/firstLower}}"), nil, utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(rel|release)((\\.([0-9]\\d*))?)?)$"), utl.PointerToString("false"), nil, utl.PointerToString("true"), utl.PointerToString("true"), nil, nil, utl.PointerToString("^(rel|release)(-|\\/)({{configuration.releasePrefix}})?([0-9|x]\\d*)(\\.([0-9|x]\\d*)(\\.([0-9|x]\\d*))?)?$"), nil, ent.PointerToWorkspaceStatus(ent.CLEAN), utl.PointerToString("false"), nil, utl.PointerToBoolean(true))
)
