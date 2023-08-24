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

/*
This is the state package for Nyx, providing objects to share the current state of execution within the program.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package state

import (
	"bytes"         // https://pkg.go.dev/bytes
	"encoding/json" // https://pkg.go.dev/encoding/json
	"fmt"           // https://pkg.go.dev/fmt
	"strconv"       // https://pkg.go.dev/strconv
	"time"          // https://pkg.go.dev/time

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	io "github.com/mooltiverse/nyx/modules/go/nyx/io"
	tpl "github.com/mooltiverse/nyx/modules/go/nyx/template"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

/*
The State class holds a number of attributes resulting from the execution of one or more command and so represents
the current status of a release process at a certain point in time.

Each command updates the state object with new or modified attributes so the same state instance is shared among
all commands.

There must be only one instance of this class for every execution and it's retrieved by Nyx.State().
*/
type State struct {
	// The current Git branch name.
	Branch *string `json:"branch,omitempty" yaml:"branch,omitempty" handlebars:"branch"`

	// The identifier to bump.
	Bump *string `json:"bump,omitempty" yaml:"bump,omitempty" handlebars:"bump"`

	// The private instance of the configuration.
	Changelog *ent.Changelog `json:"changelog,omitempty" yaml:"changelog,omitempty" handlebars:"changelog"`

	// The private instance of the configuration.
	Configuration *cnf.Configuration `json:"configuration,omitempty" yaml:"configuration,omitempty" handlebars:"configuration"`

	// The map containing the internal attributes.
	Internals *map[string]string `json:"internals,omitempty" yaml:"internals,omitempty" handlebars:"internals"`

	// The flag indicating if the version is the latest in the repository, according to the scheme.
	LatestVersion *bool `json:"latestVersion,omitempty" yaml:"latestVersion,omitempty" handlebars:"latestVersion"`

	// The list containing the released assets.
	ReleaseAssets *[]ent.Attachment `json:"releaseAssets,omitempty" yaml:"releaseAssets,omitempty" handlebars:"releaseAssets"`

	// The private instance of the release scope.
	ReleaseScope *ent.ReleaseScope `json:"releaseScope,omitempty" yaml:"releaseScope,omitempty" handlebars:"releaseScope"`

	// The private instance of the release type.
	ReleaseType *ent.ReleaseType `json:"releaseType,omitempty" yaml:"releaseType,omitempty" handlebars:"releaseType"`

	// The latest timestamp that was taken. This is initialized by default to the date and
	// time the instance of this class has been created.
	Timestamp *int64 `json:"timestamp,omitempty" yaml:"timestamp,omitempty" handlebars:"timestamp"`

	// The version that has been inferred.
	Version *string `json:"version,omitempty" yaml:"version,omitempty" handlebars:"version"`

	// The version build metadata that has been inferred.
	VersionBuildMetadata *string `json:"versionBuildMetadata,omitempty" yaml:"versionBuildMetadata,omitempty" handlebars:"versionBuildMetadata"`

	// The version major number that has been inferred.
	VersionMajorNumber *string `json:"versionMajorNumber,omitempty" yaml:"versionMajorNumber,omitempty" handlebars:"versionMajorNumber"`

	// The version minor number that has been inferred.
	VersionMinorNumber *string `json:"versionMinorNumber,omitempty" yaml:"versionMinorNumber,omitempty" versionMinorNumber:"version"`

	// The version patch number that has been inferred.
	VersionPatchNumber *string `json:"versionPatchNumber,omitempty" yaml:"versionPatchNumber,omitempty" handlebars:"versionPatchNumber"`

	// The version pre-release identifier that has been inferred.
	VersionPreReleaseIdentifier *string `json:"versionPreReleaseIdentifier,omitempty" yaml:"versionPreReleaseIdentifier,omitempty" handlebars:"versionPreReleaseIdentifier"`

	// The regular expression used to check the version against a range constraint.
	VersionRange *string `json:"versionRange,omitempty" yaml:"versionRange,omitempty" handlebars:"versionRange"`
}

/*
A flat representation of the State structure where al values are resolved and stored as local fiends instead of retrieving
them through getter methods.

This structure is detached from the State but is useful when marshalling or rendering the State.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.

Marshalling is handled by the MarshalJSON() and MarshalYAML() methods. Unmarshalling does not require any custom handling
because transient fields are to be ignored when unmarshalling.

Objects of this type can be retrieved using Flatten().
*/
type FlatState struct {
	// The current Git branch name.
	Branch *string `json:"branch,omitempty" yaml:"branch,omitempty" handlebars:"branch"`

	// The identifier to bump.
	Bump *string `json:"bump,omitempty" yaml:"bump,omitempty" handlebars:"bump"`

	// The private instance of the configuration.
	Changelog *ent.Changelog `json:"changelog,omitempty" yaml:"changelog,omitempty" handlebars:"changelog"`

	// The private instance of the configuration.
	Configuration *cnf.SimpleConfigurationLayer `json:"configuration,omitempty" yaml:"configuration,omitempty" handlebars:"configuration"`

	// The cached value for the coreVersion attribute. It's required to cache this value or marshalling/unmarshalling won't work
	CoreVersionCache *bool `json:"coreVersion,omitempty" yaml:"coreVersion,omitempty" handlebars:"coreVersion"`

	// The directory cached from the configuration. It's required to cache this value or marshalling/unmarshalling won't work
	DirectoryCache *string `json:"directory,omitempty" yaml:"directory,omitempty" handlebars:"directory"`

	// The map containing the internal attributes.
	Internals *map[string]string `json:"internals,omitempty" yaml:"internals,omitempty" handlebars:"internals"`

	// The flag indicating if the version is the latest in the repository, according to the scheme.
	LatestVersion *bool `json:"latestVersion,omitempty" yaml:"latestVersion,omitempty" handlebars:"latestVersion"`

	// The cached value for the newVersion attribute. It's required to cache this value or marshalling/unmarshalling won't work
	NewVersionCache *bool `json:"newVersion,omitempty" yaml:"newVersion,omitempty" handlebars:"newVersion"`

	// The cached value for the newRelease attribute. It's required to cache this value or marshalling/unmarshalling won't work
	NewReleaseCache *bool `json:"newRelease,omitempty" yaml:"newRelease,omitempty" handlebars:"newRelease"`

	// The list containing the released assets.
	ReleaseAssets *[]ent.Attachment `json:"releaseAssets,omitempty" yaml:"releaseAssets,omitempty" handlebars:"releaseAssets"`

	// The private instance of the release scope.
	ReleaseScope *ent.FlatReleaseScope `json:"releaseScope,omitempty" yaml:"releaseScope,omitempty" handlebars:"releaseScope"`

	// The private instance of the release type.
	ReleaseType *ent.ReleaseType `json:"releaseType,omitempty" yaml:"releaseType,omitempty" handlebars:"releaseType"`

	// The scheme cached from the configuration. It's required to cache this value or marshalling/unmarshalling won't work
	SchemeCache *ver.Scheme `json:"scheme,omitempty" yaml:"scheme,omitempty" handlebars:"scheme"`

	// The latest timestamp that was taken. This is initialized by default to the date and
	// time the instance of this class has been created.
	Timestamp *int64 `json:"timestamp,omitempty" yaml:"timestamp,omitempty" handlebars:"timestamp"`

	// The version that has been inferred.
	Version *string `json:"version,omitempty" yaml:"version,omitempty" handlebars:"version"`

	// The version build metadata that has been inferred.
	VersionBuildMetadata *string `json:"versionBuildMetadata,omitempty" yaml:"versionBuildMetadata,omitempty" handlebars:"versionBuildMetadata"`

	// The version major number that has been inferred.
	VersionMajorNumber *string `json:"versionMajorNumber,omitempty" yaml:"versionMajorNumber,omitempty" handlebars:"versionMajorNumber"`

	// The version minor number that has been inferred.
	VersionMinorNumber *string `json:"versionMinorNumber,omitempty" yaml:"versionMinorNumber,omitempty" versionMinorNumber:"version"`

	// The version patch number that has been inferred.
	VersionPatchNumber *string `json:"versionPatchNumber,omitempty" yaml:"versionPatchNumber,omitempty" handlebars:"versionPatchNumber"`

	// The version pre-release identifier that has been inferred.
	VersionPreReleaseIdentifier *string `json:"versionPreReleaseIdentifier,omitempty" yaml:"versionPreReleaseIdentifier,omitempty" handlebars:"versionPreReleaseIdentifier"`

	// The regular expression used to check the version against a range constraint.
	VersionRange *string `json:"versionRange,omitempty" yaml:"versionRange,omitempty" handlebars:"versionRange"`
}

/*
Default constructor.

Arguments are as follows:

- configuration the configuration object held by this state

Errors can be:

- NilPointerError in case configuration is nil
*/
func NewStateWith(configuration *cnf.Configuration) (*State, error) {
	log.Trace("new state object")

	if configuration == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", "configuration")}
	}

	state := State{}
	state.Configuration = configuration
	state.Internals = &map[string]string{}
	state.ReleaseAssets = &[]ent.Attachment{}
	state.ReleaseScope = ent.NewReleaseScope()
	state.TouchTimestamp()
	return &state, nil
}

/*
Loads the state attributes from a previously saved state file. All attributes are loaded from the given file
but the nested configuration is replaced by the given one.

Arguments are as follows:

  - stateFile the file to load the state from
  - configuration the configuration object to use for the resumed state. This object will be set as the
    configuration of the returned instance.

Errors can be:

- DataAccessError: in case the state file cannot be read or accessed.
- IllegalPropertyError: in case the state file has incorrect values.
*/
func Resume(stateFile string, configuration *cnf.Configuration) (*State, error) {
	state := &State{}

	err := io.LoadFromFile(stateFile, state)
	if err != nil {
		return nil, err
	}

	state.Configuration = configuration
	return state, nil
}

/*
Resolves the cached values so they're up to date when marshalling occurs.

The returned object is a copy of this one with all of the values resolved.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) Flatten() (*FlatState, error) {
	var err error
	// Invoking all the getter methods causes this object to resolve all fields, even those that weren't
	// resolved before.
	resolvedState := &FlatState{}

	resolvedState.Branch, err = s.GetBranch()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "branch"), Cause: err}
	}
	resolvedState.Bump, err = s.GetBump()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "bump"), Cause: err}
	}
	resolvedState.Changelog, err = s.GetChangelog()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "changelog"), Cause: err}
	}
	cnf := s.GetConfiguration()
	if cnf != nil {
		// use the flattened version of the configuration for full marshalling and rendering support
		resolvedState.Configuration, err = cnf.Flatten()
		if err != nil {
			return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to flatten configuration option '%s'", "configuration"), Cause: err}
		}
	}
	// Booleans cause issues when 'omitempty' as when they are false they are assumed empty and not rendered.
	// To work around this we need to convert to a pointer.
	cVersion, err := s.GetCoreVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "coreVersion"), Cause: err}
	}
	resolvedState.CoreVersionCache = &cVersion
	resolvedState.DirectoryCache, err = s.GetDirectory()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "directory"), Cause: err}
	}
	resolvedState.Internals, err = s.GetInternals()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "internals"), Cause: err}
	}
	resolvedState.LatestVersion, err = s.GetLatestVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "latestVersion"), Cause: err}
	}
	// Booleans cause issues when 'omitempty' as when they are false they are assumed empty and not rendered.
	// To work around this we need to convert to a pointer.
	nRelease, err := s.GetNewRelease()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "newRelease"), Cause: err}
	}
	resolvedState.NewReleaseCache = &nRelease
	// Booleans cause issues when 'omitempty' as when they are false they are assumed empty and not rendered.
	// To work around this we need to convert to a pointer.
	nVersion, err := s.GetNewVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "newVersion"), Cause: err}
	}
	resolvedState.NewVersionCache = &nVersion
	resolvedState.ReleaseAssets, err = s.GetReleaseAssets()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseAssets"), Cause: err}
	}
	rs, err := s.GetReleaseScope()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseScope"), Cause: err}
	}
	if rs != nil {
		// use the flattened version of the configuration for full marshalling and rendering support
		resolvedState.ReleaseScope, err = rs.Flatten()
		if err != nil {
			return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to flatten configuration option '%s'", "releaseScope"), Cause: err}
		}
	}
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseScope"), Cause: err}
	}
	resolvedState.ReleaseType, err = s.GetReleaseType()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseType"), Cause: err}
	}
	resolvedState.Timestamp, err = s.GetTimestamp()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "timestamp"), Cause: err}
	}
	resolvedState.Version, err = s.GetVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "version"), Cause: err}
	}
	resolvedState.VersionBuildMetadata, err = s.GetVersionBuildMetadata()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionBuildMetadata"), Cause: err}
	}
	resolvedState.VersionMajorNumber, err = s.GetVersionMajorNumber()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionMajorNumber"), Cause: err}
	}
	resolvedState.VersionMinorNumber, err = s.GetVersionMinorNumber()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionMinorNumber"), Cause: err}
	}
	resolvedState.VersionPatchNumber, err = s.GetVersionPatchNumber()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionPatchNumber"), Cause: err}
	}
	resolvedState.VersionPreReleaseIdentifier, err = s.GetVersionPreReleaseIdentifier()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionPreReleaseIdentifier"), Cause: err}
	}
	resolvedState.VersionRange, err = s.GetVersionRange()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "versionRange"), Cause: err}
	}
	resolvedState.SchemeCache, err = s.GetScheme()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "scheme"), Cause: err}
	}

	return resolvedState, nil
}

/*
Adds the JSON marshalling feature to the state object

This method implements the Marshaler interface in the JSON package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) MarshalJSON() ([]byte, error) {
	flatState, err := s.Flatten()
	if err != nil {
		return nil, err
	}

	// marshal the resolved copy to avoid recursion and a stack overflow
	return json.Marshal(flatState)
}

/*
Adds the YAML marshalling feature to the state object.

This method implements the Marshaler interface in the YAML package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) MarshalYAML() (interface{}, error) {
	flatState, err := s.Flatten()
	if err != nil {
		return nil, err
	}

	// marshal the resolved copy to avoid recursion and a stack overflow
	return flatState, nil
}

/*
Returns the current Git branch name.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetBranch() (*string, error) {
	return s.Branch, nil
}

/*
Returns true if the scope has a non nil branch name.
*/
func (s *State) HasBranch() bool {
	branch, err := s.GetBranch()
	if err != nil {
		return false
	}
	return branch != nil
}

/*
Sets the current Git branch name

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) SetBranch(branch *string) error {
	s.Branch = branch
	return nil
}

/*
Returns the version identifier to bump or bumped on the previous release to produce the new release, if any.
This value is only available after Nyx.infer() has run unless it's overridden by the configuration,
in which case the configuration value is returned.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetBump() (*string, error) {
	configurationBump, err := s.GetConfiguration().GetBump()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the configuration bump attribute", Cause: err}
	}
	if configurationBump == nil {
		return s.Bump, nil
	} else {
		return configurationBump, nil
	}
}

/*
*
* Returns true if the scope has a non nil bump identifier.
 */
func (s *State) HasBump() bool {
	bump, err := s.GetBump()
	if err != nil {
		return false
	}
	return bump != nil
}

/*
Sets the identifier to bump.

Since this option can be overridden by configuration this method can only be invoked when the
configuration doesn't already have a bump attribute otherwise an IllegalStateError is thrown.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
- IllegalStateError: if the configuration already has a value for the bump attribute.
*/
func (s *State) SetBump(bump *string) error {
	configurationBump, err := s.GetConfiguration().GetBump()
	if err != nil {
		return &errs.IllegalStateError{Message: "unable to read the configuration bump attribute", Cause: err}
	}
	if configurationBump == nil {
		s.Bump = bump
	} else {
		return &errs.IllegalStateError{Message: fmt.Sprintf("the state bump attribute can't be set when it's ovverridden by the configuration. Configuration bump attribute is '%s'", *bump)}
	}

	return nil
}

/*
Returns the current changelog data model.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetChangelog() (*ent.Changelog, error) {
	return s.Changelog, nil
}

/*
Returns true if the scope has a non nil changelog data model.
*/
func (s *State) HasChangelog() bool {
	changelog, err := s.GetChangelog()
	if err != nil {
		return false
	}
	return changelog != nil
}

/*
Sets the changelog data model

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/

func (s *State) SetChangelog(changelog *ent.Changelog) error {
	s.Changelog = changelog
	return nil
}

/*
Returns the configuration object. The configuration is a live reference.
*/
func (s *State) GetConfiguration() *cnf.Configuration {
	return s.Configuration
}

/*
Returns true if the version only brings core identifiers (according to the scheme), usually meaning it is an official version.
This mehod also takes into account whether the configuration requires leniency or it has a prefix configured.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetCoreVersion() (bool, error) {
	if s.HasVersion() {
		version, err := s.GetVersion()
		if err != nil {
			return false, err
		}
		scheme, err := s.GetScheme()
		if err != nil {
			return false, err
		}
		releaseLenient, err := s.GetConfiguration().GetReleaseLenient()
		if err != nil {
			return false, err
		}
		if releaseLenient != nil && *releaseLenient {
			return ver.IsCoreWithLenience(*scheme, *version, *releaseLenient), nil
		} else {
			releasePrefix, err := s.GetConfiguration().GetReleasePrefix()
			if err != nil {
				return false, err
			}
			return ver.IsCoreWithPrefix(*scheme, *version, releasePrefix), nil
		}
	} else {
		return false, nil
	}
}

/*
Returns the directory used as the working directory as it's defined by the configuration.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetDirectory() (*string, error) {
	return s.GetConfiguration().GetDirectory()
}

/*
Returns the live map of internal attributes.

Internal attributes are not documented so they must not be used by users as the implementation may change
them at any time. Commands and other implementation objects are free to store and remove their own
attributes here (i.e. for caching or store their internal state).

When handling these attributes, entities must make sure the names (keys) do not overlap, unless for
shared attributes.

This object takes no control over the values stored in this map.

Sensitive informations must not be stored here as they would be exposed when marshalling the attributes
to files.
*/
func (s *State) GetInternals() (*map[string]string, error) {
	return s.Internals, nil
}

/*
Returns the flag indicating if the version is the latest in the repository, according to the scheme.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetLatestVersion() (*bool, error) {
	if s.HasVersion() {
		return s.LatestVersion, nil
	} else {
		return nil, nil
	}
}

/*
Returns true if the scope has a non nil latest version flag.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) HasLatestVersion() bool {
	latestVersion, err := s.GetLatestVersion()
	if err != nil {
		return false
	}
	return latestVersion != nil
}

/*
Sets the flag indicating if the version is the latest in the repository, according to the scheme.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
- IllegalStateError: if the configuration already has a value for the bump attribute.
*/
func (s *State) SetLatestVersion(latestVersion *bool) error {
	s.LatestVersion = latestVersion
	return nil
}

/*
Returns true if the version is different than the previous version and a new release has to be
published on the new version.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetNewRelease() (bool, error) {
	if s.ReleaseType == nil {
		return false, nil
	}
	newVersion, err := s.GetNewVersion()
	if err != nil {
		return false, err
	}
	publish := s.ReleaseType.GetPublish()
	if publish == nil {
		return false, nil
	}
	renderedPublish, err := tpl.Render(*publish, s)
	return newVersion && tpl.ToBoolean(&renderedPublish), nil
}

/*
Returns true if the version is different than the previous version.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetNewVersion() (bool, error) {
	if s.HasVersion() {
		version, err := s.GetVersion()
		if err != nil {
			return false, err
		}
		releaseScope, err := s.GetReleaseScope()
		if err != nil {
			return false, err
		}
		previousVersion := releaseScope.GetPreviousVersion()
		if err != nil {
			return false, err
		}
		if previousVersion != nil && *version == *previousVersion {
			return false, nil
		} else {
			if s.HasReleaseType() {
				releaseType, err := s.GetReleaseType()
				if err != nil {
					return false, err
				}
				collapseVersion := releaseType.GetCollapseVersions()
				if collapseVersion != nil && *collapseVersion == true {
					primeVersion := releaseScope.GetPrimeVersion()
					if primeVersion != nil && *version == *primeVersion {
						return false, nil
					} else {
						return true, nil
					}
				} else {
					return true, nil
				}
			} else {
				return true, nil
			}
		}
	} else {
		return false, nil
	}
}

/*
Returns the list of assets published with the release.
*/
func (s *State) GetReleaseAssets() (*[]ent.Attachment, error) {
	return s.ReleaseAssets, nil
}

/*
Sets the list of assets published with the release.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) SetReleaseAssets(releaseAssets *[]ent.Attachment) error {
	s.ReleaseAssets = releaseAssets
	return nil
}

/*
Returns the object modelling the attributes defining the scope of the release.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetReleaseScope() (*ent.ReleaseScope, error) {
	return s.ReleaseScope, nil
}

/*
Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetReleaseType() (*ent.ReleaseType, error) {
	return s.ReleaseType, nil
}

/*
Returns true if the scope has a non nil release type set.
*/
func (s *State) HasReleaseType() bool {
	releaseType, err := s.GetReleaseType()
	if err != nil {
		return false
	}
	return releaseType != nil
}

/*
Sets the selected release type.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) SetReleaseType(releaseType *ent.ReleaseType) error {
	s.ReleaseType = releaseType
	return nil
}

/*
Returns the versioning scheme used as it's defined by the configuration.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetScheme() (*ver.Scheme, error) {
	return s.GetConfiguration().GetScheme()
}

/*
Returns the current timestamp.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetTimestamp() (*int64, error) {
	return s.Timestamp, nil
}

/*
Sets the state timestamp.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) SetTimestamp(timestamp *int64) error {
	s.Timestamp = timestamp
	return nil
}

/*
Returns the version inferred by Nyx, if any. If the version was overridden by configuration this will be the
same as Configuration.getVersion(). This value is only available after Nyx.infer() has run.

The returned version also has the configured prefix, if any.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersion() (*string, error) {
	configurationVersion, err := s.GetConfiguration().GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the configuration version attribute", Cause: err}
	}
	if configurationVersion == nil {
		return s.Version, nil
	} else {
		return configurationVersion, nil
	}
}

/*
Returns true if the scope has a non nil version.

Error is:

- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) HasVersion() bool {
	version, err := s.GetVersion()
	if err != nil {
		return false
	} else {
		return version != nil
	}
}

/*
Sets the version inferred by Nyx.

Since this option can be overridden by configuration this method can only be invoked when the
configuration doesn't already have a version attribute otherwise an IllegalStateError is thrown.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
- IllegalStateError: if the configuration already has a value for the bump attribute.
*/
func (s *State) SetVersion(version *string) error {
	configurationVersion, err := s.GetConfiguration().GetVersion()
	if err != nil {
		return &errs.IllegalStateError{Message: "unable to read the configuration version attribute", Cause: err}
	}
	if configurationVersion == nil {
		s.Version = version
	} else {
		return &errs.IllegalStateError{Message: fmt.Sprintf("the state version attribute can't be set when it's ovverridden by the configuration. Configuration version attribute is '%s'", *version)}
	}

	return nil
}

/*
Returns the version build metadata inferred by Nyx, if any. If the configured version scheme is not SEMVER
this method always returns nil.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionBuildMetadata() (*string, error) {
	scheme, err := s.GetScheme()
	if err != nil || scheme == nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the scheme attribute", Cause: err}
	}
	if !s.HasVersion() || ver.SEMVER != *scheme {
		return nil, nil
	}
	version, err := s.GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the version attribute", Cause: err}
	}
	semVer, err := ver.ValueOfSemanticVersionWithSanitization(*version, true)
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to parse the version attribute", Cause: err}
	}

	return semVer.GetBuild(), nil
}

/*
Returns the version major number inferred by Nyx, if any. If the configured version scheme is not SEMVER
this method always returns nil.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionMajorNumber() (*string, error) {
	scheme, err := s.GetScheme()
	if err != nil || scheme == nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the scheme attribute", Cause: err}
	}
	if !s.HasVersion() || ver.SEMVER != *scheme {
		return nil, nil
	}
	version, err := s.GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the version attribute", Cause: err}
	}
	semVer, err := ver.ValueOfSemanticVersionWithSanitization(*version, true)
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to parse the version attribute", Cause: err}
	}
	return utl.PointerToString(strconv.Itoa(semVer.GetMajor())), nil
}

/*
Returns the version minor number inferred by Nyx, if any. If the configured version scheme is not SEMVER
this method always returns nil.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionMinorNumber() (*string, error) {
	scheme, err := s.GetScheme()
	if err != nil || scheme == nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the scheme attribute", Cause: err}
	}
	if !s.HasVersion() || ver.SEMVER != *scheme {
		return nil, nil
	}
	version, err := s.GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the version attribute", Cause: err}
	}
	semVer, err := ver.ValueOfSemanticVersionWithSanitization(*version, true)
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to parse the version attribute", Cause: err}
	}
	return utl.PointerToString(strconv.Itoa(semVer.GetMinor())), nil
}

/*
Returns the version patch number inferred by Nyx, if any. If the configured version scheme is not SEMVER
this method always returns nil.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionPatchNumber() (*string, error) {
	scheme, err := s.GetScheme()
	if err != nil || scheme == nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the scheme attribute", Cause: err}
	}
	if !s.HasVersion() || ver.SEMVER != *scheme {
		return nil, nil
	}
	version, err := s.GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the version attribute", Cause: err}
	}
	semVer, err := ver.ValueOfSemanticVersionWithSanitization(*version, true)
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to parse the version attribute", Cause: err}
	}
	return utl.PointerToString(strconv.Itoa(semVer.GetPatch())), nil
}

/*
Returns the version pre-release identifier inferred by Nyx, if any. If the configured version scheme is not SEMVER
this method always returns nil.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionPreReleaseIdentifier() (*string, error) {
	scheme, err := s.GetScheme()
	if err != nil || scheme == nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the scheme attribute", Cause: err}
	}
	if !s.HasVersion() || ver.SEMVER != *scheme {
		return nil, nil
	}
	version, err := s.GetVersion()
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to read the version attribute", Cause: err}
	}
	semVer, err := ver.ValueOfSemanticVersionWithSanitization(*version, true)
	if err != nil {
		return nil, &errs.IllegalStateError{Message: "unable to parse the version attribute", Cause: err}
	}

	return semVer.GetPrerelease(), nil
}

/*
Returns the regular expression that is used to check whether or not the version is within a certain
range. This value is only available after Nyx.infer() has run.

This attribute has a value only if ReleaseType.GetVersionRange() or ReleaseType.GetVersionRangeFromBranchName() are enabled.
If ReleaseType.GetVersionRange() has a value then this attribute has the same value, otherwise if
ReleaseType.GetVersionRangeFromBranchName() is true this value has the dynamically
generated regular expression inferred from the branch name.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) GetVersionRange() (*string, error) {
	return s.VersionRange, nil
}

/*
Returns true if the scope has a non nil version range.
*/
func (s *State) HasVersionRange() bool {
	versionRange, err := s.GetVersionRange()
	if err != nil {
		return false
	}
	return versionRange != nil
}

/*
Sets the regular expression used to check the version against a range constraint.

Error is:
- DataAccessError: in case the attribute cannot be written or accessed.
- IllegalPropertyError: in case the attribute has incorrect values or it can't be resolved.
*/
func (s *State) SetVersionRange(versionRange *string) error {
	s.VersionRange = versionRange
	return nil
}

/*
Updates the current timestamp and returns the updated value.
*/
func (s *State) TouchTimestamp() *int64 {
	now := time.Now().UnixMilli()
	s.Timestamp = &now
	return &now
}

/*
Returns a multi-line summary of the most relevant attributes in the current state object.

Error is:
- DataAccessError: in case the attribute cannot be read or accessed.
- IllegalPropertyError: in case the attribute has been defined but has incorrect values or it can't be resolved.
*/
func (s *State) Summary() (string, error) {
	var buf bytes.Buffer

	if s.HasBranch() {
		branch, err := s.GetBranch()
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&buf, "branch           = %s\n", *branch)
	} else {
		fmt.Fprintf(&buf, "branch           = %s\n", "")
	}

	if s.HasBump() {
		bump, err := s.GetBump()
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&buf, "bump             = %s\n", *bump)
	} else {
		fmt.Fprintf(&buf, "bump             = %s\n", "")
	}

	coreVersion, err := s.GetCoreVersion()
	if err != nil {
		return "", err
	}
	fmt.Fprintf(&buf, "core version     = %t\n", coreVersion)

	if s.HasLatestVersion() {
		latestVersion, err := s.GetLatestVersion()
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&buf, "latest version   = %t\n", *latestVersion)
	} else {
		fmt.Fprintf(&buf, "latest version   = %s\n", "")
	}

	newRelease, err := s.GetNewRelease()
	if err != nil {
		return "", err
	}
	fmt.Fprintf(&buf, "new release      = %t\n", newRelease)

	newVersion, err := s.GetNewVersion()
	if err != nil {
		return "", err
	}
	fmt.Fprintf(&buf, "new version      = %t\n", newVersion)

	scheme, err := s.GetScheme()
	if err != nil {
		return "", err
	}
	fmt.Fprintf(&buf, "scheme           = %s\n", (*scheme).String())

	timestamp, err := s.GetTimestamp()
	if err != nil {
		return "", err
	}
	fmt.Fprintf(&buf, "timestamp        = %d\n", *timestamp)

	if s.HasVersion() {
		version, err := s.GetVersion()
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&buf, "current version  = %s\n", *version)
	} else {
		fmt.Fprintf(&buf, "current version   = %s\n", "")
	}

	releaseScope, err := s.GetReleaseScope()
	if err != nil {
		return "", err
	}
	if releaseScope == nil {
		fmt.Fprintf(&buf, "previous version = %s\n", "")
		fmt.Fprintf(&buf, "prime version    = %s\n", "")
	} else {
		if releaseScope.HasPreviousVersion() {
			fmt.Fprintf(&buf, "previous version = %s\n", *releaseScope.GetPreviousVersion())
		} else {
			fmt.Fprintf(&buf, "previous version = %s\n", "")
		}

		if releaseScope.HasPrimeVersion() {
			fmt.Fprintf(&buf, "prime version    = %s\n", *releaseScope.GetPrimeVersion())
		} else {
			fmt.Fprintf(&buf, "prime version    = %s\n", "")
		}
	}

	return buf.String(), nil
}
