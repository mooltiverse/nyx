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
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

/*
A simple configuration layer, acting as a value holder for configuration options. This object allows read/write operations.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type SimpleConfigurationLayer struct {
	// The version identifier to bump as it's defined by this configuration. A nil value means undefined.
	Bump *string `json:"bump,omitempty" yaml:"bump,omitempty" handlebars:"bump"`

	// The changelog configuration section.
	Changelog *ent.ChangelogConfiguration `json:"changelog,omitempty" yaml:"changelog,omitempty" handlebars:"changelog"`

	// The commit message convention configuration section.
	CommitMessageConventions *ent.CommitMessageConventions `json:"commitMessageConventions,omitempty" yaml:"commitMessageConventions,omitempty" handlebars:"commitMessageConventions"`

	// The path to a custom configuration file as it's defined by this configuration. A nil value means undefined.
	ConfigurationFile *string `json:"configurationFile,omitempty" yaml:"configurationFile,omitempty" handlebars:"configurationFile"`

	// The directory to use as the working directory as it's defined by this configuration. A nil value means undefined.
	Directory *string `json:"directory,omitempty" yaml:"directory,omitempty" handlebars:"directory"`

	// The value of the dry run flag as it's defined by this configuration. A nil value means undefined.
	DryRun *bool `json:"dryRun,omitempty" yaml:"dryRun,omitempty" handlebars:"dryRun"`

	// The Git configuration section.
	Git *ent.GitConfiguration `json:"git,omitempty" yaml:"git,omitempty" handlebars:"git"`

	// The the initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.
	InitialVersion *string `json:"initialVersion,omitempty" yaml:"initialVersion,omitempty" handlebars:"initialVersion"`

	// The selected preset configuration as it's defined by this configuration. A nil value means undefined.
	Preset *string `json:"preset,omitempty" yaml:"preset,omitempty" handlebars:"preset"`

	// The release assets configuration section
	ReleaseAssets *map[string]*ent.Attachment `json:"releaseAssets,omitempty" yaml:"releaseAssets,omitempty" handlebars:"releaseAssets"`

	// The flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
	// as it's defined by this configuration. A nil value means undefined.
	ReleaseLenient *bool `json:"releaseLenient,omitempty" yaml:"releaseLenient,omitempty" handlebars:"releaseLenient"`

	// The prefix to use in release name generation as it's defined by this configuration. A nil value means undefined.
	ReleasePrefix *string `json:"releasePrefix,omitempty" yaml:"releasePrefix,omitempty" handlebars:"releasePrefix"`

	// The release types configuration section.
	ReleaseTypes *ent.ReleaseTypes `json:"releaseTypes,omitempty" yaml:"releaseTypes,omitempty" handlebars:"releaseTypes"`

	// The value of the resume flag as it's defined by this configuration. A nil value means undefined.
	Resume *bool `json:"resume,omitempty" yaml:"resume,omitempty" handlebars:"resume"`

	// The scheme defined by this configuration. A nil value means undefined.
	Scheme *ver.Scheme `json:"scheme,omitempty" yaml:"scheme,omitempty" handlebars:"scheme"`

	// The services configuration section
	Services *map[string]*ent.ServiceConfiguration `json:"services,omitempty" yaml:"services,omitempty" handlebars:"services"`

	// The path to a custom shared configuration file as it's defined by this configuration. A nil value means undefined.
	SharedConfigurationFile *string `json:"sharedConfigurationFile,omitempty" yaml:"sharedConfigurationFile,omitempty" handlebars:"sharedConfigurationFile"`

	// The path to the file where the Nyx State must be saved as it's defined by this configuration. A nil value means undefined.
	StateFile *string `json:"stateFile,omitempty" yaml:"stateFile,omitempty" handlebars:"stateFile"`

	// The substitutions configuration section.
	Substitutions *ent.Substitutions `json:"substitutions,omitempty" yaml:"substitutions,omitempty" handlebars:"substitutions"`

	// The value of the summary flag as it's defined by this configuration. A nil value means undefined.
	Summary *bool `json:"summary,omitempty" yaml:"summary,omitempty" handlebars:"summary"`

	// The path to the file where the Nyx summary must be saved as it's defined by this configuration. A nil value means undefined.
	SummaryFile *string `json:"summaryFile,omitempty" yaml:"summaryFile,omitempty" handlebars:"summaryFile"`

	// The verbosity defined by this configuration. A nil value means undefined.
	Verbosity *ent.Verbosity `json:"verbosity,omitempty" yaml:"verbosity,omitempty" handlebars:"verbosity"`

	// The version defined by this configuration. A nil value means undefined.
	Version *string `json:"version,omitempty" yaml:"version,omitempty" handlebars:"version"`
}

/*
Default constructor
*/
func NewSimpleConfigurationLayer() *SimpleConfigurationLayer {
	scl := SimpleConfigurationLayer{}
	scl.setDefaults()
	return &scl
}

/*
Loads default values on the target instance
*/
func (scl *SimpleConfigurationLayer) setDefaults() {
	scl.Changelog = ent.NewChangelogConfiguration()
	scl.CommitMessageConventions = ent.NewCommitMessageConventions()
	scl.Git = ent.NewGitConfiguration()
	svra := make(map[string]*ent.Attachment)
	scl.ReleaseAssets = &svra
	scl.ReleaseTypes = ent.NewReleaseTypes()
	svsc := make(map[string]*ent.ServiceConfiguration)
	scl.Services = &svsc
	scl.Substitutions = ent.NewSubstitutions()
}

/*
Returns the version identifier to bump as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetBump() (*string, error) {
	return scl.Bump, nil
}

/*
Sets the version identifier to bump as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetBump(bump *string) {
	scl.Bump = bump
}

/*
Returns the changelog configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetChangelog() (*ent.ChangelogConfiguration, error) {
	return scl.Changelog, nil
}

/*
Sets the changelog configuration section.
*/
func (scl *SimpleConfigurationLayer) SetChangelog(changelog *ent.ChangelogConfiguration) {
	scl.Changelog = changelog
}

/*
Returns the commit message convention configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetCommitMessageConventions() (*ent.CommitMessageConventions, error) {
	return scl.CommitMessageConventions, nil
}

/*
Sets the commit message convention configuration section.
*/
func (scl *SimpleConfigurationLayer) SetCommitMessageConventions(commitMessageConventions *ent.CommitMessageConventions) {
	scl.CommitMessageConventions = commitMessageConventions
}

/*
Returns the path to a custom configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetConfigurationFile() (*string, error) {
	return scl.ConfigurationFile, nil
}

/*
Sets the path to a custom configuration file as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetConfigurationFile(configurationFile *string) {
	scl.ConfigurationFile = configurationFile
}

/*
Returns the directory to use as the working directory as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetDirectory() (*string, error) {
	return scl.Directory, nil
}

/*
Sets the directory to use as the working directory as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetDirectory(directory *string) {
	scl.Directory = directory
}

/*
Returns the value of the dry run flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetDryRun() (*bool, error) {
	return scl.DryRun, nil
}

/*
Sets the value of the dry run flag as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetDryRun(dryRun *bool) {
	scl.DryRun = dryRun
}

/*
Returns the Git configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetGit() (*ent.GitConfiguration, error) {
	return scl.Git, nil
}

/*
Sets the Git configuration section.
*/
func (scl *SimpleConfigurationLayer) SetGit(git *ent.GitConfiguration) {
	scl.Git = git
}

/*
Returns the initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetInitialVersion() (*string, error) {
	return scl.InitialVersion, nil
}

/*
Sets the initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetInitialVersion(initialVersion *string) {
	scl.InitialVersion = initialVersion
}

/*
Returns the selected preset configuration as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetPreset() (*string, error) {
	return scl.Preset, nil
}

/*
Sets the selected preset configuration as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetPreset(preset *string) {
	scl.Preset = preset
}

/*
Returns the release assets configuration section. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetReleaseAssets() (*map[string]*ent.Attachment, error) {
	return scl.ReleaseAssets, nil
}

/*
Sets the release assets configuration section. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetReleaseAssets(releaseAssets *map[string]*ent.Attachment) {
	scl.ReleaseAssets = releaseAssets
}

/*
Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetReleaseLenient() (*bool, error) {
	return scl.ReleaseLenient, nil
}

/*
Sets the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetReleaseLenient(releaseLenient *bool) {
	scl.ReleaseLenient = releaseLenient
}

/*
Returns the prefix to use in release name generation as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetReleasePrefix() (*string, error) {
	return scl.ReleasePrefix, nil
}

/*
Sets the prefix to use in release name generation as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetReleasePrefix(releasePrefix *string) {
	scl.ReleasePrefix = releasePrefix
}

/*
Returns the release types configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetReleaseTypes() (*ent.ReleaseTypes, error) {
	return scl.ReleaseTypes, nil
}

/*
Sets the release types configuration section.
*/
func (scl *SimpleConfigurationLayer) SetReleaseTypes(releaseTypes *ent.ReleaseTypes) {
	scl.ReleaseTypes = releaseTypes
}

/*
Returns the value of the resume flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetResume() (*bool, error) {
	return scl.Resume, nil
}

/*
Sets the value of the resume flag as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetResume(resume *bool) {
	scl.Resume = resume
}

/*
Returns the versioning scheme as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetScheme() (*ver.Scheme, error) {
	return scl.Scheme, nil
}

/*
Sets the versioning scheme as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetScheme(scheme *ver.Scheme) {
	scl.Scheme = scheme
}

/*
Returns the services configuration section. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetServices() (*map[string]*ent.ServiceConfiguration, error) {
	return scl.Services, nil
}

/*
Sets services configuration section. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetServices(services *map[string]*ent.ServiceConfiguration) {
	scl.Services = services
}

/*
Returns the path to a custom shared configuration file as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetSharedConfigurationFile() (*string, error) {
	return scl.SharedConfigurationFile, nil
}

/*
Sets the path to a custom shared configuration file as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetSharedConfigurationFile(sharedConfigurationFile *string) {
	scl.SharedConfigurationFile = sharedConfigurationFile
}

/*
Returns the path to the file where the Nyx State must be saved as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetStateFile() (*string, error) {
	return scl.StateFile, nil
}

/*
Sets the path to the file where the Nyx State must be saved as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetStateFile(stateFile *string) {
	scl.StateFile = stateFile
}

/*
Returns the substitutions configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetSubstitutions() (*ent.Substitutions, error) {
	return scl.Substitutions, nil
}

/*
Sets the substitutions configuration section.
*/
func (scl *SimpleConfigurationLayer) SetSubstitutions(substitutions *ent.Substitutions) {
	scl.Substitutions = substitutions
}

/*
Returns the value of the summary flag as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetSummary() (*bool, error) {
	return scl.Summary, nil
}

/*
Sets the value of the summary flag as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetSummary(summary *bool) {
	scl.Summary = summary
}

/*
Returns the path to the file where the Nyx summary must be saved as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetSummaryFile() (*string, error) {
	return scl.SummaryFile, nil
}

/*
Sets the path to the file where the Nyx summary must be saved as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetSummaryFile(summaryFile *string) {
	scl.SummaryFile = summaryFile
}

/*
Returns the logging verbosity level as it's defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetVerbosity() (*ent.Verbosity, error) {
	return scl.Verbosity, nil
}

/*
Sets the logging verbosity level as it's defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetVerbosity(verbosity *ent.Verbosity) {
	scl.Verbosity = verbosity
}

/*
Returns the version defined by this configuration. A nil value means undefined.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (scl *SimpleConfigurationLayer) GetVersion() (*string, error) {
	return scl.Version, nil
}

/*
Sets the version defined by this configuration. A nil value means undefined.
*/
func (scl *SimpleConfigurationLayer) SetVersion(version *string) {
	scl.Version = version
}
