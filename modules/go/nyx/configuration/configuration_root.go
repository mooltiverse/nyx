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
This interface models the root configuration, with global options and nested sections.
*/
type ConfigurationRoot interface {
	/*
		Returns the version identifier to bump as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetBump() (*string, error)

	/*
		Returns the changelog configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetChangelog() (*ent.ChangelogConfiguration, error)

	/*
		Returns the commit message convention configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetCommitMessageConventions() (*ent.CommitMessageConventions, error)

	/*
		Returns the path to a custom configuration file as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetConfigurationFile() (*string, error)

	/*
		Returns the directory to use as the working directory as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetDirectory() (*string, error)

	/*
		Returns the value of the dry run flag as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetDryRun() (*bool, error)

	/*
		Returns the Git configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetGit() (*ent.GitConfiguration, error)

	/*
		Returns the initial version defined by this configuration to use when no past version is available in the commit history.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetInitialVersion() (*string, error)

	/*
		Returns the selected preset configuration as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetPreset() (*string, error)

	/*
		Returns the release assets configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetReleaseAssets() (*map[string]*ent.Attachment, error)

	/*
		Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
		as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetReleaseLenient() (*bool, error)

	/*
		Returns the prefix to use in release name generation as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetReleasePrefix() (*string, error)

	/*
		Returns the release types configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetReleaseTypes() (*ent.ReleaseTypes, error)

	/*
		Returns the value of the resume flag as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetResume() (*bool, error)

	/*
		Returns the versioning scheme to use as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetScheme() (*ver.Scheme, error)

	/*
		Returns the services configuration section.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetServices() (*map[string]*ent.ServiceConfiguration, error)

	/*
		Returns the path to a custom shared configuration file as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetSharedConfigurationFile() (*string, error)

	/*
		Returns the path to the file where the Nyx State must be saved as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetStateFile() (*string, error)

	/*
		Returns the logging verbosity level as it's defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetVerbosity() (*ent.Verbosity, error)

	/*
		Returns the version defined by this configuration.

		Error is:
		- DataAccessError: in case the option cannot be read or accessed.
		- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
	*/
	GetVersion() (*string, error)
}
