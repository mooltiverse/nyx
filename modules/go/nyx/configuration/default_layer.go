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
	"sync" // https://pkg.go.dev/sync

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

var (
	// the lock used to synchronize routines
	defaultLayerLock = &sync.Mutex{}

	// The single instance for this class.
	defaultLayerInstance *DefaultLayer
)

/*
The default configuration layer. This is a singleton class so instances are to be retrieved via the GetDefaultLayerInstance() method.

The default configuration layer is used with the least priority when evaluating configuration options so it's queried
only if and when a certain ption doesn't appear in any other layer with higher priority.
*/
type DefaultLayer struct {
	// The directory returned by this layer.
	directory *string
}

/*
Returns the singleton instance of this class.
*/
func GetDefaultLayerInstance() *DefaultLayer {
	if defaultLayerInstance == nil {
		defaultLayerLock.Lock()
		defer defaultLayerLock.Unlock()
		if defaultLayerInstance == nil {
			defaultLayerInstance = &DefaultLayer{}
		}
	}

	return defaultLayerInstance
}

/*
Returns the default version identifier to bump. A nil value means undefined.
*/
func (dl *DefaultLayer) GetBump() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "bump", ent.BUMP)
	return ent.BUMP, nil
}

/*
Returns the default changelog configuration section.
*/
func (dl *DefaultLayer) GetChangelog() (*ent.ChangelogConfiguration, error) {
	log.Tracef("retrieving the default '%s' configuration option", "changelog")
	return ent.CHANGELOG, nil
}

/*
/*
Returns the default commit message convention configuration section.
*/
func (dl *DefaultLayer) GetCommitMessageConventions() (*ent.CommitMessageConventions, error) {
	log.Tracef("retrieving the default '%s' configuration option", "commitMessageConventions")
	return ent.COMMIT_MESSAGE_CONVENTIONS, nil
}

/*
Returns the default path to a custom configuration file. A nil value means undefined.
*/
func (dl *DefaultLayer) GetConfigurationFile() (*string, error) {
	if ent.CONFIGURATION_FILE == nil {
		log.Tracef("retrieving the default '%s' configuration option: '%v'", "configurationFile", "nil")
	} else {
		log.Tracef("retrieving the default '%s' configuration option: '%v'", "configurationFile", *ent.CONFIGURATION_FILE)
	}

	return ent.CONFIGURATION_FILE, nil
}

/*
Returns the default directory to use as the working directory. A nil value means undefined.
*/
func (dl *DefaultLayer) GetDirectory() (*string, error) {
	if dl.directory == nil {
		log.Tracef("retrieving the default '%s' configuration option: '%v'", "directory", *ent.DIRECTORY)
		return ent.DIRECTORY, nil
	} else {
		log.Tracef("retrieving the default '%s' configuration option: '%v'", "directory", *dl.directory)
		return dl.directory, nil
	}
}

/*
Sets the default directory to use as the working directory. A nil value means undefined.
*/
func (dl *DefaultLayer) SetDirectory(directory *string) {
	if directory == nil {
		log.Tracef("setting the default '%s' configuration option: '%v'", "directory", "nil")
	} else {
		log.Tracef("setting the default '%s' configuration option: '%v'", "directory", *directory)
	}

	dl.directory = directory
}

/*
Returns the default value of the dry run flag. A nil value means undefined.
*/
func (dl *DefaultLayer) GetDryRun() (*bool, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "dryRun", ent.DRY_RUN)
	return ent.DRY_RUN, nil
}

/*
Returns the default Git configuration section.
*/
func (dl *DefaultLayer) GetGit() (*ent.GitConfiguration, error) {
	log.Tracef("retrieving the default '%s' configuration option", "git")
	return ent.GIT, nil
}

/*
Returns the default initial version defined by this configuration to use when no past version is available in the commit history. A nil value means undefined.
*/
func (dl *DefaultLayer) GetInitialVersion() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "initialVersion", ent.INITIAL_VERSION)
	return ent.INITIAL_VERSION, nil
}

/*
Returns the default selected preset configuration. A nil value means undefined.
*/
func (dl *DefaultLayer) GetPreset() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "preset", ent.PRESET)
	return ent.PRESET, nil
}

/*
Returns the default release assets configuration section. A nil value means undefined.
*/
func (dl *DefaultLayer) GetReleaseAssets() (*map[string]*ent.Attachment, error) {
	log.Tracef("retrieving the default '%s' configuration option", "releaseAssets")
	return ent.RELEASE_ASSETS, nil
}

/*
Returns the default flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters.
A nil value means undefined.
*/
func (dl *DefaultLayer) GetReleaseLenient() (*bool, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "releaseLenient", ent.RELEASE_LENIENT)
	return ent.RELEASE_LENIENT, nil
}

/*
Returns the default prefix to use in release name generation. A nil value means undefined.
*/
func (dl *DefaultLayer) GetReleasePrefix() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "releasePrefix", ent.RELEASE_PREFIX)
	return ent.RELEASE_PREFIX, nil
}

/*
Returns the default release types configuration section.
*/
func (dl *DefaultLayer) GetReleaseTypes() (*ent.ReleaseTypes, error) {
	log.Tracef("retrieving the default '%s' configuration option", "releaseTypes")
	return ent.RELEASE_TYPES, nil
}

/*
Returns the default value of the resume flag. A nil value means undefined.
*/
func (dl *DefaultLayer) GetResume() (*bool, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "resume", ent.RESUME)
	return ent.RESUME, nil
}

/*
Returns the default versioning scheme. A nil value means undefined.
*/
func (dl *DefaultLayer) GetScheme() (*ver.Scheme, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "scheme", ent.SCHEME)
	return ent.SCHEME, nil
}

/*
Returns the default services configuration section. A nil value means undefined.
*/
func (dl *DefaultLayer) GetServices() (*map[string]*ent.ServiceConfiguration, error) {
	log.Tracef("retrieving the default '%s' configuration option", "services")
	return ent.SERVICES, nil
}

/*
Returns the default path to a custom shared configuration file. A nil value means undefined.
*/
func (dl *DefaultLayer) GetSharedConfigurationFile() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "sharedConfigurationFile", ent.SHARED_CONFIGURATION_FILE)
	return ent.SHARED_CONFIGURATION_FILE, nil
}

/*
Returns the default value of the summary flag. A nil value means undefined.
*/
func (dl *DefaultLayer) GetSummary() (*bool, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "summary", ent.SUMMARY)
	return ent.SUMMARY, nil
}

/*
Returns the default path to the file where the Nyx summary must be saved. A nil value means undefined.
*/
func (dl *DefaultLayer) GetSummaryFile() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "summaryFile", ent.SUMMARY_FILE)
	return ent.SUMMARY_FILE, nil
}

/*
Returns the default path to the file where the Nyx State must be saved. A nil value means undefined.
*/
func (dl *DefaultLayer) GetStateFile() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "stateFile", ent.STATE_FILE)
	return ent.STATE_FILE, nil
}

/*
Returns the default logging verbosity level. A nil value means undefined.
*/
func (dl *DefaultLayer) GetVerbosity() (*ent.Verbosity, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "verbosity", ent.VERBOSITY)
	return ent.VERBOSITY, nil
}

/*
Returns the default version. A nil value means undefined.
*/
func (dl *DefaultLayer) GetVersion() (*string, error) {
	log.Tracef("retrieving the default '%s' configuration option: '%v'", "version", ent.VERSION)
	return ent.VERSION, nil
}
