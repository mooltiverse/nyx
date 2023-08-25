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
This is the configuration package for Nyx, providing layered configuration logic among different configuration means.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package configuration

import (
	"encoding/json" // https://pkg.go.dev/encoding/json
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"sync"          // https://pkg.go.dev/sync

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	io "github.com/mooltiverse/nyx/modules/go/nyx/io"
	ver "github.com/mooltiverse/nyx/modules/go/version"
)

var (
	// the lock used to synchronize routines
	configurationLock = &sync.Mutex{}
)

/*
The Nyx configuration. The configuration is a live object that resolves each option lazily, only when required.
This not only improves the overall performances but is also safer as in case of malformed configuration options, only
those actually needed are resolved.

This means that even if the configuration sources don't change throughout a release process, the state of the
configuration may change every time a not yet resolved option is requested and evaluated.

The configuration is layered, where each layer represents a source of configuration options. There is a clear definition
of priorities among different layers so there is a clear precedence of options coming from one layer or another.
Thanks to this, each option can be overridden by other layer with higher priority.

There must be only one instance of this class for every execution and it's retrieved by the configuration() method
in the Nyx class.
*/
type Configuration struct {
	// The private instance of the changelog configuration section.
	changelogSection *ent.ChangelogConfiguration

	// The private instance of the commit message convention configuration section.
	commitMessageConventionsSection *ent.CommitMessageConventions

	// The private instance of the Git configuration section.
	gitSection *ent.GitConfiguration

	// The private instance of the release assets configuration section.
	releaseAssetsSection *map[string]*ent.Attachment

	// The private instance of the release types configuration section.
	releaseTypesSection *ent.ReleaseTypes

	// The private instance of the services configuration section.
	servicesSection *map[string]*ent.ServiceConfiguration

	// The private instance of the substitutions configuration section.
	substitutionsSection *ent.Substitutions

	// The internal representation of the configuration layers and their priorities.
	//
	// Since the priorities are well known the array is statically sized and each layer appears in the array
	// to the index represented by its priority.
	// Array values may be nil for all those layers that haven't been initialized yet.
	//
	// This array is initialized with default items by initializeStandardConfigurationLayers()
	layers [LAYER_PRIORITY_LENGTH]*ConfigurationLayer
}

/*
Default constructor.

Errors can be:

- DataAccessError: in case data cannot be read or accessed.
- IllegalPropertyError: in case some option has been defined but has incorrect values or it can't be resolved.
*/
func NewConfiguration() (*Configuration, error) {
	log.Trace("new configuration object")

	configuration := Configuration{}
	configuration.initializeStandardConfigurationLayers()
	err := configuration.loadStandardConfigurationFileLayers()
	if err != nil {
		return nil, err
	}
	return &configuration, nil
}

/*
Initializes the internal array of layers.

The only layers that are initialized (so non nil) are the default one and the environment variables layer.
*/
func (c *Configuration) initializeStandardConfigurationLayers() {
	log.Debugf("initializing the default configuration layer")
	var dl ConfigurationLayer = GetDefaultLayerInstance()
	c.layers[DEFAULT] = &dl

	log.Debugf("initializing the environment configuration layer")
	var ecl ConfigurationLayer = GetEnvironmentConfigurationLayerInstance()
	c.layers[ENVIRONMENT] = &ecl

	log.Debugf("initializing the command line configuration layer")
	var clcl ConfigurationLayer = GetCommandLineConfigurationLayerInstance()
	c.layers[COMMAND_LINE] = &clcl
}

/*
Returns a file built on the given path if it's already an absolute file path, otherwise make it absolute by resolving it with the
configured (or default) directory.

Arguments are as follows:

- path the file to make absolute

# Errors can be returned if

- DataAccessError: in case data cannot be read or accessed.
- IllegalPropertyError: in case some option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) getAbsoluteFilePath(path string) (string, error) {
	//file, err := filepath.Abs(fileName)
	if filepath.IsAbs(path) {
		return path, nil
	} else {
		defaultDir, err := (*GetDefaultLayerInstance()).GetDirectory()
		if err != nil {
			return path, err
		}
		return filepath.Join(*defaultDir, path), nil
	}
}

/*
Loads the various standard configuration file layers, if found, searched at their default locations.

Errors can be:

- DataAccessError: in case data cannot be read or accessed.
- IllegalPropertyError: in case some option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) loadStandardConfigurationFileLayers() error {
	log.Debug("searching for standard configuration files...")
	// load standard local configuration files first, if any
	for _, fileName := range []string{".nyx.json", ".nyx.yaml", ".nyx.yml"} {
		file, err := c.getAbsoluteFilePath(fileName)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to get the absolute path for file '%s'", fileName), Cause: err}
		}
		_, err = os.Stat(file)
		if err == nil {
			log.Debugf("standard local configuration file found at '%s'. Loading...", file)
			var scl ConfigurationLayer = NewSimpleConfigurationLayer()
			err = io.LoadFromFile(file, scl)
			if err != nil {
				return err
			}
			c.layers[STANDARD_LOCAL_FILE] = &scl

			log.Debugf("standard local configuration file '%s' loaded", file)
		} else {
			log.Debugf("standard local configuration file '%s' not found.", file)
		}
	}
	// then load standard shared configuration files, if any
	for _, fileName := range []string{".nyx-shared.json", ".nyx-shared.yaml", ".nyx-shared.yml"} {
		file, err := c.getAbsoluteFilePath(fileName)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to get the absolute path for file '%s'", fileName), Cause: err}
		}
		_, err = os.Stat(file)
		if err == nil {
			log.Debugf("standard shared configuration file found at '%s'. Loading...", file)
			var scl ConfigurationLayer = NewSimpleConfigurationLayer()
			err = io.LoadFromFile(file, scl)
			if err != nil {
				return err
			}
			c.layers[STANDARD_SHARED_FILE] = &scl

			log.Debugf("standard shared configuration file '%s' loaded", file)
		} else {
			log.Debugf("standard shared configuration file '%s' not found.", file)
		}
	}
	err := c.updateConfiguredConfigurationLayers()
	if err != nil {
		return err
	}

	return nil
}

/*
Resets the cache of resolved options.
*/
func (c *Configuration) resetCache() {
	log.Trace("clearing the configuration cache")

	configurationLock.Lock()
	defer configurationLock.Unlock()

	c.changelogSection = nil
	c.commitMessageConventionsSection = nil
	c.gitSection = nil
	c.releaseAssetsSection = nil
	c.releaseTypesSection = nil
	c.servicesSection = nil
	c.substitutionsSection = nil
}

/*
Makes sure that the configuration layers that can be configured (custom configuration file, custom shared
configuration file, preset) are added or removed into the internal layers representation, according to the
actual configuration parameters.

This method must be invoked after any core layer (not among the configured ones) changes, or after a batch
of those changes.

Errors can be:

- DataAccessError: in case data cannot be read or accessed.
- IllegalPropertyError: in case some option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) updateConfiguredConfigurationLayers() error {
	// follow the evaluation order from top to bottom to make sure that any change applied here does not
	// require further changes to the layers that we already checked

	configurationLock.Lock()
	defer configurationLock.Unlock()

	// start with the local custom configuration file
	configurationFile, err := c.GetConfigurationFile()
	if err != nil {
		return err
	}
	if configurationFile == nil {
		log.Debug("clearing the custom local configuration file, if any")
		c.layers[CUSTOM_LOCAL_FILE] = nil
	} else if *configurationFile == "" {
		log.Error("an empty path has been defined for the local custom configuration file and it will be ignored")
		c.layers[CUSTOM_LOCAL_FILE] = nil
	} else {
		file, err := c.getAbsoluteFilePath(*configurationFile)
		if err != nil {
			return err
		}
		log.Debugf("loading custom local configuration file at '%s'", file)
		var scl ConfigurationLayer = NewSimpleConfigurationLayer()
		err = io.LoadFromFile(file, scl)
		if err != nil {
			return err
		}
		c.layers[CUSTOM_LOCAL_FILE] = &scl
		log.Debugf("custom local configuration file '%s' loaded", file)
	}

	// now the local shared configuration file
	sharedConfigurationFile, err := c.GetSharedConfigurationFile()
	if err != nil {
		return err
	}
	if sharedConfigurationFile == nil {
		log.Debug("clearing the custom shared configuration file, if any")
		c.layers[CUSTOM_SHARED_FILE] = nil
	} else if *sharedConfigurationFile == "" {
		log.Error("an empty path has been defined for the local shared configuration file and it will be ignored")
		c.layers[CUSTOM_SHARED_FILE] = nil
	} else {
		file, err := c.getAbsoluteFilePath(*sharedConfigurationFile)
		if err != nil {
			return err
		}
		log.Debugf("loading custom shared configuration file at '%s'", file)
		var scl ConfigurationLayer = NewSimpleConfigurationLayer()
		err = io.LoadFromFile(file, scl)
		if err != nil {
			return err
		}
		c.layers[CUSTOM_SHARED_FILE] = &scl
		log.Debugf("custom shared configuration file '%s' loaded", file)
	}

	// now the preset
	// now the local shared configuration file
	preset, err := c.GetPreset()
	if err != nil {
		return err
	}
	if preset == nil {
		log.Debug("clearing the preset configuration, if any")
		c.layers[PRESET] = nil
	} else if *preset == "" {
		log.Error("an empty name has been defined for the preset configuration and it will be ignored")
		c.layers[PRESET] = nil
	} else {
		log.Debugf("loading preset configuration '%s'", *preset)
		var scl ConfigurationLayer
		scl, err := PresetByName(*preset)
		if err != nil {
			return err
		}
		c.layers[PRESET] = &scl
		log.Debugf("preset configuration '%s' loaded", *preset)
	}

	return nil
}

/*
Flattens the resolved configuration represented by this object into a simple configuration object
where all dynamic values have been resolved.

The returned object is detached from this one but can be used for marshalling and rendering as
al values are stored as internal fields.

The point of having this custom method is that this object doesn't hold simple values but it needs
to resolve them among the various layers, so the simple JSON marshalling feature doesn't work.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) Flatten() (*SimpleConfigurationLayer, error) {
	// The point of having this custom method is that this object doesn't hold simple values but it needs
	// to resolve them among the various layers, so the simple JSON marshalling feature doesn't work.
	//
	// The easiest way to do this is to instantiate a SimpleConfigurationLayer (which can be automalically
	// marshalled as it's a regular object) and fill it with resolved parameters from this one, then just
	// serialize that to JSON.
	//
	// Invoking all the getter methods also causes this object to resolve all fields, even those that weren't
	// resolved before.
	bump, err := c.GetBump()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "bump"), Cause: err}
	}
	changelog, err := c.GetChangelog()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "changelog"), Cause: err}
	}
	commitMessageConventions, err := c.GetCommitMessageConventions()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "commitMessageConventions"), Cause: err}
	}
	configurationFile, err := c.GetConfigurationFile()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "configurationFile"), Cause: err}
	}
	directory, err := c.GetDirectory()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "directory"), Cause: err}
	}
	dryRun, err := c.GetDryRun()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "dryRun"), Cause: err}
	}
	git, err := c.GetGit()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "git"), Cause: err}
	}
	initialVersion, err := c.GetInitialVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "initialVersion"), Cause: err}
	}
	preset, err := c.GetPreset()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "preset"), Cause: err}
	}
	releaseAssets, err := c.GetReleaseAssets()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseAssets"), Cause: err}
	}
	releaseLenient, err := c.GetReleaseLenient()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseLenient"), Cause: err}
	}
	releasePrefix, err := c.GetReleasePrefix()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releasePrefix"), Cause: err}
	}
	releaseTypes, err := c.GetReleaseTypes()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "releaseTypes"), Cause: err}
	}
	resume, err := c.GetResume()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "resume"), Cause: err}
	}
	scheme, err := c.GetScheme()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "scheme"), Cause: err}
	}
	services, err := c.GetServices()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "services"), Cause: err}
	}
	sharedConfigurationFile, err := c.GetSharedConfigurationFile()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "sharedConfigurationFile"), Cause: err}
	}
	stateFile, err := c.GetStateFile()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "stateFile"), Cause: err}
	}
	substitutions, err := c.GetSubstitutions()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "substitutions"), Cause: err}
	}
	summary, err := c.GetSummary()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "summary"), Cause: err}
	}
	summaryFile, err := c.GetSummaryFile()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "summaryFile"), Cause: err}
	}
	verbosity, err := c.GetVerbosity()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "verbosity"), Cause: err}
	}
	version, err := c.GetVersion()
	if err != nil {
		return nil, &errs.DataAccessError{Message: fmt.Sprintf("unable to resolve configuration option '%s'", "version"), Cause: err}
	}

	return &SimpleConfigurationLayer{
		Bump:                     bump,
		Changelog:                changelog,
		CommitMessageConventions: commitMessageConventions,
		ConfigurationFile:        configurationFile,
		Directory:                directory,
		DryRun:                   dryRun,
		Git:                      git,
		InitialVersion:           initialVersion,
		Preset:                   preset,
		ReleaseAssets:            releaseAssets,
		ReleaseLenient:           releaseLenient,
		ReleasePrefix:            releasePrefix,
		ReleaseTypes:             releaseTypes,
		Resume:                   resume,
		Scheme:                   scheme,
		Services:                 services,
		SharedConfigurationFile:  sharedConfigurationFile,
		Substitutions:            substitutions,
		StateFile:                stateFile,
		Summary:                  summary,
		SummaryFile:              summaryFile,
		Verbosity:                verbosity,
		Version:                  version,
	}, nil
}

/*
Adds the JSON marshalling feature to the configuration object

This method implements the Marshaler interface in the JSON package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) MarshalJSON() ([]byte, error) {
	flattenedConfiguration, err := c.Flatten()
	if err != nil {
		return nil, err
	}

	return json.Marshal(flattenedConfiguration)
}

/*
Adds the YAML marshalling feature to the configuration object.

This method implements the Marshaler interface in the YAML package to customize marshalling for this object.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) MarshalYAML() (interface{}, error) {
	flattenedConfiguration, err := c.Flatten()
	if err != nil {
		return nil, err
	}

	return flattenedConfiguration, nil
}

/*
Adds, replaces or removes the layer at the COMMAND_LINE level.

Returns a reference to this same object.

Arguments are as follows:

  - layer the configuration layer to set at the COMMAND_LINE level.
    If nil any existing configuration layer at the same level is removed (if any).

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) WithCommandLineConfiguration(layer *ConfigurationLayer) (*Configuration, error) {
	if layer == nil {
		log.Debugf("removing the existing '%v' configuration layer, if any", COMMAND_LINE)
		c.layers[COMMAND_LINE] = nil
	} else {
		log.Debugf("adding or replacing the '%v' configuration layer", COMMAND_LINE)
		c.layers[COMMAND_LINE] = layer
	}

	err := c.updateConfiguredConfigurationLayers()
	if err != nil {
		return nil, err
	}
	c.resetCache()
	return c, nil
}

/*
Adds, replaces or removes the layer at the PLUGIN level.

Returns a reference to this same object.

Arguments are as follows:

  - layer the configuration layer to set at the PLUGIN level.
    If nil any existing configuration layer at the same level is removed (if any).

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) WithPluginConfiguration(layer *ConfigurationLayer) (*Configuration, error) {
	if layer == nil {
		log.Debugf("removing the existing '%v' configuration layer, if any", PLUGIN)
		c.layers[PLUGIN] = nil
	} else {
		log.Debugf("adding or replacing the '%v' configuration layer", PLUGIN)
		c.layers[PLUGIN] = layer
	}

	err := c.updateConfiguredConfigurationLayers()
	if err != nil {
		return nil, err
	}
	c.resetCache()
	return c, nil
}

/*
Adds, replaces or removes the layer at the RUNTIME level.

Returns a reference to this same object.

Arguments are as follows:

  - layer the configuration layer to set at the RUNTIME level.
    If nil any existing configuration layer at the same level is removed (if any).

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) WithRuntimeConfiguration(layer *ConfigurationLayer) (*Configuration, error) {
	if layer == nil {
		log.Debugf("removing the existing '%v' configuration layer, if any", RUNTIME)
		c.layers[RUNTIME] = nil
	} else {
		c.layers[RUNTIME] = layer
	}

	err := c.updateConfiguredConfigurationLayers()
	if err != nil {
		return nil, err
	}
	c.resetCache()
	return c, nil
}

/*
Returns the version identifier to bump as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetBump() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "bump")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			bump, err := (*configurationLayer).GetBump()
			if err != nil {
				return nil, err
			}
			if bump != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "bump", *bump)
				return bump, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetBump()
}

/*
Returns the changelog configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetChangelog() (*ent.ChangelogConfiguration, error) {
	log.Trace("retrieving the changelog configuration")
	if c.changelogSection == nil {
		c.changelogSection = ent.NewChangelogConfiguration()
		for _, layer := range c.layers {
			if layer != nil {
				// Since all attributes of the changelog configuration are objects we assume that if they are nil
				// they have the default values and we keep non nil values as those overriding defaults.
				// The sections map is assumed to override inherited values if its size is not 0
				changelog, err := (*layer).GetChangelog()
				if err != nil {
					return nil, err
				}

				if c.changelogSection.GetAppend() == nil {
					c.changelogSection.SetAppend(changelog.GetAppend())
				}
				if c.changelogSection.GetPath() == nil {
					c.changelogSection.SetPath(changelog.GetPath())
				}
				if c.changelogSection.GetSections() == nil || len(*c.changelogSection.GetSections()) == 0 {
					c.changelogSection.SetSections(changelog.GetSections())
				}
				if c.changelogSection.GetSubstitutions() == nil || len(*c.changelogSection.GetSubstitutions()) == 0 {
					c.changelogSection.SetSubstitutions(changelog.GetSubstitutions())
				}
				if c.changelogSection.GetTemplate() == nil {
					c.changelogSection.SetTemplate(changelog.GetTemplate())
				}
			}
		}
		log.Tracef("the '%s' configuration option has been resolved", "changelog")
	}
	return c.changelogSection, nil
}

/*
Returns the commit message convention configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetCommitMessageConventions() (*ent.CommitMessageConventions, error) {
	log.Trace("retrieving the commit message conventions")
	if c.commitMessageConventionsSection == nil {
		// parse the 'enabled' items list
		enabled := make([]*string, 0)
		for _, layer := range c.layers {
			if layer != nil {
				commitMessageConventions, err := (*layer).GetCommitMessageConventions()
				if err != nil {
					return nil, err
				}
				if commitMessageConventions.GetEnabled() != nil && len(*commitMessageConventions.GetEnabled()) > 0 {
					enabled = *commitMessageConventions.GetEnabled()
					log.Tracef("the '%s.%s' configuration option value is: '%v'", "commitMessageConventions", "enabled", enabled)
					break
				}
			}
		}

		// parse the 'items' map
		items := make(map[string]*ent.CommitMessageConvention)
		for _, enabledItem := range enabled {
			for _, layer := range c.layers {
				if layer != nil {
					commitMessageConventions, err := (*layer).GetCommitMessageConventions()
					if err != nil {
						return nil, err
					}

					if commitMessageConventions != nil && (*commitMessageConventions).GetItems() != nil {
						item := (*(*commitMessageConventions).GetItems())[*enabledItem]
						if item != nil {
							items[*enabledItem] = item
							log.Tracef("the '%s.%s[%s]' configuration option has been resolved", "commitMessageConventions", "items", *enabledItem)
							break
						}
					}
				}
			}
		}

		cmc, err := ent.NewCommitMessageConventionsWith(&enabled, &items)
		if err != nil {
			return nil, err
		}
		c.commitMessageConventionsSection = cmc
	}
	return c.commitMessageConventionsSection, nil
}

/*
Returns the path to a shared configuration file as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetConfigurationFile() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "configurationFile")
	for layerPriority, configurationLayer := range c.layers {
		if configurationLayer != nil {
			// custom configuration file configuration is ignored on custom configuration file layers to avoid chaining
			if layerPriority != int(CUSTOM_LOCAL_FILE) {
				configurationFile, err := (*configurationLayer).GetConfigurationFile()
				if err != nil {
					return nil, err
				}
				if configurationFile != nil {
					log.Tracef("the '%s' configuration option value is: '%s'", "configurationFile", *configurationFile)
					return configurationFile, nil
				}
			}
		}
	}
	return GetDefaultLayerInstance().GetConfigurationFile()
}

/*
Returns the directory to use as the working directory as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetDirectory() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "directory")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			directory, err := (*configurationLayer).GetDirectory()
			if err != nil {
				return nil, err
			}
			if directory != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "directory", *directory)
				return directory, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetDirectory()
}

/*
This method allows to override the default directory that will be returned by GetDirectory().
This method must be invoked before instances of Nyx or other classes are created or the given value may be ignored.
*/
func SetDefaultDirectory(directory *string) error {
	if directory == nil {
		log.Tracef("setting the default directory '%s'", "nil")
		GetDefaultLayerInstance().SetDirectory(directory)
	} else {
		dir, err := filepath.Abs(*directory)
		if err != nil {
			return &errs.DataAccessError{Message: fmt.Sprintf("unable to get the absolute path for directory '%s'", dir), Cause: err}
		}
		log.Tracef("setting the default directory '%s'", dir)

		GetDefaultLayerInstance().SetDirectory(&dir)
	}
	return nil
}

/*
Returns the value of the dry run flag as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetDryRun() (*bool, error) {
	log.Tracef("retrieving the '%s' configuration option", "dryRun")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			dryRun, err := (*configurationLayer).GetDryRun()
			if err != nil {
				return nil, err
			}
			if dryRun != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "dryRun", *dryRun)
				return dryRun, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetDryRun()
}

/*
Returns the Git configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetGit() (*ent.GitConfiguration, error) {
	log.Trace("retrieving the Git configuration")
	if c.gitSection == nil {
		// parse the 'remotes' map
		remotes := make(map[string]*ent.GitRemoteConfiguration)
		for _, layer := range c.layers {
			if layer != nil {
				git, err := (*layer).GetGit()
				if err != nil {
					return nil, err
				}
				for remoteName, remote := range *(*git).GetRemotes() {
					if remote != nil && remotes[remoteName] == nil {
						remotes[remoteName] = remote
						log.Tracef("the '%s.%s[%s]' configuration option has been resolved", "git", "remotes", remoteName)
					}
				}
			}
		}

		gs, err := ent.NewGitConfigurationWith(&remotes)
		if err != nil {
			return nil, err
		}
		c.gitSection = gs
	}
	return c.gitSection, nil
}

/*
Returns the initial version defined by this configuration to use when no past version is available in the commit history.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetInitialVersion() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "initialVersion")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			initialVersion, err := (*configurationLayer).GetInitialVersion()
			if err != nil {
				return nil, err
			}
			if initialVersion != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "initialVersion", *initialVersion)
				return initialVersion, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetInitialVersion()
}

/*
Returns the selected preset configuration as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetPreset() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "preset")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			preset, err := (*configurationLayer).GetPreset()
			if err != nil {
				return nil, err
			}
			if preset != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "preset", *preset)
				return preset, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetPreset()
}

/*
Returns the release assets configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetReleaseAssets() (*map[string]*ent.Attachment, error) {
	log.Trace("retrieving the release assets")
	if c.releaseAssetsSection == nil {
		// parse the 'releaseAssets' map
		releaseAssetsSection := make(map[string]*ent.Attachment)
		for _, layer := range c.layers {
			if layer != nil {
				releaseAssets, err := (*layer).GetReleaseAssets()
				if err != nil {
					return nil, err
				}
				for releaseAssetName, releaseAsset := range *releaseAssets {
					if releaseAsset != nil && releaseAssetsSection[releaseAssetName] == nil {
						releaseAssetsSection[releaseAssetName] = releaseAsset
						log.Tracef("the '%s.[%s]' configuration option has been resolved", "releaseAssets", releaseAssetName)
					}
				}
			}
		}
		c.releaseAssetsSection = &releaseAssetsSection
	}
	return c.releaseAssetsSection, nil
}

/*
Returns the flag that enables tolerance in reading release names with arbitrary prefixes or extra non critical characters
as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetReleaseLenient() (*bool, error) {
	log.Tracef("retrieving the '%s' configuration option", "releaseLenient")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			releaseLenient, err := (*configurationLayer).GetReleaseLenient()
			if err != nil {
				return nil, err
			}
			if releaseLenient != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "releaseLenient", *releaseLenient)
				return releaseLenient, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetReleaseLenient()
}

/*
Returns the prefix to use in release name generation as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetReleasePrefix() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "releasePrefix")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			releasePrefix, err := (*configurationLayer).GetReleasePrefix()
			if err != nil {
				return nil, err
			}
			if releasePrefix != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "releasePrefix", *releasePrefix)
				return releasePrefix, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetReleasePrefix()
}

/*
Returns the release types configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetReleaseTypes() (*ent.ReleaseTypes, error) {
	log.Trace("retrieving the release types")
	if c.releaseTypesSection == nil {
		// parse the 'enabled' items list
		enabled := make([]*string, 0)
		for _, layer := range c.layers {
			if layer != nil {
				releaseTypes, err := (*layer).GetReleaseTypes()
				if err != nil {
					return nil, err
				}
				if releaseTypes.GetEnabled() != nil && len(*releaseTypes.GetEnabled()) > 0 {
					enabled = *releaseTypes.GetEnabled()
					log.Tracef("the '%s.%s' configuration option value is: '%v'", "releaseTypes", "enabled", enabled)
					break
				}
			}
		}

		// parse the 'publicationServices' items list
		publicationServices := make([]*string, 0)
		for _, layer := range c.layers {
			if layer != nil {
				releaseTypes, err := (*layer).GetReleaseTypes()
				if err != nil {
					return nil, err
				}
				if releaseTypes.GetPublicationServices() != nil && len(*releaseTypes.GetPublicationServices()) > 0 {
					publicationServices = *releaseTypes.GetPublicationServices()
					log.Tracef("the '%s.%s' configuration option value is: '%v'", "releaseTypes", "publicationServices", publicationServices)
					break
				}
			}
		}

		// parse the 'remoteRepositories' items list
		remoteRepositories := make([]*string, 0)
		for _, layer := range c.layers {
			if layer != nil {
				releaseTypes, err := (*layer).GetReleaseTypes()
				if err != nil {
					return nil, err
				}
				if releaseTypes.GetRemoteRepositories() != nil && len(*releaseTypes.GetRemoteRepositories()) > 0 {
					remoteRepositories = *releaseTypes.GetRemoteRepositories()
					log.Tracef("the '%s.%s' configuration option value is: '%v'", "releaseTypes", "remoteRepositories", remoteRepositories)
					break
				}
			}
		}

		// parse the 'items' map
		items := make(map[string]*ent.ReleaseType)
		for _, enabledItem := range enabled {
			for _, layer := range c.layers {
				if layer != nil {
					releaseTypes, err := (*layer).GetReleaseTypes()
					if err != nil {
						return nil, err
					}

					item := (*(*releaseTypes).GetItems())[*enabledItem]
					if item != nil {
						items[*enabledItem] = item
						log.Tracef("the '%s.%s[%s]' configuration option has been resolved", "releaseTypes", "items", *enabledItem)
						break
					}
				}
			}
		}

		rt, err := ent.NewReleaseTypesWith(&enabled, &publicationServices, &remoteRepositories, &items)
		if err != nil {
			return nil, err
		}
		c.releaseTypesSection = rt
	}
	return c.releaseTypesSection, nil
}

/*
Returns the value of the resume flag as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetResume() (*bool, error) {
	log.Tracef("retrieving the '%s' configuration option", "resume")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			resume, err := (*configurationLayer).GetResume()
			if err != nil {
				return nil, err
			}
			if resume != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "resume", *resume)
				return resume, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetResume()
}

/*
Returns the versioning scheme to use as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetScheme() (*ver.Scheme, error) {
	log.Tracef("retrieving the '%s' configuration option", "scheme")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			scheme, err := (*configurationLayer).GetScheme()
			if err != nil {
				return nil, err
			}
			if scheme != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "scheme", *scheme)
				return scheme, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetScheme()
}

/*
Returns the services configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetServices() (*map[string]*ent.ServiceConfiguration, error) {
	log.Trace("retrieving the services")
	if c.servicesSection == nil {
		// parse the 'services' map
		servicesSection := make(map[string]*ent.ServiceConfiguration)
		for _, layer := range c.layers {
			if layer != nil {
				services, err := (*layer).GetServices()
				if err != nil {
					return nil, err
				}
				for serviceName, service := range *services {
					if service != nil && servicesSection[serviceName] == nil {
						servicesSection[serviceName] = service
						log.Tracef("the '%s.[%s]' configuration option has been resolved", "services", serviceName)
					}
				}
			}
		}
		c.servicesSection = &servicesSection
	}
	return c.servicesSection, nil
}

/*
Returns the path to a custom shared configuration file as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetSharedConfigurationFile() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "sharedConfigurationFile")
	for layerPriority, configurationLayer := range c.layers {
		if configurationLayer != nil {
			// custom shared configuration file configuration is ignored on custom shared configuration file layers to avoid chaining
			if layerPriority != int(CUSTOM_SHARED_FILE) {
				configurationFile, err := (*configurationLayer).GetSharedConfigurationFile()
				if err != nil {
					return nil, err
				}
				if configurationFile != nil {
					log.Tracef("the '%s' configuration option value is: '%s'", "sharedConfigurationFile", *configurationFile)
					return configurationFile, nil
				}
			}
		}
	}
	return GetDefaultLayerInstance().GetSharedConfigurationFile()
}

/*
Returns the path to the file where the Nyx State must be saved as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetStateFile() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "stateFile")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			stateFile, err := (*configurationLayer).GetStateFile()
			if err != nil {
				return nil, err
			}
			if stateFile != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "stateFile", *stateFile)
				return stateFile, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetStateFile()
}

/*
Returns the substitutions configuration section.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetSubstitutions() (*ent.Substitutions, error) {
	log.Trace("retrieving the substitutions")
	if c.substitutionsSection == nil {
		// parse the 'enabled' items list
		enabled := make([]*string, 0)
		for _, layer := range c.layers {
			if layer != nil {
				substitutions, err := (*layer).GetSubstitutions()
				if err != nil {
					return nil, err
				}
				if substitutions.GetEnabled() != nil && len(*substitutions.GetEnabled()) > 0 {
					enabled = *substitutions.GetEnabled()
					log.Tracef("the '%s.%s' configuration option value is: '%v'", "substitutions", "enabled", enabled)
					break
				}
			}
		}

		// parse the 'items' map
		items := make(map[string]*ent.Substitution)
		for _, enabledItem := range enabled {
			for _, layer := range c.layers {
				if layer != nil {
					substitutions, err := (*layer).GetSubstitutions()
					if err != nil {
						return nil, err
					}

					if substitutions != nil && (*substitutions).GetItems() != nil {
						item := (*(*substitutions).GetItems())[*enabledItem]
						if item != nil {
							items[*enabledItem] = item
							log.Tracef("the '%s.%s[%s]' configuration option has been resolved", "substitutions", "items", *enabledItem)
							break
						}
					}
				}
			}
		}

		s, err := ent.NewSubstitutionsWith(&enabled, &items)
		if err != nil {
			return nil, err
		}
		c.substitutionsSection = s
	}
	return c.substitutionsSection, nil
}

/*
Returns the value of the summary flag as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetSummary() (*bool, error) {
	log.Tracef("retrieving the '%s' configuration option", "summary")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			summary, err := (*configurationLayer).GetSummary()
			if err != nil {
				return nil, err
			}
			if summary != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "summary", *summary)
				return summary, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetSummary()
}

/*
Returns the path to the file where the Nyx summary must be saved as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetSummaryFile() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "summaryFile")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			summaryFile, err := (*configurationLayer).GetSummaryFile()
			if err != nil {
				return nil, err
			}
			if summaryFile != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "summaryFile", *summaryFile)
				return summaryFile, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetSummaryFile()
}

/*
Returns the logging verbosity level as it's defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetVerbosity() (*ent.Verbosity, error) {
	log.Tracef("retrieving the '%s' configuration option", "verbosity")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			verbosity, err := (*configurationLayer).GetVerbosity()
			if err != nil {
				return nil, err
			}
			if verbosity != nil {
				log.Tracef("the '%s' configuration option value is: '%v'", "verbosity", *verbosity)
				return verbosity, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetVerbosity()
}

/*
Returns the version defined by this configuration.

Error is:
- DataAccessError: in case the option cannot be read or accessed.
- IllegalPropertyError: in case the option has been defined but has incorrect values or it can't be resolved.
*/
func (c *Configuration) GetVersion() (*string, error) {
	log.Tracef("retrieving the '%s' configuration option", "version")
	for _, configurationLayer := range c.layers {
		if configurationLayer != nil {
			version, err := (*configurationLayer).GetVersion()
			if err != nil {
				return nil, err
			}
			if version != nil {
				log.Tracef("the '%s' configuration option value is: '%s'", "version", *version)
				return version, nil
			}
		}
	}
	return GetDefaultLayerInstance().GetVersion()
}
