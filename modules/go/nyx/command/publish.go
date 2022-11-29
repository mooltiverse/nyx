/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package command

import (
	"fmt" // https://pkg.go.dev/fmt

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
)

const (
	// The common prefix used for all the internal state attributes managed by this class.
	PUBLISH_INTERNAL_ATTRIBUTE_PREFIX = "publish"

	// The common prefix used for all the internal state attributes managed by this class, representing an input.
	PUBLISH_INTERNAL_INPUT_ATTRIBUTE_PREFIX = PUBLISH_INTERNAL_ATTRIBUTE_PREFIX + "." + "input"

	// The common prefix used for all the internal state attributes managed by this class, representing an output.
	PUBLISH_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX = PUBLISH_INTERNAL_ATTRIBUTE_PREFIX + "." + "output"

	// The name used for the internal state attribute where we store the version.
	PUBLISH_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION = PUBLISH_INTERNAL_INPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "version"

	// The name used for the internal state attribute where we store the last version that was published by this command.
	PUBLISH_INTERNAL_OUPUT_ATTRIBUTE_STATE_VERSION = PUBLISH_INTERNAL_OUTPUT_ATTRIBUTE_PREFIX + "." + "state" + "." + "version"
)

/*
The Publish command takes care of publishing a release.

This class is not meant to be used in multi-threaded environments.
*/
type Publish struct {
	// Extend abstractCommand by composition
	abstractCommand
}

/*
Standard constructor.

Arguments are as follows:

- state the state reference
- repository the repository reference

Error is:

- NilPointerError: if a given argument is nil
*/
func NewPublish(state *stt.State, repository *git.Repository) (*Publish, error) {
	if state == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the State object cannot be nil")}
	}
	if repository == nil {
		return nil, &errs.NilPointerError{Message: fmt.Sprintf("the Repository object cannot be nil")}
	}
	log.Debugf("new Publish command object")

	res := &Publish{}
	res.abstractCommand.repository = repository
	res.abstractCommand.state = state
	return res, nil
}

/*
Publishes the release to remotes.

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Publish) publish() error {
	releaseTypes, err := c.State().GetConfiguration().GetReleaseTypes()
	if err != nil {
		return err
	}
	if releaseTypes == nil || releaseTypes.GetPublicationServices() == nil || len(*releaseTypes.GetPublicationServices()) == 0 {
		log.Debugf("no publication services have been configured")
	} else {
		dryRun, err := c.State().GetConfiguration().GetDryRun()
		if err != nil {
			return err
		}
		releaseType, err := c.State().GetReleaseType()
		if err != nil {
			return err
		}
		description, err := c.renderTemplate(releaseType.GetDescription())
		if err != nil {
			return err
		}

		version, err := c.State().GetVersion()
		if err != nil {
			return err
		}
		for _, serviceName := range *releaseTypes.GetPublicationServices() {
			log.Debugf("publishing version '%s' to '%s'", *version, *serviceName)
			if *dryRun {
				log.Infof("publish to '%s' skipped due to dry run", *serviceName)
			} else {
				service, err := c.resolveReleaseService(*serviceName)
				if err != nil {
					return err
				}
				// The first two parameters here are nil because the repository owner and name are expected to be passed
				// along with service options. This is just a place where we could override them.
				release, err := (*service).PublishRelease(nil, nil, version, *version, description)
				if err != nil {
					return err
				}
				err = c.putInternalAttribute(PUBLISH_INTERNAL_OUPUT_ATTRIBUTE_STATE_VERSION, version)
				if err != nil {
					return err
				}

				// publish release assets now
				releaseAssets, err := c.State().GetConfiguration().GetReleaseAssets()
				if err != nil {
					return err
				}
				if releaseAssets == nil || len(*releaseAssets) == 0 {
					log.Debugf("no release asset has been configured for publication")
				} else {
					for configuredAssetKey, configuredAssetValue := range *releaseAssets {
						// if the release type has configured the release types, that is considered a filter over the global release types
						// so only the ones enabled in the release type must be published
						assets := releaseType.GetAssets()
						includeAsset := false
						if assets != nil {
							for _, a := range *assets {
								if a != nil && *a == configuredAssetKey {
									includeAsset = true
								}
							}
						}
						if assets == nil || includeAsset {
							log.Debugf("publishing release asset '%s'", configuredAssetKey)

							// we need to render each asset's field before we publish, so we create a new Attachment instance with all the fields rendered from the configured asset
							assetFileName, err := c.renderTemplate(configuredAssetValue.GetFileName())
							if err != nil {
								return err
							}
							assetDescription, err := c.renderTemplate(configuredAssetValue.GetDescription())
							if err != nil {
								return err
							}
							assetPath, err := c.renderTemplate(configuredAssetValue.GetPath())
							if err != nil {
								return err
							}
							assetType, err := c.renderTemplate(configuredAssetValue.GetType())
							if err != nil {
								return err
							}
							asset := ent.NewAttachmentWith(assetFileName, assetDescription, assetPath, assetType)

							// now actually publish the asset
							release, err = (*service).PublishReleaseAssets(nil, nil, release, []ent.Attachment{*asset})
							if err != nil {
								return err
							}
							resultAssets, err := c.State().GetReleaseAssets()
							if err != nil {
								return err
							}
							resultAssetsObject := append(*resultAssets, *asset)
							resultAssets = &resultAssetsObject
							err = c.State().SetReleaseAssets(resultAssets)
							if err != nil {
								return err
							}
							log.Debugf("release asset '%s' has been published to '%s' for release '%s'", configuredAssetKey, *serviceName, (*release).GetTag())
						} else {
							log.Debugf("release asset '%s' has been configured globally but the current release type is configured to skip it", configuredAssetKey)
						}
					}
				}

				log.Debugf("version '%s' has been published to '%s'", *version, *serviceName)
			}
		}
	}
	return nil
}

/*
This method stores the state internal attributes used for up-to-date checks so that subsequent invocations
of the IsUpToDate() method can find them and determine if the command is already up to date.

This method is meant to be invoked at the end of a successful Run().

Error is:

- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Publish) storeStatusInternalAttributes() error {
	log.Debugf("storing the Publish command internal attributes to the State")
	dryRun, err := c.State().GetConfiguration().GetDryRun()
	if err != nil {
		return err
	}
	if !*dryRun {
		version, err := c.State().GetVersion()
		if err != nil {
			return err
		}
		err = c.putInternalAttribute(PUBLISH_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, version)
		if err != nil {
			return err
		}
	}
	return nil
}

/*
Returns true if this command is up to date, which means that the internal State would not
change by running the command again. It other words, when this method returns true any
invocation of the Run method is needless and idempotent about the state.

This method uses the quickest method to verify whether the state is up to date or not. This method must not rely on
dependencies and it must always evaluate its own status independently.

As a general rule this method checks if its inputs (i.e. from the configuration) have changed since the last run.

Error is:
- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (c *Publish) IsUpToDate() (bool, error) {
	log.Debugf("checking whether the Publish command is up to date")

	// Never up to date if this command hasn't stored a version yet into the state or the stored version is different than the state version
	version, err := c.State().GetVersion()
	if err != nil {
		return false, err
	}
	isVersionUpTodate, err := c.isInternalAttributeUpToDate(PUBLISH_INTERNAL_INPUT_ATTRIBUTE_STATE_VERSION, version)
	if err != nil {
		return false, err
	}
	if version == nil || !isVersionUpTodate {
		log.Debugf("the Publish command is not up to date because the internal state has no version yet or the state version doesn't match the version previously published by Publish")
		return false, nil
	}

	return isVersionUpTodate, nil
}

/*
Runs the command and returns the updated reference to the state object. In order to improve performances you should only
invoke this method when IsUpToDate returns false.

Error is:
- DataAccessError in case the configuration can't be loaded for some reason.
- IllegalPropertyError in case the configuration has some illegal options.
- GitError in case of unexpected issues when accessing the Git repository.
- ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (c *Publish) Run() (*stt.State, error) {
	newVersion, err := c.State().GetNewVersion()
	if err != nil {
		return nil, err
	}

	if newVersion {
		releaseType, err := c.State().GetReleaseType()
		if err != nil {
			return nil, err
		}
		doCommit, err := c.renderTemplateAsBoolean(releaseType.GetPublish())
		if doCommit {
			log.Debugf("the release type has the publish flag enabled")
			err = c.publish()
			if err != nil {
				return nil, err
			}
		} else {
			log.Debugf("the release type has the publish flag disabled")
		}
	} else {
		log.Debugf("no version change detected. Nothing to publish.")
	}

	err = c.storeStatusInternalAttributes()
	if err != nil {
		return nil, err
	}

	return c.State(), nil
}
