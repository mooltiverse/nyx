//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

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
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	io "github.com/mooltiverse/nyx/src/go/nyx/io"
	utl "github.com/mooltiverse/nyx/src/go/utils"
	ver "github.com/mooltiverse/nyx/src/go/version"
)

/*
Performs checks on the serialization and deserialization using example files
*/
func TestSaveAndLoadJSON(t *testing.T) {
	// load an example configuration file
	assert.NotEmpty(t, os.Getenv(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	source := NewSimpleConfigurationLayer()
	err := io.LoadFromFile(os.Getenv(EXTENDED_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), source)
	assert.NoError(t, err)

	// now save it to another file
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simplest"+fmt.Sprintf("%p", t)+".json"))
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), source)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// now load the second file into another object and then compare their fields
	target := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(savedFile.Name(), target)
	assert.NoError(t, err)

	// all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
	// so nils are to be tested against defaults, non simple nils
	sBump, _ := source.GetBump()
	tBump, _ := target.GetBump()
	if sBump == nil {
		assert.Equal(t, ent.BUMP, tBump)
	} else {
		assert.Equal(t, *sBump, *tBump)
	}

	sConfigurationFile, _ := source.GetConfigurationFile()
	tConfigurationFile, _ := target.GetConfigurationFile()
	if sConfigurationFile == nil {
		assert.Equal(t, ent.CONFIGURATION_FILE, tConfigurationFile)
	} else {
		assert.Equal(t, *sConfigurationFile, *tConfigurationFile)
	}

	// this depends on the current runtime
	//sDirectory, _ := source.GetDirectory()
	//tDirectory, _ := target.GetDirectory()
	//if sDirectory == nil {
	//	assert.Equal(t, ent.DIRECTORY, tDirectory)
	//} else {
	//	assert.Equal(t, *sDirectory, *tDirectory)
	//}

	sDryRun, _ := source.GetDryRun()
	tDryRun, _ := target.GetDryRun()
	if sDryRun == nil {
		assert.Equal(t, ent.DRY_RUN, tDryRun)
	} else {
		assert.Equal(t, *sDryRun, *tDryRun)
	}

	sInitialVersion, _ := source.GetInitialVersion()
	tInitialVersion, _ := target.GetInitialVersion()
	if sInitialVersion == nil {
		assert.Equal(t, ent.INITIAL_VERSION, tInitialVersion)
	} else {
		assert.Equal(t, *sInitialVersion, *tInitialVersion)
	}

	sPreset, _ := source.GetPreset()
	tPreset, _ := target.GetPreset()
	if sPreset == nil {
		assert.Equal(t, ent.PRESET, tPreset)
	} else {
		assert.Equal(t, *sPreset, *tPreset)
	}

	sReleaseAssets, _ := source.GetReleaseAssets()
	tReleaseAssets, _ := target.GetReleaseAssets()

	if sReleaseAssets == nil {
		assert.Equal(t, ent.RELEASE_ASSETS, tReleaseAssets)
	} else {
		for sReleaseAssetsItemKey, _ := range *sReleaseAssets {
			assert.NotNil(t, (*tReleaseAssets)[sReleaseAssetsItemKey])
			assert.Equal(t, (*sReleaseAssets)[sReleaseAssetsItemKey], (*tReleaseAssets)[sReleaseAssetsItemKey])
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetFileName(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetFileName())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetDescription(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetDescription())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetType(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetType())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetPath(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetPath())
		}
	}

	sReleaseLenient, _ := source.GetReleaseLenient()
	tReleaseLenient, _ := target.GetReleaseLenient()
	if sReleaseLenient == nil {
		assert.Equal(t, ent.RELEASE_LENIENT, tReleaseLenient)
	} else {
		assert.Equal(t, *sReleaseLenient, *tReleaseLenient)
	}

	sReleasePrefix, _ := source.GetReleasePrefix()
	tReleasePrefix, _ := target.GetReleasePrefix()
	if sReleasePrefix == nil {
		assert.Equal(t, ent.RELEASE_PREFIX, tReleasePrefix)
	} else {
		assert.Equal(t, *sReleasePrefix, *tReleasePrefix)
	}

	sResume, _ := source.GetResume()
	tResume, _ := target.GetResume()
	if sResume == nil {
		assert.Equal(t, ent.RESUME, tResume)
	} else {
		assert.Equal(t, *sResume, *tResume)
	}

	sScheme, _ := source.GetScheme()
	tScheme, _ := target.GetScheme()
	if sScheme == nil {
		assert.Equal(t, ent.SCHEME, tScheme)
	} else {
		assert.Equal(t, *sScheme, *tScheme)
	}

	sSharedConfigurationFile, _ := source.GetSharedConfigurationFile()
	tSharedConfigurationFile, _ := target.GetSharedConfigurationFile()
	if sSharedConfigurationFile == nil {
		assert.Equal(t, ent.SHARED_CONFIGURATION_FILE, tSharedConfigurationFile)
	} else {
		assert.Equal(t, *sSharedConfigurationFile, *tSharedConfigurationFile)
	}

	sSummary, _ := source.GetSummary()
	tSummary, _ := target.GetSummary()
	if sSummary == nil {
		assert.Equal(t, ent.SUMMARY, tSummary)
	} else {
		assert.Equal(t, *sSummary, *tSummary)
	}

	sSummaryFile, _ := source.GetSummaryFile()
	tSummaryFile, _ := target.GetSummaryFile()
	if sSummaryFile == nil {
		assert.Equal(t, ent.SUMMARY_FILE, tSummaryFile)
	} else {
		assert.Equal(t, *sSummaryFile, *tSummaryFile)
	}

	sStateFile, _ := source.GetStateFile()
	tStateFile, _ := target.GetStateFile()
	if sStateFile == nil {
		assert.Equal(t, ent.STATE_FILE, tStateFile)
	} else {
		assert.Equal(t, *sStateFile, *tStateFile)
	}

	sVerbosity, _ := source.GetVerbosity()
	tVerbosity, _ := target.GetVerbosity()
	if sVerbosity == nil {
		assert.Equal(t, ent.VERBOSITY, tVerbosity)
	} else {
		assert.Equal(t, *sVerbosity, *tVerbosity)
	}

	sVersion, _ := source.GetVersion()
	tVersion, _ := target.GetVersion()
	if sVersion == nil {
		assert.Equal(t, ent.VERSION, tVersion)
	} else {
		assert.Equal(t, *sVersion, *tVersion)
	}

	sChangelog, _ := source.GetChangelog()
	tChangelog, _ := target.GetChangelog()

	if sChangelog == nil {
		assert.Nil(t, tChangelog)
	} else {
		if sChangelog.GetAppend() == nil {
			assert.Nil(t, tChangelog.GetAppend())
		} else {
			assert.Equal(t, *sChangelog.GetAppend(), *tChangelog.GetAppend())
		}

		if sChangelog.GetPath() == nil {
			assert.Nil(t, tChangelog.GetPath())
		} else {
			assert.Equal(t, *sChangelog.GetPath(), *tChangelog.GetPath())
		}

		if sChangelog.GetSections() == nil {
			assert.Nil(t, tChangelog.GetSections())
		} else {
			for sChangelogSectionItemKey, _ := range *sChangelog.GetSections() {
				assert.NotNil(t, (*tChangelog.GetSections())[sChangelogSectionItemKey])
				assert.Equal(t, (*sChangelog.GetSections())[sChangelogSectionItemKey], (*tChangelog.GetSections())[sChangelogSectionItemKey])
			}
		}
		if sChangelog.GetSubstitutions() == nil {
			assert.Nil(t, tChangelog.GetSubstitutions())
		} else {
			for sChangelogSubstitutionItemKey, _ := range *sChangelog.GetSubstitutions() {
				assert.NotNil(t, (*tChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey])
				assert.Equal(t, (*sChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey], (*tChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey])
			}
		}
	}

	if sChangelog.GetTemplate() == nil {
		assert.Equal(t, ent.VERSION, tChangelog.GetTemplate())
	} else {
		assert.Equal(t, *sChangelog.GetTemplate(), *tChangelog.GetTemplate())
	}

	sCommitMessageConventions, _ := source.GetCommitMessageConventions()
	tCommitMessageConventions, _ := target.GetCommitMessageConventions()

	if sCommitMessageConventions == nil {
		assert.Equal(t, ent.COMMIT_MESSAGE_CONVENTIONS, tCommitMessageConventions)
	} else {
		if sCommitMessageConventions.GetEnabled() == nil {
			assert.Nil(t, tCommitMessageConventions.GetEnabled())
		} else {
			for sCommitMessageConventionsEnabled, _ := range *sCommitMessageConventions.GetEnabled() {
				assert.NotNil(t, (*tCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled])
				assert.Equal(t, (*sCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled], (*tCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled])
			}
			for sCommitMessageConventionsItemKey, _ := range *sCommitMessageConventions.GetItems() {
				assert.NotNil(t, (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetExpression())
				assert.Equal(t, (*(*sCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey].GetExpression()), (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey].GetExpression()))
				assert.NotNil(t, (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions())
				assert.Equal(t, (*(*sCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions(), (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions())
			}
		}
	}

	sGit, _ := source.GetGit()
	tGit, _ := target.GetGit()

	if sGit == nil {
		assert.Equal(t, ent.GIT, tGit)
	} else {
		if sGit.GetRemotes() == nil {
			assert.Nil(t, tGit.GetRemotes())
		} else {
			for sGitRemotesItemKey, _ := range *sGit.GetRemotes() {
				assert.NotNil(t, (*tGit.GetRemotes())[sGitRemotesItemKey])
				assert.Equal(t, (*sGit.GetRemotes())[sGitRemotesItemKey], (*tGit.GetRemotes())[sGitRemotesItemKey])
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetAuthenticationMethod(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetAuthenticationMethod())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPassword(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPassword())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetUser(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetUser())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPrivateKey(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPrivateKey())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPassphrase(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPassphrase())
			}
		}
	}

	sReleaseTypes, _ := source.GetReleaseTypes()
	tReleaseTypes, _ := target.GetReleaseTypes()

	if sReleaseTypes == nil {
		assert.Equal(t, ent.RELEASE_TYPES, tReleaseTypes)
	} else {
		if sReleaseTypes.GetEnabled() == nil {
			assert.Nil(t, tReleaseTypes.GetEnabled())
		} else {
			for sReleaseTypesEnabled, _ := range *sReleaseTypes.GetEnabled() {
				assert.NotNil(t, (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled])
				assert.Equal(t, (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled], (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled])
			}
		}
		if sReleaseTypes.GetPublicationServices() == nil {
			assert.Nil(t, tReleaseTypes.GetPublicationServices())
		} else {
			for sPublicationServices, _ := range *sReleaseTypes.GetPublicationServices() {
				assert.NotNil(t, (*tReleaseTypes.GetPublicationServices())[sPublicationServices])
				assert.Equal(t, (*sReleaseTypes.GetPublicationServices())[sPublicationServices], (*tReleaseTypes.GetPublicationServices())[sPublicationServices])
			}
		}
		if sReleaseTypes.GetRemoteRepositories() == nil {
			assert.Nil(t, tReleaseTypes.GetRemoteRepositories())
		} else {
			for sGitRemoteRepositoriesItemKey, _ := range *sReleaseTypes.GetRemoteRepositories() {
				assert.NotNil(t, (*tReleaseTypes.GetRemoteRepositories())[sGitRemoteRepositoriesItemKey])
			}
		}
		if sReleaseTypes.GetItems() == nil {
			assert.Nil(t, tReleaseTypes.GetItems())
		} else {
			for sReleaseTypesItemKey, _ := range *sReleaseTypes.GetItems() {
				assert.NotNil(t, (*tReleaseTypes.GetItems())[sReleaseTypesItemKey])
				assert.Equal(t, (*sReleaseTypes.GetItems())[sReleaseTypesItemKey], (*tReleaseTypes.GetItems())[sReleaseTypesItemKey])

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_ASSETS, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())[i], (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())[i])
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapseVersions(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapseVersions())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapsedVersionQualifier(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapsedVersionQualifier())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetDescription(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetDescription())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommit(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommit())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommitMessage(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommitMessage())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPush(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPush())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPushForce(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPushForce())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTag(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTag())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagForce(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagForce())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagMessage(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagMessage())

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_NAMES, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames() {
						assert.Equal(t, *(*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())[i], *(*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())[i])
					}
				}

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetQualifier(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetQualifier())
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetValue(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetValue())
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetPosition(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetPosition())
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchBranches(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchBranches())

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())
				} else {
					for sMatchEnvironmentVariablesItemKey, _ := range *(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())[sMatchEnvironmentVariablesItemKey], (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())[sMatchEnvironmentVariablesItemKey])
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchWorkspaceStatus(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchWorkspaceStatus())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublish(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublish())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishDraft(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishDraft())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishPreRelease(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishPreRelease())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetReleaseName(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetReleaseName())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRange(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRange())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRangeFromBranchName(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRangeFromBranchName())
			}
		}
	}

	sServices, _ := source.GetServices()
	tServices, _ := target.GetServices()

	if sServices == nil {
		assert.Equal(t, ent.SERVICES, tServices)
	} else {
		for sServicesItemKey, _ := range *sServices {
			assert.NotNil(t, (*tServices)[sServicesItemKey])
			assert.Equal(t, (*sServices)[sServicesItemKey], (*tServices)[sServicesItemKey])
			assert.Equal(t, (*(*sServices)[sServicesItemKey]).GetType(), (*(*tServices)[sServicesItemKey]).GetType())
			if (*(*sServices)[sServicesItemKey]).GetOptions() == nil {
				assert.Nil(t, (*(*tServices)[sServicesItemKey]).GetOptions())
			} else {
				for sServicesOptionItemKey, _ := range *(*(*sServices)[sServicesItemKey]).GetOptions() {
					assert.NotNil(t, (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey])
					assert.Equal(t, (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey], (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey])
				}
			}
		}
	}

	sSubstitutions, _ := source.GetSubstitutions()
	tSubstitutions, _ := target.GetSubstitutions()

	if sSubstitutions == nil {
		assert.Equal(t, ent.SUBSTITUTIONS, tSubstitutions)
	} else {
		if sSubstitutions.GetEnabled() == nil {
			assert.Nil(t, tSubstitutions.GetEnabled())
		} else {
			for sSubstitutionsEnabled, _ := range *sSubstitutions.GetEnabled() {
				assert.NotNil(t, (*tSubstitutions.GetEnabled())[sSubstitutionsEnabled])
				assert.Equal(t, (*sSubstitutions.GetEnabled())[sSubstitutionsEnabled], (*tSubstitutions.GetEnabled())[sSubstitutionsEnabled])
			}
			for sSubstitutionsItemKey, _ := range *sSubstitutions.GetItems() {
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetFiles())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetFiles()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetFiles()))
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetMatch())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetMatch()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetMatch()))
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetReplace())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetReplace()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetReplace()))
			}
		}
	}
}

func TestSaveAndLoadYAML(t *testing.T) {
	// load an example configuration file
	assert.NotEmpty(t, os.Getenv(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), "A configuration file path must be passed to this test as an environment variable but it was not set")
	source := NewSimpleConfigurationLayer()
	err := io.LoadFromFile(os.Getenv(EXTENDED_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE), source)
	assert.NoError(t, err)

	// now save it to another file
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, err := os.Create(filepath.Join(tempDir, "simplest"+fmt.Sprintf("%p", t)+".yaml"))
	assert.NoError(t, err)
	err = io.Save(savedFile.Name(), source)
	assert.NoError(t, err)

	defer os.Remove(savedFile.Name())

	// now load the second file into another object and then compare their fields
	target := NewSimpleConfigurationLayer()
	err = io.LoadFromFile(savedFile.Name(), target)
	assert.NoError(t, err)

	// all the following tests must consider that a value that was not defined in the source configuration gets its default value when unmarshalling
	// so nils are to be tested against defaults, non simple nils
	sBump, _ := source.GetBump()
	tBump, _ := target.GetBump()
	if sBump == nil {
		assert.Equal(t, ent.BUMP, tBump)
	} else {
		assert.Equal(t, *sBump, *tBump)
	}

	sConfigurationFile, _ := source.GetConfigurationFile()
	tConfigurationFile, _ := target.GetConfigurationFile()
	if sConfigurationFile == nil {
		assert.Equal(t, ent.CONFIGURATION_FILE, tConfigurationFile)
	} else {
		assert.Equal(t, *sConfigurationFile, *tConfigurationFile)
	}

	// this depends on the current runtime
	//sDirectory, _ := source.GetDirectory()
	//tDirectory, _ := target.GetDirectory()
	//if sDirectory == nil {
	//	assert.Equal(t, ent.DIRECTORY, tDirectory)
	//} else {
	//	assert.Equal(t, *sDirectory, *tDirectory)
	//}

	sDryRun, _ := source.GetDryRun()
	tDryRun, _ := target.GetDryRun()
	if sDryRun == nil {
		assert.Equal(t, ent.DRY_RUN, tDryRun)
	} else {
		assert.Equal(t, *sDryRun, *tDryRun)
	}

	sInitialVersion, _ := source.GetInitialVersion()
	tInitialVersion, _ := target.GetInitialVersion()
	if sInitialVersion == nil {
		assert.Equal(t, ent.INITIAL_VERSION, tInitialVersion)
	} else {
		assert.Equal(t, *sInitialVersion, *tInitialVersion)
	}

	sPreset, _ := source.GetPreset()
	tPreset, _ := target.GetPreset()
	if sPreset == nil {
		assert.Equal(t, ent.PRESET, tPreset)
	} else {
		assert.Equal(t, *sPreset, *tPreset)
	}

	sReleaseAssets, _ := source.GetReleaseAssets()
	tReleaseAssets, _ := target.GetReleaseAssets()

	if sReleaseAssets == nil {
		assert.Equal(t, ent.RELEASE_ASSETS, tReleaseAssets)
	} else {
		for sReleaseAssetsItemKey, _ := range *sReleaseAssets {
			assert.NotNil(t, (*tReleaseAssets)[sReleaseAssetsItemKey])
			assert.Equal(t, (*sReleaseAssets)[sReleaseAssetsItemKey], (*tReleaseAssets)[sReleaseAssetsItemKey])
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetFileName(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetFileName())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetDescription(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetDescription())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetType(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetType())
			assert.Equal(t, (*(*sReleaseAssets)[sReleaseAssetsItemKey]).GetPath(), (*(*tReleaseAssets)[sReleaseAssetsItemKey]).GetPath())
		}
	}

	sReleaseLenient, _ := source.GetReleaseLenient()
	tReleaseLenient, _ := target.GetReleaseLenient()
	if sReleaseLenient == nil {
		assert.Equal(t, ent.RELEASE_LENIENT, tReleaseLenient)
	} else {
		assert.Equal(t, *sReleaseLenient, *tReleaseLenient)
	}

	sReleasePrefix, _ := source.GetReleasePrefix()
	tReleasePrefix, _ := target.GetReleasePrefix()
	if sReleasePrefix == nil {
		assert.Equal(t, ent.RELEASE_PREFIX, tReleasePrefix)
	} else {
		assert.Equal(t, *sReleasePrefix, *tReleasePrefix)
	}

	sResume, _ := source.GetResume()
	tResume, _ := target.GetResume()
	if sResume == nil {
		assert.Equal(t, ent.RESUME, tResume)
	} else {
		assert.Equal(t, *sResume, *tResume)
	}

	sScheme, _ := source.GetScheme()
	tScheme, _ := target.GetScheme()
	if sScheme == nil {
		assert.Equal(t, ent.SCHEME, tScheme)
	} else {
		assert.Equal(t, *sScheme, *tScheme)
	}

	sSharedConfigurationFile, _ := source.GetSharedConfigurationFile()
	tSharedConfigurationFile, _ := target.GetSharedConfigurationFile()
	if sSharedConfigurationFile == nil {
		assert.Equal(t, ent.SHARED_CONFIGURATION_FILE, tSharedConfigurationFile)
	} else {
		assert.Equal(t, *sSharedConfigurationFile, *tSharedConfigurationFile)
	}

	sSummary, _ := source.GetSummary()
	tSummary, _ := target.GetSummary()
	if sSummary == nil {
		assert.Equal(t, ent.SUMMARY, tSummary)
	} else {
		assert.Equal(t, *sSummary, *tSummary)
	}

	sSummaryFile, _ := source.GetSummaryFile()
	tSummaryFile, _ := target.GetSummaryFile()
	if sSummaryFile == nil {
		assert.Equal(t, ent.SUMMARY_FILE, tSummaryFile)
	} else {
		assert.Equal(t, *sSummaryFile, *tSummaryFile)
	}

	sStateFile, _ := source.GetStateFile()
	tStateFile, _ := target.GetStateFile()
	if sStateFile == nil {
		assert.Equal(t, ent.STATE_FILE, tStateFile)
	} else {
		assert.Equal(t, *sStateFile, *tStateFile)
	}

	sVerbosity, _ := source.GetVerbosity()
	tVerbosity, _ := target.GetVerbosity()
	if sVerbosity == nil {
		assert.Equal(t, ent.VERBOSITY, tVerbosity)
	} else {
		assert.Equal(t, *sVerbosity, *tVerbosity)
	}

	sVersion, _ := source.GetVersion()
	tVersion, _ := target.GetVersion()
	if sVersion == nil {
		assert.Equal(t, ent.VERSION, tVersion)
	} else {
		assert.Equal(t, *sVersion, *tVersion)
	}

	sChangelog, _ := source.GetChangelog()
	tChangelog, _ := target.GetChangelog()

	if sChangelog == nil {
		assert.Nil(t, tChangelog)
	} else {
		if sChangelog.GetAppend() == nil {
			assert.Nil(t, tChangelog.GetAppend())
		} else {
			assert.Equal(t, *sChangelog.GetAppend(), *tChangelog.GetAppend())
		}

		if sChangelog.GetPath() == nil {
			assert.Nil(t, tChangelog.GetPath())
		} else {
			assert.Equal(t, *sChangelog.GetPath(), *tChangelog.GetPath())
		}

		if sChangelog.GetSections() == nil {
			assert.Nil(t, tChangelog.GetSections())
		} else {
			for sChangelogSectionItemKey, _ := range *sChangelog.GetSections() {
				assert.NotNil(t, (*tChangelog.GetSections())[sChangelogSectionItemKey])
				assert.Equal(t, (*sChangelog.GetSections())[sChangelogSectionItemKey], (*tChangelog.GetSections())[sChangelogSectionItemKey])
			}
		}
		if sChangelog.GetSubstitutions() == nil {
			assert.Nil(t, tChangelog.GetSubstitutions())
		} else {
			for sChangelogSubstitutionItemKey, _ := range *sChangelog.GetSubstitutions() {
				assert.NotNil(t, (*tChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey])
				assert.Equal(t, (*sChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey], (*tChangelog.GetSubstitutions())[sChangelogSubstitutionItemKey])
			}
		}
	}

	if sChangelog.GetTemplate() == nil {
		assert.Equal(t, ent.VERSION, tChangelog.GetTemplate())
	} else {
		assert.Equal(t, *sChangelog.GetTemplate(), *tChangelog.GetTemplate())
	}

	sCommitMessageConventions, _ := source.GetCommitMessageConventions()
	tCommitMessageConventions, _ := target.GetCommitMessageConventions()

	if sCommitMessageConventions == nil {
		assert.Equal(t, ent.COMMIT_MESSAGE_CONVENTIONS, tCommitMessageConventions)
	} else {
		if sCommitMessageConventions.GetEnabled() == nil {
			assert.Nil(t, tCommitMessageConventions.GetEnabled())
		} else {
			for sCommitMessageConventionsEnabled, _ := range *sCommitMessageConventions.GetEnabled() {
				assert.NotNil(t, (*tCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled])
				assert.Equal(t, (*sCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled], (*tCommitMessageConventions.GetEnabled())[sCommitMessageConventionsEnabled])
			}
			for sCommitMessageConventionsItemKey, _ := range *sCommitMessageConventions.GetItems() {
				assert.NotNil(t, (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetExpression())
				assert.Equal(t, (*(*sCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey].GetExpression()), (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey].GetExpression()))
				assert.NotNil(t, (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions())
				assert.Equal(t, (*(*sCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions(), (*(*tCommitMessageConventions.GetItems())[sCommitMessageConventionsItemKey]).GetBumpExpressions())
			}
		}
	}

	sGit, _ := source.GetGit()
	tGit, _ := target.GetGit()

	if sGit == nil {
		assert.Equal(t, ent.GIT, tGit)
	} else {
		if sGit.GetRemotes() == nil {
			assert.Nil(t, tGit.GetRemotes())
		} else {
			for sGitRemotesItemKey, _ := range *sGit.GetRemotes() {
				assert.NotNil(t, (*tGit.GetRemotes())[sGitRemotesItemKey])
				assert.Equal(t, (*sGit.GetRemotes())[sGitRemotesItemKey], (*tGit.GetRemotes())[sGitRemotesItemKey])
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetAuthenticationMethod(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetAuthenticationMethod())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPassword(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPassword())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetUser(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetUser())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPrivateKey(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPrivateKey())
				assert.Equal(t, (*(*sGit.GetRemotes())[sGitRemotesItemKey]).GetPassphrase(), (*(*tGit.GetRemotes())[sGitRemotesItemKey]).GetPassphrase())
			}
		}
	}

	sReleaseTypes, _ := source.GetReleaseTypes()
	tReleaseTypes, _ := target.GetReleaseTypes()

	if sReleaseTypes == nil {
		assert.Equal(t, ent.RELEASE_TYPES, tReleaseTypes)
	} else {
		if sReleaseTypes.GetEnabled() == nil {
			assert.Nil(t, tReleaseTypes.GetEnabled())
		} else {
			for sReleaseTypesEnabled, _ := range *sReleaseTypes.GetEnabled() {
				assert.NotNil(t, (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled])
				assert.Equal(t, (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled], (*tReleaseTypes.GetEnabled())[sReleaseTypesEnabled])
			}
		}
		if sReleaseTypes.GetPublicationServices() == nil {
			assert.Nil(t, tReleaseTypes.GetPublicationServices())
		} else {
			for sPublicationServices, _ := range *sReleaseTypes.GetPublicationServices() {
				assert.NotNil(t, (*tReleaseTypes.GetPublicationServices())[sPublicationServices])
				assert.Equal(t, (*sReleaseTypes.GetPublicationServices())[sPublicationServices], (*tReleaseTypes.GetPublicationServices())[sPublicationServices])
			}
		}
		if sReleaseTypes.GetRemoteRepositories() == nil {
			assert.Nil(t, tReleaseTypes.GetRemoteRepositories())
		} else {
			for sGitRemoteRepositoriesItemKey, _ := range *sReleaseTypes.GetRemoteRepositories() {
				assert.NotNil(t, (*tReleaseTypes.GetRemoteRepositories())[sGitRemoteRepositoriesItemKey])
			}
		}
		if sReleaseTypes.GetItems() == nil {
			assert.Nil(t, tReleaseTypes.GetItems())
		} else {
			for sReleaseTypesItemKey, _ := range *sReleaseTypes.GetItems() {
				assert.NotNil(t, (*tReleaseTypes.GetItems())[sReleaseTypesItemKey])
				assert.Equal(t, (*sReleaseTypes.GetItems())[sReleaseTypesItemKey], (*tReleaseTypes.GetItems())[sReleaseTypesItemKey])

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_ASSETS, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())[i], (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetAssets())[i])
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapseVersions(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapseVersions())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapsedVersionQualifier(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetCollapsedVersionQualifier())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetDescription(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetDescription())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommit(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommit())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommitMessage(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitCommitMessage())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPush(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPush())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPushForce(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitPushForce())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTag(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTag())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagForce(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagForce())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagMessage(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagMessage())

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_NAMES, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames() {
						assert.Equal(t, *(*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())[i], *(*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetGitTagNames())[i])
					}
				}

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
				} else {
					assert.NotNil(t, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
					assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())
					for i, _ := range *(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetQualifier(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetQualifier())
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetValue(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetValue())
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetPosition(), (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetIdentifiers())[i].GetPosition())
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchBranches(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchBranches())

				if (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables() == nil {
					assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())
				} else {
					for sMatchEnvironmentVariablesItemKey, _ := range *(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables() {
						assert.Equal(t, (*(*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())[sMatchEnvironmentVariablesItemKey], (*(*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchEnvironmentVariables())[sMatchEnvironmentVariablesItemKey])
					}
				}

				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchWorkspaceStatus(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetMatchWorkspaceStatus())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublish(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublish())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishDraft(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishDraft())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishPreRelease(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetPublishPreRelease())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetReleaseName(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetReleaseName())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRange(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRange())
				assert.Equal(t, (*(*sReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRangeFromBranchName(), (*(*tReleaseTypes.GetItems())[sReleaseTypesItemKey]).GetVersionRangeFromBranchName())
			}
		}
	}

	sServices, _ := source.GetServices()
	tServices, _ := target.GetServices()

	if sServices == nil {
		assert.Equal(t, ent.SERVICES, tServices)
	} else {
		for sServicesItemKey, _ := range *sServices {
			assert.NotNil(t, (*tServices)[sServicesItemKey])
			assert.Equal(t, (*sServices)[sServicesItemKey], (*tServices)[sServicesItemKey])
			assert.Equal(t, (*(*sServices)[sServicesItemKey]).GetType(), (*(*tServices)[sServicesItemKey]).GetType())
			if (*(*sServices)[sServicesItemKey]).GetOptions() == nil {
				assert.Nil(t, (*(*tServices)[sServicesItemKey]).GetOptions())
			} else {
				for sServicesOptionItemKey, _ := range *(*(*sServices)[sServicesItemKey]).GetOptions() {
					assert.NotNil(t, (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey])
					assert.Equal(t, (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey], (*(*(*sServices)[sServicesItemKey]).GetOptions())[sServicesOptionItemKey])
				}
			}
		}
	}

	sSubstitutions, _ := source.GetSubstitutions()
	tSubstitutions, _ := target.GetSubstitutions()

	if sSubstitutions == nil {
		assert.Equal(t, ent.SUBSTITUTIONS, tSubstitutions)
	} else {
		if sSubstitutions.GetEnabled() == nil {
			assert.Nil(t, tSubstitutions.GetEnabled())
		} else {
			for sSubstitutionsEnabled, _ := range *sSubstitutions.GetEnabled() {
				assert.NotNil(t, (*tSubstitutions.GetEnabled())[sSubstitutionsEnabled])
				assert.Equal(t, (*sSubstitutions.GetEnabled())[sSubstitutionsEnabled], (*tSubstitutions.GetEnabled())[sSubstitutionsEnabled])
			}
			for sSubstitutionsItemKey, _ := range *sSubstitutions.GetItems() {
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetFiles())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetFiles()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetFiles()))
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetMatch())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetMatch()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetMatch()))
				assert.NotNil(t, (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey]).GetReplace())
				assert.Equal(t, (*(*sSubstitutions.GetItems())[sSubstitutionsItemKey].GetReplace()), (*(*tSubstitutions.GetItems())[sSubstitutionsItemKey].GetReplace()))
			}
		}
	}
}

/*
Performs checks on the serialization of the Configuration object, rather than just layers
*/
func TestSerializationWithMultipleConfigurationLayersJSON(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()

	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetBump(utl.PointerToString("alpha"))
	mediumPriorityConfigurationLayerMock.SetBump(utl.PointerToString("beta"))
	highPriorityConfigurationLayerMock.SetBump(utl.PointerToString("gamma"))

	lpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(nil, utl.PointerToString("CHANGELOG1.md"), &map[string]string{"SectionA1": "regexA1", "SectionA2": "regexA2"}, utl.PointerToString("changelog1.tpl"), &map[string]string{"Expression1": "string1"})
	lowPriorityConfigurationLayerMock.SetChangelog(lpChangelogConfiguration)
	mpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("head"), utl.PointerToString("CHANGELOG2.md"), &map[string]string{"SectionB1": "regexB1", "SectionB2": "regexB2"}, utl.PointerToString("changelog2.tpl"), &map[string]string{"Expression2": "string2"})
	mediumPriorityConfigurationLayerMock.SetChangelog(mpChangelogConfiguration)
	hpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("tail"), utl.PointerToString("CHANGELOG2.md"), &map[string]string{"SectionC1": "regexC1", "SectionC2": "regexC2"}, utl.PointerToString("changelog3.tpl"), &map[string]string{"Expression3": "string3"})
	highPriorityConfigurationLayerMock.SetChangelog(hpChangelogConfiguration)

	lpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention1")}, &map[string]*ent.CommitMessageConvention{"convention1": ent.NewCommitMessageConventionWith(utl.PointerToString("expr1"), &map[string]string{})})
	lowPriorityConfigurationLayerMock.SetCommitMessageConventions(lpCommitMessageConventions)
	mpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention2")}, &map[string]*ent.CommitMessageConvention{"convention2": ent.NewCommitMessageConventionWith(utl.PointerToString("expr2"), &map[string]string{})})
	mediumPriorityConfigurationLayerMock.SetCommitMessageConventions(mpCommitMessageConventions)
	hpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention3")}, &map[string]*ent.CommitMessageConvention{"convention3": ent.NewCommitMessageConventionWith(utl.PointerToString("expr3"), &map[string]string{})})
	highPriorityConfigurationLayerMock.SetCommitMessageConventions(hpCommitMessageConventions)

	lowPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	highPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	lowPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("some/directory"))
	mediumPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("some/other/directory"))
	highPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("the/right/directory"))

	lowPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(false))

	lpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("jdoe1"), utl.PointerToString("pwd1"), nil, nil), "replica": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("stiger1"), utl.PointerToString("sec1"), nil, nil)})
	lowPriorityConfigurationLayerMock.SetGit(lpGitConfiguration)
	mpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("jdoe2"), utl.PointerToString("pwd2"), nil, nil), "clone": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("stiger2"), utl.PointerToString("sec2"), nil, nil)})
	mediumPriorityConfigurationLayerMock.SetGit(mpGitConfiguration)
	hpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), utl.PointerToString("jdoe3"), utl.PointerToString("pwd3"), utl.PointerToString("key3"), utl.PointerToString("passphrase3"))})
	highPriorityConfigurationLayerMock.SetGit(hpGitConfiguration)

	lowPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("9.9.9"))
	mediumPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("8.8.8"))
	highPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("7.7.7"))

	lowPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(SIMPLE_NAME))
	mediumPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(SIMPLE_NAME))
	highPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(EXTENDED_NAME))

	lowPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetA1": ent.NewAttachmentWith(utl.PointerToString("assetA1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetA1.txt"), utl.PointerToString("text/plain")), "assetA2": ent.NewAttachmentWith(utl.PointerToString("assetA2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetA2.bin"), utl.PointerToString("application/octet-stream"))})
	mediumPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetB1": ent.NewAttachmentWith(utl.PointerToString("assetB1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetB1.txt"), utl.PointerToString("text/plain")), "assetB2": ent.NewAttachmentWith(utl.PointerToString("assetB2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetB2.bin"), utl.PointerToString("application/octet-stream"))})
	highPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetC1": ent.NewAttachmentWith(utl.PointerToString("assetC1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetC1.txt"), utl.PointerToString("text/plain")), "assetC2": ent.NewAttachmentWith(utl.PointerToString("assetC2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetC2.bin"), utl.PointerToString("application/octet-stream"))})

	lowPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	mediumPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	highPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))

	lowPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("lpprefix"))
	mediumPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("mpprefix"))
	highPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("hpprefix"))

	lpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type1")}, &[]*string{utl.PointerToString("service1")}, &[]*string{utl.PointerToString("remote1")}, &map[string]*ent.ReleaseType{"type1": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(false), utl.PointerToString("{{branch1}}"), utl.PointerToString("Release description 1"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease1"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	lowPriorityConfigurationLayerMock.SetReleaseTypes(lpReleaseTypes)
	mpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type2")}, &[]*string{utl.PointerToString("service2")}, &[]*string{utl.PointerToString("remote2")}, &map[string]*ent.ReleaseType{"type2": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{branch2}}"), utl.PointerToString("Release description 2"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease2"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	mediumPriorityConfigurationLayerMock.SetReleaseTypes(mpReleaseTypes)
	hpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type3")}, &[]*string{utl.PointerToString("service3")}, &[]*string{utl.PointerToString("remote3")}, &map[string]*ent.ReleaseType{"type3": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{branch3}}"), utl.PointerToString("Release description 3"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease3"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	highPriorityConfigurationLayerMock.SetReleaseTypes(hpReleaseTypes)

	lowPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(false))

	lowPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))
	mediumPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))
	highPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))

	lowPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken1", "REPOSITORY_NAME": "ignoredrepo1", "REPOSITORY_OWNER": "ignoredowner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken2", "REPOSITORY_NAME": "ignoredrepo2", "REPOSITORY_OWNER": "ignoredowner2"})})
	mediumPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo1", "REPOSITORY_OWNER": "owner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo2", "REPOSITORY_OWNER": "owner2"})})
	highPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo3", "REPOSITORY_OWNER": "owner3"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo4", "REPOSITORY_OWNER": "owner4"})})

	lowPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	highPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	lowPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.yaml"))
	mediumPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.yaml"))
	highPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.json"))

	lpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution1")}, &map[string]*ent.Substitution{"substitution1": ent.NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))})
	lowPriorityConfigurationLayerMock.SetSubstitutions(lpSubstitutions)
	mpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution2")}, &map[string]*ent.Substitution{"substitution2": ent.NewSubstitutionWith(utl.PointerToString("glob2"), utl.PointerToString("match2"), utl.PointerToString("replace2"))})
	mediumPriorityConfigurationLayerMock.SetSubstitutions(mpSubstitutions)
	hpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution3")}, &map[string]*ent.Substitution{"substitution3": ent.NewSubstitutionWith(utl.PointerToString("glob3"), utl.PointerToString("match3"), utl.PointerToString("replace3"))})
	highPriorityConfigurationLayerMock.SetSubstitutions(hpSubstitutions)

	lowPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(false))

	lowPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.low"))
	mediumPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.medium"))
	highPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.high"))

	lowPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.TRACE))
	mediumPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.INFO))
	highPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.DEBUG))

	lowPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("11.12.13"))
	mediumPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("21.22.23"))
	highPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("31.32.33"))

	// inject the layers and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	// serialize the overall configuration to the standard location
	serializedonfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx-configuration.json"))
	defer os.Remove(serializedonfigurationFile.Name())
	io.Save(serializedonfigurationFile.Name(), configuration)

	// deserialize the whole configuration to a simple layer to check values were resolved and serialized correctly
	deserializedConfigurationLayer := NewSimpleConfigurationLayer()
	err := io.LoadFromFile(serializedonfigurationFile.Name(), deserializedConfigurationLayer)
	assert.NoError(t, err)

	// now check for all values
	hpBump, _ := highPriorityConfigurationLayerMock.GetBump()
	bump, _ := deserializedConfigurationLayer.GetBump()
	assert.Equal(t, *hpBump, *bump)

	hpChangelog, _ := highPriorityConfigurationLayerMock.GetChangelog()
	changelog, _ := deserializedConfigurationLayer.GetChangelog()
	assert.Equal(t, *hpChangelog.GetAppend(), *changelog.GetAppend())
	assert.Equal(t, *hpChangelog.GetPath(), *changelog.GetPath())
	assert.Equal(t, (*hpChangelog.GetSections())["SectionC1"], (*changelog.GetSections())["SectionC1"])
	assert.Equal(t, (*hpChangelog.GetSections())["SectionC2"], (*changelog.GetSections())["SectionC2"])
	assert.Equal(t, (*hpChangelog.GetSubstitutions())["Expression3"], (*changelog.GetSubstitutions())["Expression3"])
	assert.Equal(t, *hpChangelog.GetTemplate(), *changelog.GetTemplate())

	commitMessageConventions, _ := deserializedConfigurationLayer.GetCommitMessageConventions()
	assert.Equal(t, *(*hpCommitMessageConventions.GetEnabled())[0], *(*commitMessageConventions.GetEnabled())[0])
	assert.Equal(t, *(*hpCommitMessageConventions.GetItems())["convention3"].GetExpression(), *(*commitMessageConventions.GetItems())["convention3"].GetExpression())

	hpConfigurationFile, _ := highPriorityConfigurationLayerMock.GetConfigurationFile()
	configurationFile, _ := deserializedConfigurationLayer.GetConfigurationFile()
	assert.Equal(t, *hpConfigurationFile, *configurationFile)

	hpDirectory, _ := highPriorityConfigurationLayerMock.GetDirectory()
	directory, _ := deserializedConfigurationLayer.GetDirectory()
	assert.Equal(t, *hpDirectory, *directory)

	hpDryRun, _ := highPriorityConfigurationLayerMock.GetDryRun()
	dryRun, _ := deserializedConfigurationLayer.GetDryRun()
	assert.Equal(t, *hpDryRun, *dryRun)

	lpGit, _ := lowPriorityConfigurationLayerMock.GetGit()
	mpGit, _ := mediumPriorityConfigurationLayerMock.GetGit()
	hpGit, _ := highPriorityConfigurationLayerMock.GetGit()
	git, _ := deserializedConfigurationLayer.GetGit()
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetAuthenticationMethod(), (*git.GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, *(*hpGit.GetRemotes())["origin"].GetPassword(), *(*git.GetRemotes())["origin"].GetPassword())
	assert.Equal(t, *(*hpGit.GetRemotes())["origin"].GetUser(), *(*git.GetRemotes())["origin"].GetUser())
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetPrivateKey(), (*git.GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetPassphrase(), (*git.GetRemotes())["origin"].GetPassphrase())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetAuthenticationMethod(), (*git.GetRemotes())["replica"].GetAuthenticationMethod())
	assert.Equal(t, *(*lpGit.GetRemotes())["replica"].GetPassword(), *(*git.GetRemotes())["replica"].GetPassword())
	assert.Equal(t, *(*lpGit.GetRemotes())["replica"].GetUser(), *(*git.GetRemotes())["replica"].GetUser())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetPrivateKey(), (*git.GetRemotes())["replica"].GetPrivateKey())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetPassphrase(), (*git.GetRemotes())["replica"].GetPassphrase())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetAuthenticationMethod(), (*git.GetRemotes())["clone"].GetAuthenticationMethod())
	assert.Equal(t, *(*mpGit.GetRemotes())["clone"].GetPassword(), *(*git.GetRemotes())["clone"].GetPassword())
	assert.Equal(t, *(*mpGit.GetRemotes())["clone"].GetUser(), *(*git.GetRemotes())["clone"].GetUser())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetPrivateKey(), (*git.GetRemotes())["clone"].GetPrivateKey())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetPassphrase(), (*git.GetRemotes())["clone"].GetPassphrase())

	hpInitialVersion, _ := highPriorityConfigurationLayerMock.GetInitialVersion()
	initialVersion, _ := deserializedConfigurationLayer.GetInitialVersion()
	assert.Equal(t, *hpInitialVersion, *initialVersion)

	hpPreset, _ := highPriorityConfigurationLayerMock.GetPreset()
	preset, _ := deserializedConfigurationLayer.GetPreset()
	assert.Equal(t, *hpPreset, *preset)

	hpReleaseAssets, _ := highPriorityConfigurationLayerMock.GetReleaseAssets()
	releaseAssets, _ := deserializedConfigurationLayer.GetReleaseAssets()
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetFileName(), *(*releaseAssets)["assetC1"].GetFileName())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetDescription(), *(*releaseAssets)["assetC1"].GetDescription())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetPath(), *(*releaseAssets)["assetC1"].GetPath())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetType(), *(*releaseAssets)["assetC1"].GetType())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetType(), *(*releaseAssets)["assetC2"].GetType())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetDescription(), *(*releaseAssets)["assetC2"].GetDescription())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetPath(), *(*releaseAssets)["assetC2"].GetPath())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetType(), *(*releaseAssets)["assetC2"].GetType())

	hpReleaseLenient, _ := highPriorityConfigurationLayerMock.GetReleaseLenient()
	releaseLenient, _ := deserializedConfigurationLayer.GetReleaseLenient()
	assert.Equal(t, *hpReleaseLenient, *releaseLenient)

	hpReleasePrefix, _ := highPriorityConfigurationLayerMock.GetReleasePrefix()
	releasePrefix, _ := deserializedConfigurationLayer.GetReleasePrefix()
	assert.Equal(t, *hpReleasePrefix, *releasePrefix)

	releaseTypes, _ := deserializedConfigurationLayer.GetReleaseTypes()
	assert.Equal(t, *(*(*hpReleaseTypes).GetEnabled())[0], *(*(*releaseTypes).GetEnabled())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetPublicationServices())[0], *(*(*releaseTypes).GetPublicationServices())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetRemoteRepositories())[0], *(*(*releaseTypes).GetRemoteRepositories())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetCollapseVersions(), *(*(*releaseTypes).GetItems())["type3"].GetCollapseVersions())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetCollapsedVersionQualifier(), *(*(*releaseTypes).GetItems())["type3"].GetCollapsedVersionQualifier())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetDescription(), *(*(*releaseTypes).GetItems())["type3"].GetDescription())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetFilterTags(), *(*(*releaseTypes).GetItems())["type3"].GetFilterTags())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitCommit(), *(*(*releaseTypes).GetItems())["type3"].GetGitCommit())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitCommitMessage(), *(*(*releaseTypes).GetItems())["type3"].GetGitCommitMessage())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitPush(), *(*(*releaseTypes).GetItems())["type3"].GetGitPush())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitPushForce(), *(*(*releaseTypes).GetItems())["type3"].GetGitPushForce())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTag(), *(*(*releaseTypes).GetItems())["type3"].GetGitTag())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagForce(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagForce())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagMessage(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagMessage())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagNames(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagNames())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetMatchBranches(), *(*(*releaseTypes).GetItems())["type3"].GetMatchBranches())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublish(), *(*(*releaseTypes).GetItems())["type3"].GetPublish())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublishDraft(), *(*(*releaseTypes).GetItems())["type3"].GetPublishDraft())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublishPreRelease(), *(*(*releaseTypes).GetItems())["type3"].GetPublishPreRelease())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetReleaseName(), *(*(*releaseTypes).GetItems())["type3"].GetReleaseName())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetVersionRange(), *(*(*releaseTypes).GetItems())["type3"].GetVersionRange())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetVersionRangeFromBranchName(), *(*(*releaseTypes).GetItems())["type3"].GetVersionRangeFromBranchName())

	hpResume, _ := highPriorityConfigurationLayerMock.GetResume()
	resume, _ := deserializedConfigurationLayer.GetResume()
	assert.Equal(t, *hpResume, *resume)

	hpScheme, _ := highPriorityConfigurationLayerMock.GetScheme()
	scheme, _ := deserializedConfigurationLayer.GetScheme()
	assert.Equal(t, *hpScheme, *scheme)

	hpServices, _ := highPriorityConfigurationLayerMock.GetServices()
	services, _ := deserializedConfigurationLayer.GetServices()
	assert.Equal(t, *(*hpServices)["github"].GetType(), *(*services)["github"].GetType())
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["AUTHENTICATION_TOKEN"], (*(*services)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["REPOSITORY_NAME"], (*(*services)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["REPOSITORY_OWNER"], (*(*services)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, *(*hpServices)["gitlab"].GetType(), *(*services)["gitlab"].GetType())
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"], (*(*services)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["REPOSITORY_NAME"], (*(*services)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["REPOSITORY_OWNER"], (*(*services)["gitlab"].GetOptions())["REPOSITORY_OWNER"])

	hpSharedConfigurationFile, _ := highPriorityConfigurationLayerMock.GetSharedConfigurationFile()
	sharedConfigurationFile, _ := deserializedConfigurationLayer.GetSharedConfigurationFile()
	assert.Equal(t, *hpSharedConfigurationFile, *sharedConfigurationFile)

	hpStateFile, _ := highPriorityConfigurationLayerMock.GetStateFile()
	stateFile, _ := deserializedConfigurationLayer.GetStateFile()
	assert.Equal(t, *hpStateFile, *stateFile)

	substitutions, _ := deserializedConfigurationLayer.GetSubstitutions()
	assert.Equal(t, *(*hpSubstitutions.GetEnabled())[0], *(*substitutions.GetEnabled())[0])
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetFiles(), *(*substitutions.GetItems())["substitution3"].GetFiles())
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetMatch(), *(*substitutions.GetItems())["substitution3"].GetMatch())
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetReplace(), *(*substitutions.GetItems())["substitution3"].GetReplace())

	hpSummary, _ := highPriorityConfigurationLayerMock.GetSummary()
	summary, _ := deserializedConfigurationLayer.GetSummary()
	assert.Equal(t, *hpSummary, *summary)

	hpSummaryFile, _ := highPriorityConfigurationLayerMock.GetSummaryFile()
	summaryFile, _ := deserializedConfigurationLayer.GetSummaryFile()
	assert.Equal(t, *hpSummaryFile, *summaryFile)

	hpVerbosity, _ := highPriorityConfigurationLayerMock.GetVerbosity()
	verbosity, _ := deserializedConfigurationLayer.GetVerbosity()
	assert.Equal(t, *hpVerbosity, *verbosity)

	hpVersion, _ := highPriorityConfigurationLayerMock.GetVersion()
	version, _ := deserializedConfigurationLayer.GetVersion()
	assert.Equal(t, *hpVersion, *version)
}

/*
Performs checks on the serialization of the Configuration object, rather than just layers
*/
func TestSerializationWithMultipleConfigurationLayersYAML(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	SetDefaultDirectory(&tempDir)

	lowPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	mediumPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()
	highPriorityConfigurationLayerMock := NewSimpleConfigurationLayer()

	configuration, _ := NewConfiguration()
	lowPriorityConfigurationLayerMock.SetBump(utl.PointerToString("alpha"))
	mediumPriorityConfigurationLayerMock.SetBump(utl.PointerToString("beta"))
	highPriorityConfigurationLayerMock.SetBump(utl.PointerToString("gamma"))

	lpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(nil, utl.PointerToString("CHANGELOG1.md"), &map[string]string{"SectionA1": "regexA1", "SectionA2": "regexA2"}, utl.PointerToString("changelog1.tpl"), &map[string]string{"Expression1": "string1"})
	lowPriorityConfigurationLayerMock.SetChangelog(lpChangelogConfiguration)
	mpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("head"), utl.PointerToString("CHANGELOG2.md"), &map[string]string{"SectionB1": "regexB1", "SectionB2": "regexB2"}, utl.PointerToString("changelog2.tpl"), &map[string]string{"Expression2": "string2"})
	mediumPriorityConfigurationLayerMock.SetChangelog(mpChangelogConfiguration)
	hpChangelogConfiguration, _ := ent.NewChangelogConfigurationWith(utl.PointerToString("tail"), utl.PointerToString("CHANGELOG3.md"), &map[string]string{"SectionC1": "regexC1", "SectionC2": "regexC2"}, utl.PointerToString("changelog3.tpl"), &map[string]string{"Expression3": "string3"})
	highPriorityConfigurationLayerMock.SetChangelog(hpChangelogConfiguration)

	lpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention1")}, &map[string]*ent.CommitMessageConvention{"convention1": ent.NewCommitMessageConventionWith(utl.PointerToString("expr1"), &map[string]string{})})
	lowPriorityConfigurationLayerMock.SetCommitMessageConventions(lpCommitMessageConventions)
	mpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention2")}, &map[string]*ent.CommitMessageConvention{"convention2": ent.NewCommitMessageConventionWith(utl.PointerToString("expr2"), &map[string]string{})})
	mediumPriorityConfigurationLayerMock.SetCommitMessageConventions(mpCommitMessageConventions)
	hpCommitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("convention3")}, &map[string]*ent.CommitMessageConvention{"convention3": ent.NewCommitMessageConventionWith(utl.PointerToString("expr3"), &map[string]string{})})
	highPriorityConfigurationLayerMock.SetCommitMessageConventions(hpCommitMessageConventions)

	lowPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	highPriorityConfigurationLayerMock.SetConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	lowPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("some/directory"))
	mediumPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("some/other/directory"))
	highPriorityConfigurationLayerMock.SetDirectory(utl.PointerToString("the/right/directory"))

	lowPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetDryRun(utl.PointerToBoolean(false))

	lpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("jdoe1"), utl.PointerToString("pwd1"), nil, nil), "replica": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("stiger1"), utl.PointerToString("sec1"), nil, nil)})
	lowPriorityConfigurationLayerMock.SetGit(lpGitConfiguration)
	mpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("jdoe2"), utl.PointerToString("pwd2"), nil, nil), "clone": ent.NewGitRemoteConfigurationWith(nil, utl.PointerToString("stiger2"), utl.PointerToString("sec2"), nil, nil)})
	mediumPriorityConfigurationLayerMock.SetGit(mpGitConfiguration)
	hpGitConfiguration, _ := ent.NewGitConfigurationWith(&map[string]*ent.GitRemoteConfiguration{"origin": ent.NewGitRemoteConfigurationWith(ent.PointerToAuthenticationMethod(ent.PUBLIC_KEY), utl.PointerToString("jdoe3"), utl.PointerToString("pwd3"), utl.PointerToString("key3"), utl.PointerToString("passphrase3"))})
	highPriorityConfigurationLayerMock.SetGit(hpGitConfiguration)

	lowPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("9.9.9"))
	mediumPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("8.8.8"))
	highPriorityConfigurationLayerMock.SetInitialVersion(utl.PointerToString("7.7.7"))

	lowPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(SIMPLE_NAME))
	mediumPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(SIMPLE_NAME))
	highPriorityConfigurationLayerMock.SetPreset(utl.PointerToString(EXTENDED_NAME))

	lowPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetA1": ent.NewAttachmentWith(utl.PointerToString("assetA1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetA1.txt"), utl.PointerToString("text/plain")), "assetA2": ent.NewAttachmentWith(utl.PointerToString("assetA2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetA2.bin"), utl.PointerToString("application/octet-stream"))})
	mediumPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetB1": ent.NewAttachmentWith(utl.PointerToString("assetB1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetB1.txt"), utl.PointerToString("text/plain")), "assetB2": ent.NewAttachmentWith(utl.PointerToString("assetB2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetB2.bin"), utl.PointerToString("application/octet-stream"))})
	highPriorityConfigurationLayerMock.SetReleaseAssets(&map[string]*ent.Attachment{"assetC1": ent.NewAttachmentWith(utl.PointerToString("assetC1.txt"), utl.PointerToString("Text Asset"), utl.PointerToString("assetC1.txt"), utl.PointerToString("text/plain")), "assetC2": ent.NewAttachmentWith(utl.PointerToString("assetC2.bin"), utl.PointerToString("Binary Asset"), utl.PointerToString("assetC2.bin"), utl.PointerToString("application/octet-stream"))})

	lowPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	mediumPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
	highPriorityConfigurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))

	lowPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("lpprefix"))
	mediumPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("mpprefix"))
	highPriorityConfigurationLayerMock.SetReleasePrefix(utl.PointerToString("hpprefix"))

	lpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type1")}, &[]*string{utl.PointerToString("service1")}, &[]*string{utl.PointerToString("remote1")}, &map[string]*ent.ReleaseType{"type1": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(false), utl.PointerToString("{{branch1}}"), utl.PointerToString("Release description 1"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease1"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	lowPriorityConfigurationLayerMock.SetReleaseTypes(lpReleaseTypes)
	mpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type2")}, &[]*string{utl.PointerToString("service2")}, &[]*string{utl.PointerToString("remote2")}, &map[string]*ent.ReleaseType{"type2": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{branch2}}"), utl.PointerToString("Release description 2"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease2"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	mediumPriorityConfigurationLayerMock.SetReleaseTypes(mpReleaseTypes)
	hpReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("type3")}, &[]*string{utl.PointerToString("service3")}, &[]*string{utl.PointerToString("remote3")}, &map[string]*ent.ReleaseType{"type3": ent.NewReleaseTypeWith(&[]*string{utl.PointerToString("asset1"), utl.PointerToString("asset2")}, utl.PointerToBoolean(true), utl.PointerToString("{{branch3}}"), utl.PointerToString("Release description 3"), utl.PointerToString("^({{configuration.releasePrefix}})?([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)$"), utl.PointerToString("true"), utl.PointerToString("Committing {{version}}"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("true"), utl.PointerToString("Tagging {{version}}"), &[]*string{utl.PointerToString("one"), utl.PointerToString("two"), utl.PointerToString("three")}, &[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("build"), utl.PointerToString("12"), ent.PointerToPosition(ent.BUILD))}, utl.PointerToString(""), &map[string]string{"PATH": ".*"}, nil, utl.PointerToString("true"), utl.PointerToString("false"), utl.PointerToString("true"), utl.PointerToString("myrelease3"), utl.PointerToString(""), utl.PointerToBoolean(false))})
	highPriorityConfigurationLayerMock.SetReleaseTypes(hpReleaseTypes)

	lowPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetResume(utl.PointerToBoolean(false))

	lowPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))
	mediumPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))
	highPriorityConfigurationLayerMock.SetScheme(ver.PointerToScheme(ver.SEMVER))

	lowPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken1", "REPOSITORY_NAME": "ignoredrepo1", "REPOSITORY_OWNER": "ignoredowner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "ignoredtoken2", "REPOSITORY_NAME": "ignoredrepo2", "REPOSITORY_OWNER": "ignoredowner2"})})
	mediumPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo1", "REPOSITORY_OWNER": "owner1"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo2", "REPOSITORY_OWNER": "owner2"})})
	highPriorityConfigurationLayerMock.SetServices(&map[string]*ent.ServiceConfiguration{"github": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITHUB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo3", "REPOSITORY_OWNER": "owner3"}), "gitlab": ent.NewServiceConfigurationWith(ent.PointerToProvider(ent.GITLAB), &map[string]string{"AUTHENTICATION_TOKEN": "{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}", "REPOSITORY_NAME": "repo4", "REPOSITORY_OWNER": "owner4"})})

	lowPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	mediumPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_YAML_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))
	highPriorityConfigurationLayerMock.SetSharedConfigurationFile(utl.PointerToString(os.Getenv(SIMPLEST_JSON_EXAMPLE_CONFIGURATION_FILE_ENVIRONMENT_VARIABLE)))

	lowPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.json"))
	mediumPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.json"))
	highPriorityConfigurationLayerMock.SetStateFile(utl.PointerToString("file.yaml"))

	lpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution1")}, &map[string]*ent.Substitution{"substitution1": ent.NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))})
	lowPriorityConfigurationLayerMock.SetSubstitutions(lpSubstitutions)
	mpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution2")}, &map[string]*ent.Substitution{"substitution2": ent.NewSubstitutionWith(utl.PointerToString("glob2"), utl.PointerToString("match2"), utl.PointerToString("replace2"))})
	mediumPriorityConfigurationLayerMock.SetSubstitutions(mpSubstitutions)
	hpSubstitutions, _ := ent.NewSubstitutionsWith(&[]*string{utl.PointerToString("substitution3")}, &map[string]*ent.Substitution{"substitution3": ent.NewSubstitutionWith(utl.PointerToString("glob3"), utl.PointerToString("match3"), utl.PointerToString("replace3"))})
	highPriorityConfigurationLayerMock.SetSubstitutions(hpSubstitutions)

	lowPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(true))
	mediumPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(true))
	highPriorityConfigurationLayerMock.SetSummary(utl.PointerToBoolean(false))

	lowPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.low"))
	mediumPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.medium"))
	highPriorityConfigurationLayerMock.SetSummaryFile(utl.PointerToString("summary.high"))

	lowPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.TRACE))
	mediumPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.INFO))
	highPriorityConfigurationLayerMock.SetVerbosity(ent.PointerToVerbosity(ent.DEBUG))

	lowPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("11.12.13"))
	mediumPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("21.22.23"))
	highPriorityConfigurationLayerMock.SetVersion(utl.PointerToString("31.32.33"))

	// inject the layers and test the new value is returned from that
	var lpl ConfigurationLayer = lowPriorityConfigurationLayerMock
	var mpl ConfigurationLayer = mediumPriorityConfigurationLayerMock
	var hpl ConfigurationLayer = highPriorityConfigurationLayerMock
	configuration.WithPluginConfiguration(&lpl)
	configuration.WithCommandLineConfiguration(&mpl)
	configuration.WithRuntimeConfiguration(&hpl)

	// serialize the overall configuration to the standard location
	serializedonfigurationFile, _ := os.Create(filepath.Join(tempDir, ".nyx-configuration.yaml"))
	defer os.Remove(serializedonfigurationFile.Name())
	io.Save(serializedonfigurationFile.Name(), configuration)

	// deserialize the whole configuration to a simple layer to check values were resolved and serialized correctly
	deserializedConfigurationLayer := NewSimpleConfigurationLayer()
	err := io.LoadFromFile(serializedonfigurationFile.Name(), deserializedConfigurationLayer)
	assert.NoError(t, err)

	// now check for all values
	hpBump, _ := highPriorityConfigurationLayerMock.GetBump()
	bump, _ := deserializedConfigurationLayer.GetBump()
	assert.Equal(t, *hpBump, *bump)

	hpChangelog, _ := highPriorityConfigurationLayerMock.GetChangelog()
	changelog, _ := deserializedConfigurationLayer.GetChangelog()
	assert.Equal(t, *hpChangelog.GetAppend(), *changelog.GetAppend())
	assert.Equal(t, *hpChangelog.GetPath(), *changelog.GetPath())
	assert.Equal(t, (*hpChangelog.GetSections())["SectionC1"], (*changelog.GetSections())["SectionC1"])
	assert.Equal(t, (*hpChangelog.GetSections())["SectionC2"], (*changelog.GetSections())["SectionC2"])
	assert.Equal(t, (*hpChangelog.GetSubstitutions())["Expression3"], (*changelog.GetSubstitutions())["Expression3"])
	assert.Equal(t, *hpChangelog.GetTemplate(), *changelog.GetTemplate())

	commitMessageConventions, _ := deserializedConfigurationLayer.GetCommitMessageConventions()
	assert.Equal(t, *(*hpCommitMessageConventions.GetEnabled())[0], *(*commitMessageConventions.GetEnabled())[0])
	assert.Equal(t, *(*hpCommitMessageConventions.GetItems())["convention3"].GetExpression(), *(*commitMessageConventions.GetItems())["convention3"].GetExpression())

	hpConfigurationFile, _ := highPriorityConfigurationLayerMock.GetConfigurationFile()
	configurationFile, _ := deserializedConfigurationLayer.GetConfigurationFile()
	assert.Equal(t, *hpConfigurationFile, *configurationFile)

	hpDirectory, _ := highPriorityConfigurationLayerMock.GetDirectory()
	directory, _ := deserializedConfigurationLayer.GetDirectory()
	assert.Equal(t, *hpDirectory, *directory)

	hpDryRun, _ := highPriorityConfigurationLayerMock.GetDryRun()
	dryRun, _ := deserializedConfigurationLayer.GetDryRun()
	assert.Equal(t, *hpDryRun, *dryRun)

	lpGit, _ := lowPriorityConfigurationLayerMock.GetGit()
	mpGit, _ := mediumPriorityConfigurationLayerMock.GetGit()
	hpGit, _ := highPriorityConfigurationLayerMock.GetGit()
	git, _ := deserializedConfigurationLayer.GetGit()
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetAuthenticationMethod(), (*git.GetRemotes())["origin"].GetAuthenticationMethod())
	assert.Equal(t, *(*hpGit.GetRemotes())["origin"].GetPassword(), *(*git.GetRemotes())["origin"].GetPassword())
	assert.Equal(t, *(*hpGit.GetRemotes())["origin"].GetUser(), *(*git.GetRemotes())["origin"].GetUser())
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetPrivateKey(), (*git.GetRemotes())["origin"].GetPrivateKey())
	assert.Equal(t, (*hpGit.GetRemotes())["origin"].GetPassphrase(), (*git.GetRemotes())["origin"].GetPassphrase())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetAuthenticationMethod(), (*git.GetRemotes())["replica"].GetAuthenticationMethod())
	assert.Equal(t, *(*lpGit.GetRemotes())["replica"].GetPassword(), *(*git.GetRemotes())["replica"].GetPassword())
	assert.Equal(t, *(*lpGit.GetRemotes())["replica"].GetUser(), *(*git.GetRemotes())["replica"].GetUser())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetPrivateKey(), (*git.GetRemotes())["replica"].GetPrivateKey())
	assert.Equal(t, (*lpGit.GetRemotes())["replica"].GetPassphrase(), (*git.GetRemotes())["replica"].GetPassphrase())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetAuthenticationMethod(), (*git.GetRemotes())["clone"].GetAuthenticationMethod())
	assert.Equal(t, *(*mpGit.GetRemotes())["clone"].GetPassword(), *(*git.GetRemotes())["clone"].GetPassword())
	assert.Equal(t, *(*mpGit.GetRemotes())["clone"].GetUser(), *(*git.GetRemotes())["clone"].GetUser())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetPrivateKey(), (*git.GetRemotes())["clone"].GetPrivateKey())
	assert.Equal(t, (*mpGit.GetRemotes())["clone"].GetPassphrase(), (*git.GetRemotes())["clone"].GetPassphrase())

	hpInitialVersion, _ := highPriorityConfigurationLayerMock.GetInitialVersion()
	initialVersion, _ := deserializedConfigurationLayer.GetInitialVersion()
	assert.Equal(t, *hpInitialVersion, *initialVersion)

	hpPreset, _ := highPriorityConfigurationLayerMock.GetPreset()
	preset, _ := deserializedConfigurationLayer.GetPreset()
	assert.Equal(t, *hpPreset, *preset)

	hpReleaseAssets, _ := highPriorityConfigurationLayerMock.GetReleaseAssets()
	releaseAssets, _ := deserializedConfigurationLayer.GetReleaseAssets()
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetFileName(), *(*releaseAssets)["assetC1"].GetFileName())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetDescription(), *(*releaseAssets)["assetC1"].GetDescription())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetPath(), *(*releaseAssets)["assetC1"].GetPath())
	assert.Equal(t, *(*hpReleaseAssets)["assetC1"].GetType(), *(*releaseAssets)["assetC1"].GetType())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetType(), *(*releaseAssets)["assetC2"].GetType())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetDescription(), *(*releaseAssets)["assetC2"].GetDescription())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetPath(), *(*releaseAssets)["assetC2"].GetPath())
	assert.Equal(t, *(*hpReleaseAssets)["assetC2"].GetType(), *(*releaseAssets)["assetC2"].GetType())

	hpReleaseLenient, _ := highPriorityConfigurationLayerMock.GetReleaseLenient()
	releaseLenient, _ := deserializedConfigurationLayer.GetReleaseLenient()
	assert.Equal(t, *hpReleaseLenient, *releaseLenient)

	hpReleasePrefix, _ := highPriorityConfigurationLayerMock.GetReleasePrefix()
	releasePrefix, _ := deserializedConfigurationLayer.GetReleasePrefix()
	assert.Equal(t, *hpReleasePrefix, *releasePrefix)

	releaseTypes, _ := deserializedConfigurationLayer.GetReleaseTypes()
	assert.Equal(t, *(*(*hpReleaseTypes).GetEnabled())[0], *(*(*releaseTypes).GetEnabled())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetPublicationServices())[0], *(*(*releaseTypes).GetPublicationServices())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetRemoteRepositories())[0], *(*(*releaseTypes).GetRemoteRepositories())[0])
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetCollapseVersions(), *(*(*releaseTypes).GetItems())["type3"].GetCollapseVersions())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetCollapsedVersionQualifier(), *(*(*releaseTypes).GetItems())["type3"].GetCollapsedVersionQualifier())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetDescription(), *(*(*releaseTypes).GetItems())["type3"].GetDescription())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetFilterTags(), *(*(*releaseTypes).GetItems())["type3"].GetFilterTags())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitCommit(), *(*(*releaseTypes).GetItems())["type3"].GetGitCommit())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitCommitMessage(), *(*(*releaseTypes).GetItems())["type3"].GetGitCommitMessage())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitPush(), *(*(*releaseTypes).GetItems())["type3"].GetGitPush())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitPushForce(), *(*(*releaseTypes).GetItems())["type3"].GetGitPushForce())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTag(), *(*(*releaseTypes).GetItems())["type3"].GetGitTag())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagForce(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagForce())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagMessage(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagMessage())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetGitTagNames(), *(*(*releaseTypes).GetItems())["type3"].GetGitTagNames())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetMatchBranches(), *(*(*releaseTypes).GetItems())["type3"].GetMatchBranches())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublish(), *(*(*releaseTypes).GetItems())["type3"].GetPublish())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublishDraft(), *(*(*releaseTypes).GetItems())["type3"].GetPublishDraft())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetPublishPreRelease(), *(*(*releaseTypes).GetItems())["type3"].GetPublishPreRelease())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetReleaseName(), *(*(*releaseTypes).GetItems())["type3"].GetReleaseName())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetVersionRange(), *(*(*releaseTypes).GetItems())["type3"].GetVersionRange())
	assert.Equal(t, *(*(*hpReleaseTypes).GetItems())["type3"].GetVersionRangeFromBranchName(), *(*(*releaseTypes).GetItems())["type3"].GetVersionRangeFromBranchName())

	hpResume, _ := highPriorityConfigurationLayerMock.GetResume()
	resume, _ := deserializedConfigurationLayer.GetResume()
	assert.Equal(t, *hpResume, *resume)

	hpScheme, _ := highPriorityConfigurationLayerMock.GetScheme()
	scheme, _ := deserializedConfigurationLayer.GetScheme()
	assert.Equal(t, *hpScheme, *scheme)

	hpServices, _ := highPriorityConfigurationLayerMock.GetServices()
	services, _ := deserializedConfigurationLayer.GetServices()
	assert.Equal(t, *(*hpServices)["github"].GetType(), *(*services)["github"].GetType())
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["AUTHENTICATION_TOKEN"], (*(*services)["github"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["REPOSITORY_NAME"], (*(*services)["github"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, (*(*hpServices)["github"].GetOptions())["REPOSITORY_OWNER"], (*(*services)["github"].GetOptions())["REPOSITORY_OWNER"])
	assert.Equal(t, *(*hpServices)["gitlab"].GetType(), *(*services)["gitlab"].GetType())
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"], (*(*services)["gitlab"].GetOptions())["AUTHENTICATION_TOKEN"])
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["REPOSITORY_NAME"], (*(*services)["gitlab"].GetOptions())["REPOSITORY_NAME"])
	assert.Equal(t, (*(*hpServices)["gitlab"].GetOptions())["REPOSITORY_OWNER"], (*(*services)["gitlab"].GetOptions())["REPOSITORY_OWNER"])

	hpSharedConfigurationFile, _ := highPriorityConfigurationLayerMock.GetSharedConfigurationFile()
	sharedConfigurationFile, _ := deserializedConfigurationLayer.GetSharedConfigurationFile()
	assert.Equal(t, *hpSharedConfigurationFile, *sharedConfigurationFile)

	hpStateFile, _ := highPriorityConfigurationLayerMock.GetStateFile()
	stateFile, _ := deserializedConfigurationLayer.GetStateFile()
	assert.Equal(t, *hpStateFile, *stateFile)

	substitutions, _ := deserializedConfigurationLayer.GetSubstitutions()
	assert.Equal(t, *(*hpSubstitutions.GetEnabled())[0], *(*substitutions.GetEnabled())[0])
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetFiles(), *(*substitutions.GetItems())["substitution3"].GetFiles())
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetMatch(), *(*substitutions.GetItems())["substitution3"].GetMatch())
	assert.Equal(t, *(*hpSubstitutions.GetItems())["substitution3"].GetReplace(), *(*substitutions.GetItems())["substitution3"].GetReplace())

	hpSummary, _ := highPriorityConfigurationLayerMock.GetSummary()
	summary, _ := deserializedConfigurationLayer.GetSummary()
	assert.Equal(t, *hpSummary, *summary)

	hpSummaryFile, _ := highPriorityConfigurationLayerMock.GetSummaryFile()
	summaryFile, _ := deserializedConfigurationLayer.GetSummaryFile()
	assert.Equal(t, *hpSummaryFile, *summaryFile)

	hpVerbosity, _ := highPriorityConfigurationLayerMock.GetVerbosity()
	verbosity, _ := deserializedConfigurationLayer.GetVerbosity()
	assert.Equal(t, *hpVerbosity, *verbosity)

	hpVersion, _ := highPriorityConfigurationLayerMock.GetVersion()
	version, _ := deserializedConfigurationLayer.GetVersion()
	assert.Equal(t, *hpVersion, *version)
}
