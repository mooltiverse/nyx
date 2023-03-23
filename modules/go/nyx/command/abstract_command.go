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

package command

import (
	"fmt"     // https://pkg.go.dev/fmt
	"os"      // https://pkg.go.dev/os
	"strings" // https://pkg.go.dev/strings

	regexp2 "github.com/dlclark/regexp2" // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"     // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	git "github.com/mooltiverse/nyx/modules/go/nyx/git"
	svc "github.com/mooltiverse/nyx/modules/go/nyx/services"
	svcapi "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	tpl "github.com/mooltiverse/nyx/modules/go/nyx/template"
)

/*
The common superclass for Nyx commands.

This class is not meant to be used in multi-threaded environments.

All implementing classes must set the State and the Repository members.
*/
type abstractCommand struct {
	// The private instance of the Git repository.
	repository *git.Repository

	// The private instance of the state.
	state *stt.State
}

/*
Returns the state object.
*/
func (ac *abstractCommand) State() *stt.State {
	return ac.state
}

/*
Returns the repository object.
*/
func (ac *abstractCommand) Repository() *git.Repository {
	return ac.repository
}

/*
Returns the name of the current branch or a commit SHA-1 if the repository is in the detached head state.

Error is:
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (ac *abstractCommand) getCurrentBranch() (string, error) {
	return (*ac.repository).GetCurrentBranch()
}

/*
Returns the SHA-1 identifier of the last commit in the current branch.

Error is:
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (ac *abstractCommand) getLatestCommit() (string, error) {
	return (*ac.repository).GetLatestCommit()
}

/*
Returns true if the repository is in a clean state (no uncommitted changes).

Error is:
- GitError in case of unexpected issues when accessing the Git repository.
*/
func (ac *abstractCommand) isRepositoryClean() (bool, error) {
	return (*ac.repository).IsClean()
}

/*
Returns true if the internal attributes map contains an attribute with the given name and its value
equals the given expected value.

Arguments are as follows:

- attributeName the name of the attribute to check
- expectedValue the expected value of the attribute. It may be nil

Error is only returned under internal unexpected conditions.
*/
func (ac *abstractCommand) isInternalAttributeUpToDate(attributeName string, expectedValue *string) (bool, error) {
	internals, err := ac.state.GetInternals()
	if err != nil {
		return false, err
	}
	val, ok := (*internals)[attributeName]
	if ok {
		if expectedValue == nil {
			return val == "null", nil
		} else {
			return val == *expectedValue, nil
		}
	} else {
		return false, nil
	}
}

/*
Retrieves the attribute with the given name from the internal attributes map, if available, otherwise nil

Arguments are as follows:

- attributeName the name of the attribute to get

Error is only returned under internal unexpected conditions.
*/
func (ac *abstractCommand) getInternalAttribute(attributeName string) (*string, error) {
	internals, err := ac.state.GetInternals()
	if err != nil {
		return nil, err
	}
	if val, ok := (*internals)[attributeName]; ok {
		return &val, nil
	} else {
		if val == "null" {
			return nil, nil
		} else {
			return nil, nil
		}
	}
}

/*
Stores the attribute with the given name to the internal attributes map.

Arguments are as follows:

- attributeName the name of the attribute to store
- attributeValue the value of the attribute. It may be nil

Error is only returned under internal unexpected conditions.
*/
func (ac *abstractCommand) putInternalAttribute(attributeName string, attributeValue *string) error {
	internals, err := ac.state.GetInternals()
	if err != nil {
		return err
	}
	if attributeValue == nil {
		(*internals)[attributeName] = "null"
	} else {
		(*internals)[attributeName] = *attributeValue
	}
	return nil
}

/*
Renders the given template using the internal State object as the context.

Arguments are as follows:

- template the string template to render.

Error is:
- IllegalPropertyError in case the given template can't be rendered.
*/
func (ac *abstractCommand) renderTemplate(template *string) (*string, error) {
	if template == nil {
		return nil, nil
	}
	if "" == strings.TrimSpace(*template) {
		return template, nil
	} else {
		flatState, err := ac.state.Flatten()
		if err != nil {
			return nil, &errs.IllegalStateError{Message: fmt.Sprintf("the internal state cannot be flattened for rendering"), Cause: err}
		}
		res, err := tpl.Render(*template, flatState)
		if err != nil {
			return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("template '%s' cannot be rendered using the current state", *template), Cause: err}
		}
		return &res, nil
	}
}

/*
Renders the given template using the internal State object as the context, returning
the boolean value, according to templates.ToBoolean.

Arguments are as follows:

- template the string template to render.

Error is:
- IllegalPropertyError in case the given template can't be rendered.
*/
func (ac *abstractCommand) renderTemplateAsBoolean(template *string) (bool, error) {
	res, err := ac.renderTemplate(template)
	if err != nil {
		return false, err
	} else {
		return tpl.ToBoolean(res), nil
	}
}

/*
Renders the given template using the internal State object as the context, returning
the integer value, according to templates.ToInteger.

Arguments are as follows:

- template the string template to render.

Error is:
- IllegalPropertyError in case the given template can't be rendered.
*/
func (ac *abstractCommand) renderTemplateAsInteger(template *string) (int64, error) {
	res, err := ac.renderTemplate(template)
	if err != nil {
		return 0, err
	} else {
		return tpl.ToInteger(res), nil
	}
}

/*
Resolves the given options by rendering each value of the given map as a template. Keys are left unchanged.

Arguments are as follows:

- options the options to resolve.

Error is:
- IllegalPropertyError in case the given values can't be rendered as templates.
*/
func (ac *abstractCommand) resolveServiceOptions(options map[string]string) (map[string]string, error) {
	if options == nil {
		return nil, nil
	}

	resolvedOptions := make(map[string]string, len(options))
	log.Debugf("resolving templates for '%d'", len(options))
	for optionKey, optionValue := range options {
		renderedValue, err := ac.renderTemplate(&optionValue)
		if err != nil {
			return resolvedOptions, err
		}
		resolvedOptions[optionKey] = *renderedValue
	}

	return resolvedOptions, nil
}

/*
Returns the ReleaseService with the given configuration name and also resolves its configuration option templates.

Arguments are as follows:

- name the name of the service configuration.

Error is:
  - DataAccessError in case the configuration can't be loaded for some reason.
  - IllegalPropertyError in case the configuration has some illegal options.
  - ReleaseError if the task is unable to complete for reasons due to the release process.
  - UnsupportedOperationError if the service configuration exists but the service class does not
    support the RELEASES feature.
*/
func (ac *abstractCommand) resolveReleaseService(name string) (*svcapi.ReleaseService, error) {
	services, err := ac.state.GetConfiguration().GetServices()
	if err != nil {
		return nil, err
	}
	if services == nil {
		log.Debugf("no services have been configured. Please configure them using the services option.")
		return nil, nil
	}

	log.Debugf("resolving the service configuration among available ones: '%v'", *services)
	if serviceConfiguration, ok := (*services)[name]; ok {
		log.Debugf("instantiating service '%s' of type '%s' with '%d' options", name, serviceConfiguration.GetType().String(), len(*serviceConfiguration.GetOptions()))
		resolvedOptions, err := ac.resolveServiceOptions(*serviceConfiguration.GetOptions())
		if err != nil {
			return nil, err
		}
		serviceInstance, err := svc.ReleaseServiceInstance(*serviceConfiguration.GetType(), resolvedOptions)
		if err != nil {
			return nil, err
		}
		return &serviceInstance, nil
	} else {
		log.Debugf("No service with name '%s' has been configured", name)
		return nil, nil
	}
}

/*
Selects the right release type among those configured based on their matching attributes.

Error is:
  - DataAccessError in case the configuration can't be loaded for some reason.
  - IllegalPropertyError in case the configuration has some illegal options.
  - GitError in case of unexpected issues when accessing the Git repository.
  - ReleaseError if the task is unable to complete for reasons due to the release process.
*/
func (ac *abstractCommand) resolveReleaseType() (*ent.ReleaseType, error) {
	releaseTypes, err := ac.state.GetConfiguration().GetReleaseTypes()
	if err != nil {
		return nil, err
	}
	if releaseTypes == nil || releaseTypes.GetEnabled() == nil || len(*releaseTypes.GetEnabled()) == 0 {
		return nil, &errs.ReleaseError{Message: fmt.Sprintf("no release types have been configured. Please configure them using the releaseTypes option.")}
	}

	log.Debugf("resolving the release type among enabled ones")
	for _, releaseTypeName := range *releaseTypes.GetEnabled() {
		log.Debugf("evaluating release type: '%s'", *releaseTypeName)
		releaseType, ok := (*releaseTypes.GetItems())[*releaseTypeName]
		if !ok {
			return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("release type '%s' is configured among enabled ones but is not configured", *releaseTypeName)}
		}

		// evaluate the matching criteria: branch name
		if releaseType.GetMatchBranches() == nil || "" == strings.TrimSpace(*releaseType.GetMatchBranches()) {
			log.Debugf("release type '%s' does not specify any branch name requirement", *releaseTypeName)
		} else {
			matchBranchesRendered, err := ac.renderTemplate(releaseType.GetMatchBranches())
			if err != nil {
				return nil, err
			}
			if "" == strings.TrimSpace(*matchBranchesRendered) {
				log.Debugf("release type '%s' specifies a match branches template '%s' that evaluates to an empty regular expression", *releaseTypeName, *releaseType.GetMatchBranches())
			} else {
				log.Debugf("release type '%s' specifies a match branches template '%s' that evaluates to regular expression: '%s'", *releaseTypeName, *releaseType.GetMatchBranches(), *matchBranchesRendered)
				re, err := regexp2.Compile(*matchBranchesRendered, 0)
				if err != nil {
					return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("release type '%s' has a malformed matchBranches regular expression: '%s' (was '%s' before rendering the template rendering)", *releaseTypeName, *matchBranchesRendered, *releaseType.GetMatchBranches()), Cause: err}
				}
				currentBranch, err := ac.getCurrentBranch()
				if err != nil {
					return nil, err
				}
				match, err := re.MatchString(currentBranch)
				if match {
					log.Debugf("current branch '%s' successfully matched by release type '%s' matchBranches regular expression '%s'", currentBranch, *releaseTypeName, *matchBranchesRendered)
				} else {
					log.Debugf("current branch '%s' not matched by release type '%s' matchBranches regular expression '%s'. Skipping release type '%s'", currentBranch, *releaseTypeName, *matchBranchesRendered, *releaseTypeName)
					continue
				}
			}
		}

		// evaluate the matching criteria: environment variables
		if releaseType.GetMatchEnvironmentVariables() == nil || len(*releaseType.GetMatchEnvironmentVariables()) == 0 {
			log.Debugf("release type '%s'  does not specify any environment variable requirement", *releaseTypeName)
		} else {
			mismatch := false
			for varName, varVarueRegExp := range *releaseType.GetMatchEnvironmentVariables() {
				log.Debugf("evaluating environment variable '%s' as required by release type '%s'", varName, *releaseTypeName)

				varValue := os.Getenv(varName)

				if "" == strings.TrimSpace(varValue) {
					log.Debugf("environment variable '%s' is required by release type '%s' but is not defined in the current environment. Skipping release type '%s'", varName, *releaseTypeName, *releaseTypeName)
					mismatch = true
					continue
				}

				if "" == strings.TrimSpace(varVarueRegExp) {
					log.Debugf("environment variable '%s' value successfully matched by release type '%s' regular expression '%s'", varName, *releaseTypeName, varVarueRegExp)
				} else {
					re, err := regexp2.Compile(varVarueRegExp, 0)
					if err != nil {
						return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("release type '%s' has a malformed environment variable regular expression '%s' to match for environment variable '%s'", *releaseTypeName, varVarueRegExp, varName), Cause: err}
					}
					match, err := re.MatchString(varValue)
					if match {
						log.Debugf("environment variable '%s' value successfully matched by release type '%s' regular expression '%s'", varName, *releaseTypeName, varVarueRegExp)
					} else {
						log.Debugf("environment variable '%s' value not matched by release type '%s' regular expression '%s'", varName, *releaseTypeName, varVarueRegExp)
						mismatch = true
						continue
					}
				}
			}

			if mismatch {
				log.Debugf("environment variables not matched by release type '%s'", *releaseTypeName)
				continue
			}
		}

		// evaluate the matching criteria: workspace status
		if releaseType.GetMatchWorkspaceStatus() == nil {
			log.Debugf("release type '%s' does not specify any workspace status requirement", *releaseTypeName)
		} else {
			repoClean, err := ac.isRepositoryClean()
			if err != nil {
				return nil, err
			}
			repoCleanString := ""
			if repoClean {
				repoCleanString = "CLEAN"
			} else {
				repoCleanString = "DIRTY"
			}

			if (*releaseType.GetMatchWorkspaceStatus() == ent.CLEAN && repoClean) || (*releaseType.GetMatchWorkspaceStatus() == ent.DIRTY && !repoClean) {
				log.Debugf("current repository status '%s' successfully matched by release type '%s' matchWorkspaceStatus filter '%s'", repoCleanString, *releaseTypeName, (*releaseType.GetMatchWorkspaceStatus()).String())
			} else {
				log.Debugf("current repository status '%s' not matched by release type '%s' matchWorkspaceStatus filter '%s'. Skipping release type '%s'", repoCleanString, *releaseTypeName, (*releaseType.GetMatchWorkspaceStatus()).String(), *releaseTypeName)
				continue
			}
		}

		// if we reached this point the release type matches all of the filters so it can be returned
		log.Debugf("release type '%s' has been selected", *releaseTypeName)
		return releaseType, nil
	}
	return nil, &errs.IllegalPropertyError{Message: "no suitable release types have been configured or none of the configured release types matches the current environment"}
}
