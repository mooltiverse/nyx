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

package services

import (
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/modules/go/errors"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	api "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gitlab "github.com/mooltiverse/nyx/modules/go/nyx/services/gitlab"
)

/*
The generic entry point to inspect and retrieve service implementations.
*/
type ServiceFactory struct {
}

/*
Returns an instance for the given provider using the given options.

Arguments are as follows:

  - provider the provider to retrieve the instance for.
  - options the map of options for the requested service. It may be nil if the requested
    service does not require the options map. To know if the service needs rhese options and, if so, which
    entries are to be present please check with the specific service.

Errors can be:

  - NilPointerError if the given provider is nil or the given options map is nil
    and the service instance does not allow nil options
  - IllegalArgumentError if the given provider is not supported or some entries in the given options
    map are illegal for some reason
  - UnsupportedOperationError if the service provider does not support the GIT_HOSTING feature.
*/
func GitHostingServiceInstance(provider ent.Provider, options map[string]string) (api.GitHostingService, error) {
	instance, err := Instance(provider, options)
	if err != nil {
		return nil, err
	}
	if instance.Supports(api.GIT_HOSTING) {
		service, castOK := instance.(api.GitHostingService)
		if castOK {
			return service, nil
		} else {
			return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider supports the %s feature but instances do not implement the %s interface", provider, api.GIT_HOSTING, "GitHostingService")}
		}
	} else {
		return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider does not support the %s feature", provider, api.GIT_HOSTING)}
	}
}

/*
Returns an instance for the given provider using the given options.

Arguments are as follows:

  - provider the provider to retrieve the instance for.
  - options the map of options for the requested service. It may be nil if the requested
    service does not require the options map. To know if the service needs rhese options and, if so, which
    entries are to be present please check with the specific service.

Errors can be:

  - NilPointerError if the given provider is nil or the given options map is nil
    and the service instance does not allow nil options
  - IllegalArgumentError if the given provider is not supported or some entries in the given options
    map are illegal for some reason
*/
func Instance(provider ent.Provider, options map[string]string) (api.Service, error) {
	switch provider {
	case ent.GITHUB:
		return github.Instance(options)
	case ent.GITLAB:
		return gitlab.Instance(options)
	default:
		// this is never reached, but in case...
		panic("unknown Provider. This means the switch/case statement needs to be updated")
	}
}

/*
Returns an instance for the given provider using the given options.

Arguments are as follows:

  - provider the provider to retrieve the instance for.
  - options the map of options for the requested service. It may be nil if the requested
    service does not require the options map. To know if the service needs rhese options and, if so, which
    entries are to be present please check with the specific service.

Errors can be:

  - NilPointerError if the given provider is nil or the given options map is nil
    and the service instance does not allow nil options
  - IllegalArgumentError if the given provider is not supported or some entries in the given options
    map are illegal for some reason
  - UnsupportedOperationError if the service provider does not support the RELEASES feature.
*/
func ReleaseServiceInstance(provider ent.Provider, options map[string]string) (api.ReleaseService, error) {
	instance, err := Instance(provider, options)
	if err != nil {
		return nil, err
	}
	if instance.Supports(api.RELEASES) {
		service, castOK := instance.(api.ReleaseService)
		if castOK {
			return service, nil
		} else {
			return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider supports the %s feature but instances do not implement the %s interface", provider, api.RELEASES, "ReleaseService")}
		}
	} else {
		return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider does not support the %s feature", provider, api.RELEASES)}
	}
}

/*
Returns an instance for the given provider using the given options.

Arguments are as follows:

  - provider the provider to retrieve the instance for.
  - options the map of options for the requested service. It may be nil if the requested
    service does not require the options map. To know if the service needs rhese options and, if so, which
    entries are to be present please check with the specific service.

Errors can be:

  - NilPointerError if the given provider is nil or the given options map is nil
    and the service instance does not allow nil options
  - IllegalArgumentError if the given provider is not supported or some entries in the given options
    map are illegal for some reason
  - UnsupportedOperationError if the service provider does not support the USERS feature.
*/
func UserServiceInstance(provider ent.Provider, options map[string]string) (api.UserService, error) {
	instance, err := Instance(provider, options)
	if err != nil {
		return nil, err
	}
	if instance.Supports(api.USERS) {
		service, castOK := instance.(api.UserService)
		if castOK {
			return service, nil
		} else {
			return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider supports the %s feature but instances do not implement the %s interface", provider, api.USERS, "UserService")}
		}
	} else {
		return nil, &errs.UnsupportedOperationError{Message: fmt.Sprintf("the %s provider does not support the %s feature", provider, api.USERS)}
	}
}
