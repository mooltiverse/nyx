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

package entities

/*
A simple configuration layer, acting as a value holder for configuration options. This object allows read/write operations.

This structure is JSON and YAML aware so all objects are properly managed for marshalling and unmarshalling. This comes with a downside
as all internal fields must be exported (have the first capital letter in their names) or they can't be marshalled.
*/
type ServiceConfiguration struct {
	// The map of service configuration options.
	Options *map[string]string `json:"options,omitempty" yaml:"options,omitempty"`

	// The type of service
	ServiceType *Provider `json:"type,omitempty" yaml:"type,omitempty"`
}

/*
Default constructor.
*/
func NewServiceConfiguration() *ServiceConfiguration {
	return &ServiceConfiguration{}
}

/*
Standard constructor.

Arguments are as follows:

- serviceType the type of service.
- options the map of service configuration options.
*/
func NewServiceConfigurationWith(serviceType *Provider, options *map[string]string) *ServiceConfiguration {
	sc := ServiceConfiguration{}

	sc.Options = options
	sc.ServiceType = serviceType

	return &sc
}

/*
Returns the map of service configuration options. A nil value means undefined.
*/
func (sc *ServiceConfiguration) GetOptions() *map[string]string {
	return sc.Options
}

/*
Sets the map of service configuration options. A nil value means undefined.
*/
func (sc *ServiceConfiguration) SetOptions(options *map[string]string) {
	sc.Options = options
}

/*
Returns the service type. A nil value means undefined.
*/
func (sc *ServiceConfiguration) GetType() *Provider {
	return sc.ServiceType
}

/*
Sets the service type. A nil value means undefined.
*/
func (sc *ServiceConfiguration) SetType(serviceType *Provider) {
	sc.ServiceType = serviceType
}
