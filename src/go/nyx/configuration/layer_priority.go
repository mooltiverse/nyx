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

/*
This enumeration models configuration layer levels and their priorities. The order used to define
each item matters and is represented by the ordinal.
*/
type layerPriority byte

const (
	// The layer that can be used at runtime to override all other layers
	RUNTIME layerPriority = 0

	// The layer that reads values from command line options
	COMMAND_LINE layerPriority = 1

	// The layer that reads values from environment variables
	ENVIRONMENT layerPriority = 2

	// The layer that models a configuration from a script or plugin
	PLUGIN layerPriority = 3

	// The layer that models a local configuration file from a custom location
	CUSTOM_LOCAL_FILE layerPriority = 4

	// The layer that models a local configuration file from a standard location
	STANDARD_LOCAL_FILE layerPriority = 5

	// The layer that models a shared configuration file from a custom location
	CUSTOM_SHARED_FILE layerPriority = 6

	// The layer that models a shared configuration file from a standard location
	STANDARD_SHARED_FILE layerPriority = 7

	// The layer that models the optional preset configuration.
	PRESET layerPriority = 8

	// The layer that models default options.
	DEFAULT layerPriority = 9

	// The number of items in this enum type
	LAYER_PRIORITY_LENGTH int = 10
)

/*
Returns the string representation of the verbosity level
*/
func (lp layerPriority) String() string {
	switch lp {
	case RUNTIME:
		return "RUNTIME"
	case COMMAND_LINE:
		return "COMMAND_LINE"
	case ENVIRONMENT:
		return "ENVIRONMENT"
	case PLUGIN:
		return "PLUGIN"
	case CUSTOM_LOCAL_FILE:
		return "CUSTOM_LOCAL_FILE"
	case STANDARD_LOCAL_FILE:
		return "STANDARD_LOCAL_FILE"
	case CUSTOM_SHARED_FILE:
		return "CUSTOM_SHARED_FILE"
	case STANDARD_SHARED_FILE:
		return "STANDARD_SHARED_FILE"
	case PRESET:
		return "PRESET"
	case DEFAULT:
		return "DEFAULT"
	default:
		// this is never reached, but in case...
		panic("unknown Layer Priority. This means the switch/case statement needs to be updated")
	}
}
