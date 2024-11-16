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
	"fmt" // https://pkg.go.dev/fmt

	errs "github.com/mooltiverse/nyx/src/go/errors"
)

/*
Returns a configuration layer instance by its name.

Arguments are as follows:

- name the name of the configuration layer to be returned.

Errors can be:

- IllegalPropertyError in case the given name can't be resolved to a preset.
*/
func PresetByName(s string) (*SimpleConfigurationLayer, error) {
	switch s {
	case EXTENDED_NAME:
		return NewExtendedPreset(), nil
	case SIMPLE_NAME:
		return NewSimplePreset(), nil
	default:
		return nil, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal preset name '%s'", s)}
	}
}
