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
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestPresetsPresetByName(t *testing.T) {
	simpleConfigurationLayer, err := PresetByName("")
	assert.Error(t, err)
	assert.Nil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName("notapreset")
	assert.Error(t, err)
	assert.Nil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName(SIMPLE_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)

	simpleConfigurationLayer, err = PresetByName(EXTENDED_NAME)
	assert.NoError(t, err)
	assert.NotNil(t, simpleConfigurationLayer)
}
