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

package entities

import (
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestNewServiceConfiguration(t *testing.T) {
	sc := NewServiceConfiguration()

	// default constructor has its fields set to default values
	assert.Nil(t, sc.GetOptions())
	assert.Nil(t, sc.GetType())
}

func TestNewServiceConfigurationWith(t *testing.T) {
	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	sc := NewServiceConfigurationWith(PointerToProvider(GITHUB), &m)

	assert.Equal(t, &m, sc.GetOptions())
	assert.Equal(t, PointerToProvider(GITHUB), sc.GetType())
}

func TestServiceConfigurationGetOptions(t *testing.T) {
	serviceConfiguration := NewServiceConfiguration()

	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	serviceConfiguration.SetOptions(&m)
	assert.Equal(t, &m, serviceConfiguration.GetOptions())
}

func TestServiceConfigurationGetType(t *testing.T) {
	serviceConfiguration := NewServiceConfiguration()

	serviceConfiguration.SetType(PointerToProvider(GITHUB))
	assert.Equal(t, PointerToProvider(GITHUB), serviceConfiguration.GetType())
}
