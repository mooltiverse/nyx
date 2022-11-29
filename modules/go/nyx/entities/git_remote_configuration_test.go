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

	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

func TestGitRemoteConfigurationNewGitRemoteConfiguration(t *testing.T) {
	rgc := NewGitRemoteConfiguration()

	// default constructor has its fields set to default values
	assert.Nil(t, rgc.GetUser())
	assert.Nil(t, rgc.GetPassword())
}

func TestGitRemoteConfigurationNewGitRemoteConfigurationWith(t *testing.T) {
	rgc := NewGitRemoteConfigurationWith(utl.PointerToString("u1"), utl.PointerToString("p1"))

	u := rgc.GetUser()
	assert.Equal(t, "u1", *u)
	p := rgc.GetPassword()
	assert.Equal(t, "p1", *p)
}

func TestGitRemoteConfigurationGetUser(t *testing.T) {
	remoteGitConfiguration := NewGitRemoteConfiguration()

	remoteGitConfiguration.SetUser(utl.PointerToString("u1"))
	u := remoteGitConfiguration.GetUser()
	assert.Equal(t, "u1", *u)
}

func TestGitRemoteConfigurationGetPassword(t *testing.T) {
	remoteGitConfiguration := NewGitRemoteConfiguration()

	remoteGitConfiguration.SetPassword(utl.PointerToString("p1"))
	p := remoteGitConfiguration.GetPassword()
	assert.Equal(t, "p1", *p)
}
