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

	utl "github.com/mooltiverse/nyx/src/go/utils"
)

func TestGitConfigurationNewGitConfiguration(t *testing.T) {
	gitConfiguration := NewGitConfiguration()

	// default constructor has its fields set to default values
	assert.NotNil(t, gitConfiguration.GetRemotes())
}

func TestGitConfigurationNewGitConfigurationWith(t *testing.T) {
	remotes := make(map[string]*GitRemoteConfiguration)
	remotes["r1"] = NewGitRemoteConfigurationWith(PointerToAuthenticationMethod(USER_PASSWORD), utl.PointerToString("u1"), utl.PointerToString("p1"), utl.PointerToString("k1"), utl.PointerToString("h1"))
	remotes["r2"] = NewGitRemoteConfigurationWith(PointerToAuthenticationMethod(PUBLIC_KEY), utl.PointerToString("u2"), utl.PointerToString("p2"), utl.PointerToString("k2"), utl.PointerToString("h2"))

	gitConfiguration, err := NewGitConfigurationWith(&remotes)
	assert.NoError(t, err)

	assert.Equal(t, &remotes, gitConfiguration.GetRemotes())

	// also test error conditions when nil parameters are passed
	_, err = NewGitConfigurationWith(nil)
	assert.NotNil(t, err)
}

func TestGitConfigurationGetRemotes(t *testing.T) {
	gitConfiguration := NewGitConfiguration()

	remotes := make(map[string]*GitRemoteConfiguration)
	remotes["r1"] = NewGitRemoteConfigurationWith(PointerToAuthenticationMethod(USER_PASSWORD), utl.PointerToString("u1"), utl.PointerToString("p1"), utl.PointerToString("k1"), utl.PointerToString("h1"))
	remotes["r2"] = NewGitRemoteConfigurationWith(PointerToAuthenticationMethod(PUBLIC_KEY), utl.PointerToString("u2"), utl.PointerToString("p2"), utl.PointerToString("k2"), utl.PointerToString("h2"))

	err := gitConfiguration.SetRemotes(&remotes)
	assert.NoError(t, err)
	assert.Equal(t, &remotes, gitConfiguration.GetRemotes())

	// also test error conditions when nil parameters are passed
	err = gitConfiguration.SetRemotes(nil)
	assert.NotNil(t, err)
}
