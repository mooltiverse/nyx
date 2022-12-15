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

func TestCommitMessageConventionsNewCommitMessageConventions(t *testing.T) {
	cmc := NewCommitMessageConventions()

	// default constructor has its fields set to default values
	assert.Nil(t, cmc.GetEnabled())
	assert.Nil(t, cmc.GetItems())
}

func TestCommitMessageConventionsNewCommitMessageConventionsWith(t *testing.T) {
	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	cmm := NewCommitMessageConventionWith(utl.PointerToString("regex1"), &m)

	items := make(map[string]*CommitMessageConvention)
	items["one"] = cmm

	enabled := []*string{utl.PointerToString("one")}

	cmc, err := NewCommitMessageConventionsWith(&enabled, &items)
	assert.NoError(t, err)

	assert.Equal(t, &enabled, cmc.GetEnabled())
	assert.Equal(t, &items, cmc.GetItems())

	// also test error conditions when nil parameters are passed
	_, err = NewCommitMessageConventionsWith(nil, &items)
	assert.NotNil(t, err)
	_, err = NewCommitMessageConventionsWith(&enabled, nil)
	assert.NotNil(t, err)
}

func TestCommitMessageConventionsGetEnabled(t *testing.T) {
	cmc := NewCommitMessageConventions()

	enabled := []*string{utl.PointerToString("one")}
	err := cmc.SetEnabled(&enabled)
	assert.Equal(t, &enabled, cmc.GetEnabled())

	// also test error conditions when nil parameters are passed
	err = cmc.SetEnabled(nil)
	assert.NotNil(t, err)
}

func TestCommitMessageConventionsGetItems(t *testing.T) {
	cmc := NewCommitMessageConventions()

	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	cmm := NewCommitMessageConventionWith(utl.PointerToString("regex1"), &m)

	items := make(map[string]*CommitMessageConvention)
	items["one"] = cmm

	err := cmc.SetItems(&items)
	assert.NoError(t, err)
	assert.Equal(t, &items, cmc.GetItems())

	// also test error conditions when nil parameters are passed
	err = cmc.SetItems(nil)
	assert.NotNil(t, err)
}
