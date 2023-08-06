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

	utl "github.com/mooltiverse/nyx/modules/go/utils"
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestSubstitutionsNewSubstitutions(t *testing.T) {
	cmc := NewSubstitutions()

	// default constructor has its fields set to default values
	assert.Nil(t, cmc.GetEnabled())
	assert.Nil(t, cmc.GetItems())
}

func TestSubstitutionsNewSubstitutionsWith(t *testing.T) {
	s1 := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	items := make(map[string]*Substitution)
	items["one"] = s1

	enabled := []*string{utl.PointerToString("one")}

	s, err := NewSubstitutionsWith(&enabled, &items)
	assert.NoError(t, err)

	assert.Equal(t, &enabled, s.GetEnabled())
	assert.Equal(t, &items, s.GetItems())

	// also test error conditions when nil parameters are passed
	_, err = NewSubstitutionsWith(nil, &items)
	assert.NotNil(t, err)
	_, err = NewSubstitutionsWith(&enabled, nil)
	assert.NotNil(t, err)
}

func TestSubstitutionsGetEnabled(t *testing.T) {
	s := NewSubstitutions()

	enabled := []*string{utl.PointerToString("one")}
	err := s.SetEnabled(&enabled)
	assert.Equal(t, &enabled, s.GetEnabled())

	// also test error conditions when nil parameters are passed
	err = s.SetEnabled(nil)
	assert.NotNil(t, err)
}

func TestSubstitutionsGetItems(t *testing.T) {
	s := NewSubstitutions()

	s1 := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	items := make(map[string]*Substitution)
	items["one"] = s1

	err := s.SetItems(&items)
	assert.NoError(t, err)
	assert.Equal(t, &items, s.GetItems())

	// also test error conditions when nil parameters are passed
	err = s.SetItems(nil)
	assert.NotNil(t, err)
}
