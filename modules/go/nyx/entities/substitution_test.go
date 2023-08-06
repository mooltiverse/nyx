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

func TestSubstitutionNewSubstitution(t *testing.T) {
	s := NewSubstitution()

	// default constructor has its fields set to default values
	assert.Nil(t, s.GetFiles())
	assert.Nil(t, s.GetMatch())
	assert.Nil(t, s.GetReplace())
}

func TestSubstitutionNewSubstitutionWith(t *testing.T) {
	s := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	f := s.GetFiles()
	assert.Equal(t, "glob1", *f)
	m := s.GetMatch()
	assert.Equal(t, "match1", *m)
	r := s.GetReplace()
	assert.Equal(t, "replace1", *r)
}

func TestSubstitutionGetFiles(t *testing.T) {
	s := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	f := s.GetFiles()
	assert.Equal(t, "glob1", *f)
}

func TestSubstitutionGetMatch(t *testing.T) {
	s := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	m := s.GetMatch()
	assert.Equal(t, "match1", *m)
}

func TestSubstitutionGetReplace(t *testing.T) {
	s := NewSubstitutionWith(utl.PointerToString("glob1"), utl.PointerToString("match1"), utl.PointerToString("replace1"))

	r := s.GetReplace()
	assert.Equal(t, "replace1", *r)
}
