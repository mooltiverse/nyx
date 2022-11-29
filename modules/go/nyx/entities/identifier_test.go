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

func TestPositionString(t *testing.T) {
	assert.Equal(t, "PRE_RELEASE", PRE_RELEASE.String())
	assert.Equal(t, "BUILD", BUILD.String())
}

func TestPositionValueOfPosition(t *testing.T) {
	position, err := ValueOfPosition("PRE_RELEASE")
	assert.NoError(t, err)
	assert.Equal(t, PRE_RELEASE, position)
	position, err = ValueOfPosition("BUILD")
	assert.NoError(t, err)
	assert.Equal(t, BUILD, position)
}

func TestNewIdentifier(t *testing.T) {
	identifier := NewIdentifier()

	// default constructor has its fields set to default values
	assert.Nil(t, identifier.GetQualifier())
	assert.Nil(t, identifier.GetValue())
	// this is undefined
	//assert.Equal(t, entities.Position(""), *identifier.GetPosition())
}

func TestNewIdentifierWith(t *testing.T) {
	identifier := NewIdentifierWith(utl.PointerToString("alpha"), utl.PointerToString("any"), PointerToPosition(PRE_RELEASE))

	assert.Equal(t, "alpha", *identifier.GetQualifier())
	assert.Equal(t, "any", *identifier.GetValue())
	assert.Equal(t, PRE_RELEASE, *identifier.GetPosition())
}

func TestIdentifierGetQualifier(t *testing.T) {
	identifier := NewIdentifier()

	identifier.SetQualifier(utl.PointerToString("alpha"))
	assert.Equal(t, "alpha", *identifier.GetQualifier())
}

func TestIdentifierGetValue(t *testing.T) {
	identifier := NewIdentifier()

	identifier.SetValue(utl.PointerToString("any"))
	assert.Equal(t, "any", *identifier.GetValue())
}

func TestIdentifierGetPosition(t *testing.T) {
	identifier := NewIdentifier()

	identifier.SetPosition(PointerToPosition(PRE_RELEASE))
	assert.Equal(t, PRE_RELEASE, *identifier.GetPosition())
}
