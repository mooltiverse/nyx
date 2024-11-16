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

func TestChangelogConfigurationNewChangelogConfiguration(t *testing.T) {
	cc := NewChangelogConfiguration()

	// default constructor has its fields set to default values
	assert.Nil(t, cc.GetAppend())
	assert.Nil(t, cc.GetPath())
	assert.Equal(t, 0, len(*cc.GetSections()))
	assert.Equal(t, 0, len(*cc.GetSubstitutions()))
	assert.Nil(t, cc.GetTemplate())
}

func TestChangelogConfigurationNewChangelogConfigurationWith(t *testing.T) {
	sections := make(map[string]string)
	sections["Section1"] = "regex1"
	sections["Section2"] = "regex2"

	substitutions := make(map[string]string)
	substitutions["Expression1"] = "string1"

	cc, err := NewChangelogConfigurationWith(utl.PointerToString("tail"), utl.PointerToString("CHANGELOG.md"), &sections, utl.PointerToString("changelog.tpl"), &substitutions)
	assert.NoError(t, err)

	a := cc.GetAppend()
	assert.Equal(t, "tail", *a)
	p := cc.GetPath()
	assert.Equal(t, "CHANGELOG.md", *p)
	s1 := cc.GetSections()
	assert.Equal(t, &sections, s1)
	t1 := cc.GetTemplate()
	assert.Equal(t, "changelog.tpl", *t1)
	s2 := cc.GetSubstitutions()
	assert.Equal(t, &substitutions, s2)

	// also test error conditions when nil parameters are passed
	_, err = NewChangelogConfigurationWith(nil, utl.PointerToString("CHANGELOG.md"), nil, utl.PointerToString("changelog.tpl"), &substitutions)
	assert.NotNil(t, err)
}

func TestChangelogConfigurationGetAppend(t *testing.T) {
	cc := NewChangelogConfiguration()

	cc.SetAppend(utl.PointerToString("tail"))
	a := cc.GetAppend()
	assert.Equal(t, "tail", *a)
}

func TestChangelogConfigurationGetPath(t *testing.T) {
	cc := NewChangelogConfiguration()

	cc.SetPath(utl.PointerToString("CHANGELOG.md"))
	p := cc.GetPath()
	assert.Equal(t, "CHANGELOG.md", *p)
}

func TestChangelogConfigurationGetSections(t *testing.T) {
	sections := make(map[string]string)
	sections["Section1"] = "regex1"
	sections["Section2"] = "regex2"

	cc := NewChangelogConfiguration()

	err := cc.SetSections(&sections)
	assert.NoError(t, err)
	cc1 := cc.GetSections()
	assert.Equal(t, &sections, cc1)

	// also test error conditions when nil parameters are passed
	err = cc.SetSections(nil)
	assert.NotNil(t, err)
}

func TestChangelogConfigurationGetTemplate(t *testing.T) {
	cc := NewChangelogConfiguration()

	cc.SetTemplate(utl.PointerToString("changelog.tpl"))
	t1 := cc.GetTemplate()
	assert.Equal(t, "changelog.tpl", *t1)
}

func TestChangelogConfigurationGetSubstitutions(t *testing.T) {
	substitutions := make(map[string]string)
	substitutions["Expression1"] = "string1"

	cc := NewChangelogConfiguration()

	err := cc.SetSubstitutions(&substitutions)
	assert.NoError(t, err)
	s := cc.GetSubstitutions()
	assert.Equal(t, &substitutions, s)

	// also test error conditions when nil parameters are passed
	err = cc.SetSubstitutions(nil)
	assert.NotNil(t, err)
}
