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

func TestCommitMessageConventionNewCommitMessageConvention(t *testing.T) {
	cmc := NewCommitMessageConvention()

	// default constructor has its fields set to default values
	assert.Nil(t, cmc.GetExpression())
	assert.Nil(t, cmc.GetBumpExpressions())
}

func TestCommitMessageConventionNewCommitMessageConventionWith(t *testing.T) {
	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	cmm := NewCommitMessageConventionWith(utl.PointerToString("regex1"), &m)

	e := cmm.GetExpression()
	assert.Equal(t, "regex1", *e)
	m2 := cmm.GetBumpExpressions()
	assert.Equal(t, &m, m2)
}

func TestCommitMessageConventionGetExpression(t *testing.T) {
	cmc := NewCommitMessageConvention()

	cmc.SetExpression(utl.PointerToString("regex1"))
	cvq := cmc.GetExpression()
	assert.Equal(t, "regex1", *cvq)
}

func TestCommitMessageConventionGetBumpExpressions(t *testing.T) {
	m := make(map[string]string)
	m["k1"] = "v1"
	m["k2"] = "v2"

	cmc := NewCommitMessageConvention()

	cmc.SetBumpExpressions(&m)
	mev := cmc.GetBumpExpressions()
	assert.Equal(t, &m, mev)
}
