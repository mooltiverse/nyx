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

func TestAttachmentNewAttachmentWith(t *testing.T) {
	a := NewAttachmentWith(utl.PointerToString("n1"), utl.PointerToString("d1"), utl.PointerToString("p1"), utl.PointerToString("t1"))

	fn := a.GetFileName()
	assert.Equal(t, "n1", *fn)
	d := a.GetDescription()
	assert.Equal(t, "d1", *d)
	p := a.GetPath()
	assert.Equal(t, "p1", *p)
	tt := a.GetType()
	assert.Equal(t, "t1", *tt)
}

func TestAttachmentGetFileName(t *testing.T) {
	a := &Attachment{}

	a.SetFileName(utl.PointerToString("n1"))
	fn := a.GetFileName()
	assert.Equal(t, "n1", *fn)
}

func TestAttachmentGetDescription(t *testing.T) {
	a := &Attachment{}

	a.SetDescription(utl.PointerToString("d1"))
	d := a.GetDescription()
	assert.Equal(t, "d1", *d)
}

func TestAttachmentGetPath(t *testing.T) {
	a := &Attachment{}

	a.SetPath(utl.PointerToString("p1"))
	p := a.GetPath()
	assert.Equal(t, "p1", *p)
}

func TestAttachmentGetType(t *testing.T) {
	a := &Attachment{}

	a.SetType(utl.PointerToString("t1"))
	tt := a.GetType()
	assert.Equal(t, "t1", *tt)
}
