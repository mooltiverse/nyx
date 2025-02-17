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

package io

import (
	"net/url" // https://pkg.go.dev/net/url
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

type Todo struct {
	// For the type of data returned see:
	// - https://jsonplaceholder.typicode.com/todos/1
	id        *string
	userId    *string
	title     *string
	completed *bool
}

func TestFileMapperLoadFromURL(t *testing.T) {
	target := Todo{}

	path, err := url.Parse("https://jsonplaceholder.typicode.com/todos/1")
	assert.NoError(t, err)

	err = LoadFromURL(*path, &target)
	assert.NoError(t, err)
}
