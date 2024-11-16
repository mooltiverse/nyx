//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package io_test

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cnf "github.com/mooltiverse/nyx/src/go/nyx/configuration"
	io "github.com/mooltiverse/nyx/src/go/nyx/io"
	stt "github.com/mooltiverse/nyx/src/go/nyx/state"
)

func TestFileMapperStateFileJSON(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, _ := os.Create(filepath.Join(tempDir, "state"+fmt.Sprintf("%p", t)+".json"))
	defer os.Remove(savedFile.Name())

	cnf, _ := cnf.NewConfiguration()
	state, _ := stt.NewStateWith(cnf)
	err := io.Save(savedFile.Name(), state)
	assert.NoError(t, err)

	_, err = os.Stat(savedFile.Name())
	assert.NoError(t, err)
}

func TestFileMapperStateFileNoExtension(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, _ := os.Create(filepath.Join(tempDir, "state"+fmt.Sprintf("%p", t)))
	defer os.Remove(savedFile.Name())

	cnf, _ := cnf.NewConfiguration()
	state, _ := stt.NewStateWith(cnf)
	err := io.Save(savedFile.Name(), state)
	assert.NoError(t, err)

	_, err = os.Stat(savedFile.Name())
	assert.NoError(t, err)
}

func TestFileMapperStateFileYAML(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, _ := os.Create(filepath.Join(tempDir, "state"+fmt.Sprintf("%p", t)+".yaml"))
	defer os.Remove(savedFile.Name())

	cnf, _ := cnf.NewConfiguration()
	state, _ := stt.NewStateWith(cnf)
	err := io.Save(savedFile.Name(), state)
	assert.NoError(t, err)

	_, err = os.Stat(savedFile.Name())
	assert.NoError(t, err)
}
