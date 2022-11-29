//go:build unit
// +build unit

// Only run these tests as part of the unit test suite, when the 'unit' build flag is passed (i.e. running go test --tags=unit)

/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package templates

import (
	"fmt"           // https://pkg.go.dev/fmt
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestFunctionsLower(t *testing.T) {
	assert.Equal(t, "", lower(""))
	assert.Equal(t, "0", lower("0"))
	assert.Equal(t, "01234", lower("01234"))
	assert.Equal(t, "lower", lower("lower"))
	assert.Equal(t, "upper", lower("upper"))
	assert.Equal(t, "mixed", lower("mIxEd"))
}

func TestFunctionsUpper(t *testing.T) {
	assert.Equal(t, "", upper(""))
	assert.Equal(t, "0", upper("0"))
	assert.Equal(t, "01234", upper("01234"))
	assert.Equal(t, "LOWER", upper("lower"))
	assert.Equal(t, "UPPER", upper("upper"))
	assert.Equal(t, "MIXED", upper("mIxEd"))
}

func TestFunctionsTrim(t *testing.T) {
	assert.Equal(t, "", trim(""))
	assert.Equal(t, "0", trim("0"))
	assert.Equal(t, "01  234", trim("   01  234   "))
}

func TestFunctionsFirst(t *testing.T) {
	assert.Equal(t, "", first(""))
	assert.Equal(t, "aB1", first("aB1"))
	assert.Equal(t, "", first(" aB1 "))
	assert.Equal(t, "", first("!£$%&b2/()=?'^ì*+§°#@.;c3"))
	assert.Equal(t, "a1", first("a1!£$%&b2/()=?'^ì*+§°#@.;c3"))
}

func TestFunctionsFirstLower(t *testing.T) {
	assert.Equal(t, "", firstLower(""))
	assert.Equal(t, "ab1", firstLower("aB1"))
	assert.Equal(t, "", firstLower(" aB1 "))
	assert.Equal(t, "", firstLower("!£$%&b2/()=?'^ì*+§°#@.;c3"))
	assert.Equal(t, "ab1", firstLower("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"))
}

func TestFunctionsFirstUpper(t *testing.T) {
	assert.Equal(t, "", firstUpper(""))
	assert.Equal(t, "AB1", firstUpper("aB1"))
	assert.Equal(t, "", firstUpper(" aB1 "))
	assert.Equal(t, "", firstUpper("!£$%&b2/()=?'^ì*+§°#@.;c3"))
	assert.Equal(t, "AB1", firstUpper("aB1!£$%&b2/()=?'^ì*+§°#@.;c3"))
}

func TestFunctionsLast(t *testing.T) {
	assert.Equal(t, "", last(""))
	assert.Equal(t, "aB1", last("aB1"))
	assert.Equal(t, "", last(" aB1 "))
	assert.Equal(t, "", last("a1!£$%&b2/()=?'^ì*+§°#@.;"))
	assert.Equal(t, "c3", last("a1!£$%&b2/()=?'^ì*+§°#@.;c3"))
}

func TestFunctionsLastLower(t *testing.T) {
	assert.Equal(t, "", lastLower(""))
	assert.Equal(t, "ab1", lastLower("aB1"))
	assert.Equal(t, "", lastLower(" aB1 "))
	assert.Equal(t, "", lastLower("a1!£$%&b2/()=?'^ì*+§°#@.;"))
	assert.Equal(t, "cd3", lastLower("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"))
}

func TestFunctionsLastUpper(t *testing.T) {
	assert.Equal(t, "", lastUpper(""))
	assert.Equal(t, "AB1", lastUpper("aB1"))
	assert.Equal(t, "", lastUpper(" aB1 "))
	assert.Equal(t, "", lastUpper("a1!£$%&b2/()=?'^ì*+§°#@.;"))
	assert.Equal(t, "CD3", lastUpper("a1!£$%&b2/()=?'^ì*+§°#@.;cD3"))
}

func TestFunctionsSanitize(t *testing.T) {
	assert.Equal(t, "", sanitize(""))
	assert.Equal(t, "aB1", sanitize("aB1"))
	assert.Equal(t, "aB1", sanitize(" aB1 "))
	assert.Equal(t, "abc123", sanitize("\\!£$%&abc123/()=?'^ì*+§°#@.;"))
}

func TestFunctionsSanitizeLower(t *testing.T) {
	assert.Equal(t, "", sanitizeLower(""))
	assert.Equal(t, "ab1", sanitizeLower("aB1"))
	assert.Equal(t, "ab1", sanitizeLower(" aB1 "))
	assert.Equal(t, "abc123", sanitizeLower("\\!£$%&aBc123/()=?'^ì*+§°#@.;"))
}

func TestFunctionsSanitizeUpper(t *testing.T) {
	assert.Equal(t, "", sanitizeUpper(""))
	assert.Equal(t, "AB1", sanitizeUpper("aB1"))
	assert.Equal(t, "AB1", sanitizeUpper(" aB1 "))
	assert.Equal(t, "ABC123", sanitizeUpper("\\!£$%&aBc123/()=?'^ì*+§°#@.;"))
}

func TestFunctionsShort5(t *testing.T) {
	assert.Equal(t, "", short5(""))
	assert.Equal(t, "0", short5("0"))
	assert.Equal(t, "01234", short5("01234"))
	assert.Equal(t, "01234", short5("012345"))
	assert.Equal(t, "01234", short5("0123456789"))
}

func TestFunctionsShort6(t *testing.T) {
	assert.Equal(t, "", short6(""))
	assert.Equal(t, "0", short6("0"))
	assert.Equal(t, "01234", short6("01234"))
	assert.Equal(t, "012345", short6("012345"))
	assert.Equal(t, "012345", short6("0123456"))
	assert.Equal(t, "012345", short6("01234567"))
	assert.Equal(t, "012345", short6("0123456789"))
}

func TestFunctionsShort7(t *testing.T) {
	assert.Equal(t, "", short7(""))
	assert.Equal(t, "0", short7("0"))
	assert.Equal(t, "01234", short7("01234"))
	assert.Equal(t, "012345", short7("012345"))
	assert.Equal(t, "0123456", short7("0123456"))
	assert.Equal(t, "0123456", short7("01234567"))
	assert.Equal(t, "0123456", short7("0123456789"))
}

func TestFunctionsTimestampISO8601(t *testing.T) {
	assert.Equal(t, "1970-01-01T00:00:00", timestampISO8601("0"))
	assert.Equal(t, "2020-01-01T12:00:00", timestampISO8601("1577880000000"))
}

func TestFunctionsTimestampYYYYMMDDHHMMSS(t *testing.T) {
	assert.Equal(t, "19700101000000", timestampYYYYMMDDHHMMSS("0"))
	assert.Equal(t, "20200101120000", timestampYYYYMMDDHHMMSS("1577880000000"))
}

func TestFunctionsEnvironmentVariable(t *testing.T) {
	// on CI platforms the OS variable may not be defined, causing this test to fail, so let's make it conditional
	if os.Getenv("OS") == "" {
		assert.Equal(t, "", environmentVariable("OS"))
	} else {
		assert.Equal(t, os.Getenv("OS"), environmentVariable("OS"))
	}
}

func TestFunctionsEnvironmentUser(t *testing.T) {
	// the input value is ignored by this function, it always returns the system user name
	assert.NotEqual(t, "", environmentUser("any"))
}

func TestFunctionsFileContent(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, _ := os.Create(filepath.Join(tempDir, "templatesfilecontenttest"+fmt.Sprintf("%p", t)+".txt"))
	defer os.Remove(savedFile.Name())
	savedFile.Write([]byte("file content to test"))

	assert.Equal(t, "", fileContent("afilethatdoesnotexists"))
	assert.Equal(t, "file content to test", fileContent(savedFile.Name()))
}

func TestFunctionsFileExists(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	fileName := filepath.Join(tempDir, "templatesfileexiststest"+fmt.Sprintf("%p", t)+".txt")
	file, _ := os.Create(fileName)
	defer os.Remove(file.Name())

	assert.Equal(t, "false", fileExists("afilethatdoesnotexists"))
	assert.Equal(t, "true", fileExists(fileName))
}
