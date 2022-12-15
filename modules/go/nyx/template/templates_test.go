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
	"strconv"       // https://pkg.go.dev/strconv
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

const (
	// A test template to be used with a nil scope
	TEMPLATE_WITH_NULL_SCOPE = "{{#items}}\nName: {{name}}\n\nPrice: {{price}}\n  {{#features}}\n  Feature: {{description}}\n  {{/features}}\n{{/items}}"

	// The expected output for the template to be used with a nil scope
	TEMPLATE_WITH_NULL_SCOPE_OUTPUT = ""

	// A test template to be used with a mock scope
	TEMPLATE_WITH_MOCK_SCOPE = "{{#items}}\nName: {{name}}\n\nPrice: {{price}}\n  {{#features}}\n  Feature: {{description}}\n  {{/features}}\n{{/items}}"

	// The expected output for the template to be used with a mock scope
	TEMPLATE_WITH_MOCK_SCOPE_OUTPUT = "Name: Item 1\n\nPrice: $19.99\n  Feature: New!\n  Feature: Awesome!\nName: Item 2\n\nPrice: $29.99\n  Feature: Old.\n  Feature: Ugly.\n"
)

var (
	/*
	   A fixture with valid structured data to test templates.

	   Each returned argument has the fields:
	   - template: the template string
	*/
	wellKnownValidTemplates = []struct {
		template *string
	}{
		{template: utl.PointerToString("{{}}")},
		{template: utl.PointerToString("{{ }}")},
		{template: utl.PointerToString(" {{ }} ")},
		{template: utl.PointerToString("{{scheme}}")},
		{template: utl.PointerToString(" {{ scheme }} ")},
	}

	/*
	   A fixture with invalid structured data to test templates.

	   Each returned argument has the fields:
	   - template: the template string
	*/
	wellKnownInvalidTemplates = []struct {
		template *string
	}{
		{template: utl.PointerToString("{}}")},
		{template: utl.PointerToString("}}")},
		{template: utl.PointerToString("{{}")},
		{template: utl.PointerToString("{{")},
		{template: utl.PointerToString("{}")},
		{template: utl.PointerToString("{")},
		{template: utl.PointerToString("}")},
		{template: utl.PointerToString("")},
	}
)

/*
IsTemplate
*/
func TestTemplatesIsTemplateWithValidString(t *testing.T) {
	for i, tt := range wellKnownValidTemplates {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			assert.True(t, IsTemplate(*tt.template))
		})
	}
}

func TestTemplatesIsTemplateWithInvalidString(t *testing.T) {
	for i, tt := range wellKnownInvalidTemplates {
		// just use the interation number for the description here
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			assert.False(t, IsTemplate(*tt.template))
		})
	}
}

/*
ToBoolean
*/
func TestTemplatesToBoolean(t *testing.T) {
	assert.False(t, ToBoolean(utl.PointerToString("")))
	assert.False(t, ToBoolean(utl.PointerToString("    ")))
	assert.False(t, ToBoolean(utl.PointerToString("false")))
	assert.False(t, ToBoolean(utl.PointerToString("FALSE")))

	assert.False(t, ToBoolean(utl.PointerToString("1")))
	assert.False(t, ToBoolean(utl.PointerToString("11")))
	assert.False(t, ToBoolean(utl.PointerToString("a")))
	assert.False(t, ToBoolean(utl.PointerToString("!")))
	assert.False(t, ToBoolean(utl.PointerToString("lkjhewm,òlkàòld wq dsedewdwedw")))

	assert.True(t, ToBoolean(utl.PointerToString("true")))
	assert.True(t, ToBoolean(utl.PointerToString("TRUE")))
}

/*
ToInteger
*/
func TestTemplatesToInteger(t *testing.T) {
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("    ")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("a")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("!")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("lkjhewm,òlkàòld wq dsedewdwedw")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("1.0")))
	assert.Equal(t, int64(0), ToInteger(utl.PointerToString("1,0")))
	assert.Equal(t, int64(1), ToInteger(utl.PointerToString("1")))
	assert.Equal(t, int64(100), ToInteger(utl.PointerToString("100")))
	assert.Equal(t, int64(9999999), ToInteger(utl.PointerToString("9999999")))
}

func TestTemplatesRenderLower(t *testing.T) {
	output, _ := Render("{{#lower}}abcde 12345 FGHIJ{{/lower}}", nil)
	assert.Equal(t, "abcde 12345 fghij", output)
}

func TestTemplatesRenderTrim(t *testing.T) {
	output, _ := Render("{{#trim}} abcde 12345 FGHIJ  {{/trim}}", nil)
	assert.Equal(t, "abcde 12345 FGHIJ", output)
}

func TestTemplatesRenderUpper(t *testing.T) {
	output, _ := Render("{{#upper}}abcde 12345 FGHIJ{{/upper}}", nil)
	assert.Equal(t, "ABCDE 12345 FGHIJ", output)
}

func TestTemplatesRenderFirst(t *testing.T) {
	output, _ := Render("{{#first}}ABCDE 12345 FGHIJ{{/first}}", nil)
	assert.Equal(t, "ABCDE", output)
}

func TestTemplatesRenderFirstLower(t *testing.T) {
	output, _ := Render("{{#firstLower}}ABCDE 12345 FGHIJ{{/firstLower}}", nil)
	assert.Equal(t, "abcde", output)
}

func TestTemplatesRenderFirstUpper(t *testing.T) {
	output, _ := Render("{{#firstUpper}}abcde 12345 FGHIJ{{/firstUpper}}", nil)
	assert.Equal(t, "ABCDE", output)
}

func TestTemplatesRenderLast(t *testing.T) {
	output, _ := Render("{{#last}}ABCDE 12345 FGHIJ{{/last}}", nil)
	assert.Equal(t, "FGHIJ", output)
}

func TestTemplatesRenderLastLower(t *testing.T) {
	output, _ := Render("{{#lastLower}}ABCDE 12345 FGHIJ{{/lastLower}}", nil)
	assert.Equal(t, "fghij", output)
}

func TestTemplatesRenderLastUpper(t *testing.T) {
	output, _ := Render("{{#lastUpper}}abcde 12345 FGHIJ{{/lastUpper}}", nil)
	assert.Equal(t, "FGHIJ", output)
}

func TestTemplatesRenderSanitize(t *testing.T) {
	output, _ := Render("{{#sanitize}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitize}}", nil)
	assert.Equal(t, "aBc123defgHI", output)
}

func TestTemplatesRenderSanitizeLower(t *testing.T) {
	output, _ := Render("{{#sanitizeLower}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeLower}}", nil)
	assert.Equal(t, "abc123defghi", output)
}

func TestTemplatesRenderSanitizeUpper(t *testing.T) {
	output, _ := Render("{{#sanitizeUpper}} a!B£c$ 123 d%e&f/g(H)I  {{/sanitizeUpper}}", nil)
	assert.Equal(t, "ABC123DEFGHI", output)
}

func TestTemplatesRenderShort5(t *testing.T) {
	output, _ := Render("{{#short5}}1234567890{{/short5}}", nil)
	assert.Equal(t, "12345", output)
}

func TestTemplatesRenderShort6(t *testing.T) {
	output, _ := Render("{{#short6}}1234567890{{/short6}}", nil)
	assert.Equal(t, "123456", output)
}

func TestTemplatesRenderShort7(t *testing.T) {
	output, _ := Render("{{#short7}}1234567890{{/short7}}", nil)
	assert.Equal(t, "1234567", output)
}

func TestTemplatesRenderTimestampISO8601(t *testing.T) {
	output, _ := Render("{{#timestampISO8601}}0{{/timestampISO8601}}", nil)
	assert.Equal(t, "1970-01-01T00:00:00", output)
}

func TestTemplatesRenderTimestampYYYYMMDDHHMMSS(t *testing.T) {
	output, _ := Render("{{#timestampYYYYMMDDHHMMSS}}0{{/timestampYYYYMMDDHHMMSS}}", nil)
	assert.Equal(t, "19700101000000", output)
}

func TestTemplatesRenderEnvironmentVariable(t *testing.T) {
	// just test it's not empty as it changes depending on the runtime environment
	output, _ := Render("{{#environmentVariable}}PATH{{/environmentVariable}}", nil)
	assert.NotEqual(t, "", output)
}

func TestTemplatesRenderEnvironmentUser(t *testing.T) {
	// just test it's not empty as it changes depending on the runtime environment
	output, _ := Render("{{environmentUser}}", nil)
	assert.NotEqual(t, "", output)
	output, _ = Render("{{#environmentUser}}{{/environmentUser}}", nil)
	assert.NotEqual(t, "", output)
}

func TestTemplatesRenderFileContent(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFile, _ := os.Create(filepath.Join(tempDir, "templates1"+fmt.Sprintf("%p", t)+".txt"))
	defer os.Remove(savedFile.Name())
	savedFile.Write([]byte("test12345"))
	output, _ := Render("{{#fileContent}}"+savedFile.Name()+"{{/fileContent}}", nil)
	assert.Equal(t, "test12345", output)
}

func TestTemplatesRenderFileExists(t *testing.T) {
	tempDir, _ := os.MkdirTemp("", fmt.Sprintf("%p", t))
	savedFileName := filepath.Join(tempDir, "templates2"+fmt.Sprintf("%p", t)+".txt")
	output, _ := Render("{{#fileExists}}"+savedFileName+"{{/fileExists}}", nil)
	assert.Equal(t, "false", output)
	savedFile, _ := os.Create(savedFileName)
	defer os.Remove(savedFile.Name())
	output, _ = Render("{{#fileExists}}"+savedFile.Name()+"{{/fileExists}}", nil)
	assert.Equal(t, "true", output)
}

/*
Render with nil scope
*/
func TestTemplatesRenderWithNilScope(t *testing.T) {
	output, err := Render(TEMPLATE_WITH_NULL_SCOPE, nil)
	assert.NoError(t, err)
	assert.Equal(t, TEMPLATE_WITH_NULL_SCOPE_OUTPUT, output)
}

/*
A test struct use as a scope to render templates.
*/
type TestScope struct {
	Items *[]*TestItem
}

type TestItem struct {
	Name     *string
	Price    *string
	Features *[]*TestFeature
}

type TestFeature struct {
	Description *string
}

var testScope1 = &TestScope{Items: &[]*TestItem{&TestItem{Name: utl.PointerToString("Item 1"), Price: utl.PointerToString("$19.99"), Features: &[]*TestFeature{&TestFeature{Description: utl.PointerToString("New!")}, &TestFeature{Description: utl.PointerToString("Awesome!")}}}, &TestItem{Name: utl.PointerToString("Item 2"), Price: utl.PointerToString("$29.99"), Features: &[]*TestFeature{&TestFeature{Description: utl.PointerToString("Old.")}, &TestFeature{Description: utl.PointerToString("Ugly.")}}}}}

/*
Render with mock
*/
func TestTemplatesRenderWithMock(t *testing.T) {
	output, err := Render(TEMPLATE_WITH_MOCK_SCOPE, testScope1)
	assert.NoError(t, err)
	assert.Equal(t, TEMPLATE_WITH_MOCK_SCOPE_OUTPUT, output)
}
