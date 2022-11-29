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

package git

import (
	"testing" // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestMessageFromStringWithoutFooters1(t *testing.T) {
	messageString := `subject line
line 2
line 3`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}

func TestMessageFromStringWithoutFooters2(t *testing.T) {
	messageString := `subject line

line 2
line 3`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}

func TestMessageFromStringWithoutFooters3(t *testing.T) {
	messageString := `subject line

line 2
line 3
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}

func TestMessageFromStringWithoutFooters4(t *testing.T) {
	messageString := `subject line

line 2
line 3
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}

func TestMessageFromStringWithoutFooters5(t *testing.T) {
	messageString := `subject line


`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}

func TestMessageFromStringWithFooters1(t *testing.T) {
	messageString := `subject line

k1: v1
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 1, len(message.GeFooters()))
	assert.Equal(t, "v1", message.GeFooters()["k1"])
}

func TestMessageFromStringWithFooters2(t *testing.T) {
	messageString := `subject line

k1: v1
k2: v2
k3: v3
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 3, len(message.GeFooters()))
	assert.Equal(t, "v1", message.GeFooters()["k1"])
	assert.Equal(t, "v2", message.GeFooters()["k2"])
	assert.Equal(t, "v3", message.GeFooters()["k3"])
}

func TestMessageFromStringWithFooters3(t *testing.T) {
	messageString := `subject line

k1 and more: v1
k2: v2: vvv
k3: v3 and something
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 3, len(message.GeFooters()))
	assert.Equal(t, "v1", message.GeFooters()["k1 and more"])
	assert.Equal(t, "v2: vvv", message.GeFooters()["k2"])
	assert.Equal(t, "v3 and something", message.GeFooters()["k3"])
}

func TestMessageFromStringWithWrongFooters1(t *testing.T) {
	messageString := `subject line
k1 and more: v1
k2: v2: vvv
k3: v3 and something
`

	message := messageFromString(messageString)
	assert.Equal(t, "subject line", message.GetShortMessage())
	assert.Equal(t, messageString, message.GetFullMessage())
	assert.Equal(t, 0, len(message.GeFooters()))
}
