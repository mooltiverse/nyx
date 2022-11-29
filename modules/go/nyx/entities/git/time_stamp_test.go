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
	"time"    // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert
)

func TestNewTimeStampFrom(t *testing.T) {
	tm := time.Now()
	timeStamp := NewTimeStampFrom(tm)

	assert.Equal(t, tm.UnixMilli(), timeStamp.GeTimeStamp())
	assert.Equal(t, *tm.Location(), *timeStamp.GeTimeZone())

	assert.Equal(t, time.UnixMilli(tm.UnixMilli()).String(), timeStamp.String())
}

func TestNewTimeStampWith(t *testing.T) {
	tm := time.Now()
	timeStamp := NewTimeStampWith(tm.UnixMilli())

	assert.Equal(t, tm.UnixMilli(), timeStamp.GeTimeStamp())
	assert.Nil(t, timeStamp.GeTimeZone())

	assert.Equal(t, time.UnixMilli(tm.UnixMilli()).String(), timeStamp.String())
}

func TestNewTimeStampWithIn(t *testing.T) {
	tm := time.Now()
	timeStamp := NewTimeStampWithIn(tm.UnixMilli(), *time.UTC)

	assert.Equal(t, tm.UnixMilli(), timeStamp.GeTimeStamp())
	assert.Equal(t, time.UTC, timeStamp.GeTimeZone())

	assert.Equal(t, time.UnixMilli(tm.UnixMilli()).In(time.UTC).String(), timeStamp.String())
}
