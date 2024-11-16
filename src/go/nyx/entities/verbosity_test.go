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

	log "github.com/sirupsen/logrus" // https://github.com/Sirupsen/logrus, https://pkg.go.dev/github.com/sirupsen/logrus
)

func TestVerbosityGetLevel(t *testing.T) {
	var verbosity Verbosity

	verbosity = FATAL
	assert.Equal(t, log.FatalLevel, verbosity.GetLevel())
	verbosity = ERROR
	assert.Equal(t, log.ErrorLevel, verbosity.GetLevel())
	verbosity = WARNING
	assert.Equal(t, log.WarnLevel, verbosity.GetLevel())
	verbosity = INFO
	assert.Equal(t, log.InfoLevel, verbosity.GetLevel())
	verbosity = DEBUG
	assert.Equal(t, log.DebugLevel, verbosity.GetLevel())
	verbosity = TRACE
	assert.Equal(t, log.TraceLevel, verbosity.GetLevel())
}

func TestVerbosityFromLevel(t *testing.T) {
	verbosity, err := FromLevel(log.PanicLevel)
	assert.NoError(t, err)
	assert.Equal(t, FATAL, verbosity)
	verbosity, err = FromLevel(log.FatalLevel)
	assert.NoError(t, err)
	assert.Equal(t, FATAL, verbosity)
	verbosity, err = FromLevel(log.ErrorLevel)
	assert.NoError(t, err)
	assert.Equal(t, ERROR, verbosity)
	verbosity, err = FromLevel(log.WarnLevel)
	assert.NoError(t, err)
	assert.Equal(t, WARNING, verbosity)
	verbosity, err = FromLevel(log.InfoLevel)
	assert.NoError(t, err)
	assert.Equal(t, INFO, verbosity)
	verbosity, err = FromLevel(log.DebugLevel)
	assert.NoError(t, err)
	assert.Equal(t, DEBUG, verbosity)
	verbosity, err = FromLevel(log.TraceLevel)
	assert.NoError(t, err)
	assert.Equal(t, TRACE, verbosity)
}

func TestVerbosityString(t *testing.T) {
	assert.Equal(t, "FATAL", FATAL.String())
	assert.Equal(t, "ERROR", ERROR.String())
	assert.Equal(t, "WARNING", WARNING.String())
	assert.Equal(t, "INFO", INFO.String())
	assert.Equal(t, "DEBUG", DEBUG.String())
	assert.Equal(t, "TRACE", TRACE.String())
}

func TestVerbosityValueOfVerbosity(t *testing.T) {
	verbosity, err := ValueOfVerbosity("FATAL")
	assert.NoError(t, err)
	assert.Equal(t, FATAL, verbosity)
	verbosity, err = ValueOfVerbosity("ERROR")
	assert.NoError(t, err)
	assert.Equal(t, ERROR, verbosity)
	verbosity, err = ValueOfVerbosity("WARNING")
	assert.NoError(t, err)
	assert.Equal(t, WARNING, verbosity)
	verbosity, err = ValueOfVerbosity("INFO")
	assert.NoError(t, err)
	assert.Equal(t, INFO, verbosity)
	verbosity, err = ValueOfVerbosity("DEBUG")
	assert.NoError(t, err)
	assert.Equal(t, DEBUG, verbosity)
	verbosity, err = ValueOfVerbosity("TRACE")
	assert.NoError(t, err)
	assert.Equal(t, TRACE, verbosity)
}
