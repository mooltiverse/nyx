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
	"fmt" // https://pkg.go.dev/fmt

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus

	errs "github.com/mooltiverse/nyx/modules/go/errors"
)

/*
This type maps log levels and the corresponding configuration options and state attributes,
whose string representations may not always match the log level values.

In other words, here is the mapping between logging levels and the string values users can use in configuration
and state to ask for those logging levels.
*/
type Verbosity string

const (
	// The fatal log level.
	FATAL Verbosity = "FATAL"

	// The error log level.
	ERROR Verbosity = "ERROR"

	// The warning log level.
	WARNING Verbosity = "WARNING"

	// The info log level.
	INFO Verbosity = "INFO"

	// The debug log level.
	DEBUG Verbosity = "DEBUG"

	// The trace log level.
	TRACE Verbosity = "TRACE"
)

/*
Returns the logging level corresponding to this verbosity level
*/
func (v Verbosity) GetLevel() log.Level {
	switch v {
	case FATAL:
		return log.FatalLevel
	case ERROR:
		return log.ErrorLevel
	case WARNING:
		return log.WarnLevel
	case INFO:
		return log.InfoLevel
	case DEBUG:
		return log.DebugLevel
	case TRACE:
		return log.TraceLevel
	default:
		// this is never reached, but in case, return INFO as the default
		return log.InfoLevel
	}
}

/*
Returns the verbosity corresponding to the given logging level.

Errors can be:

- IllegalPropertyError in case an unknown log level is passed
*/
func FromLevel(l log.Level) (Verbosity, error) {
	switch l {
	case log.PanicLevel, log.FatalLevel:
		return FATAL, nil
	case log.ErrorLevel:
		return ERROR, nil
	case log.WarnLevel:
		return WARNING, nil
	case log.InfoLevel:
		return INFO, nil
	case log.DebugLevel:
		return DEBUG, nil
	case log.TraceLevel:
		return TRACE, nil
	default:
		// this is never reached, but in case, return INFO as the default, with an error
		return INFO, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal log level '%s'", l)}
	}
}

/*
Returns the string representation of the verbosity level
*/
func (v Verbosity) String() string {
	switch v {
	case FATAL:
		return "FATAL"
	case ERROR:
		return "ERROR"
	case WARNING:
		return "WARNING"
	case INFO:
		return "INFO"
	case DEBUG:
		return "DEBUG"
	case TRACE:
		return "TRACE"
	default:
		// this is never reached, but in case...
		panic("unknown Verbosity. This means the switch/case statement needs to be updated")
	}
}

/*
Returns the verbosity corresponding to the given string.

Errors can be:

- IllegalPropertyError in case an unknown log level is passed
*/
func ValueOfVerbosity(s string) (Verbosity, error) {
	switch s {
	case "FATAL":
		return FATAL, nil
	case "ERROR":
		return ERROR, nil
	case "WARNING":
		return WARNING, nil
	case "INFO":
		return INFO, nil
	case "DEBUG":
		return DEBUG, nil
	case "TRACE":
		return TRACE, nil
	default:
		return INFO, &errs.IllegalPropertyError{Message: fmt.Sprintf("illegal log level '%s'", s)}
	}
}
