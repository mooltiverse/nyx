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

/*
This package provides common errors used within the Nyx project.
*/
package errors

/*
This error models an issue pertaining data access. Examples are when data can't be
read from or written to the file system or from/to the network.

You can create errors like this as:
&DataAccessError{Message: fmt.Sprintf("error accessing data: %s", description)}
*/
type DataAccessError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e DataAccessError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e DataAccessError) GetCause() error {
	return e.Cause
}

/*
A generic error raised when some Git related issue is encountered.

You can create errors like this as:
&GitError{Message: "illegal operation ...."}
*/
type GitError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e GitError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e GitError) GetCause() error {
	return e.Cause
}

/*
This error models an issue pertaining the value of an argument.

You can create errors like this as:
&IllegalArgumentError{Message: fmt.Sprintf("illegal argument '%s'", propertyName)}
*/
type IllegalArgumentError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e IllegalArgumentError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e IllegalArgumentError) GetCause() error {
	return e.Cause
}

/*
This error models an issue pertaining the value of a property.

You can create errors like this as:
&IllegalPropertyError{Message: fmt.Sprintf("illegal property '%s'", propertyName)}
*/
type IllegalPropertyError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e IllegalPropertyError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e IllegalPropertyError) GetCause() error {
	return e.Cause
}

/*
This error models an issue pertaining the current state of an object.

You can create errors like this as:
&IllegalStateError{Message: "illegal state ...."}
*/
type IllegalStateError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e IllegalStateError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e IllegalStateError) GetCause() error {
	return e.Cause
}

/*
This error models an issue pertaining issues on I/O.

You can create errors like this as:
&IOError{Message: "error is ...."}
*/
type IOError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e IOError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e IOError) GetCause() error {
	return e.Cause
}

/*
This error models an issue about a value being nil when nil is not allowed.

You can create errors like this as:
&NilPointerError{Message: fmt.Sprintf("nil pointer '%s'", propertyName)}
*/
type NilPointerError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e NilPointerError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e NilPointerError) GetCause() error {
	return e.Cause
}

/*
This error models an issue about a regular expression syntax.

You can create errors like this as:
&PatternSyntaxError{Message: fmt.Sprintf("regular expression '%s' can't be compiled", expression)}
*/
type PatternSyntaxError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e PatternSyntaxError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e PatternSyntaxError) GetCause() error {
	return e.Cause
}

/*
An error raised when Nyx encounters an issue during the business operations.

You can create errors like this as:
&ReleaseError{Message: fmt.Sprintf("release error: %s", description)}
*/
type ReleaseError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e ReleaseError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e ReleaseError) GetCause() error {
	return e.Cause
}

/*
An error meaning that security constraints failed their checks.

You can create errors like this as:
&SecurityError{Message: fmt.Sprintf("security error: %s", description)}
*/
type SecurityError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e SecurityError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e SecurityError) GetCause() error {
	return e.Cause
}

/*
This error models an issue pertaining a service.

You can create errors like this as:
&ServiceError{Message: fmt.Sprintf("service error: %s", description)}
*/
type ServiceError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e ServiceError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e ServiceError) GetCause() error {
	return e.Cause
}

/*
An error meaning that something in the transport or connection went wrong.

You can create errors like this as:
&TransportError{Message: fmt.Sprintf("error accessing data: %s", description)}
*/
type TransportError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e TransportError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e TransportError) GetCause() error {
	return e.Cause
}

/*
An error meaning that a certain operation is not supported by the implementation.

You can create errors like this as:
&UnsupportedOperationError{Message: fmt.Sprintf("operation is not supported: %s", description)}
*/
type UnsupportedOperationError struct {
	// The error message
	Message string

	// The optional wrapped error
	Cause error
}

// Returns the error message
func (e UnsupportedOperationError) Error() string {
	if e.Cause == nil {
		return e.Message
	} else {
		return e.Message + ": " + e.Cause.Error()
	}
}

// Returns the wrapped error, if any, or nil
func (e UnsupportedOperationError) GetCause() error {
	return e.Cause
}
