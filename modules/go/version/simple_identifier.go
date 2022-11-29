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

package version

import (
	"fmt"     // https://pkg.go.dev/fmt
	"reflect" // https://pkg.go.dev/reflect
)

/*
An identifier modelling a simple object value like string or integer.

In Semantic Versioning 2.0.0> parlance, an identifier can be
numeric or alphanumeric.
*/
type simpleIdentifier struct {
	// The value held by this identifier
	value interface{}
}

/*
Tests the given object for equality against this instance.
*/
func (si simpleIdentifier) equals(obj interface{}) bool {
	if obj == nil {
		return false
	}
	return reflect.DeepEqual(si, obj)
}

/*
Returns the underlying value held by this identifier.
*/
func (si simpleIdentifier) getValue() interface{} {
	return si.value
}

/*
Returns a string representation of the object, coherent with the identifier scope,
suitable for generating a version string.
*/
func (si simpleIdentifier) String() string {
	return fmt.Sprint(si.getValue())
}
