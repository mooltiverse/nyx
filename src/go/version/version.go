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

/*
The interface of Version classes.
*/
type Version interface {
	/*
		Returns a new instance with the number identified by the given value bumped. The supported identifiers depend
		on the concrete subclass.

		Arguments are as follows:

		- id the name of the identifier to bump

		Errors can be returned if:

		- the given string is empty, contains illegal characters or does not represent
		  a valid identifier to be bumped
	*/
	BumpVersion(id string) (Version, error)

	/*
		Returns true if this version is equal to the given object, false otherwise
	*/
	Equals(obj interface{}) bool

	/*
		Returns the scheme that identifies the implementation
	*/
	GetScheme() Scheme

	/*
		Returns the string representation of this version
	*/
	String() string
}
