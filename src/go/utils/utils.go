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
This package provides simple utils used as code and time savers within the Nyx project.
*/
package utils

/*
Returns a pointer to the boolean passed as parameter.

This is useful for inline assignment of a constant boolean value.
*/
func PointerToBoolean(b bool) *bool {
	return &b
}

/*
Returns a pointer to the int passed as parameter.

This is useful for inline assignment of a constant int value.
*/
func PointerToInt(i int) *int {
	return &i
}

/*
Returns a pointer to the int passed as parameter.

This is useful for inline assignment of a constant int value.
*/
func PointerToInt64(i int64) *int64 {
	return &i
}

/*
Returns a pointer to the string passed as parameter.

This is useful for inline assignment of a constant string value.
*/
func PointerToString(s string) *string {
	return &s
}
