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

package api

/*
An abstraction over services.
*/
type Service interface {
	/*
		Safely checks if the underlying implementation supports the given operation. If this
		method returns true then the underlying class will not raise any
		UnsupportedOperationError when invoking the specific methods.

		Arguments are as follows:

		- feature the feature to check for support.
	*/
	Supports(feature Feature) bool
}
