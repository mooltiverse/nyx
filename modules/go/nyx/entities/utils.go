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

/*
Returns a pointer to the authentication method passed as parameter.

This is useful for inline assignment of a constant scheme value.
*/
func PointerToAuthenticationMethod(a AuthenticationMethod) *AuthenticationMethod {
	return &a
}

/*
Returns a pointer to the position passed as parameter.

This is useful for inline assignment of a constant scheme value.
*/
func PointerToPosition(p Position) *Position {
	return &p
}

/*
Returns a pointer to the provider passed as parameter.

This is useful for inline assignment of a constant scheme value.
*/
func PointerToProvider(p Provider) *Provider {
	return &p
}

/*
Returns a pointer to the workspace status passed as parameter.

This is useful for inline assignment of a constant verbosity value.
*/
func PointerToWorkspaceStatus(ws WorkspaceStatus) *WorkspaceStatus {
	return &ws
}

/*
Returns a pointer to the verbosity passed as parameter.

This is useful for inline assignment of a constant verbosity value.
*/
func PointerToVerbosity(v Verbosity) *Verbosity {
	return &v
}
