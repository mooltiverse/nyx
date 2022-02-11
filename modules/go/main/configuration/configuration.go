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
This is the configuration package for Nyx, providing layered configuration logic among different configuration means.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package configuration

import (
	"fmt" // https://golang.org/pkg/fmt/
)

/*
Placeholder function, used just to test module and package dependencies when bootstrapping the Go project.

TODO: This function must be removed as soon as there is some other significant function exposed to other modules.
*/
func Check() {
	fmt.Println("Configuration is OK")
}
