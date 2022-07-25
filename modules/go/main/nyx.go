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
This is the main package for Nyx, also providing the entry point.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package main

import (
	"fmt" // https://golang.org/pkg/fmt/

	configuration "github.com/mooltiverse/nyx/modules/go/main/configuration"
	version "github.com/mooltiverse/nyx/modules/go/version"
)

const ()

var (
	release = "undefined" // This must be passed using 'go build' ldflags
)

/*
Entry point.
*/
func main() {
	fmt.Println("Main is OK")
	configuration.Check()
	version.Check()
}
