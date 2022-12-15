//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

/*
 * Copyright 2020 Mooltiverse
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package command_test

import (
	"fmt"     // https://pkg.go.dev/fmt
	"os"      // https://pkg.go.dev/os
	"strings" // https://pkg.go.dev/strings
	"testing" // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	cmdtpl "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/command/template"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

var (

	/*
	   A fixture with valid structured data to test parseable (into version range regexps) branch names
	   and the version range check regular expressions to be generated from them.

	   Each returned argument has the fields:
	   - branch name: the name of a branch
	   - regex: the regular expression that is expected to be generated from that branch name
	*/
	wellKnownParseableBranchNames = []struct {
		branchName string
		regex      string
	}{
		// the underlying Git library (go-git) doesn't cope very well with so many branches in a repository
		// and causes panic. This is why some of the following are commented out, so we reduce the number of branches
		{branchName: "1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "v1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		/*{branchName: "rel1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},*/
		{branchName: "rel1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		/*{branchName: "relv1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},*/
		{branchName: "rel/1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		/*{branchName: "rel/1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},*/
		{branchName: "rel/v1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		/*{branchName: "rel/v1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.x.x", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2.x", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2.3", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},*/

		{branchName: "1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		/*{branchName: "v1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "v1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "relv1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relv1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/v1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.x.x-abc.123+def.456", regex: "^1\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2.x-abc.123+def.456", regex: "^1\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-v1.2.3-abc.123+def.456", regex: "^1\\.2\\.3(?:(?:-|\\+).*)?$"},*/

		{branchName: "x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		/*{branchName: "vx", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "relx", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "relvx", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.x.x", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2.x", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2.3", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},*/

		{branchName: "x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "x.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		/*{branchName: "vx-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "vx.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "relx-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relx.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "relvx-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "relvx.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/x.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "rel/vx.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-x.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.x.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2.x-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.(0|[1-9]\\d*)(?:(?:-|\\+).*)?$"},
		{branchName: "release-vx.2.3-abc.123+def.456", regex: "^(0|[1-9]\\d*)\\.2\\.3(?:(?:-|\\+).*)?$"},*/
	}

	/*
	   A fixture with valid structured data to test unparseable (into version range regexps) branch names.

	   Each returned argument has the fields:
	   - branch name: the name of a branch
	*/
	wellKnownUnparseableBranchNames = []struct {
		branchName string
	}{
		// these values are unparseable because of invalid suffixes not separated by - or +
		{branchName: "1abc"},
		{branchName: "1.xabc"},
		{branchName: "1.x.xabc"},
		{branchName: "1.2abc"},
		{branchName: "1.2.xabc"},
		{branchName: "1.2.3abc"},
		{branchName: "xabc"},
		{branchName: "x.xabc"},
		{branchName: "x.x.xabc"},
		{branchName: "x.2abc"},
		{branchName: "x.2.xabc"},
		{branchName: "x.2.3abc"},
		{branchName: "vabc"},
		{branchName: "v1abc"},
		{branchName: "v1.xabc"},
		{branchName: "v1.x.xabc"},
		{branchName: "v1.2abc"},
		{branchName: "v1.2.3abc"},
		{branchName: "vxabc"},
		{branchName: "vx.xabc"},
		{branchName: "vx.x.xabc"},
		{branchName: "vx.2abc"},
		{branchName: "vx.2.3abc"},
		{branchName: "rel1abc"},
		{branchName: "rel1.xabc"},
		{branchName: "rel1.x.xabc"},
		{branchName: "rel1.2abc"},
		{branchName: "rel1.2.xabc"},
		{branchName: "rel1.2.3abc"},
		{branchName: "relxabc"},
		{branchName: "relx.xabc"},
		{branchName: "relx.x.xabc"},
		{branchName: "relx.2abc"},
		{branchName: "relx.2.xabc"},
		{branchName: "relx.2.3abc"},
		{branchName: "relvabc"},
		{branchName: "relv1abc"},
		{branchName: "relv1.xabc"},
		{branchName: "relv1.x.xabc"},
		{branchName: "relv1.2abc"},
		{branchName: "relv1.2.3abc"},
		{branchName: "relvabc"},
		{branchName: "relvxabc"},
		{branchName: "relvx.xabc"},
		{branchName: "relvx.x.xabc"},
		{branchName: "relvx.2abc"},
		{branchName: "relvx.2.3abc"},
		{branchName: "rel/1abc"},
		{branchName: "rel/1.xabc"},
		{branchName: "rel/1.x.xabc"},
		{branchName: "rel/1.2abc"},
		{branchName: "rel/1.2.xabc"},
		{branchName: "rel/1.2.3abc"},
		{branchName: "rel/xabc"},
		{branchName: "rel/x.xabc"},
		{branchName: "rel/x.x.xabc"},
		{branchName: "rel/x.2abc"},
		{branchName: "rel/x.2.xabc"},
		{branchName: "rel/x.2.3abc"},
		{branchName: "rel/vabc"},
		{branchName: "rel/v1abc"},
		{branchName: "rel/v1.xabc"},
		{branchName: "rel/v1.x.xabc"},
		{branchName: "rel/v1.2abc"},
		{branchName: "rel/v1.2.3abc"},
		{branchName: "rel/vabc"},
		{branchName: "rel/vxabc"},
		{branchName: "rel/vx.xabc"},
		{branchName: "rel/vx.x.xabc"},
		{branchName: "rel/vx.2abc"},
		{branchName: "rel/vx.2.3abc"},
		{branchName: "release-1abc"},
		{branchName: "release-1.xabc"},
		{branchName: "release-1.x.xabc"},
		{branchName: "release-1.2abc"},
		{branchName: "release-1.2.xabc"},
		{branchName: "release-1.2.3abc"},
		{branchName: "release-xabc"},
		{branchName: "release-x.xabc"},
		{branchName: "release-x.x.xabc"},
		{branchName: "release-x.2abc"},
		{branchName: "release-x.2.xabc"},
		{branchName: "release-x.2.3abc"},
		{branchName: "release-vabc"},
		{branchName: "release-v1abc"},
		{branchName: "release-v1.xabc"},
		{branchName: "release-v1.x.xabc"},
		{branchName: "release-v1.2abc"},
		{branchName: "release-v1.2.3abc"},
		{branchName: "release-vabc"},
		{branchName: "release-vxabc"},
		{branchName: "release-vx.xabc"},
		{branchName: "release-vx.x.xabc"},
		{branchName: "release-vx.2abc"},
		{branchName: "release-vx.2.3abc"},
	}
)

/*
Returns true if s1 contains all the items from s2.
*/
func containsAllIdentifiers(s1 *[]*ent.Identifier, s2 *[]*ent.Identifier) bool {
	if len(*s2) > len(*s1) {
		return false
	}
	if s1 != nil && s2 != nil {
		for _, s2Item := range *s2 {
			found := false
			for _, s1Item := range *s1 {
				if (((*s1Item).GetQualifier() == nil && (*s2Item).GetQualifier() == nil) || *((*s1Item).GetQualifier()) == *((*s2Item).GetQualifier())) &&
					(((*s1Item).GetValue() == nil && (*s2Item).GetValue() == nil) || *((*s1Item).GetValue()) == *((*s2Item).GetValue())) &&
					(((*s1Item).GetPosition() == nil && (*s2Item).GetPosition() == nil) || *((*s1Item).GetPosition()) == *((*s2Item).GetPosition())) {
					found = true
					continue
				}
			}
			if !found {
				return false
			}
		}
		return true
	} else {
		return false
	}
}

func TestInferConstructor(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, command)
		})
	}
}

/*
Check that the State method never returns a nil object
*/
func TestInferState(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, (*command).State())
		})
	}
}

/*
Check that the IsUpToDate() returns false when the command instance is just created and true after one execution in a repository
with at least one commit and in a clean state.
*/
func TestInferIsUpToDate(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// running in an empty repository, with no commits, throws an error
			_, err = (*command).Run()
			assert.Error(t, err)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// and running again with no changes must still be up to date
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that the IsUpToDate() always returns true even when the repository is dirty.
*/
func TestInferIsUpToDateInDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// if we add uncommitted files it must return still return true
			(*command).Script().AddRandomTextWorkbenchFiles(1)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// still true even after staging
			(*command).Script().Stage()
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// but returns false with a new commit as long as it doesn't run again
			(*command).Script().AndCommitWithTag("111.122.144")
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result with a commit message convention configured
*/
func TestInferIdempotencyWithCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"minor": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run a first time
			_, err := (*command).Run()
			assert.NoError(t, err)

			// collect its state values
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newRelease, _ := (*command).State().GetNewRelease()
			newVersion, _ := (*command).State().GetNewVersion()
			releaseScope, _ := (*command).State().GetReleaseScope()
			commits := releaseScope.GetCommits()
			significantCommits := releaseScope.GetSignificantCommits()
			previousVersion := releaseScope.GetPreviousVersion()
			previousVersionCommit := releaseScope.GetPreviousVersionCommit()
			releaseType, _ := (*command).State().GetReleaseType()
			matchBranches := releaseType.GetMatchBranches()
			scheme, _ := (*command).State().GetScheme()
			timestamp, _ := (*command).State().GetTimestamp()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()

			// collect repository values
			branches := (*command).Script().GetBranches()
			commitIDs := (*command).Script().GetCommitIDs()
			lastCommitID := (*command).Script().GetLastCommitID()
			tags := (*command).Script().GetTags()

			// run again and check that all values are still the same
			upToDate, err := (*command).IsUpToDate()
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ := (*command).State().GetBranch()
			bump2, _ := (*command).State().GetBump()
			newRelease2, _ := (*command).State().GetNewRelease()
			newVersion2, _ := (*command).State().GetNewVersion()
			releaseScope2, _ := (*command).State().GetReleaseScope()
			commits2 := releaseScope2.GetCommits()
			significantCommits2 := releaseScope2.GetSignificantCommits()
			previousVersion2 := releaseScope2.GetPreviousVersion()
			previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ := (*command).State().GetReleaseType()
			matchBranches2 := releaseType2.GetMatchBranches()
			scheme2, _ := (*command).State().GetScheme()
			timestamp2, _ := (*command).State().GetTimestamp()
			version2, _ := (*command).State().GetVersion()
			versionRange2, _ := (*command).State().GetVersionRange()
			branches2 := (*command).Script().GetBranches()
			commitIDs2 := (*command).Script().GetCommitIDs()
			lastCommitID2 := (*command).Script().GetLastCommitID()
			tags2 := (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			upToDate, err = (*command).IsUpToDate()
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// check that some values have changed
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.NotEqual(t, commits, commits2)
			assert.NotEqual(t, significantCommits, significantCommits2)
			assert.NotEqual(t, previousVersion, previousVersion2)
			assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, timestamp, timestamp2)
			assert.NotEqual(t, version, version2)
			assert.NotEqual(t, commitIDs, commitIDs2)
			assert.NotEqual(t, lastCommitID, lastCommitID2)
			assert.NotEqual(t, tags, tags2)

			// collect state values again
			branch, _ = (*command).State().GetBranch()
			bump, _ = (*command).State().GetBump()
			newRelease, _ = (*command).State().GetNewRelease()
			newVersion, _ = (*command).State().GetNewVersion()
			releaseScope, _ = (*command).State().GetReleaseScope()
			commits = releaseScope.GetCommits()
			significantCommits = releaseScope.GetSignificantCommits()
			previousVersion = releaseScope.GetPreviousVersion()
			previousVersionCommit = releaseScope.GetPreviousVersionCommit()
			releaseType, _ = (*command).State().GetReleaseType()
			matchBranches = releaseType.GetMatchBranches()
			scheme, _ = (*command).State().GetScheme()
			timestamp, _ = (*command).State().GetTimestamp()
			version, _ = (*command).State().GetVersion()
			versionRange, _ = (*command).State().GetVersionRange()

			// collect repository values again
			branches = (*command).Script().GetBranches()
			commitIDs = (*command).Script().GetCommitIDs()
			lastCommitID = (*command).Script().GetLastCommitID()
			tags = (*command).Script().GetTags()

			// run again and make sure values didn't change
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// once more, also considering that its still up to date
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result without a commit message convention configured
*/
func TestInferIdempotencyWithoutCommitMessageConvention(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// run a first time
			_, err := (*command).Run()
			assert.NoError(t, err)

			// collect its state values
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newRelease, _ := (*command).State().GetNewRelease()
			newVersion, _ := (*command).State().GetNewVersion()
			releaseScope, _ := (*command).State().GetReleaseScope()
			commits := releaseScope.GetCommits()
			significantCommits := releaseScope.GetSignificantCommits()
			previousVersion := releaseScope.GetPreviousVersion()
			previousVersionCommit := releaseScope.GetPreviousVersionCommit()
			releaseType, _ := (*command).State().GetReleaseType()
			matchBranches := releaseType.GetMatchBranches()
			scheme, _ := (*command).State().GetScheme()
			timestamp, _ := (*command).State().GetTimestamp()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()

			// collect repository values
			branches := (*command).Script().GetBranches()
			commitIDs := (*command).Script().GetCommitIDs()
			lastCommitID := (*command).Script().GetLastCommitID()
			tags := (*command).Script().GetTags()

			// run again and check that all values are still the same
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ := (*command).State().GetBranch()
			bump2, _ := (*command).State().GetBump()
			newRelease2, _ := (*command).State().GetNewRelease()
			newVersion2, _ := (*command).State().GetNewVersion()
			releaseScope2, _ := (*command).State().GetReleaseScope()
			commits2 := releaseScope2.GetCommits()
			significantCommits2 := releaseScope2.GetSignificantCommits()
			previousVersion2 := releaseScope2.GetPreviousVersion()
			previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ := (*command).State().GetReleaseType()
			matchBranches2 := releaseType2.GetMatchBranches()
			scheme2, _ := (*command).State().GetScheme()
			timestamp2, _ := (*command).State().GetTimestamp()
			version2, _ := (*command).State().GetVersion()
			versionRange2, _ := (*command).State().GetVersionRange()
			branches2 := (*command).Script().GetBranches()
			commitIDs2 := (*command).Script().GetCommitIDs()
			lastCommitID2 := (*command).Script().GetLastCommitID()
			tags2 := (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// check that some values have changed
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.NotEqual(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.NotEqual(t, previousVersion, previousVersion2)
			assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, timestamp, timestamp2)
			assert.NotEqual(t, version, version2)
			assert.NotEqual(t, commitIDs, commitIDs2)
			assert.NotEqual(t, lastCommitID, lastCommitID2)
			assert.NotEqual(t, tags, tags2)

			// collect state values again
			branch, _ = (*command).State().GetBranch()
			bump, _ = (*command).State().GetBump()
			newRelease, _ = (*command).State().GetNewRelease()
			newVersion, _ = (*command).State().GetNewVersion()
			releaseScope, _ = (*command).State().GetReleaseScope()
			commits = releaseScope.GetCommits()
			significantCommits = releaseScope.GetSignificantCommits()
			previousVersion = releaseScope.GetPreviousVersion()
			previousVersionCommit = releaseScope.GetPreviousVersionCommit()
			releaseType, _ = (*command).State().GetReleaseType()
			matchBranches = releaseType.GetMatchBranches()
			scheme, _ = (*command).State().GetScheme()
			timestamp, _ = (*command).State().GetTimestamp()
			version, _ = (*command).State().GetVersion()
			versionRange, _ = (*command).State().GetVersionRange()

			// collect repository values again
			branches = (*command).Script().GetBranches()
			commitIDs = (*command).Script().GetCommitIDs()
			lastCommitID = (*command).Script().GetLastCommitID()
			tags = (*command).Script().GetTags()

			// run again and make sure values didn't change
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// once more, also considering that its still up to date
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

/*
Check that multiple runs yield to the same result without a commit message convention configured
*/
func TestInferIdempotencyInDirtyRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// run a first time
			_, err := (*command).Run()
			assert.NoError(t, err)

			// collect its state values
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newRelease, _ := (*command).State().GetNewRelease()
			newVersion, _ := (*command).State().GetNewVersion()
			releaseScope, _ := (*command).State().GetReleaseScope()
			commits := releaseScope.GetCommits()
			significantCommits := releaseScope.GetSignificantCommits()
			previousVersion := releaseScope.GetPreviousVersion()
			previousVersionCommit := releaseScope.GetPreviousVersionCommit()
			releaseType, _ := (*command).State().GetReleaseType()
			matchBranches := releaseType.GetMatchBranches()
			scheme, _ := (*command).State().GetScheme()
			timestamp, _ := (*command).State().GetTimestamp()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()

			// collect repository values
			branches := (*command).Script().GetBranches()
			commitIDs := (*command).Script().GetCommitIDs()
			lastCommitID := (*command).Script().GetLastCommitID()
			tags := (*command).Script().GetTags()

			// add some uncommitted changes
			(*command).Script().UpdateAllWorkbenchFiles()

			// run again and check that all values are still the same
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ := (*command).State().GetBranch()
			bump2, _ := (*command).State().GetBump()
			newRelease2, _ := (*command).State().GetNewRelease()
			newVersion2, _ := (*command).State().GetNewVersion()
			releaseScope2, _ := (*command).State().GetReleaseScope()
			commits2 := releaseScope2.GetCommits()
			significantCommits2 := releaseScope2.GetSignificantCommits()
			previousVersion2 := releaseScope2.GetPreviousVersion()
			previousVersionCommit2 := releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ := (*command).State().GetReleaseType()
			matchBranches2 := releaseType2.GetMatchBranches()
			scheme2, _ := (*command).State().GetScheme()
			timestamp2, _ := (*command).State().GetTimestamp()
			version2, _ := (*command).State().GetVersion()
			versionRange2, _ := (*command).State().GetVersionRange()
			branches2 := (*command).Script().GetBranches()
			commitIDs2 := (*command).Script().GetCommitIDs()
			lastCommitID2 := (*command).Script().GetLastCommitID()
			tags2 := (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// add some commits to the repository and after one run the task should be up to date
			(*command).Script().AndCommitWithTag("111.122.133")
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// check that some values have changed
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.NotEqual(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.NotEqual(t, previousVersion, previousVersion2)
			assert.NotEqual(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, timestamp, timestamp2)
			assert.NotEqual(t, version, version2)
			assert.NotEqual(t, commitIDs, commitIDs2)
			assert.NotEqual(t, lastCommitID, lastCommitID2)
			assert.NotEqual(t, tags, tags2)

			// collect state values again
			branch, _ = (*command).State().GetBranch()
			bump, _ = (*command).State().GetBump()
			newRelease, _ = (*command).State().GetNewRelease()
			newVersion, _ = (*command).State().GetNewVersion()
			releaseScope, _ = (*command).State().GetReleaseScope()
			commits = releaseScope.GetCommits()
			significantCommits = releaseScope.GetSignificantCommits()
			previousVersion = releaseScope.GetPreviousVersion()
			previousVersionCommit = releaseScope.GetPreviousVersionCommit()
			releaseType, _ = (*command).State().GetReleaseType()
			matchBranches = releaseType.GetMatchBranches()
			scheme, _ = (*command).State().GetScheme()
			timestamp, _ = (*command).State().GetTimestamp()
			version, _ = (*command).State().GetVersion()
			versionRange, _ = (*command).State().GetVersionRange()

			// collect repository values again
			branches = (*command).Script().GetBranches()
			commitIDs = (*command).Script().GetCommitIDs()
			lastCommitID = (*command).Script().GetLastCommitID()
			tags = (*command).Script().GetTags()

			// add some uncommitted changes
			(*command).Script().UpdateAllWorkbenchFiles()

			// run again and make sure values didn't change
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)

			// add some uncommitted changes
			(*command).Script().UpdateAllWorkbenchFiles()

			// once more, also considering that its still up to date
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
			_, err = (*command).Run()
			assert.NoError(t, err)

			branch2, _ = (*command).State().GetBranch()
			bump2, _ = (*command).State().GetBump()
			newRelease2, _ = (*command).State().GetNewRelease()
			newVersion2, _ = (*command).State().GetNewVersion()
			releaseScope2, _ = (*command).State().GetReleaseScope()
			commits2 = releaseScope2.GetCommits()
			significantCommits2 = releaseScope2.GetSignificantCommits()
			previousVersion2 = releaseScope2.GetPreviousVersion()
			previousVersionCommit2 = releaseScope2.GetPreviousVersionCommit()
			releaseType2, _ = (*command).State().GetReleaseType()
			matchBranches2 = releaseType2.GetMatchBranches()
			scheme2, _ = (*command).State().GetScheme()
			timestamp2, _ = (*command).State().GetTimestamp()
			version2, _ = (*command).State().GetVersion()
			versionRange2, _ = (*command).State().GetVersionRange()
			branches2 = (*command).Script().GetBranches()
			commitIDs2 = (*command).Script().GetCommitIDs()
			lastCommitID2 = (*command).Script().GetLastCommitID()
			tags2 = (*command).Script().GetTags()
			assert.Equal(t, branch, branch2)
			assert.Equal(t, bump, bump2)
			assert.Equal(t, newRelease, newRelease2)
			assert.Equal(t, newVersion, newVersion2)
			assert.Equal(t, commits, commits2)
			assert.Equal(t, significantCommits, significantCommits2)
			assert.Equal(t, previousVersion, previousVersion2)
			assert.Equal(t, previousVersionCommit, previousVersionCommit2)
			assert.Equal(t, matchBranches, matchBranches2)
			assert.Equal(t, scheme, scheme2)
			assert.Equal(t, timestamp, timestamp2)
			assert.Equal(t, version, version2)
			assert.Equal(t, versionRange, versionRange2)
			assert.Equal(t, branches, branches2)
			assert.Equal(t, commitIDs, commitIDs2)
			assert.Equal(t, lastCommitID, lastCommitID2)
			assert.Equal(t, tags, tags2)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchReleaseTypeWithNonExistingReleaseTypeThrowsError(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetGitCommitMessage(utl.PointerToString("MATCHED"))      // use this value to see if the release type has been matched
			releaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			releaseType.SetMatchEnvironmentVariables(nil)
			releaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("nonexisting"), utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.Error(t, err)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchReleaseTypeBasedOnBranchName(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			unmatchedReleaseType := ent.NewReleaseType()
			unmatchedReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED"))        // use this value to see if the release type has been matched
			unmatchedReleaseType.SetMatchBranches(utl.PointerToString("^nonexistendbranch$")) // match only a branch name that doesn't exist
			unmatchedReleaseType.SetMatchEnvironmentVariables(nil)
			unmatchedReleaseType.SetMatchWorkspaceStatus(nil)
			matchedReleaseType := ent.NewReleaseType()
			matchedReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED"))      // use this value to see if the release type has been matched
			matchedReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			matchedReleaseType.SetMatchEnvironmentVariables(nil)
			matchedReleaseType.SetMatchWorkspaceStatus(nil)
			fallbackReleaseType := ent.NewReleaseType()
			fallbackReleaseType.SetGitCommitMessage(utl.PointerToString("FALLBACK")) // use this value to see if the release type has been matched
			//fallbackReleaseType.SetMatchBranches("")           // match any branch name
			fallbackReleaseType.SetMatchEnvironmentVariables(nil)
			fallbackReleaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("unmatched"), utl.PointerToString("matched"), utl.PointerToString("fallback")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"unmatched": unmatchedReleaseType, "matched": matchedReleaseType, "fallback": fallbackReleaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the master branch must match the first release type
			(*command).Script().Checkout("master")
			_, err := (*command).Run()
			assert.NoError(t, err)
			releaseType, _ := (*command).State().GetReleaseType()
			assert.Equal(t, "MATCHED", *releaseType.GetGitCommitMessage())
			assert.Equal(t, "^(master|main)$", *releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Nil(t, releaseType.GetMatchWorkspaceStatus())

			// any other branch must match the fallback release type
			(*command).Script().Checkout("integration")
			_, err = (*command).Run()
			assert.NoError(t, err)
			releaseType, _ = (*command).State().GetReleaseType()
			assert.Equal(t, "FALLBACK", *releaseType.GetGitCommitMessage())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Nil(t, releaseType.GetMatchWorkspaceStatus())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchReleaseTypeBasedOnEnvironmentVariables(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// since we can't set environment variables here we just use the PATH variable, which is always present
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			unmatchedReleaseType := ent.NewReleaseType()
			unmatchedReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED")) // use this value to see if the release type has been matched
			//unmatchedReleaseType.SetMatchBranches("") // match any branch name
			unmatchedReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": "^nonexistingvalue$"}) // require the PATH variable to be present, with a non existing value
			unmatchedReleaseType.SetMatchWorkspaceStatus(nil)
			matchedPathReleaseType := ent.NewReleaseType()
			matchedPathReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED PATH")) // use this value to see if the release type has been matched
			//matchedPathReleaseType.SetMatchBranches("") // match any branch name
			matchedPathReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": ".*"})
			matchedPathReleaseType.SetMatchWorkspaceStatus(nil)
			fallbackReleaseType := ent.NewReleaseType()
			fallbackReleaseType.SetGitCommitMessage(utl.PointerToString("FALLBACK")) // use this value to see if the release type has been matched
			//fallbackReleaseType.SetMatchBranches("")           // match any branch name
			fallbackReleaseType.SetMatchEnvironmentVariables(nil)
			fallbackReleaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("unmatched"), utl.PointerToString("matchedpath"), utl.PointerToString("fallback")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"unmatched": unmatchedReleaseType, "matchedpath": matchedPathReleaseType, "fallback": fallbackReleaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the PATH variable must be matched (with any value)
			_, err := (*command).Run()
			assert.NoError(t, err)
			releaseType, _ := (*command).State().GetReleaseType()
			assert.Equal(t, "MATCHED PATH", *releaseType.GetGitCommitMessage())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.NotNil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Nil(t, releaseType.GetMatchWorkspaceStatus())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchCleanReleaseTypeBasedOnWorkspaceStatus(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			unmatchedReleaseType := ent.NewReleaseType()
			unmatchedReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED"))        // use this value to see if the release type has been matched
			unmatchedReleaseType.SetMatchBranches(utl.PointerToString("^nonexistendbranch$")) // match only a branch name that doesn't exist
			unmatchedReleaseType.SetMatchEnvironmentVariables(nil)
			unmatchedReleaseType.SetMatchWorkspaceStatus(nil)
			matchedCleanReleaseType := ent.NewReleaseType()
			matchedCleanReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED CLEAN")) // use this value to see if the release type has been matched
			//matchedCleanReleaseType.SetMatchBranches("") // match any branch name
			matchedCleanReleaseType.SetMatchEnvironmentVariables(nil)
			matchedCleanReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN)) // the workspace must be clean to match
			matchedDirtyReleaseType := ent.NewReleaseType()
			matchedDirtyReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED DIRTY")) // use this value to see if the release type has been matched
			//matchedDirtyReleaseType.SetMatchBranches("") // match any branch name
			matchedDirtyReleaseType.SetMatchEnvironmentVariables(nil)
			matchedDirtyReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.DIRTY)) // the workspace must be clean to match
			fallbackReleaseType := ent.NewReleaseType()
			fallbackReleaseType.SetGitCommitMessage(utl.PointerToString("FALLBACK")) // use this value to see if the release type has been matched
			//fallbackReleaseType.SetMatchBranches("")           // match any branch name
			fallbackReleaseType.SetMatchEnvironmentVariables(nil)
			fallbackReleaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("unmatched"), utl.PointerToString("matchedclean"), utl.PointerToString("matcheddirty"), utl.PointerToString("fallback")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"unmatched": unmatchedReleaseType, "matchedclean": matchedCleanReleaseType, "matcheddirty": matchedDirtyReleaseType, "fallback": fallbackReleaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the CLEAN release type must be matched
			(*command).Script().Checkout("master")
			_, err := (*command).Run()
			assert.NoError(t, err)
			releaseType, _ := (*command).State().GetReleaseType()
			assert.Equal(t, "MATCHED CLEAN", *releaseType.GetGitCommitMessage())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.CLEAN, *releaseType.GetMatchWorkspaceStatus())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchDirtyReleaseTypeBasedOnWorkspaceStatus(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			unmatchedReleaseType := ent.NewReleaseType()
			unmatchedReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED"))        // use this value to see if the release type has been matched
			unmatchedReleaseType.SetMatchBranches(utl.PointerToString("^nonexistendbranch$")) // match only a branch name that doesn't exist
			unmatchedReleaseType.SetMatchEnvironmentVariables(nil)
			unmatchedReleaseType.SetMatchWorkspaceStatus(nil)
			matchedCleanReleaseType := ent.NewReleaseType()
			matchedCleanReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED CLEAN")) // use this value to see if the release type has been matched
			//matchedCleanReleaseType.SetMatchBranches("") // match any branch name
			matchedCleanReleaseType.SetMatchEnvironmentVariables(nil)
			matchedCleanReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN)) // the workspace must be clean to match
			matchedDirtyReleaseType := ent.NewReleaseType()
			matchedDirtyReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED DIRTY")) // use this value to see if the release type has been matched
			//matchedDirtyReleaseType.SetMatchBranches("") // match any branch name
			matchedDirtyReleaseType.SetMatchEnvironmentVariables(nil)
			matchedDirtyReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.DIRTY)) // the workspace must be clean to match
			fallbackReleaseType := ent.NewReleaseType()
			fallbackReleaseType.SetGitCommitMessage(utl.PointerToString("FALLBACK")) // use this value to see if the release type has been matched
			//fallbackReleaseType.SetMatchBranches("")           // match any branch name
			fallbackReleaseType.SetMatchEnvironmentVariables(nil)
			fallbackReleaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("unmatched"), utl.PointerToString("matchedclean"), utl.PointerToString("matcheddirty"), utl.PointerToString("fallback")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"unmatched": unmatchedReleaseType, "matchedclean": matchedCleanReleaseType, "matcheddirty": matchedDirtyReleaseType, "fallback": fallbackReleaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// make uncommitted changes
			(*command).Script().UpdateAllWorkbenchFiles()

			// the DIRTY release type must be matched
			_, err := (*command).Run()
			assert.NoError(t, err)
			releaseType, _ := (*command).State().GetReleaseType()
			assert.Equal(t, "MATCHED DIRTY", *releaseType.GetGitCommitMessage())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.DIRTY, *releaseType.GetMatchWorkspaceStatus())
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferMatchReleaseTypeBasedOnBranchNameAndEnvironmentVariablesAndWorkspaceStatus(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// since we can't set environment variables here we just use the PATH variable, which is always present
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add some fictional release types
			unmatchedReleaseType := ent.NewReleaseType()
			unmatchedReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED"))                          // use this value to see if the release type has been matched
			unmatchedReleaseType.SetMatchBranches(utl.PointerToString("^nonexistendbranch$"))                   // match only a branch name that doesn't exist
			unmatchedReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": "^nonexistingvalue$"}) // require the PATH variable to be present, with a non existing value
			unmatchedReleaseType.SetMatchWorkspaceStatus(nil)
			unmatchedByBranchReleaseType := ent.NewReleaseType()
			unmatchedByBranchReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED BY BRANCH"))  // use this value to see if the release type has been matched
			unmatchedByBranchReleaseType.SetMatchBranches(utl.PointerToString("^nonexistendbranch$"))     // match only a branch name that doesn't exist
			unmatchedByBranchReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": ".*"})   // require the PATH variable to be present, with any value
			unmatchedByBranchReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN)) // the workspace must be clean to match
			unmatchedByEnvironmentVariablesReleaseType := ent.NewReleaseType()
			unmatchedByEnvironmentVariablesReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED BY ENVIRONMENT VARIABLES")) // use this value to see if the release type has been matched
			unmatchedByEnvironmentVariablesReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$"))                       // match main and master
			unmatchedByEnvironmentVariablesReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": "^nonexistingvalue$"}) // require the PATH variable to be present, with a non existing value
			unmatchedByEnvironmentVariablesReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN))               // the workspace must be clean to match
			unmatchedByWorkspaceStatusReleaseType := ent.NewReleaseType()
			unmatchedByWorkspaceStatusReleaseType.SetGitCommitMessage(utl.PointerToString("UNMATCHED BY WORKSPACE STATUS")) // use this value to see if the release type has been matched
			unmatchedByWorkspaceStatusReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$"))                  // match main and master
			unmatchedByWorkspaceStatusReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": ".*"})            // require the PATH variable to be present, with any value
			unmatchedByWorkspaceStatusReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.DIRTY))          // the workspace must be dirty to match
			matchedReleaseType := ent.NewReleaseType()
			matchedReleaseType.SetGitCommitMessage(utl.PointerToString("MATCHED"))              // use this value to see if the release type has been matched
			matchedReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$"))         // match main and master
			matchedReleaseType.SetMatchEnvironmentVariables(&map[string]string{"PATH": ".*"})   // require the PATH variable to be present, with any value
			matchedReleaseType.SetMatchWorkspaceStatus(ent.PointerToWorkspaceStatus(ent.CLEAN)) // the workspace must be clean to match
			fallbackReleaseType := ent.NewReleaseType()
			fallbackReleaseType.SetGitCommitMessage(utl.PointerToString("FALLBACK")) // use this value to see if the release type has been matched
			//fallbackReleaseType.SetMatchBranches("")           // match any branch name
			fallbackReleaseType.SetMatchEnvironmentVariables(nil)
			fallbackReleaseType.SetMatchWorkspaceStatus(nil)
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("unmatched"), utl.PointerToString("unmatchedbybranch"), utl.PointerToString("unmatchedbyenvironmentvariables"), utl.PointerToString("unmatchedbyworkspacestatus"), utl.PointerToString("matched"), utl.PointerToString("fallback")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"unmatched": unmatchedReleaseType, "unmatchedbybranch": unmatchedByBranchReleaseType, "unmatchedbyenvironmentvariables": unmatchedByEnvironmentVariablesReleaseType, "unmatchedbyworkspacestatus": unmatchedByWorkspaceStatusReleaseType, "matched": matchedReleaseType, "fallback": fallbackReleaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().Checkout("master")
			// the PATH variable must be matched (with any value)
			_, err := (*command).Run()
			assert.NoError(t, err)
			releaseType, _ := (*command).State().GetReleaseType()
			assert.Equal(t, "MATCHED", *releaseType.GetGitCommitMessage())
			assert.Equal(t, "^(master|main)$", *releaseType.GetMatchBranches())
			assert.NotNil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.CLEAN, *releaseType.GetMatchWorkspaceStatus())

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraNonIntegerPrereleaseIdentifierThrowsError(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("nonint"), utl.PointerToString("abc"), ent.PointerToPosition(ent.PRE_RELEASE))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.Error(t, err)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraIntegerPrereleaseIdentifier(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("identifier1"), utl.PointerToString("123"), ent.PointerToPosition(ent.PRE_RELEASE))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.0.5-identifier1.123", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraIntegerPrereleaseIdentifierOverExistingOnes(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("identifier1"), utl.PointerToString("123"), ent.PointerToPosition(ent.PRE_RELEASE))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #1"))
			(*command).Script().AndTag("0.1.0-tag1.identifier1.999", nil)
			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #2"))

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.1.1-tag1.identifier1.123", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraBuildIdentifier(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("identifier1"), utl.PointerToString("abc"), ent.PointerToPosition(ent.BUILD))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.0.5+identifier1.abc", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraBuildIdentifierOverExistingOnes(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("identifier1"), utl.PointerToString("abc"), ent.PointerToPosition(ent.BUILD))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #1"))
			(*command).Script().AndTag("0.1.0+tag1.identifier1.999", nil)
			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #2"))

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.1.1+tag1.identifier1.abc", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraMultipleIdentifiers(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("p1"), utl.PointerToString("123"), ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("p2"), nil, ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("p3"), utl.PointerToString("456"), ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("b1"), utl.PointerToString("abc"), ent.PointerToPosition(ent.BUILD)), ent.NewIdentifierWith(utl.PointerToString("b2"), nil, nil /* BUILD is the default position */), ent.NewIdentifierWith(utl.PointerToString("b3"), utl.PointerToString("def"), ent.PointerToPosition(ent.BUILD))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.0.5-p1.123.p2.p3.456+b1.abc.b2.b3.def", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferExtraMultipledentifiersOverExistingOnes(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("p1"), utl.PointerToString("123"), ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("p2"), nil, ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("p3"), utl.PointerToString("456"), ent.PointerToPosition(ent.PRE_RELEASE)), ent.NewIdentifierWith(utl.PointerToString("b1"), utl.PointerToString("abc"), ent.PointerToPosition(ent.BUILD)), ent.NewIdentifierWith(utl.PointerToString("b2"), nil, nil /* BUILD is the default position */), ent.NewIdentifierWith(utl.PointerToString("b3"), utl.PointerToString("def"), ent.PointerToPosition(ent.BUILD))})
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #1"))
			(*command).Script().AndTag("0.1.0-tag1.p2.999+b3.zzz", nil)
			(*command).Script().AndCommitWith(utl.PointerToString("Untagged commit #2"))

			_, err := (*command).Run()
			assert.NoError(t, err)

			// existing extra identifiers will come first with their values overwritten only if the new identifier has a value,
			// then the new identifiers are appended to the end. If a previous identifier has a value and the new one doesn't,
			// the old value remains instead of being cleared by the new one because there is no means to know if that previous
			// value was the identifier value or a tag itself
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "0.1.1-tag1.p2.999.p1.123.p3.456+b3.def.b1.abc.b2", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferVersionRangeCheckWithStaticMatchingExpression(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetVersionRange(utl.PointerToString("^0\\.0\\.([0-9]*)$"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "0.0.5", *version)
			assert.Equal(t, "^0\\.0\\.([0-9]*)$", *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferVersionRangeCheckWithStaticNonMatchingExpression(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetVersionRange(utl.PointerToString("^1\\.2\\.([0-9]*)$"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the generated version does not comply with the version range so it raises an error
			_, err := (*command).Run()
			assert.Error(t, err)

			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "^1\\.2\\.([0-9]*)$", *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferVersionRangeCheckWithStaticMalformedExpression(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetVersionRange(utl.PointerToString("^1\\.2\\.((((((([0-9]*)$"))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the generated version does not comply with the version range so it raises an error
			_, err := (*command).Run()
			assert.Error(t, err)

			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "^1\\.2\\.((((((([0-9]*)$", *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferVersionRangeCheckWithDynamicExpressionInferredFromParseableBranchNames(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetVersionRangeFromBranchName(utl.PointerToBoolean(true))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// These tests may fail when running in a standalone context because the Run() does not
			// imply up-to-date checks and the execution may inherit stale State values from previous
			// runs. Keep this in mind if they fail and uncomment this conditional so that
			// tests are executed only in non-standalone contexts.
			for _, args := range wellKnownParseableBranchNames {
				t.Run(args.branchName, func(t *testing.T) {
					(*command).Script().Checkout(args.branchName)                                // create the branch
					(*command).Script().AndCommitWith(utl.PointerToString("An untagged commit")) // create a commit

					(*command).Run() // version range check may fail here but we don't care, we're just testing the regex that was inferred
					//assert.Error(t, err)

					versionRange, _ := (*command).State().GetVersionRange()
					assert.Equal(t, args.regex, *versionRange, fmt.Sprintf("testing with branch '%s', the expected inferred dynamic version range check regular expression was '%s' but the actual result was '%s'", args.branchName, args.regex, *versionRange))
				})
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferVersionRangeCheckWithDynamicExpressionInferredFromUnparseableBranchNames(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add some fictional release types
			releaseType := ent.NewReleaseType()
			releaseType.SetVersionRangeFromBranchName(utl.PointerToBoolean(true))
			releaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("matched")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"matched": releaseType})
			configurationLayerMock.SetReleaseTypes(releaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// These tests may fail when running in a standalone context because the Run() does not
			// imply up-to-date checks and the execution may inherit stale State values from previous
			// runs. Keep this in mind if they fail and uncomment this conditional so that
			// tests are executed only in non-standalone contexts.
			for _, args := range wellKnownUnparseableBranchNames {
				t.Run(args.branchName, func(t *testing.T) {
					(*command).Script().Checkout(args.branchName)                                // create the branch
					(*command).Script().AndCommitWith(utl.PointerToString("An untagged commit")) // create a commit

					_, err := (*command).Run() // this always throws an error with these branches
					assert.Error(t, err)

					versionRange, _ := (*command).State().GetVersionRange()
					// the command is not able to parse a valid version range expression from these values so it must yield to nil
					assert.Nil(t, versionRange, fmt.Sprintf("testing with branch '%s', the expected inferred dynamic version range check regular expression was nil but the actual result was '%v'", args.branchName, versionRange))
				})
			}
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferErrorOnRunWithValidButEmptyGitRepository(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			_, err := (*command).Run() // this always throws an error with these branches
			assert.Error(t, err)

		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnly(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, ent.INITIAL_VERSION, version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndInitialVersionOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_INITIAL_VERSION := "12.13.14"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetInitialVersion(&CUSTOM_INITIAL_VERSION)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, CUSTOM_INITIAL_VERSION, *releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, CUSTOM_INITIAL_VERSION, *releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, CUSTOM_INITIAL_VERSION, *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndVersionOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_VERSION := "1.2.3"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetVersion(&CUSTOM_VERSION)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Nil(t, branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Nil(t, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Nil(t, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, CUSTOM_VERSION, *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMajorOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "major"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpMinorOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "minor"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.2.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpPatchOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "patch"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithInitialCommitOnlyAndBumpAlphaOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "alpha"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetRootCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-alpha.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpMajorOverriddenByUserInRepoWithJustOneInitialVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "major"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpMinorOverriddenByUserInRepoWithJustOneInitialVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "minor"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.2.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpPatchOverriddenByUserInRepoWithJustOneInitialVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "patch"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpAlphaOverriddenByUserInRepoWithJustOneInitialVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "alpha"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-alpha.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithReleaseLenientInRepoWithPrefixedVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "2.2.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithoutReleaseLenientInRepoWithoutPrefixedVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, ent.INITIAL_VERSION, releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag(*ent.INITIAL_VERSION), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, ent.INITIAL_VERSION, version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithoutLenientAndWithPrefixReleaseInRepoWithJustOneInitialVersionCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.INITIAL_VERSION()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
			configurationLayerMock.SetReleasePrefix(utl.PointerToString("release-"))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "release-2.2.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.4", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeInRepoWithSimpleLinearCommitHistoryAndNonSignificantCommitsWithInitialVersionOverride(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetInitialVersion(utl.PointerToString("12.13.14"))
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.4", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithVersionOverriddenByUserInRepoWithFurtherNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetVersion(utl.PointerToString("1.2.3"))
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Nil(t, branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Nil(t, releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Nil(t, releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.2.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpMajorOverrideInRepoWithFurtherNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "major"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpMinorOverrideInRepoWithFurtherNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "minor"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpPatchOverrideInRepoWithFurtherNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "patch"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.5", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithBumpAlphaOverrideInRepoWithFurtherNonSignificantCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "alpha"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, CUSTOM_BUMP, *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.4-alpha.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "2.2.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithoutReleaseLenientAndWithoutPrefixInRepoWithFurtherNonSignificantPrefixedCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.4", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithoutReleaseLenientAndWithPrefixInRepoWithFurtherNonSignificantPrefixedCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(false))
			configurationLayerMock.SetReleasePrefix(utl.PointerToString("release-"))
			(*command).Script().AndCommitWithTag("release-2.2.2")
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, ent.BUMP, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "release-2.2.2", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("release-2.2.2"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "release-2.2.2", *version)
			assert.Nil(t, versionRange)
		})

	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeInRepoWithOverlappingTagsCommit(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_WITH_OVERLAPPING_TAGS()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			CUSTOM_BUMP := "patch"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			configurationLayerMock.SetBump(&CUSTOM_BUMP)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.6", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.7", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithAlwaysPositiveCommitConventionInRepoWithFurtherNonSignificantPrefixedCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"minor": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 2, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, releaseScope.GetSignificantCommits()[0].GetSHA(), (*command).Script().GetCommitIDs()[0])
			assert.Equal(t, releaseScope.GetSignificantCommits()[1].GetSHA(), (*command).Script().GetCommitIDs()[1])
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingDefaultReleaseTypeWithAlwaysNegativeCommitConventionInRepoWithFurtherNonSignificantPrefixedCommits(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.ONE_BRANCH_SHORT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)

			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.4", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.4"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSE_VERSIONS, releaseType.GetCollapseVersions())
			assert.Equal(t, ent.RELEASE_TYPE_COLLAPSED_VERSION_QUALIFIER, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			if ent.RELEASE_TYPE_IDENTIFIERS == nil {
				assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers(ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers()))
				for i := 0; i < len(*ent.RELEASE_TYPE_IDENTIFIERS); i++ {
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*ent.RELEASE_TYPE_IDENTIFIERS)[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_BRANCHES, releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.4", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			MATCHING_RELEASE_TYPE_NAME := "mainline" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)

			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 2, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			MATCHING_RELEASE_TYPE_NAME := "mainline" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)

			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.5", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInMainBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("main")
			MATCHING_RELEASE_TYPE_NAME := "mainline" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "main", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.1.0", *releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, "0.1.0", *releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 4, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.1.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInMainBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("main")
			MATCHING_RELEASE_TYPE_NAME := "mainline" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "main", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.1.0", *releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, "0.1.0", *releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInIntegrationBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("integration")
			MATCHING_RELEASE_TYPE_NAME := "integration" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "integration", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6-integration.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInIntegrationBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("integration")
			MATCHING_RELEASE_TYPE_NAME := "integration" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "integration", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-integration.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInDevelopmentBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("development")
			MATCHING_RELEASE_TYPE_NAME := "integration" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "development", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.1.0", *releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, "0.1.0", *releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 4, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.1.1-development.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInDevelopmentBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("development")
			MATCHING_RELEASE_TYPE_NAME := "integration" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "development", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.1.0", *releaseScope.GetPreviousVersion())
			assert.Nil(t, releaseScope.GetPreviousVersionCommit())
			assert.Equal(t, "0.1.0", *releaseScope.GetPrimeVersion())
			assert.Nil(t, releaseScope.GetPrimeVersionCommit())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInAlphaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("alpha")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "alpha", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-alpha.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-alpha.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6-alpha.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInAlphaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("alpha")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "alpha", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-alpha.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-alpha.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-alpha.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInBetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("beta")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "beta", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-beta.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-beta.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6-beta.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInBetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("beta")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "beta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-beta.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-beta.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-beta.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInGammaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("gamma")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "gamma", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 7, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[6], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 7, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6-gamma.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInGammaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("gamma")
			MATCHING_RELEASE_TYPE_NAME := "maturity" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "gamma", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 7, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[6], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.5", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV0xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("v0.x")
			MATCHING_RELEASE_TYPE_NAME := "maintenance" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "v0.x", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 2, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.8", *version)
			assert.NotNil(t, *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInV0xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("v0.x")
			MATCHING_RELEASE_TYPE_NAME := "maintenance" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "v0.x", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 2, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[1], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.7", *version)
			assert.NotNil(t, *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInV1xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("v1.x")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the generated version does not comply with the version range so it raises an error
			_, err := (*command).Run()
			assert.Error(t, err)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel0xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("rel/0.x")
			MATCHING_RELEASE_TYPE_NAME := "release" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "rel/0.x", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-rel.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-rel.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-rel.3", *version)
			assert.NotNil(t, *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInRel0xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("rel/0.x")
			MATCHING_RELEASE_TYPE_NAME := "release" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "rel/0.x", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-rel.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-rel.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-rel.2", *version)
			assert.NotNil(t, *versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInRel1xBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("rel/1.x")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// the generated version does not comply with the version range so it raises an error
			_, err := (*command).Run()
			assert.Error(t, err)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureSSOBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("feature/SSO")
			MATCHING_RELEASE_TYPE_NAME := "feature" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "feature/SSO", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-featuresso.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-featuresso.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-featuresso.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureSSOBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("feature/SSO")
			MATCHING_RELEASE_TYPE_NAME := "feature" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "feature/SSO", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-featuresso.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-featuresso.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-featuresso.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInFeatureIN12345Branch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("feature/IN-12345")
			MATCHING_RELEASE_TYPE_NAME := "feature" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "feature/IN-12345", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 7, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[6], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 7, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-featurein12345.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInFeatureIN12345Branch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("feature/IN-12345")
			MATCHING_RELEASE_TYPE_NAME := "feature" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "feature/IN-12345", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 7, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[6], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.5", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInHotfix98765Branch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("hotfix-98765")
			MATCHING_RELEASE_TYPE_NAME := "hotfix" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "hotfix-98765", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.8-hotfix98765.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.8-hotfix98765.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.8-hotfix98765.3", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInHotfix98765Branch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("hotfix-98765")
			MATCHING_RELEASE_TYPE_NAME := "hotfix" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "hotfix-98765", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.8-hotfix98765.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.8-hotfix98765.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.7", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.7"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.8-hotfix98765.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInInternalBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("internal")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "internal", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-internal.1+timestamp.003"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			// the version contains the timestamp which is variable so let's test the start string and the overall length
			assert.True(t, strings.HasPrefix(*version, "0.0.6-internal.2+timestamp."), fmt.Sprintf("Version '%s' was expected to start with '%s'", *version, "0.0.6-internal.2+timestamp."))
			assert.Equal(t, len("0.0.6-internal.2+timestamp.")+14, len(*version)) // the timestamp is 14 characters long
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInInternalBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("internal")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "internal", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-internal.1+timestamp.003"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomebranchBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("somebranch")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "somebranch", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 7, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			// the version contains the timestamp which is variable so let's test the start string and the overall length
			assert.True(t, strings.HasPrefix(*version, "0.0.6-internal.1+timestamp."), fmt.Sprintf("Version '%s' was expected to start with '%s'", *version, "0.0.6-internal.1+timestamp."))
			assert.Equal(t, len("0.0.6-internal.1+timestamp.")+14, len(*version)) // the timestamp is 14 characters long
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomebranchBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("somebranch")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "somebranch", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 4, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[3], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.2", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.2"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-integration.2", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysPositiveCommitConventionInSomeotherbranchBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("someotherbranch")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "someotherbranch", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 5, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			// the version contains the timestamp which is variable so let's test the start string and the overall length
			assert.True(t, strings.HasPrefix(*version, "0.0.6-internal.1+timestamp."), fmt.Sprintf("Version '%s' was expected to start with '%s'", *version, "0.0.6-internal.1+timestamp."))
			assert.Equal(t, len("0.0.6-internal.1+timestamp.")+14, len(*version)) // the timestamp is 14 characters long
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingExtendedPresetReleaseTypesWithAlwaysNegativeCommitConventionInSomeotherbranchBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("someotherbranch")
			MATCHING_RELEASE_TYPE_NAME := "internal" // the name of the release type that must be matched in the branch
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add the 'extended' preset, which comes with standard release types
			configurationLayerMock.SetPreset(utl.PointerToString(cnf.EXTENDED_NAME))
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			releaseTypes, _ := (*command).State().GetConfiguration().GetReleaseTypes()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "someotherbranch", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-integration.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-integration.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapseVersions(), releaseType.GetCollapseVersions())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetCollapsedVersionQualifier(), releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetDescription(), releaseType.GetDescription())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetFilterTags(), releaseType.GetFilterTags())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommit(), releaseType.GetGitCommit())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitCommitMessage(), releaseType.GetGitCommitMessage())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitPush(), releaseType.GetGitPush())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTag(), releaseType.GetGitTag())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetGitTagMessage(), releaseType.GetGitTagMessage())
			if (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers() == nil {
				assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers())
			} else {
				assert.True(t, containsAllIdentifiers((*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers(), releaseType.GetIdentifiers()))
				for i := 0; i < len(*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers()); i++ {
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetQualifier(), (*releaseType.GetIdentifiers())[i].GetQualifier())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetValue(), (*releaseType.GetIdentifiers())[i].GetValue())
					assert.Equal(t, (*(*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetIdentifiers())[i].GetPosition(), (*releaseType.GetIdentifiers())[i].GetPosition())
				}
			}
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchBranches(), releaseType.GetMatchBranches())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchEnvironmentVariables(), releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetMatchWorkspaceStatus(), releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetPublish(), releaseType.GetPublish())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRange(), releaseType.GetVersionRange())
			assert.Equal(t, (*(*releaseTypes.GetItems())[MATCHING_RELEASE_TYPE_NAME]).GetVersionRangeFromBranchName(), releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-integration.1", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomReleaseTypeWithAlwaysPositiveCommitConventionInInternalBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("internal")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"patch": ".*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configReleaseType.SetGitCommit(utl.PointerToString("true"))
			configReleaseType.SetGitPush(utl.PointerToString("true"))
			configReleaseType.SetGitTag(utl.PointerToString("true"))
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("customId"), utl.PointerToString("999"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetPublish(utl.PointerToString("true"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseType": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().AndAddFiles() // add some uncommitted changes to be committed by the Mark command
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "internal", *branch)
			assert.Equal(t, "patch", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-internal.1+timestamp.003"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 1, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, "true", *releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, "true", *releaseType.GetGitPush())
			assert.Equal(t, "true", *releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, "customId", *(*releaseType.GetIdentifiers())[0].GetQualifier())
			assert.Equal(t, "999", *(*releaseType.GetIdentifiers())[0].GetValue())
			assert.Equal(t, ent.PRE_RELEASE, *(*releaseType.GetIdentifiers())[0].GetPosition())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Nil(t, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, "true", *releaseType.GetPublish())
			assert.Nil(t, releaseType.GetVersionRange())
			assert.False(t, *releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.True(t, newRelease)
			assert.Equal(t, "0.0.6-internal.2.customId.999+timestamp.003", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomReleaseTypeWithAlwaysNegativeCommitConventionInInternalBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.EXTENDED_PRESET_BRANCHES_SHORT_UNMERGED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("internal")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that accepts all non nil messages and dumps the minor identifier for each
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that always enables committing, tagging and pushing
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configReleaseType.SetGitCommit(utl.PointerToString("true"))
			configReleaseType.SetGitPush(utl.PointerToString("true"))
			configReleaseType.SetGitTag(utl.PointerToString("true"))
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("customId"), utl.PointerToString("999"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetPublish(utl.PointerToString("true"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseType")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseType": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			(*command).Script().AndAddFiles() // add some uncommitted changes to be committed by the Mark command
			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			versionRange, _ := (*command).State().GetVersionRange()
			assert.Equal(t, "internal", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.6-internal.1+timestamp.003"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.5", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.5"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, "true", *releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, "true", *releaseType.GetGitPush())
			assert.Equal(t, "true", *releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, "customId", *(*releaseType.GetIdentifiers())[0].GetQualifier())
			assert.Equal(t, "999", *(*releaseType.GetIdentifiers())[0].GetValue())
			assert.Equal(t, ent.PRE_RELEASE, *(*releaseType.GetIdentifiers())[0].GetPosition())
			assert.Nil(t, releaseType.GetMatchBranches())
			assert.Nil(t, releaseType.GetMatchEnvironmentVariables())
			assert.Nil(t, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, "true", *releaseType.GetPublish())
			assert.Nil(t, releaseType.GetVersionRange())
			assert.False(t, *releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.6-internal.1+timestamp.003", *version)
			assert.Nil(t, versionRange)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithInferringCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithInferringCommitConventionInTaggedwithoutbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("taggedwithoutbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "taggedwithoutbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.2.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.2.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.2.3", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.2.3"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.2.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithInferringCommitConventionInTaggedwithbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("taggedwithbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "taggedwithbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.3.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.3.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.3.3", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.3.3"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.3.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithInferringCommitConventionInUntaggedwithoutbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("untaggedwithoutbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "untaggedwithoutbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithInferringCommitConventionInUntaggedwithbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("untaggedwithbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "untaggedwithbump", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, "^(master|main)$", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInAlphaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("alpha")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "alpha", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-alpha.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-alpha.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-alpha.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInBetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("beta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "beta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-beta.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-beta.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-beta.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInGammaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("gamma")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "gamma", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInDeltaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("delta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "delta", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-delta.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInEpsilonBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("epsilon")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "epsilon", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "1.0.0-epsilon.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-epsilon.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-epsilon.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInZetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("zeta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "zeta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-zeta.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-zeta.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-zeta.4", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInEtaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("eta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "eta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "1.0.0-eta.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-eta.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-eta.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithInferringCommitConventionInThetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("theta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "theta", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-theta.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInTaggedwithoutbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("taggedwithoutbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "taggedwithoutbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.2.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.2.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.2.3", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.2.3"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.2.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInTaggedwithbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("taggedwithbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "taggedwithbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.3.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.3.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.3.3", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.3.3"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.3.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInUntaggedwithoutbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("untaggedwithoutbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "untaggedwithoutbump", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomFlatReleaseTypeWithExtraIdentifierWithInferringCommitConventionInUntaggedwithbumpBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("untaggedwithbump")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configReleaseType := ent.NewReleaseType()
			configReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configReleaseType.SetMatchBranches(utl.PointerToString(".*"))
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "untaggedwithbump", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-extra.5", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInMasterBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("master")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "master", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.False(t, *releaseType.GetCollapseVersions())
			assert.Nil(t, releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Nil(t, releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, 1, len(*releaseType.GetIdentifiers()))
			assert.Equal(t, "^(master|main)$", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInAlphaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("alpha")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "alpha", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-alpha.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-alpha.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-alpha.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInBetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("beta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "beta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-beta.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-beta.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-beta.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInGammaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("gamma")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "gamma", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.0.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInDeltaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("delta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "delta", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-number.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInEpsilonBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("epsilon")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "epsilon", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "1.0.0-epsilon.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-epsilon.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-epsilon.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInZetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("zeta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "zeta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 0, len(releaseScope.GetCommits()))
			assert.Nil(t, releaseScope.GetInitialCommit())
			assert.Nil(t, releaseScope.GetFinalCommit())
			assert.Equal(t, "1.0.0-zeta.4", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-zeta.4"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-zeta.4", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithInferringCommitConventionInEtaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("eta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "eta", *branch)
			assert.Nil(t, bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 1, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[0], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "1.0.0-eta.3", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("1.0.0-eta.3"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 0, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.False(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "1.0.0-eta.3", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}

func TestInferRunUsingCustomCollapsedReleaseTypeWithExtraIdentifiersWithExtraIdentifierWithInferringCommitConventionInThetaBranch(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.INFER, gittools.FIVE_BRANCH_UNMERGED_BUMPING_COLLAPSED()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			(*command).Script().Checkout("theta")
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			// add a mock convention that takes the commit message as the identifier to bump, if any
			commitMessageConventions, _ := ent.NewCommitMessageConventionsWith(&[]*string{utl.PointerToString("testConvention")},
				&map[string]*ent.CommitMessageConvention{"testConvention": ent.NewCommitMessageConventionWith(utl.PointerToString(".*"),
					&map[string]string{"major": "^major.*", "minor": "^minor.*", "patch": "^patch.*"})})
			configurationLayerMock.SetCommitMessageConventions(commitMessageConventions)
			// add a custom release type that matches any branch
			configMainReleaseType := ent.NewReleaseType()
			configMainReleaseType.SetIdentifiers(&[]*ent.Identifier{ent.NewIdentifierWith(utl.PointerToString("extra"), utl.PointerToString("5"), ent.PointerToPosition(ent.PRE_RELEASE))})
			configMainReleaseType.SetMatchBranches(utl.PointerToString("^(master|main)$")) // match main and master
			configCollapsedReleaseType := ent.NewReleaseType()
			configCollapsedReleaseType.SetCollapseVersions(utl.PointerToBoolean(true))
			configCollapsedReleaseType.SetCollapsedVersionQualifier(utl.PointerToString("number"))
			configCollapsedReleaseType.SetFilterTags(utl.PointerToString("^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$"))
			configCollapsedReleaseType.SetMatchBranches(utl.PointerToString(".*")) // match any branch (this is the fallback release type)
			configReleaseTypes, _ := ent.NewReleaseTypesWith(&[]*string{utl.PointerToString("testReleaseTypeMain"), utl.PointerToString("testReleaseTypeCollapsed")},
				&[]*string{}, &[]*string{},
				&map[string]*ent.ReleaseType{"testReleaseTypeMain": configMainReleaseType, "testReleaseTypeCollapsed": configCollapsedReleaseType})
			configurationLayerMock.SetReleaseTypes(configReleaseTypes)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			_, err := (*command).Run()
			assert.NoError(t, err)

			releaseType, _ := (*command).State().GetReleaseType()
			releaseScope, _ := (*command).State().GetReleaseScope()
			branch, _ := (*command).State().GetBranch()
			bump, _ := (*command).State().GetBump()
			newVersion, _ := (*command).State().GetNewVersion()
			newRelease, _ := (*command).State().GetNewRelease()
			scheme, _ := (*command).State().GetScheme()
			version, _ := (*command).State().GetVersion()
			assert.Equal(t, "theta", *branch)
			assert.Equal(t, "minor", *bump)
			assert.Equal(t, ent.SCHEME, scheme)
			assert.Equal(t, 3, len(releaseScope.GetCommits()))
			assert.Equal(t, (*command).Script().GetCommitIDs()[2], releaseScope.GetInitialCommit().GetSHA())
			assert.Equal(t, (*command).Script().GetLastCommitID(), releaseScope.GetFinalCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPreviousVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPreviousVersionCommit().GetSHA())
			assert.Equal(t, "0.0.1", *releaseScope.GetPrimeVersion())
			assert.Equal(t, *(*command).Script().GetCommitByTag("0.0.1"), releaseScope.GetPrimeVersionCommit().GetSHA())
			assert.Equal(t, 3, len(releaseScope.GetSignificantCommits()))
			assert.True(t, *releaseType.GetCollapseVersions())
			assert.Equal(t, "number", *releaseType.GetCollapsedVersionQualifier())
			assert.Equal(t, ent.RELEASE_TYPE_DESCRIPTION, releaseType.GetDescription())
			assert.Equal(t, "^([0-9]\\d*)\\.([0-9]\\d*)\\.([0-9]\\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\\.([0-9]\\d*))?)?$", *releaseType.GetFilterTags())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT, releaseType.GetGitCommit())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_COMMIT_MESSAGE, releaseType.GetGitCommitMessage())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_PUSH, releaseType.GetGitPush())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG, releaseType.GetGitTag())
			assert.Equal(t, ent.RELEASE_TYPE_GIT_TAG_MESSAGE, releaseType.GetGitTagMessage())
			assert.Equal(t, ent.RELEASE_TYPE_IDENTIFIERS, releaseType.GetIdentifiers())
			assert.Equal(t, ".*", *releaseType.GetMatchBranches())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_ENVIRONMENT_VARIABLES, releaseType.GetMatchEnvironmentVariables())
			assert.Equal(t, ent.RELEASE_TYPE_MATCH_WORKSPACE_STATUS, releaseType.GetMatchWorkspaceStatus())
			assert.Equal(t, ent.RELEASE_TYPE_PUBLISH, releaseType.GetPublish())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE, releaseType.GetVersionRange())
			assert.Equal(t, ent.RELEASE_TYPE_VERSION_RANGE_FROM_BRANCH_NAME, releaseType.GetVersionRangeFromBranchName())
			assert.True(t, newVersion)
			assert.False(t, newRelease)
			assert.Equal(t, "0.1.0-number.1", *version)
		})
	}
	log.SetLevel(logLevel) // restore the original logging level
}
