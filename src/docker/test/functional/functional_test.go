//go:build functional
// +build functional

// Only run these tests as part of the functional test suite, when the 'functional' build flag is passed (i.e. running go test --tags=functional)

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
This package provides functional tests.
*/
package docker_functional_test

import (
	"testing" // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	. "github.com/mooltiverse/nyx/src/go/nyx/test/functional"
)

/*
Returns the input slice with the unsuitable test suites removed.

Suites are unsiotable (and removed), if:
  - they use a remote repository (on the local filesystem) because within a Docker container the
    remote directory isn't reachable
*/
func filterSuitableSuites(suites []*TestSuite) []*TestSuite {
	res := []*TestSuite{}
	for _, suite := range suites {
		if suite.RemoteRepoName != nil {
			// unsuitable, skip
		} else {
			res = append(res, suite)
		}
	}
	return res
}

func TestFunctional(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, suite := range filterSuitableSuites(WellKnownFunctionalTestSuites()) {
		t.Run(suite.Name+" (Docker)", func(t *testing.T) {
			err := suite.Test(t, NewDockerExecutionContext())
			assert.NoError(t, err)
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}
