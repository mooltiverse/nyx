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

package services_test

import (
	"testing" // https://pkg.go.dev/testing

	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/src/go/nyx/entities"
	svc "github.com/mooltiverse/nyx/src/go/nyx/services"
	svcapi "github.com/mooltiverse/nyx/src/go/nyx/services/api"
)

var (
	// use this slice to parametrize tests based on the providers
	serviceProviders = []ent.Provider{
		ent.GITHUB,
		ent.GITLAB,
	}

	// use this slice to parametrize tests based on the features
	serviceFeatures = []svcapi.Feature{
		svcapi.GIT_HOSTING,
		svcapi.RELEASES,
		svcapi.RELEASE_ASSETS,
		svcapi.USERS,
	}
)

func TestServiceFactoryGitHostingServiceInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, p := range serviceProviders {
		t.Run(p.String(), func(t *testing.T) {
			_, err := svc.GitHostingServiceInstance(p, map[string]string{})
			assert.NoError(t, err)
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}

func TestServiceFactoryInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, p := range serviceProviders {
		t.Run(p.String(), func(t *testing.T) {
			_, err := svc.Instance(p, map[string]string{})
			assert.NoError(t, err)
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}

func TestServiceFactoryReleaseServiceInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, p := range serviceProviders {
		t.Run(p.String(), func(t *testing.T) {
			_, err := svc.ReleaseServiceInstance(p, map[string]string{})
			assert.NoError(t, err)
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}

func TestServiceFactoryUserServiceInstance(t *testing.T) {
	logLevel := log.GetLevel()   // save the previous logging level
	log.SetLevel(log.ErrorLevel) // set the logging level to filter out warnings produced during tests

	for _, p := range serviceProviders {
		t.Run(p.String(), func(t *testing.T) {
			_, err := svc.UserServiceInstance(p, map[string]string{})
			assert.NoError(t, err)
		})
	}

	log.SetLevel(logLevel) // restore the original logging level
}
