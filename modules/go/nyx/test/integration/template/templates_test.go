//go:build integration
// +build integration

// Only run these tests as part of the integration test suite, when the 'integration' build flag is passed (i.e. running go test --tags=integration)

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

package template_test

import (
	"math"    // https://pkg.go.dev/math
	"testing" // https://pkg.go.dev/testing
	"time"    // https://pkg.go.dev/time

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
	gitent "github.com/mooltiverse/nyx/modules/go/nyx/entities/git"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	. "github.com/mooltiverse/nyx/modules/go/nyx/template"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

const (
	// A test template to be used with a state scope
	TEMPLATE_WITH_STATE_SCOPE = "Version: {{version}} (bumping '{{bump}}' on {{configuration.initialVersion}} using lenient ({{configuration.releaseLenient}}))\nScheme: {{scheme}}\nTimestamp: {{timestamp}}\nPrevious Version: {{releaseScope.previousVersion}} at {{#short5}}{{releaseScope.previousVersionCommit.sha}}{{/short5}}\n\nCommits:\n{{#releaseScope.commits}}\n  {{.}}\n{{/releaseScope.commits}}\n"

	// The expected output for the template to be used with a state scope
	TEMPLATE_WITH_STATE_SCOPE_OUTPUT = "Version: 9.8.7 (bumping 'theta' on 1.2.3 using lenient (true))\nScheme: SEMVER\nTimestamp: 9223372036854775807\nPrevious Version: 4.5.6 at 05cbf\n\nCommits:\n  d40fcded9e516158a2901f5657794931528af106\n  9bed70fac8a27a4b14b6b12307d034bc59da85c3\n  ef6a6481adb2df26bc7eebfde465e5c2f3e93539\n"
)

/*
Returns a State instance to be used as the scope for template rendering.
*/
func getStateScope() *stt.State {
	configurationLayerMock := cnf.NewSimpleConfigurationLayer()
	configuration, _ := cnf.NewConfiguration()
	configurationLayerMock.SetBump(utl.PointerToString("theta"))
	configurationLayerMock.SetReleaseLenient(utl.PointerToBoolean(true))
	configurationLayerMock.SetInitialVersion(utl.PointerToString("1.2.3"))

	var cl cnf.ConfigurationLayer = configurationLayerMock
	configuration.WithRuntimeConfiguration(&cl)

	state, _ := stt.NewStateWith(configuration)

	state.SetVersion(utl.PointerToString("9.8.7"))
	state.SetTimestamp(utl.PointerToInt64(math.MaxInt64))
	releaseScope, _ := state.GetReleaseScope()
	releaseScope.SetPreviousVersion(utl.PointerToString("4.5.6"))
	releaseScope.SetPreviousVersionCommit(gitent.NewCommitWith("05cbfd58fadbec3d96b220a0054d96875aa37011", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("4.5.6", "05cbfd58fadbec3d96b220a0054d96875aa37011", false)}))
	releaseScope.SetPrimeVersion(utl.PointerToString("1.0.0"))
	releaseScope.SetPrimeVersionCommit(gitent.NewCommitWith("e8fa442504d91a0187865c74093a5a4212a805f9", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{*gitent.NewTagWith("1.0.0", "e8fa442504d91a0187865c74093a5a4212a805f9", false)}))
	commits := releaseScope.GetCommits()
	commits = append(commits, gitent.NewCommitWith("d40fcded9e516158a2901f5657794931528af106", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("9bed70fac8a27a4b14b6b12307d034bc59da85c3", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	commits = append(commits, gitent.NewCommitWith("ef6a6481adb2df26bc7eebfde465e5c2f3e93539", 0, []string{}, *gitent.NewActionWith(*gitent.NewIdentityWith("Jim", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewActionWith(*gitent.NewIdentityWith("Sam", ""), *gitent.NewTimeStampFrom(time.Now())), *gitent.NewMessageWith("full", "short", map[string]string{}), []gitent.Tag{}))
	releaseScope.SetCommits(commits)

	return state
}

/*
Render with state
*/
func TestTemplatesRenderWithState(t *testing.T) {
	value, err := getStateScope().Flatten() // use the flattened version of the object or it won't render all fields
	output, err := Render(TEMPLATE_WITH_STATE_SCOPE, value)
	assert.NoError(t, err)
	assert.Equal(t, TEMPLATE_WITH_STATE_SCOPE_OUTPUT, output)
}
