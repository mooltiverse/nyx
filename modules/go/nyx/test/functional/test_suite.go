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
package functional_test

import (
	"bytes"         // https://pkg.go.dev/bytes
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"strings"       // https://pkg.go.dev/strings
	"testing"       // https://pkg.go.dev/testing
	"time"          // https://pkg.go.dev/time

	regexp2 "github.com/dlclark/regexp2"        // https://pkg.go.dev/github.com/dlclark/regexp2, we need to use this instead of the standard 'regexp' to have support for lookarounds (look ahead), even if this implementation is a little slower
	log "github.com/sirupsen/logrus"            // https://pkg.go.dev/github.com/sirupsen/logrus
	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
	svc "github.com/mooltiverse/nyx/modules/go/nyx/services"
	svcapi "github.com/mooltiverse/nyx/modules/go/nyx/services/api"
	github "github.com/mooltiverse/nyx/modules/go/nyx/services/github"
	gitlab "github.com/mooltiverse/nyx/modules/go/nyx/services/gitlab"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
	gitutil "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/util"
	utl "github.com/mooltiverse/nyx/modules/go/utils"
)

/*
The test suite:
- defines a fixture of input data and parameters for the test
- defines a fixture of checks to run after Nyx execution, in the form of assertions
- runs the test, according to the above fixture
- performs the checks, according to the above fixture
*/
type TestSuite struct {
	// The test suite short descriptive name
	Name string

	// The Nyx command to run. If nil the default command will be executed
	NyxCommand *string

	// The configuration arguments to pass on the command line (without the Nyx command)
	Args []string

	// The map of environment variables to use when running Nyx and returns the suite. Each entry key is the variable
	// name, while the value is the variable value.
	Env map[string]string

	// The map of files to create in the repository directory when running Nyx. Keys are file names
	// (relative to the Git repository), while values are file contents
	Files map[string]string

	// The checks to run against file contents after running Nyx. Keys are file names, values are strings
	// or regular expressions to match within the file.
	// Checks will be tried using values as plain strings first and only if the plain
	// string can't be found in the file it will be used as a regular expression. The check will fail only if
	// both tries fail. If the file does not exist or it doesn't match the given content the assertion will fail.
	FileContentChecks map[string][]string

	// The name of an optional hosting service ('github' or 'gitlab') to use to create a remote repository first,
	// then clone and apply the scenario on. This is also used to test release publishing.
	// This is different (and doesn't clash with) than the remote repository configured with RemoteRepoName.
	// Please note that enabling hosting services will slightly slow down tests.
	// Also note that if the hosting service requires some credentials, they must be passed by some configuration means.
	HostingRepoService *string

	// The checks to run against the hosted repository releases after running Nyx. The tags herein contained must all be
	// present in the hosting service as release names.
	// The slice contains tags that must correspond to release names on the hosting service
	// (which must be configured using HostingRepoService.
	HostedReleaseTags []string

	// The checks to run against repository tags after running Nyx. The tags herein contained must all be
	// present in the repository.
	RepositoryTags []string

	// The checks to run against remote repository tags after running Nyx. The tags herein contained must all be
	// present in the remote repository (which must be configured).
	// Please note that in order for this to work the remote repository RemoteRepoName must be configured
	// or assertions will fail.
	RemoteRepositoryTags []string

	// The name of an optional bare remote repository to create on the local file system and configured as a remote
	// against the main repository to test that 'push' operations reflect changes in the remote as well.
	// An example to pass here is 'replica'. You should not use 'origin' here to avoid name clashes with remote hosting
	// services that may also be used for test.
	// This is different (and doesn't clash with) than the hosted repository configured with HostingRepoService.
	RemoteRepoName *string

	// The test scenario, used to set up the Git repository to a well known state
	Scenario gittools.Scenario
}

/*
Tests the suite using the given command and returns the error.

This method sets up the environment, runs Nyx and performs the checks.

Arguments are as follows:

  - t the test object
  - context the execution context to use to run Nyx
*/
func (ts *TestSuite) Test(t *testing.T, context ExecutionContext) error {
	var err error
	// set up the hosted service references, if configured
	var gitHostingService *svcapi.GitHostingService
	var gitHostingReleaseService *svcapi.ReleaseService
	var gitHostingUserService *svcapi.UserService
	var gitHostedRepository *svcapi.GitHostedRepository
	var provider ent.Provider
	if ts.HostingRepoService != nil {
		randomID := gitutil.RandomAlphabeticString(5, 901)
		provider, err = ent.ValueOfProvider(strings.ToUpper(*ts.HostingRepoService))
		if err != nil {
			panic(err)
		}
		hostingServiceOptions := map[string]string{}
		switch provider {
		case ent.GITHUB:
			// the 'gitHubTestUserToken' environment variable is set by the build script
			assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
			hostingServiceOptions = map[string]string{github.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitHubTestUserToken")}
		case ent.GITLAB:
			// the 'gitLabTestUserToken' environment variable is set by the build script
			assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
			hostingServiceOptions = map[string]string{gitlab.AUTHENTICATION_TOKEN_OPTION_NAME: os.Getenv("gitLabTestUserToken")}
		default:
			panic("unknown provider")
		}

		ghs, err := svc.GitHostingServiceInstance(provider, hostingServiceOptions)
		gitHostingService = &ghs
		if err != nil {
			panic(err)
		}
		us, err := svc.UserServiceInstance(provider, hostingServiceOptions)
		gitHostingUserService = &us
		if err != nil {
			panic(err)
		}
		rs, err := svc.ReleaseServiceInstance(provider, hostingServiceOptions)
		gitHostingReleaseService = &rs
		if err != nil {
			panic(err)
		}
		gitHostedRepository, err = (*gitHostingService).CreateGitRepository(randomID, utl.PointerToString("Test repository "+randomID), false, true)

		// if we clone too quickly next calls may fail
		time.Sleep(4000 * time.Millisecond)
	}

	// set up the Git repository
	var script *gittools.Script
	log.Debugf("setting up the Git repository scenario")
	if gitHostingService == nil {
		// in case no hosting service is used, just prepare the repository locally
		s := ts.Scenario.Realize()
		script = &s
		log.Debugf("Git repository scenario created in: %s", script.GetWorkingDirectory())
	} else {
		// in case an hosting service is used, create the local repository by applying the scenarion on a clone of the hosted repository
		switch provider {
		case ent.GITHUB:
			// the 'gitHubTestUserToken' environment variable is set by the build script
			assert.NotEmpty(t, os.Getenv("gitHubTestUserToken"), "A GitHub authentication token must be passed to this test as an environment variable but it was not set")
			s := ts.Scenario.ApplyOnCloneFromWithUserNameAndPassword((*gitHostedRepository).GetHTTPURL(), utl.PointerToString(os.Getenv("gitHubTestUserToken")), utl.PointerToString(""))
			script = &s
		case ent.GITLAB:
			// the 'gitLabTestUserToken' environment variable is set by the build script
			assert.NotEmpty(t, os.Getenv("gitLabTestUserToken"), "A GitLab authentication token must be passed to this test as an environment variable but it was not set")
			s := ts.Scenario.ApplyOnCloneFromWithUserNameAndPassword((*gitHostedRepository).GetHTTPURL(), utl.PointerToString("PRIVATE-TOKEN"), utl.PointerToString(os.Getenv("gitLabTestUserToken")))
			script = &s
		default:
			panic("unknown provider")
		}
		log.Debugf("Git repository scenario created in: %s on clone from %s", script.GetWorkingDirectory(), (*gitHostedRepository).GetHTTPURL())
	}
	defer os.RemoveAll(script.GetWorkingDirectory()) // clean up

	// set up the remote repository, if configured
	var remoteScript *gittools.Script
	if ts.RemoteRepoName != nil {
		rs := gittools.BARE().RealizeBare(true)
		remoteScript = &rs
		script.AddRemote(remoteScript.GetWorkingDirectory(), *ts.RemoteRepoName) // use the GitDirectory even if it's a bare repository as it's managed internally and still points to the repo dir
		defer os.RemoveAll(remoteScript.GetWorkingDirectory())
	}

	// Create the files in the repository
	log.Debugf("creating files in repository")
	if ts.Files != nil && len(ts.Files) > 0 {
		for fileName, fileContent := range ts.Files {
			err := os.WriteFile(filepath.Join(script.GetWorkingDirectory(), fileName), []byte(fileContent), 0644)
			if err != nil {
				panic(err)
			}
		}
	}

	// Prepare the command to run
	log.Debugf("setting up the command to run")
	cmdArgs := ts.Args
	if ts.NyxCommand != nil {
		cmdArgs = append(cmdArgs, *ts.NyxCommand)
	}
	cmd := context.GetCommand(script.GetWorkingDirectory(), ts.Env, cmdArgs)

	// Run the command
	log.Debugf("running command: %v", cmd.String())
	log.Debugf("   in directory              : %v", cmd.Dir)
	log.Debugf("   with %d environment variables", len(cmd.Env))
	//log.Tracef("   with %d environment variables: %v", len(cmd.Env), cmd.Env) // keep this at the trace level as it may expose the token values
	var outBuffer bytes.Buffer
	cmd.Stdout = &outBuffer
	cmd.Stderr = &outBuffer
	runErr := cmd.Run()
	log.Debugf("command output is: *** START ***")
	if runErr != nil || log.IsLevelEnabled(log.DebugLevel) {
		outBuffer.WriteTo(os.Stdout)
	}
	log.Debugf("command output is: ***  END  ***")
	if runErr == nil {
		log.Debugf("command executed without errors")
	} else {
		log.Infof("command executed with error: %v", runErr)
	}
	assert.NoError(t, runErr, "Nyx was expected to run without errors but an error was returned: %v", runErr)

	// Run the checks on file contents
	log.Debugf("running file content checks, if any")
	if ts.FileContentChecks != nil && len(ts.FileContentChecks) > 0 {
		for fileName, fileContentsToCheck := range ts.FileContentChecks {
			absoluteFilePath := filepath.Join(script.GetWorkingDirectory(), fileName)
			_, err := os.Stat(absoluteFilePath)
			assert.NoError(t, err, "file %s does not exist after running Nyx: %v", fileName, err)
			if err != nil {
				continue
			}
			fileContent, err := os.ReadFile(absoluteFilePath)
			assert.NoError(t, err, "file %s can't be read after running Nyx: %v", fileName, err)
			if err != nil {
				continue
			}
			for _, fileContentItemToCheck := range fileContentsToCheck {
				// try first if the file contains the plain string
				plainStringContained := strings.Contains(string(fileContent), fileContentItemToCheck)
				regexMatched := false
				if plainStringContained {
					log.Debugf("file %s contains plain string '%s'", fileName, fileContentItemToCheck)
				} else {
					// if it's not contained as a plain string try matching it as a regular expression
					log.Debugf("file %s does not contain plain string '%s', now trying to interpret the string as a regular expression and matching it", fileName, fileContentItemToCheck)
					re, err := regexp2.Compile(fileContentItemToCheck, 0)
					assert.NoError(t, err, "cannot compile regular expression '%s'", fileContentItemToCheck)
					if err != nil {
						continue
					}
					regexMatched, err = re.MatchString(string(fileContent))
					assert.NoError(t, err, "cannot evaluate regular expression '%s' against '%s'", fileContentItemToCheck, absoluteFilePath)
					if err != nil {
						continue
					}
				}
				assert.True(t, plainStringContained || regexMatched, "file %s was expected to contain plain string '%s' or match it as a regular expression but it doesn't", fileName, fileContentItemToCheck)
				if !(plainStringContained || regexMatched) {
					log.Errorf("file %s content is:", fileName)
					log.Errorln(string(fileContent))
				}
			}
		}
	}

	// Run the checks on the Git repository
	log.Debugf("running tag repository checks")
	if ts.RepositoryTags != nil && len(ts.RepositoryTags) > 0 {
		for _, tagToCheck := range ts.RepositoryTags {
			tagFound := false
			for tagName, _ := range script.GetTags() {
				if tagToCheck == tagName {
					tagFound = true
					break
				}
			}
			assert.True(t, tagFound, "repository was supposed to contain tag '%s' but it only contains tags '%v'", tagToCheck, script.GetTags())
		}
	}

	// Run the checks on the remote Git repository on the local machine
	log.Debugf("running tag remote repository checks")
	if ts.RemoteRepositoryTags != nil && len(ts.RemoteRepositoryTags) > 0 {
		assert.NotNil(t, remoteScript, "checks on the remote repository tags have been defined but the remote repository hasn't been configured")
		for _, tagToCheck := range ts.RemoteRepositoryTags {
			tagFound := false
			for tagName, _ := range remoteScript.GetTags() {
				if tagToCheck == tagName {
					tagFound = true
					break
				}
			}
			assert.True(t, tagFound, "remote repository '%s' was supposed to contain tag '%s' but it only contains tags '%v'", *ts.RemoteRepoName, tagToCheck, remoteScript.GetTags())
		}
	}

	// Run the checks on the hosting Git service
	log.Debugf("running hosting release checks")
	// It's unlikely that we actually test these because the 'services' configuration, required for this to work,
	// need the hosted repository name and owned in advance (in the configuration), but since the repository is
	// created dynamically, this will unlikely be tested
	if ts.HostedReleaseTags != nil && len(ts.HostedReleaseTags) > 0 {
		// if we read too quickly we often get a 404 from the server so let's wait a short while
		time.Sleep(2000 * time.Millisecond)

		assert.NotNil(t, gitHostingReleaseService, "checks on the hosting service releases have been defined but the hosting service hasn't been configured")
		for _, releaseToCheck := range ts.HostedReleaseTags {
			user, err := (*gitHostingUserService).GetAuthenticatedUser()
			assert.NoError(t, err, "unable to get the authenticated user data from the hosting service")
			release, err := (*gitHostingReleaseService).GetReleaseByTag(utl.PointerToString((*user).GetUserName()), utl.PointerToString((*gitHostedRepository).GetName()), releaseToCheck)
			assert.NoError(t, err, "unable to query the releases from the hosting release service for project: %v", (*gitHostedRepository).GetHTTPURL())
			assert.NotNil(t, release, "hosting service '%s' (project '%v') was supposed to contain release '%s' but it doesn't", provider.String(), (*gitHostedRepository).GetHTTPURL(), releaseToCheck)
		}
	}

	log.Debugf("test complete")

	// clean up
	if gitHostedRepository != nil {
		switch provider {
		case ent.GITHUB:
			defer (*gitHostingService).DeleteGitRepository((*gitHostedRepository).GetName())
		case ent.GITLAB:
			defer (*gitHostingService).DeleteGitRepository((*gitHostedRepository).GetID())
		default:
			panic("unknown provider")
		}
	}

	return runErr
}
