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
	"os"            // https://pkg.go.dev/os
	"path/filepath" // https://pkg.go.dev/path/filepath
	"testing"       // https://pkg.go.dev/testing

	assert "github.com/stretchr/testify/assert" // https://pkg.go.dev/github.com/stretchr/testify/assert

	cmd "github.com/mooltiverse/nyx/src/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/src/go/nyx/configuration"
	cmdtpl "github.com/mooltiverse/nyx/src/go/nyx/test/integration/command/template"
	gittools "github.com/mooltiverse/nyx/src/go/nyx/test/integration/git/tools"
	utl "github.com/mooltiverse/nyx/src/go/utils"
)

func TestCleanConstructor(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, command)
		})
	}
}

func TestCleanState(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.FROM_SCRATCH()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			assert.NotNil(t, (*command).State())
		})
	}
}

func TestCleanIsUpToDate(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)

			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)

			// always up to date unless we have generated artifacts
			assert.True(t, upToDate)
		})
	}
}

func TestCleanIsUpToDateWithStateFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			stateFilePath := "state-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			configurationLayerMock.SetStateFile(&stateFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			stateFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), stateFilePath))
			assert.NoError(t, err)

			// now it's not up do date anymore
			_, err = stateFile.Stat()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			stateFile.Close()

			_, err = (*command).Run()
			assert.NoError(t, err)

			// now it's up do date again
			_, err = stateFile.Stat()
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
}

func TestCleanIsUpToDateWithSummaryFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			summaryFilePath := "summary-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			configurationLayerMock.SetSummaryFile(&summaryFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			summaryFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), summaryFilePath))
			assert.NoError(t, err)

			// now it's not up do date anymore
			_, err = summaryFile.Stat()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			summaryFile.Close()

			_, err = (*command).Run()
			assert.NoError(t, err)

			// now it's up do date again
			_, err = summaryFile.Stat()
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
}

func TestCleanIsUpToDateWithChangelogFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			changelogFilePath := "changelog-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			changelogFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), changelogFilePath))
			assert.NoError(t, err)

			// now it's not up do date anymore
			_, err = changelogFile.Stat()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			changelogFile.Close()

			_, err = (*command).Run()
			assert.NoError(t, err)

			// now it's up do date again
			_, err = changelogFile.Stat()
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
}

func TestCleanIdempotency(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			stateFilePath := "state-file.txt"
			summaryFilePath := "summary-file.txt"
			changelogFilePath := "changelog-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			configurationLayerMock.SetStateFile(&stateFilePath)
			configurationLayerMock.SetSummaryFile(&summaryFilePath)
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)
			upToDate, err := (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			stateFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), stateFilePath))
			assert.NoError(t, err)
			summaryFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), summaryFilePath))
			assert.NoError(t, err)
			changelogFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), changelogFilePath))
			assert.NoError(t, err)

			// now it's not up do date anymore
			_, err = stateFile.Stat()
			assert.NoError(t, err)
			_, err = summaryFile.Stat()
			assert.NoError(t, err)
			_, err = changelogFile.Stat()
			assert.NoError(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.False(t, upToDate)
			stateFile.Close()
			summaryFile.Close()
			changelogFile.Close()

			_, err = (*command).Run()
			assert.NoError(t, err)

			// now it's up do date again
			_, err = os.Stat(filepath.Join((*command).Script().GetWorkingDirectory(), stateFilePath))
			assert.Error(t, err)
			_, err = os.Stat(filepath.Join((*command).Script().GetWorkingDirectory(), summaryFilePath))
			assert.Error(t, err)
			_, err = os.Stat(filepath.Join((*command).Script().GetWorkingDirectory(), changelogFilePath))
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)

			// run again and test for idempotency
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = os.Stat(stateFilePath)
			assert.Error(t, err)
			_, err = os.Stat(summaryFilePath)
			assert.Error(t, err)
			_, err = os.Stat(changelogFilePath)
			assert.Error(t, err)
			upToDate, err = (*command).IsUpToDate()
			assert.NoError(t, err)
			assert.True(t, upToDate)
		})
	}
}

func TestCleanRunDeleteStateFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			stateFilePath := "state-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			configurationLayerMock.SetStateFile(&stateFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)

			stateFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), stateFilePath))
			assert.NoError(t, err)

			_, err = stateFile.Stat()
			assert.NoError(t, err)
			stateFile.Close()

			// now running the clean must delete the file
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = stateFile.Stat()
			assert.Error(t, err)

			// run again and test for idempotency
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = stateFile.Stat()
			assert.Error(t, err)
		})
	}
}

func TestCleanRunDeleteSummaryFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			summaryFilePath := "summary-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			configurationLayerMock.SetSummaryFile(&summaryFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)

			summaryFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), summaryFilePath))
			assert.NoError(t, err)

			_, err = summaryFile.Stat()
			assert.NoError(t, err)
			summaryFile.Close()

			// now running the clean must delete the file
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = summaryFile.Stat()
			assert.Error(t, err)

			// run again and test for idempotency
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = summaryFile.Stat()
			assert.Error(t, err)
		})
	}
}

func TestCleanRunDeleteChangelogFile(t *testing.T) {
	for _, command := range cmdtpl.CommandInvocationProxies(cmd.CLEAN, gittools.INITIAL_COMMIT()) {
		t.Run((*command).GetContextName(), func(t *testing.T) {
			defer os.RemoveAll((*command).Script().GetWorkingDirectory())
			changelogFilePath := "changelog-file.txt"
			configurationLayerMock := cnf.NewSimpleConfigurationLayer()
			configurationLayerMock.SetDirectory(utl.PointerToString((*command).Script().GetWorkingDirectory()))
			changelogConfiguration, _ := configurationLayerMock.GetChangelog()
			changelogConfiguration.SetPath(&changelogFilePath)
			var configurationLayer cnf.ConfigurationLayer
			configurationLayer = configurationLayerMock
			(*command).State().GetConfiguration().WithRuntimeConfiguration(&configurationLayer)

			// run once, to start
			_, err := (*command).Run()
			assert.NoError(t, err)

			changelogFile, err := os.Create(filepath.Join((*command).Script().GetWorkingDirectory(), changelogFilePath))
			assert.NoError(t, err)

			_, err = changelogFile.Stat()
			assert.NoError(t, err)
			changelogFile.Close()

			// now running the clean must delete the file
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = changelogFile.Stat()
			assert.Error(t, err)

			// run again and test for idempotency
			_, err = (*command).Run()
			assert.NoError(t, err)
			_, err = changelogFile.Stat()
			assert.Error(t, err)
		})
	}
}
