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

package command_template

import (
	cmd "github.com/mooltiverse/nyx/modules/go/nyx/command"
	stt "github.com/mooltiverse/nyx/modules/go/nyx/state"
	gittools "github.com/mooltiverse/nyx/modules/go/nyx/test/integration/git/tools"
)

const (
	// The context name returned by this proxy.
	STANDALONE_CONTEXT_NAME = "standalone"
)

/*
This is a proxy implementation to be used in test templates and allows to run a command
standalone, so each method invocation is dispatched directly to the backing Command instance.
*/
type StandaloneCommandProxy struct {
	// The backing command instance
	command *cmd.Command

	// The underlying Git scenario script
	script gittools.Script
}

/*
Constructor.

Arguments are as follows:

- command the backing standalone command instance
- script the underlying Git scenario script
*/
func NewStandaloneCommandProxy(command *cmd.Command, script gittools.Script) *CommandProxy {
	proxy := &StandaloneCommandProxy{}
	proxy.command = command
	proxy.script = script
	var commandProxy CommandProxy
	commandProxy = proxy
	return &commandProxy
}

func (p *StandaloneCommandProxy) GetContextName() string {
	return STANDALONE_CONTEXT_NAME
}

func (p *StandaloneCommandProxy) Script() gittools.Script {
	return p.script
}

func (p *StandaloneCommandProxy) State() *stt.State {
	return (*p.command).State()
}

func (p *StandaloneCommandProxy) IsUpToDate() (bool, error) {
	return (*p.command).IsUpToDate()
}

func (p *StandaloneCommandProxy) Run() (*stt.State, error) {
	return (*p.command).Run()
}
