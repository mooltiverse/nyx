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
	cmd "github.com/mooltiverse/nyx/src/go/nyx/command"
	nyx "github.com/mooltiverse/nyx/src/go/nyx/nyx"
	stt "github.com/mooltiverse/nyx/src/go/nyx/state"
	gittools "github.com/mooltiverse/nyx/src/go/nyx/test/integration/git/tools"
)

const (
	// The context name returned by this proxy.
	NYX_CONTEXT_NAME = "nyx"
)

/*
This is a proxy implementation to be used in test templates and allows to run a command
through the Nyx class business methods.
*/
type NyxCommandProxy struct {
	// The Nyx class private instance.
	nyx *nyx.Nyx

	// The command to be invoked by this class.
	command cmd.Commands

	// The underlying Git scenario script
	script gittools.Script
}

/*
Constructor.

Arguments are as follows:

- nyx the Nyx instance to use to run the command
- command the command to run when the Run method is invoked.
- script the underlying Git scenario script
*/
func NewNyxCommandProxy(nyx *nyx.Nyx, command cmd.Commands, script gittools.Script) *CommandProxy {
	proxy := &NyxCommandProxy{}
	proxy.nyx = nyx
	proxy.command = command
	proxy.script = script
	var commandProxy CommandProxy
	commandProxy = proxy
	return &commandProxy
}

func (p *NyxCommandProxy) GetContextName() string {
	return NYX_CONTEXT_NAME
}

func (p *NyxCommandProxy) Script() gittools.Script {
	return p.script
}

func (p *NyxCommandProxy) State() *stt.State {
	state, err := (*p.nyx).State()
	if err != nil {
		panic(err)
	}
	return state
}

func (p *NyxCommandProxy) IsUpToDate() (bool, error) {
	return (*p.nyx).IsUpToDate(p.command)
}

func (p *NyxCommandProxy) Run() (*stt.State, error) {
	err := (*p.nyx).Run(p.command)
	return p.State(), err
}
