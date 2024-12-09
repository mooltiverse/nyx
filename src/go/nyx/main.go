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
This package provides the command line entry point for Nyx.

See https://mooltiverse.github.io/nyx/guide/developer/go/ for the developer's guide.
*/
package main

import (
	"fmt"     // https://golang.org/pkg/fmt
	"os"      // https://golang.org/pkg/os
	"strings" // https://golang.org/pkg/strings

	log "github.com/sirupsen/logrus" // https://pkg.go.dev/github.com/sirupsen/logrus
	slices "golang.org/x/exp/slices" // https://pkg.go.dev/golang.org/x/exp/slices

	err "github.com/mooltiverse/nyx/src/go/errors"
	cmd "github.com/mooltiverse/nyx/src/go/nyx/command"
	cnf "github.com/mooltiverse/nyx/src/go/nyx/configuration"
)

const (
	// the default command to run when no command is set on the command line
	DEFAULT_COMMAND = cmd.INFER
)

var (
	release = "undefined" // This is overwritten using 'go build' ldflags
)

/*
Scans the given command line arguments and selects the command to run among the arguments.

An error is returned if an unrecognized command is selected or multiple commands are defined.

If no command is defined then the default command is returned.

Arguments are as follows:

- args the command line arguments, it must not contain the first command line argument (as it's the executable name)
*/
func selectCommand(args []string) (cmd.Commands, error) {
	log.Debugf("selecting the command to run")

	var commands []cmd.Commands
	// scan the command line arguments to find the commands
	// commands should be the only arguments without a '-' or 'prefix'
	for _, arg := range args {
		if arg != "" && !strings.HasPrefix(arg, "-") {
			commandValue, err := cmd.ValueOfCommands(strings.ToUpper(arg))
			if err != nil {
				return DEFAULT_COMMAND, err
			}
			commands = append(commands, commandValue)
		}
	}

	if len(commands) > 1 {
		return DEFAULT_COMMAND, &err.IllegalPropertyError{Message: fmt.Sprintf("only one command can be specified, while multiple commands have been passed '%v'", commands)}
	}
	if len(commands) == 1 {
		log.Debugf("command to run is %s", commands[0].String())
		return commands[0], nil
	}
	log.Debugf("no command has been passed on the command line, running the default %s", DEFAULT_COMMAND.String())
	return DEFAULT_COMMAND, nil
}

/*
Entry point.
*/
func main() {
	// check if the user has set the --help flag, in which case just print the help and exit
	if slices.Contains(os.Args, cnf.HELP_ARGUMENT_NAME) {
		fmt.Println()
		fmt.Printf("Nyx version: %s\n", release)
		fmt.Println()
		cnf.PrintHelp()
		fmt.Println()
		os.Exit(0)
	}

	nyx := NewNyx()

	// set the global logger verbosity as soon as possible
	configuration, err := nyx.Configuration()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	verbosity, err := configuration.GetVerbosity()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	log.SetLevel(verbosity.GetLevel())

	command, err := selectCommand(os.Args[1:])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	err = nyx.Run(command)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	summary, err := configuration.GetSummary()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	if summary != nil && *summary {
		state, err := nyx.State()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		summary, err := state.Summary()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		fmt.Println(summary)
	}

	os.Exit(0)
}
