---
title: Nyx Main
layout: single
toc: true
permalink: /guide/developer/go/nyx-main/
---

You can use the main Nyx library to embed it into your project and use all or some of its features. The [`github.com/mooltiverse/nyx/modules/go/nyx`](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx) package brings the [`Nyx`](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx#Nyx){:target="_blank"} structure that is the entry point for all the available commands and features.

## Get the library

To install the module locally run:

```bash
go get github.com/mooltiverse/nyx/modules/go/nyx
```

To import it in your source code:

```go
import "github.com/mooltiverse/nyx/modules/go/nyx"
```

## API docs

Thanks to [Go docs](https://godocs.io/) you can browse the API docs at [this URL](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx).

## Using the library

Using the library is simple. You just need to create a [`Nyx`](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx#Nyx){:target="_blank"} instance and run the `publish` command. It takes just one line of code, like:

```go
package main

import nyx "github.com/mooltiverse/nyx/modules/go/nyx"

func main() {
    n := nyx.NewNyx()
    err := n.Publish() // This is it!
}
```

In this example Nyx loads the configuration from the files it optionally finds at their [default locations]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) and runs the `publish` command, which also implies `infer`, `mark` and `make`.

You can get more control on the behavior by injecting some configuration programmatically and running tasks one by one. You can also start Nyx in a specific directory, get access to the internal Git [repository](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx#Nyx.Repository){:target="_blank"} object and even the internal [state](https://godocs.io/github.com/mooltiverse/nyx/modules/go/nyx#Nyx.State){:target="_blank"}, like in this example:

```go
package main

import (
    "fmt"
    nyx "github.com/mooltiverse/nyx/modules/go/nyx"
    cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
)

func main() {
    n := nyx.NewNyxIn("~/project") // Nyx now runs on the '~/project' directory

    // Create a new configuration layer, set some options, and add it on top
    // of other layers at the 'command line' layer level
    configurationLayer := cnf.NewSimpleConfigurationLayer()
    dryRun := true
    prefix := "rel"
    configurationLayer.SetDryRun(&dryRun) // make it run dry
    configurationLayer.SetReleasePrefix(&prefix) // make it use 'rel' as the prefix for generated versions

    var cl cnf.ConfigurationLayer = configurationLayer // this is for casting, but you can use reflections
    n.Configuration().WithCommandLineConfiguration(&cl) // inject the configuration

    err := n.Infer() // let Nyx infer values from the Git repository

    // now we have plenty of values in the State, let's read some...
    branch, _ := n.State().GetBranch()
    version, _ := n.State().GetVersion()
    fmt.Println(*branch)
    fmt.Println(*version)

    // it might be a good place to run some custom tasks of yours, i.e. using the Git Repository
    // let's say you create a RELEASE_NOTES.md file and want to commit it
    n.Repository().Commit("Adding RELEASE_NOTES.md")

    // then run the remaining tasks one by one
    n.Make()
    n.Mark()
    n.Publish()
}
```

### Logging

Nyx uses [Logrus](https://godocs.io/github.com/sirupsen/logrus) for logging.

You can control verbosity by setting configuration parameters using one of the [available means]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) or programmatically by injecting a configuration layer like:

```go
package main

import (
    "fmt"
    nyx "github.com/mooltiverse/nyx/modules/go/nyx"
    cnf "github.com/mooltiverse/nyx/modules/go/nyx/configuration"
    ent "github.com/mooltiverse/nyx/modules/go/nyx/entities"
)

func main() {
    n := nyx.NewNyxIn("~/project") // Nyx now runs on the '~/project' directory

    // Create a new configuration layer, set some options, and add it on top
    // of other layers at the 'command line' layer level
    configurationLayer := cnf.NewSimpleConfigurationLayer()
    verbosity := ent.INFO
    configurationLayer.SetVerbosity(&verbosity) // set logging to INFO

    var cl cnf.ConfigurationLayer = configurationLayer // this is for casting, but you can use reflections
    n.Configuration().WithRuntimeConfiguration(&cl) // inject the configuration
}
```
