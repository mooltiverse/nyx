---
title: Usage
layout: single
toc: true
permalink: /guide/user/introduction/usage/
---

## Available commands

Nyx provides the same features in all the available versions but the means to invoke each may change. This table summarizes the available commands and gives the available name for each when using different versions.

| Name                                        | Command Line Command                   | Gradle Task Name                       |
| ------------------------------------------- | -------------------------------------- | -------------------------------------- |
| [Amend](#amend)                             | `amend`                                | [`nyxAmend`](#nyxamend)                |
| [Clean](#clean)                             | `clean`                                | [`nyxClean`](#nyxclean)                |
| [Help](#help)                               | `help`                                 | N/A                                    |
| [Infer](#infer)                             | `infer`                                | [`nyxInfer`](#nyxinfer)                |
| [Make](#make)                               | `make`                                 | [`nyxMake`](#nyxmake)                  |
| [Publish](#publish)                         | `publish`                              | [`nyxPublish`](#nyxpublish)            |

### Amend

TODO: write this section
{: .notice--warning}

### Clean

Reverts the local state to initial, deleting all the local artifacts that have been created, if any.

This command/task has no dependencies.

### Help

Prints the synopis and usage instructions and exits.

This command has no dependencies.

### Infer

TODO: write this section
{: .notice--warning}

### Make

TODO: write this section
{: .notice--warning}

### Publish

TODO: write this section
{: .notice--warning}


## Using the command line

TODO: write this section
{: .notice--warning}

### Synopsis

TODO: write this section
{: .notice--warning}

### Exit codes

TODO: write this section
{: .notice--warning}

## Using the Gradle plugin

TODO: write this section
{: .notice--warning}

All the examples in this page assume you're using tha plain `gradle` command. If you're using the (recommended) [wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) just change instances of the `gradle` command with `./gradlew`.
{: .notice--info}

You can find more on the Gradle Plugin internals [here]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/gradle-plugin.md %}).

### Requisites

In order to run the Gradle plugin you need:

* Java release `1.8` or later
* Gradle release `5.5` or later. Tests have been successfully executed up to release `6.8.2`. [Here](https://gradle.org/releases/) you can find the list of available releases.

### Core tasks

Once the plugin is applied the following tasks and [dependencies](https://docs.gradle.org/current/userguide/tutorial_using_tasks.html#sec:task_dependencies) are available. All tasks belong to the `Release` group.

When running Gradle there is no specific task to run the [`help`](#help) command but you can use Gradle's commands like `gradle --help` to print the generic command line help or `gradle tasks [--all]` to see the list of available tasks. See the [Gradle Command-Line Interface](https://docs.gradle.org/current/userguide/command_line_interface.html) for more.
{: .notice--info}

#### `nyxAmend`

Runs the [`amend`](#amend) command.

This task has no efferent dependecies defined but [`nyxInfer`](#nyxinfer) depends on on this task.

#### `nyxClean`

Runs the [`clean`](#clean) command.

This task has no efferent dependecies but if the `clean` lifecycle task is defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) plugin) it is attached with a dependency on this task.

#### `nyxInfer`

Runs the [`infer`](#infer) command.

This task depends on the [`nyxAmend`](#nyxamend) task while [`nyxMake`](#nyxmake) depends on on this task.

#### `nyxMake`

Runs the [`make`](#make) command.

This task depends on the [`nyxInfer`](#nyxinfer) task. Moreover, if lifecycle tasks like `assemble` and `build` are defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) they are attached with a dependency on this task.

#### `nyxPublish`

Runs the [`publish`](#publish) command.

This task depends on the [`nyxMake`](#nyxmake) task while the [`release`](#release) task is attached to depend on this task.

### Lifecycle tasks

The following additional [lifecycle tasks](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks) are added for convenience.

#### `release`

The `release` lifecycle task is created if there is no task with the same name already defined. This task provides no actions by itself but is just meant to let you run `gradle release` to run the entire release process.

The newly created or existing `release` task is attached to depend on the [`nyxPublish`](#nyxpublish) task.