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
| [Arrange](#arrange)                         | `arrange`                              | [`nyxArrange`](#nyxarrange)            |
| [Clean](#clean)                             | `clean`                                | [`nyxClean`](#nyxclean)                |
| [Help](#help)                               | `help`                                 | N/A                                    |
| [Infer](#infer)                             | `infer`                                | [`nyxInfer`](#nyxinfer)                |
| [Make](#make)                               | `make`                                 | [`nyxMake`](#nyxmake)                  |
| [Mark](#mark)                               | `mark`                                 | [`nyxMark`](#nyxmark)                  |
| [Publish](#publish)                         | `publish`                              | [`nyxPublish`](#nyxpublish)            |

### Arrange

TODO: write this section
{: .notice--warning}

### Clean

Reverts the local state to initial, deleting all the local artifacts that have been created, if any.

This command/task has no dependencies.

### Help

Prints the synopis and usage instructions and exits.

This command has no dependencies.

### Infer

Scans the Git repository searching for significant information used to generate the new release. This command never applies any change as it just collects and computes all the required information to be used next.

### Make

TODO: write this section
{: .notice--warning}

### Mark

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
* Gradle release `6.0` or later. Tests have been successfully executed up to release `6.8.2`. [Here](https://gradle.org/releases/) you can find the list of available releases.

### Apply the plugin

To apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) (using the [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block)) edit your `build.gradle` file and add the new plugin:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

Now you need to [configure the plugin](#configure-the-plugin) before you can run any task.

### Configure the plugin

You have different means to configure the plugin and in these sections we'll introduce you to all of them. Whichever combination of configuration means you use, see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) for a detailed description of each option.

The [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option is not handled like other options because it would overlap [Gradle's standard `version` project property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties). The plugin reads and writes the standard Gradle option to provide a consistent behavior and reduce impact on existing build scripts. See [here]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#the-version-in-gradle-scripts) for more.
{: .notice--warning}

TODO: insert links here to the various configuration means and a short description on how to use a hybrid configuration approach, mixing different means.
{: .notice--warning}

#### Using the extension

The easier way to configure Nyx is via a Gradle *extension*, which is a configuration block named `nyx` you add inside your `build.gradle` script, like this:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

nyx {
  // configuration options here...
  // TODO: add the bare minimum options here
}
```

#### Using configuration files

TODO: write this section
{: .notice--warning}

### Core tasks

Once the plugin is applied the following tasks and [dependencies](https://docs.gradle.org/current/userguide/tutorial_using_tasks.html#sec:task_dependencies) are available. All tasks belong to the `Release` group.

When running Gradle there is no specific task to run the [`help`](#help) command but you can use Gradle's commands like `gradle --help` to print the generic command line help or `gradle tasks [--all]` to see the list of available tasks. See the [Gradle Command-Line Interface](https://docs.gradle.org/current/userguide/command_line_interface.html) for more.
{: .notice--info}

#### `nyxArrange`

Runs the [`arrange`](#arrange) command.

This task has no efferent dependecies defined but [`nyxInfer`](#nyxinfer) depends on on this task.

#### `nyxClean`

Runs the [`clean`](#clean) command.

This task has no efferent dependecies but if the `clean` lifecycle task is defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) plugin) it is attached with a dependency on this task.

#### `nyxInfer`

Runs the [`infer`](#infer) command.

This task depends on the [`nyxArrange`](#nyxarrange) task while [`nyxMake`](#nyxmake) depends on on this task.

#### `nyxMake`

Runs the [`make`](#make) command.

This task depends on the [`nyxInfer`](#nyxinfer) task while [`nyxMark`](#nyxmark) depends on on this task. Moreover, if lifecycle tasks like `assemble` and `build` are defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) they are attached with a dependency on this task.

#### `nyxMark`

Runs the [`mark`](#mark) command.

This task depends on the [`nyxMake`](#nyxmake) task while [`nyxPublish`](#nyxpublish) depends on on this task.

#### `nyxPublish`

Runs the [`publish`](#publish) command.

This task depends on the [`nyxMark`](#nyxmark) task while the [`release`](#release) task is attached to depend on this task.

### Lifecycle tasks

The following additional [lifecycle tasks](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks) are added for convenience.

#### `release`

The `release` lifecycle task is created if there is no task with the same name already defined. This task provides no actions by itself but is just meant to let you run `gradle release` to run the entire release process.

The newly created or existing `release` task is attached to depend on the [`nyxPublish`](#nyxpublish) task.
