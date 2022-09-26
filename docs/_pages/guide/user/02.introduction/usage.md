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
| Clean                                       | `clean`                                | [`nyxClean`](#nyxclean)                |
| Help                                        | `help`                                 | N/A                                    |
| Infer                                       | `infer`                                | [`nyxInfer`](#nyxinfer)                |
| Make                                        | `make`                                 | [`nyxMake`](#nyxmake)                  |
| Mark                                        | `mark`                                 | [`nyxMark`](#nyxmark)                  |
| Publish                                     | `publish`                              | [`nyxPublish`](#nyxpublish)            |

## Using the command line

The command line is not yet available and this section is marked as a TODO.
{: .notice--warning}

### Synopsis

The command line is not yet available and this section is marked as a TODO.
{: .notice--warning}

### Exit codes

The command line is not yet available and this section is marked as a TODO.
{: .notice--warning}

## Using the Gradle plugin

When using Gradle, the native Nyx plugin is the easiest and most effective way of using Nyx.

All the examples in this page assume you're using the plain `gradle` command. If you're using the (recommended) [wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) just change instances of the `gradle` command with `./gradlew`.
{: .notice--info}

Some extra information on the Gradle Plugin internals is available [here]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/gradle-plugin.md %}).
{: .notice--info}

### Requisites

In order to run the Gradle plugin you need:

* Java release `11` or later
* Gradle release `6.0` or later. Tests have been successfully executed up to release `7.5.1`. [Here](https://gradle.org/releases/) you can find the list of available releases.

### Apply the plugin

You can apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) as a *project plugin* or as a *settings plugin*. You're suggested to use the *settings plugin* to make sure you have properties are evaluated early in the build lifecycle as detailed in [this post]({{ site.baseurl }}{% link _posts/2020-01-01-the-gradle-version-project-property-is-unspecified.md %}).

In both cases you can use the same [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block):

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

Add the above definition to the `settings.gradle` file to deploy the plugin as a *settings plugin* (suggested) or to the `build.gradle` file to deploy the plugin as a *project plugin*.

### Configure the plugin

You have different means to configure the plugin and in these sections we'll introduce you to all of them. Whichever combination of configuration means you use, see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) for a detailed description of each option.

The [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option is not handled like other options because it would overlap [Gradle's standard `version` project property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties). The plugin reads and writes the standard Gradle option to provide a consistent behavior and reduce impact on existing build scripts. See [here]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#the-version-in-gradle-scripts) for more.
{: .notice--warning}

You can also mix different configuration means according to the [configuration evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) in order to use what better suits your needs. Mixed configurations are often used to reuse some parts or entire configurations across projects and organizations or to override some inherited values on a project basis.

Examples of different configuration files are available [here]({{ site.baseurl }}/examples/).

#### Using the extension

The easier way to configure Nyx is via a Gradle *extension*, which is a configuration block named `nyx` you add inside your `settings.gradle` or `build.gradle` script (depending on where you applied the plugin), like this:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

nyx {
  // configuration block
}
```

The *configuration block* shown above is where you can place configuration options defined in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) using the [Gradle]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#gradle) syntax.

#### Using configuration files

If you rather prefer to configure Nyx by means of configuration files you can use one of the [default configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) that are looked up automatically or set the [`configurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file) global option to look for a file in a custom location.

#### Multi-project builds

If you have a [multi-project build](https://docs.gradle.org/current/userguide/multi_project_builds.html) and apply the plugin in the `settings.gradle` file you can only apply it to the root project as that's how Gradle restricts the `settings.gradle`. On the other hand, if you use the `settings.gradle`, the Nyx plugin should be applied to the root project only as it's supposed to be where the Git repository is located.

Nyx makes no hard checks whether it's applied to the root project or a subproject so if you know what you're doing you can apply the plugin to one of the subprojects as well.
{: .notice--warning}

To use the project version from subprojects you should use the `rootProject.version` property instead of the simple `version` or `project.version` or you may get the undesired *unspecified* value in place of the expected version.

If using the *project plugin*, in order to be sure the version has been set before you use it, add a dependency from the task you read the `rootProject.version` to the [`nyxInfer`](#nyxinfer) task in the root project, like:

```groovy
tasks.register('myTask')  {
    dependsOn ':nyxInfer'
    doLast {
        println rootProject.version
    }
}
```

or

```groovy
tasks.myTask.dependsOn rootProject.tasks.nyxInfer
```

This dependency is not needed when using the *settings plugin*.

You may also propagate the root project version to all sub projects if that's what you need, and to do so you can add the following block in the root project build script:

```groovy
subprojects {
    version = rootProject.version
}
```

### Core tasks

Once the plugin is applied the following tasks and [dependencies](https://docs.gradle.org/current/userguide/tutorial_using_tasks.html#sec:task_dependencies) are available. All tasks belong to the `Release` group.

When running Gradle there is no specific task to run the [`help`](#help) command but you can use Gradle's commands like `gradle --help` to print the generic command line help or `gradle tasks [--all]` to see the list of available tasks. See the [Gradle Command-Line Interface](https://docs.gradle.org/current/userguide/command_line_interface.html) for more.
{: .notice--info}

#### `nyxClean`

Runs the `clean` command.

This task has no efferent dependecies but if the `clean` lifecycle task is defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) plugin) it is attached with a dependency on this task.

#### `nyxInfer`

Runs the `infer` command.

This task has no efferent dependecies while [`nyxMake`](#nyxmake) depends on on this task.

When the plugin is [applied](#apply-the-plugin) as a *settings plugin* you don't need to run this task explicitly as it's implicitly executed in the [initialization](https://docs.gradle.org/current/userguide/build_lifecycle.html) phase.

Otherwise, when the plugin is applied as a *project plugin*, since this is the task that actually makes available all the information required for a new release (including the [Gradle's `version` property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties)), you should make all tasks that need that information [dependent](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:adding_dependencies_to_tasks) on this, directly or indirectly.

For example:

```groovy
tasks.myTask.dependsOn rootProject.tasks.nyxInfer
```

or 

```groovy
tasks.register('myTask')  {
    dependsOn ':nyxInfer'
    doLast {
        ...
    }
}
```

See [this post]({{ site.baseurl }}{% link _posts/2020-01-01-the-gradle-version-project-property-is-unspecified.md %}) for more on the early inference of the `version` and other properties.
{: .notice--info}

#### `nyxMake`

Runs the `make` command.

This task depends on the [`nyxInfer`](#nyxinfer) task while [`nyxMark`](#nyxmark) depends on on this task. Moreover, if an `assemble` lifecycle tasks is defined (like when using the [Base](https://docs.gradle.org/current/userguide/base_plugin.html) plugin) it is attached with a dependency on this task.

#### `nyxMark`

Runs the `mark` command.

This task depends on the [`nyxMake`](#nyxmake) task while [`nyxPublish`](#nyxpublish) depends on on this task.

#### `nyxPublish`

Runs the `publish` command.

This task depends on the [`nyxMark`](#nyxmark) task while the [`release`](#release) task is attached to depend on this task.

### Lifecycle tasks

The following additional [lifecycle tasks](https://docs.gradle.org/current/userguide/more_about_tasks.html#sec:lifecycle_tasks) are added for convenience.

#### `release`

The `release` lifecycle task is created if there is no task with the same name already defined. This task provides no actions by itself but is just meant to let you run `gradle release` to run the entire release process.

The newly created or existing `release` task is attached to depend on the [`nyxPublish`](#nyxpublish) task.

### Accessing the Nyx State extra project property from build scripts

Another nifty feature that may save you a bunch of coding is accessing the [Nyx State]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) from within a Gradle script.

The entire State is bound to the project *extra properties* with the `nyxState` property name so, starting from that, you can read any property exported by Nyx.

The `nyxState` property is only available after at least one [core task](#core-tasks) has executed. When using the [settings plugin](#apply-the-plugin) this is not an issue as the tasks run automatically in the early phases. Not all State properties are available at the same type (after the same task execution), please check out the [reference]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) for each property to know when it's available.
{: .notice--info}

This way you can have all the Nyx properties handy without even [storing the State file]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) on disk. For example, to only run a task if the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) contains [significant changes]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits) you can check the `project.nyxState.releaseScope.significantCommits` list property while to reuse the same timestamp used by Nyx you can read the [`project.nyxState.significant`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#timestamp).

Example:

```groovy
task dumpSomeDiagnostics() {
    dependsOn nyxInfer
    doLast {
        println project.nyxState.bump
        println project.nyxState.directory.getAbsolutePath()
        println project.nyxState.scheme.toString()
        println Long.valueOf(project.nyxState.timestamp).toString()
        println project.nyxState.version
    }
}
```

In this example `dumpSomeDiagnostics` represents some arbitrary task that depends on [`nyxInfer`](#nyxinfer) just to make sure the `nyxState` is available and is used to print a few attributes from the state.