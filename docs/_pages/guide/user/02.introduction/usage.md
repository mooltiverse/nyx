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
| Infer                                       | `infer`                                | [`nyxInfer`](#nyxinfer)                |
| Make                                        | `make`                                 | [`nyxMake`](#nyxmake)                  |
| Mark                                        | `mark`                                 | [`nyxMark`](#nyxmark)                  |
| Publish                                     | `publish`                              | [`nyxPublish`](#nyxpublish)            |

## Using the command line

### Download the binary

You can find the latest binaries for any platform in the [latest release](https://github.com/mooltiverse/nyx/releases/latest) assets.

Make sure you store the binary so that it's available from the `PATH` and it has execution permissions.

This guide assumes you rename the executable as `nyx`, regardless of the platform (or `nyx.exe` on Windows).
{: .notice--info}

This is all you need to use Nyx on the command line.

### Synopsis

Nyx comes with a whole lot of arguments that you can pass on the command line to let you control every single aspect of its behavior. You can see them all by running `nyx --help`, from which we have an abbreviated output here:

```bash
nyx --help

Nyx version: {{ site.data.nyx.version }}

Usage:
    nyx [arguments] [command]

Commands are:
    clean               reverts the repository to its initial state and removes files created by other commands, if any
    infer               inspects the commit history and repository status and computes the project version
    make                produces artifacts (i.e. changelog) as per the configuration
    mark                commits, tags and pushes, according to the configuration and the repository status
    publish             publish the new release, if any, to the configured services

Global arguments are:
    [...]

Changelog arguments are:
    [...]

Commit Message Conventions arguments are:
    [...]

Git arguments are:
    [...]

Git arguments are:
    [...]

Release Type arguments are:
    [...]

Services arguments are:
    [...]
```

As you can see you can give as much arguments as you like and one command at the end. Arguments start with a single (`-`) or double dash (`--`) and are detailed in the full output from `nyx --help` and in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}#configuration-file).

The command needs to appear at the end of the command line with no dash sign. If you don't specify any command Nyx will run the default one `infer`. For an introduction on what every command does see [below](#commands) or [How Nyx Works]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}).

#### Dynamic argument names

One thing that is important to note is that many arguments names are dynamic and this may be confusing at first as it's different than common arguments on other tools. For example, the [`--services-<NAME>-type=<TYPE>`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#type) argument has a dynamic `<NAME>` in the attribute name.

This is to allow users to define the `<TYPE>` value for arbitrary services whose name is dynamically configured. When passing this as an actual argument on the command line also the `<NAME>` part needs to be replaced so for example `--services-gh-type=GITHUB` sets the type `GITHUB` for a service instance configured under the name `gh`.

This kind of notation implies that the internal configuration model always creates a service configuration named `gh` whenever an argument like this is provided. This is somewhat different from configuration files where you need to create a `gh` section and then define additional options underneath.

Since argument names are dynamically defined Nyx will not complain in case of unsupported arguments.
{: .notice--info}

### Run the command

All you have to do is run the command according to the synopsis above. Remember to run Nyx from the Git project directory or pass the [`--directory`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#directory) argument.

Also keep in mind that when no command is given on the command line [`infer`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is executed by default. This will generate and display the latest version for the project but will not change anything in the project directory. To run other commands you can specify them explicitly, as shown below.

### Configuration

You have different means to configure the tool. Whichever combination you use, see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) for a detailed description of each option.

In some cases you might prefer using command line arguments while in others configuration files (or even a combination of) might be the way you pick. In order to support some edge cases, all options can also be passed as environment variables.

Different configuration methods can be combined together with well known priorities of each means over the others so you have complete freedom to create configuration baselines and override values as you need. This might be especially useful for large organizations. See the [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) for more on this.

#### Using command line arguments

With the above said, you can start easy with:

```bash
nyx --preset=simple infer
```

This runs the *infer* command and uses the [*simple* preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}).

Keep in mind that while presets are a handy way to avoid repeating streamlined configurations, you don't need to use them and you can define your own from scratch by whatever configuration means.

#### Using configuration files

If you rather prefer to configure Nyx by means of configuration files you can use one of the [default configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) that are looked up automatically or set the [`configurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file) global option to look for a file in a custom location.

Nonetheless, you can override configuration options from configuration files with command line arguments.

### The Nyx State

You can enable writing the Nyx [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) file by using the [`--state-file`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) argument. You can have it in JSON or YAML, depending on the extension you set. Example:

```bash
nyx --state-file=nyx-state.json infer
```

After this command runs you can read the `nyx-state.json` file to inspect its contents and use them for other purposes. Check out the [State Reference]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) to know more about the contents and how powerful this can be.

### Exit codes

Nyx always returns 0 as the exit code unless some error occured, in which case 1 or other values other than 0 are returned.

## Using the Docker image

Many teams prefer using Docker containers for their CI/CD and local development environments. If that's your case you can use the Docker container that comes out of the box with Nyx.

The Docker images are published onto the two main registries:

* [Docker Hub](https://hub.docker.com/repository/docker/mooltiverse/nyx) (the default)
* [GitHub Container Registry](https://github.com/mooltiverse/nyx/pkgs/container/nyx)

Nyx' Docker image has a small footprint (less than 10Mb) and uses [Alpine Linux](https://www.alpinelinux.org/) as the base image.

All the examples in this page assume you're using the `latest` image pulling it from [Docker Hub](https://hub.docker.com/repository/docker/mooltiverse/nyx). They also assume you're making an ephemeral use of the container (you run it and dispose it every time). In case you need advanced use cases other than these please refer to the official [Docker Documentation](https://docs.docker.com/).
{: .notice--info}

### Requisites

All you need is a recent version of [Docker](https://www.docker.com/) installed and running.

### Pull the image

Let's start by pulling the image from the public registry. Open a shell and run:

```bash
$ docker pull mooltiverse/nyx:latest
latest: Pulling from mooltiverse/nyx
ab6db1bc80d0: Pull complete
[...]
Digest: sha256:976f4821d643e02fc55c884cd6c9af5e958011132145150b7dd97e01d71ba055
Status: Downloaded newer image for mooltiverse/latest
mooltiverse/latest
```

and make sure the new image is there:

```bash
$ docker image ls
REPOSITORY                                            TAG                 IMAGE ID       CREATED        SIZE
mooltiverse/nyx                                       latest              a14cbc284e81   2 days ago     7.35MB
```

### Run the container

To run the container you simply run a command like:

```bash
$ docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest
```

A few things to note here:

1. there is no explicit command executed within the container because [`nyx infer`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is executed by default. This will generate and display the latest version for the project but will not change anything in the project directory. To run other commands you can specify them explicitly, as shown below
2. a local folder (`/local/path/to/project`) is mounted from the host into the container at the default location `/project`. You need to change `/local/path/to/project` to whatever path is hosting your Git repository, or mount a Docker volume hosting the Git repository in case you already have one (in this case, pass your volume name to the command line like `docker run -it --rm -v project-volume:/project mooltiverse/nyx:latest [...]`, where `project-volume` has to be changed to your volume name)
3. Nyx is using the default configuration means ([configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars) available in the project directory at standard locations)

When running on Linux or Mac hosts you may also want to map the host user ID to the container user in order to avoid issue with file permissions. You can do this by adding this option to the command line: `-u $(id -u):$(id -g)`. See the [Docker run reference](https://docs.docker.com/engine/reference/run/) for more.
{: .notice--info}

#### Running specific commands

In order to run a specific command you need to pass it on the command line, like:

```bash
$ docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest <COMMAND>
```

So if you need to run the [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) command, run the container as:

```bash
$ docker run -it --rm -v /local/path/to/project:/project mooltiverse/nyx:latest publish
```

#### Mounting the project volume or folder

You can pass Nyx your project folder as a path on the host or as a Docker volume. What you need to do is:

* replace `/local/path/to/project` with an actual host path when the project directory is shared from the Docker host to the container, or
* replace `/local/path/to/project` with a Docker volume name when the project directory is already in a Docker volume

For more on volumes and mounts please see [the official Docker documentation](https://docs.docker.com/storage/volumes/).

#### Passing the configuration to Nyx

Generally speaking, configuring Nyx within a container is just like using it from the [command line](#using-the-command-line). This means that if [configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars) are available in the project directory at their default locations they will be loaded as usual and if additional options are passed on the [command line]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#command-line-options) they will be used.

The one caveat abous passing configuration as [environment variables]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#environment-variables) in a Docker container is about setting those variables by means of one or more `-e` [flags](https://docs.docker.com/engine/reference/run/#env-environment-variables).

## Using the Gradle plugin

When using Gradle, the native Nyx plugin is the easiest and most effective way of using Nyx. The plugin can be used with the Groovy or the Kotlin syntax, although this documentation always refers to Groovy unless otherwise specified.

All the examples in this page assume you're using the plain `gradle` command. If you're using the (recommended) [wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) just change instances of the `gradle` command with `./gradlew`.
{: .notice--info}

Some extra information on the Gradle Plugin internals is available [here]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/gradle-plugin.md %}).
{: .notice--info}

### Apply the plugin

You can apply the [plugin](https://plugins.gradle.org/plugin/com.mooltiverse.oss.nyx) as a *project plugin* or as a *settings plugin*. You're suggested to use the *settings plugin* to make sure you have properties are evaluated early in the build lifecycle as detailed in [this post]({{ site.baseurl }}{% link _posts/2020-01-01-the-gradle-version-project-property-is-unspecified.md %}).

When using Groovy you can use the same [plugin DSL](https://docs.gradle.org/current/userguide/plugins.html#sec:plugins_block) in both cases:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}
```

Add the above definition to the `settings.gradle` file to deploy the plugin as a *settings plugin* (suggested) or to the `build.gradle` file to deploy the plugin as a *project plugin*.

If you're using Kotlin you can use this example for your `build.gradle.kts` or `settings.gradle.kts` when applying the *project plugin* or *settings plugin*, respectively:

```kotlin
plugins {
  id("com.mooltiverse.oss.nyx") version "{{ site.data.nyx.version }}"
}
```

### Configure the plugin

You have different means to configure the plugin and in these sections we'll introduce you to all of them. Whichever combination of configuration means you use, see the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) for a detailed description of each option.

The [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option is not handled like other options because it would overlap [Gradle's standard `version` project property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties). The plugin reads and writes the standard Gradle option to provide a consistent behavior and reduce impact on existing build scripts. See [here]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#the-version-in-gradle-scripts) for more.
{: .notice--warning}

You can also mix different configuration means according to the [configuration evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) in order to use what better suits your needs. Mixed configurations are often used to reuse some parts or entire configurations across projects and organizations or to override some inherited values on a project basis.

Examples of different configuration files are available [here]({{ site.baseurl }}/examples/).

#### Using the extension

The easier way to configure Nyx is via a Gradle *extension*.

When using Groovy the extension is a configuration block named `nyx` you add inside your `settings.gradle` or `build.gradle` script (depending on where and how you applied the plugin, as a *settings plugin* or *project plugin*, respectively), like this:

```groovy
plugins {
  id "com.mooltiverse.oss.nyx" version "{{ site.data.nyx.version }}"
}

nyx {
  // a few examples
  dryRun = false
  resume = false
  stateFile = '.nyx-state.yml'
  verbosity = "INFO"
}
```

The *configuration block* shown above is where you can place configuration options defined in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) using the [Gradle]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#gradle) syntax.

When using Kotlin you still have a configuration block very similar to the one above but with some differences.

Configuring the *project plugin* using Kotlin you add something like this to your `build.gradle.kts`:

```kotlin
plugins {
  id("com.mooltiverse.oss.nyx") version "{{ site.data.nyx.version }}"
}

nyx {
  // a few examples
  dryRun.set(false)
  resume.set(false)
  stateFile.set(".nyx-state.yml")
  verbosity.set("DEBUG")
}
```

As you can see Kotlin uses a different way to set values. If, instead, you're configuring the *settings plugin* in Kotlin your `settings.gradle.kts` file looks like:

```kotlin
plugins {
  id("com.mooltiverse.oss.nyx") version "{{ site.data.nyx.version }}"
}

configure<com.mooltiverse.oss.nyx.gradle.NyxExtension> {
  // a few examples
  dryRun.set(false)
  resume.set(false)
  stateFile.set(".nyx-state.yml")
  verbosity.set("DEBUG")
}
```

So, when using Kotlin, the difference between configuring the *project plugin* or *settings plugin* is in the way the configuration block starts.

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