---
title: Global Options
layout: single
toc: true
permalink: /guide/user/configuration-reference/global-options/
---

These are the top level options in the configuration:

| Name                                                      | Type    | Command Line Option                                       | Environment Variable                                          | Configuration File Option                                     | Default  |
| --------------------------------------------------------- | ------- | --------------------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------------------------- | -------- |
| [`amendBranch`](#amend-branch)                            | boolean | `--amend-branch=true|false`                               | `NYX_AMEND_BRANCH=true|false`                                 | `amendBranch`                                                 | `false`  |
| [`branch`](#branch)                                       | string  | `--branch=<NAME>`                                         | `NYX_BRANCH=<NAME>`                                           | `branch`                                                      | Current branch pointed by `HEAD` |
| [`bump`](#bump)                                           | string  | `--bump=<NAME>`                                           | `NYX_BUMP=<NAME>`                                             | `bump`                                                        |          |
| [`commitMessageConventions`](#commit-message-conventions) | object  | [`commitMessageConventions`](#commit-message-conventions) | See [`commitMessageConventions`](#commit-message-conventions) | See [`commitMessageConventions`](#commit-message-conventions) |          |
| [`configurationFile`](#configuration-file)                | string  | `-c <PATH>`, `--configuration-file=<PATH>`                | `NYX_CONFIGURATION_FILE=<PATH>`                               | `configurationFile`                                           | N/A      |
| [`directory`](#directory)                                 | string  | `-d <PATH>`, `--directory=<PATH>`                         | `NYX_DIRECTORY=<PATH>`                                        | `directory`                                                   | Current working directory |
| [`dryRun`](#dry-run)                                      | boolean | `--dry-run=true|false`                                    | `NYX_DRY_RUN=true|false`                                      | `dryRun`                                                      | `false`  |
| [`environment`](#environment)                             | string  | `--environment=<NAME>`                                    | `NYX_ENVIRONMENT=<NAME>`                                      | `environment`                                                 | `auto`   |
| [`environments`](#environments)                           | object  | See [`environments`](#environments)                       | See [`environments`](#environments)                           | See [`environments`](#environments)                           |          |
| [`extraIdentifiers`](#extra-identifiers)                  | object  | See [`extraIdentifiers`](#extra-identifiers)              | See [`extraIdentifiers`](#extra-identifiers)                  | See [`extraIdentifiers`](#extra-identifiers)                  |          |
| [`initialCommit`](#initial-and-final-commit)              | string  | `--initial-commit=<SHA>`                                  | `NYX_INITIAL_COMMIT=<SHA>`                                    | `initialCommit`                                               | N/A      |
| [`initialVersion`](#initial-version)                      | string  | `--initial-version=<VERSION>`                             | `NYX_INITIAL_VERSION=<VERSION>`                               | `initialVersion`                                              | Depends on the [version scheme](#scheme) |
| [`finalCommit`](#initial-and-final-commit)                | string  | `--final-commit=<SHA>`                                    | `NYX_FINAL_COMMIT=<SHA>`                                      | `finalCommit`                                                 | Current commit pointed by `HEAD` |
| [`preset`](#preset)                                       | string  | `--preset=<NAME>`                                         | `NYX_PRESET=<NAME>`                                           | `preset`                                                      |          |
| [`releasePrefix`](#release-prefix)                        | string  | `--release-prefix=<PREFIX>`                               | `NYX_RELEASE_PREFIX=<PREFIX>`                                 | `releasePrefix`                                               | `v`      |
| [`releasePrefixLenient`](#release-prefix-lenient)         | boolean | `--release-prefix-lenient=true|false`                     | `NYX_RELEASE_PREFIX_LENIENT=true|false`                       | `releasePrefixLenient`                                        | `true`   |
| [`releaseType`](#release-type)                            | string  | `--release-type=<NAME>`                                   | `NYX_RELEASE_TYPE=<NAME>`                                     | `releaseType`                                                 | `auto`   |
| [`releaseTypes`](#release-types)                          | object  | See [`releaseTypes`](#release-types)                      | See [`releaseTypes`](#release-types)                          | See [`releaseTypes`](#release-types)                          |          |
| [`scheme`](#scheme)                                       | string  | `--scheme=<NAME>`                                         | `NYX_SCHEME=<NAME>`                                           | `scheme`                                                      | `semver` |
| [`services`](#services)                                   | object  | See [`services`](#services)                               | See [`services`](#services)                                   | See [`services`](#services)                                   |          |
| [`sharedConfigurationFile`](#shared-configuration-file)   | string  | `--shared-configuration-file=<PATH>`                      | `NYX_SHARED_CONFIGURATION_FILE=<PATH>`                        | `sharedConfigurationFile`                                     | N/A      |
| [`stateFile`](#state-file)                                | string  | `--state-file=<PATH>`                                     | `NYX_STATE_FILE=<PATH>`                                       | `stateFile`                                                   | N/A      |
| [`verbosity`](#verbosity)                                 | string  | `--verbosity=<LEVEL>`                                     | `NYX_VERBOSITY=<LEVEL>`                                       | `verbosity`                                                   | `quiet`  |
| [`version`](#version)                                     | string  | `-v <VERSION>`, `--version=<VERSION>`                     | `NYX_VERSION=<VERSION>`                                       | `version`                                                     | N/A      |

### Amend branch

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `amendBranch`                                                                            |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--amend-branch=true|false`                                                              |
| Environment Variable      | `NYX_AMEND_BRANCH=true|false`                                                            |
| Configuration File Option | `amendBranch`                                                                            |
| Related state attributes  | [`branch`]({{ site.baseurl }}/reference/state/#release-scope-attributes)                  |


By default Nyx does not perform any action in the [amend]({{ site.baseurl }}/reference/usage/#amend) phase unless you turn this option `true`. If you do, Nyx [checks out](https://git-scm.com/docs/git-checkout) and [pulls](https://git-scm.com/docs/git-pull) the given [branch](#branch), if needed.

When this option is `true` the [branch](#branch) option must also be set to let Nyx know which branch to check out.

Amending the current branch might be useful when the local repository is in the *[detached HEAD state](https://www.git-tower.com/learn/git/faq/detached-head-when-checkout-commit/)*.

Before enabling this option make sure you understand the consequences as explained for the [amend]({{ site.baseurl }}/reference/usage/#amend) command. When unsure just leave this option to default value (`false`).

### Branch

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `branch`                                                                                 |
| Type                      | string                                                                                   |
| Default                   | Current branch pointed by `HEAD`                                                         |
| Command Line Option       | `--branch=<NAME>`                                                                        |
| Environment Variable      | `NYX_BRANCH=<NAME>`                                                                      |
| Configuration File Option | `branch`                                                                                 |
| Related state attributes  | [`branch`]({{ site.baseurl }}/reference/state/#release-scope-attributes)                  |

By default Nyx infers the current Git branch by the local repository, which is the branch referred by `HEAD`.

This option overrides the default behavior and forces a specific branch to be used. If you use this option in conjunction with [`amendBranch`](#amend-branch) then Nyx will also try to checkout the given branch and make it current in the [amend]({{ site.baseurl }}/reference/usage/#amend) phase, otherwise it will just check if the current branch matches with this value and fail if they don't match. In this latter scenario you use this option to enforce a certain branch to be current.

Overriding the branch and using the [amend]({{ site.baseurl }}/reference/usage/#amend) command might be useful when the local repository is in the *[detached HEAD state](https://www.git-tower.com/learn/git/faq/detached-head-when-checkout-commit/)*.

When unsure just leave this option unset.

### Bump

TODO: write this section
{: .notice--warning}

### Configuration file

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `configurationFile`                                                                      |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `-c <PATH>`, `--configuration-file=<PATH>`                                               |
| Environment Variable      | `NYX_CONFIGURATION_FILE=<PATH>`                                                          |
| Configuration File Option | `configurationFile`                                                                      |
| Related state attributes  | [`directory`]({{ site.baseurl }}/reference/state/#global-attributes)                     |

This option allows you to load a configuration file from a location other than default ones. This can be a relative or absolute path to a local file or an URL to load a remote file. This configuration file can override other options, as per the [evaluation order](#evaluation-order), and can be authored as `.yaml`, `.json` or `.properties` (the format is inferred by the file extension) just like the default configuration files.

Configuration files have higher priority over [shared configuration files](#shared-configuration-file) so they can override specific options inherited from shared files.

Please consider using a [standard location for configuration files](#evaluation-order) before using a custom one, so you don't even need to use this option.
{: .notice--info}

This option can be used as a command line option, as an environment variable and also inside a configuration file. However, only **standard** configuration files can contain this option to specify another custom file to override values from. This option cannot be used in **custom** configuration files to avoid chaining.
{: .notice--info}

### Directory

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `directory`                                                                              |
| Type                      | string                                                                                   |
| Default                   | Current working directory                                                                |
| Command Line Option       | `-d <PATH>`, `--directory=<PATH>`                                                        |
| Environment Variable      | `NYX_DIRECTORY=<PATH>`                                                                   |
| Configuration File Option | `directory`                                                                              |
| Related state attributes  | [`directory`]({{ site.baseurl }}/reference/state/#global-attributes)                     |

Sets the working directory for Nyx. The directory is where Nyx searches for the Git repository and is also used as the base path when relative paths to local files or directories.

By default Nyx uses the process' working directory for this.

### Dry run

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `dryRun`                                                                                 |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--dry-run=true|false`                                                                   |
| Environment Variable      | `NYX_DRY_RUN=true|false`                                                                 |
| Configuration File Option | `dryRun`                                                                                 |
| Related state attributes  |                                                                                          |

When this flag is set to `true` (default is `false`) Nyx will not take any action that alters the repository state, either local or remote. Instead it will just print the actions that would be taken if this flag was not set.

When enabling this flag you probably want to raise the [verbosity](#verbosity).

### Environment

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `environment`                                                                            |
| Type                      | string                                                                                   |
| Default                   | `auto`                                                                                   |
| Command Line Option       | `--environment=<NAME>`                                                                   |
| Environment Variable      | `NYX_ENVIRONMENT=<NAME>`                                                                 |
| Configuration File Option | `environment`                                                                            |
| Related state attributes  | [`environment`]({{ site.baseurl }}/reference/state/#environment-attributes)              |

Forces Nyx to select the [configured environment](#environments) with this name instead of trying to [detect it based on runtime facts](#auto-detect).

Defaults to `auto`, which tells Nyx to detect the runtime environment. You should use this only when Nyx is not able to detect the environment automatically and you have configured no suitable [default environment](#default-environment) (or multiple ones).

### Initial and final commit

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `initialCommit`, `finalCommit`                                                           |
| Type                      | string                                                                                   |
| Default                   | [inferred]({{ site.baseurl }}/reference/how-nyx-works/) from the workspace               |
| Command Line Option       | `--initial-commit=<SHA>`, `--final-commit=<SHA>`                                         |
| Environment Variable      | `NYX_INITIAL_COMMIT=<SHA>`, `NYX_FINAL_COMMIT=<SHA>`                                     |
| Configuration File Option | `initialCommit`, `finalCommit`                                                           |
| Related state attributes  | [`initialCommit`]({{ site.baseurl }}/reference/state/#release-scope-attributes), [`finalCommit`]({{ site.baseurl }}/reference/state/#release-scope-attributes) |

Allowed values for initial and final commits are valid SHAs, either long or short.

By default Nyx defines the **release scope** by using the current commit identified by `HEAD` as the **final commit** and the one right after the latest released commit as the **initial commit**, as explained [here]({{ site.baseurl }}/reference/how-nyx-works/).

You can override the **release scope** by setting a custom initial commit, final commit, or both. This might be useful to issue a release after the repository has gone forward with other commits. A few rules to keep in mind when setting the scope manually:

* the initial commit must be reachable from the end commit by reading the *parent* recursively
* if you override the initial commit only, the final commit is the one referenced by the `HEAD`
* if you override the final commit only, the initial commit is determined by following the [default logic]({{ site.baseurl }}/reference/how-nyx-works/)
* when overriding the initial commit the [version](#version) must be overridden as well as the **latest release** can't be automatically determined

Be careful when overriding the release scope as you may cause [version constraints]({{ site.baseurl }}/reference/release-strategy/#release-types) issues or other inconsistencies in you releases.
{: .notice--warning}

Examples of issue you may cause when manually setting the scope:

* you may create a release whose scope contains commits that have already been included in other releases
* you may leave a gap between commits, like when the initial commit and the previously released commit are not adjacent
* you may try to release a version whose number doesn't comply with ordering constraints (i.e. `v1.1.0` and `v1.1.1` were already issued and you try to make a new release less than `v1.1.0`)

Of course you may use the above at your advantage but **extreme caution** must be used when overriding the scope.

### Initial version

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `initialVersion`                                                                         |
| Type                      | string                                                                                   |
| Default                   | Depends on the configured [version scheme](#scheme)                                      |
| Command Line Option       | `--initial-version=<VERSION>`                                                            |
| Environment Variable      | `NYX_INITIAL_VERSION=<VERSION>`                                                          |
| Configuration File Option | `initialVersion`                                                                         |
| Related state attributes  | [`previousVersion`]({{ site.baseurl }}/reference/state/#release-scope-attributes), [`primeVersion`]({{ site.baseurl }}/reference/state/#release-scope-attributes) |

The default version to use when no previous version can be [inferred]({{ site.baseurl }}/reference/how-nyx-works/) from the commit history. When not specified this value is taken from the configured [version scheme](#scheme).

This value is ignored when a previous version is [inferred]({{ site.baseurl }}/reference/how-nyx-works/) from the commit history or when the [version](#version) is overridden.

This might be useful for project bootstrapping only (from Nyx's perspective) when the default initial version just doesn't fit your need. Remember that this value will be considered **just once** because after the next release (with [tagging](#git-tag) enabled) further executions will be able to infer the previous version.

### Preset

TODO: write this section
{: .notice--warning}

### Release prefix

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releasePrefix`                                                                          |
| Type                      | string                                                                                   |
| Default                   | `v`                                                                                      |
| Command Line Option       | `--release-prefix=<PREFIX>`                                                              |
| Environment Variable      | `NYX_RELEASE_PREFIX=<PREFIX>`                                                            |
| Configuration File Option | `releasePrefix`                                                                          |
| Related state attributes  |                                                                                          |

It's a common practice to add a leading string to version numbers to give releases a name. Common prefixes are `v` or `rel` but you might use anything, or no prefix at all.

Nyx uses `v` as a default prefix so, provided versions like `x.y.x`, releases and tags are named `vx.y.x`. If you wish not to use any prefix set this option to null or an empty string.

When specifying a custom prefix, only use alphanumeric characters (no numbers or special characters).

Release names are not necessarily equal to versions. For example version `1.2.3` can be issued by the release named `v1.2.3`. This is a small difference but helps [coping with the strict interpretation of valid version identifiers]({{ site.baseurl }}{% link _posts/2020-01-01-what-is-the-difference-between-version-and-release.md %}).
{: .notice--info}

This option only affects the way Nyx **generates** release names and tags, while [`releasePrefixLenient`](#release-prefix-lenient) controls how tolerant Nyx is when **reading** release tags from the Git repository.

### Release prefix lenient

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releasePrefixLenient`                                                                   |
| Type                      | boolean                                                                                  |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--release-prefix-lenient=true|false`                                                    |
| Environment Variable      | `NYX_RELEASE_PREFIX_LENIENT=true|false`                                                  |
| Configuration File Option | `releasePrefixLenient`                                                                   |
| Related state attributes  |                                                                                          |

When this option is enabled (by default), Nyx will attempt to tolerate prefixes when **reading** Git tags. When `true`, tags like `vx.y.x`, `relx.y.x` etc will be detected as release tags (on version `x.y.x`), regardless of the [prefix Nyx uses to **generate** release names](#release-prefix). On the other hand, when this is `false`, Nyx will only detect tags strictly complying with the [Release Prefix](#release-prefix) so, for instance, when [Release Prefix](#release-prefix) is set to `v`, Nyx will only detect tags of the form `vx.y.x` while if the [Release Prefix](#release-prefix) is not used, only `x.y.x` tags are detected.

The prefixes of tags parsed by Nyx with this option enabled don't need to use the same [prefix used to create release names](#release-prefix).

### Release type

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseType`                                                                            |
| Type                      | string                                                                                   |
| Default                   | `auto`                                                                                   |
| Command Line Option       | `--release-type=<NAME>`                                                                  |
| Environment Variable      | `NYX_RELEASE_TYPE=<NAME>`                                                                |
| Configuration File Option | `releaseType`                                                                            |
| Related state attributes  | [`releaseType`]({{ site.baseurl }}/reference/state/#release-type-attributes)             |

Forces Nyx to select the [configured release type](#release-types) with this [name](#name-release-type) instead of trying to detect it automatically by [matching branches](#on-branches).

Defaults to `auto`, which tells Nyx to detect the release type.

You should use this only when Nyx is not able to detect the release type automatically and you have configured no suitable default.

### Scheme

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `scheme`                                                                                 |
| Type                      | string                                                                                   |
| Default                   | `semver`                                                                                 |
| Command Line Option       | `--scheme=<NAME>`                                                                        |
| Environment Variable      | `NYX_SCHEME=<NAME>`                                                                      |
| Configuration File Option | `scheme`                                                                                 |
| Related state attributes  | [`scheme`]({{ site.baseurl }}/reference/state/#global-attributes)                        |

Selects the [version scheme]({{ site.baseurl }}/reference/version-schemes/) to use. Defaults to [`semver`]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver).

### Shared configuration file

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `sharedConfigurationFile`                                                                |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--shared-configuration-file=<PATH>`                                                     |
| Environment Variable      | `NYX_SHARED_CONFIGURATION_FILE=<PATH>`                                                   |
| Configuration File Option | `sharedConfigurationFile`                                                                |
| Related state attributes  |                                                                                          |

This option allows you to load a **shared** configuration file from a location other than default ones. This can be a relative or absolute path to a local file or an URL to load a remote file. This configuration file can be authored as `.yaml`, `.json` or `.properties` (the format is inferred by the file extension) just like the default configuration files.

Shared configuration files are meant to share a common set of configuration options among projects and they have lower priority over [regular configuration files](#configuration-file) which, in turn, can be used to override specific options inherited from shared files.

Please consider using a [standard location for configuration files](#evaluation-order) before using a custom one, so you don't even need to use this option.
{: .notice--info}

This option can be used as a command line option, as an environment variable and also inside a configuration file. However, only **standard** configuration files can contain this option to specify another custom file to override values from. This option cannot be used in **custom** configuration files to avoid chaining.
{: .notice--info}

### State file

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `stateFile`                                                                              |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--state-file=<PATH>`                                                                    |
| Environment Variable      | `NYX_STATE_FILE=<PATH>`                                                                  |
| Configuration File Option | `stateFile`                                                                              |
| Related state attributes  |                                                                                          |

Enables the creation of the [state file]({{ site.baseurl }}/reference/output/#state-file) where Nyx stores its findings and generated values.

The value passed here can be:

* a simple file name that will be interpred as local to the current working directory
* a relative path that will be interpreted as relative to the current working directory
* an absolute file name

Nyx will infer the format of the file by the extension, where available ones are [`.properties`](https://en.wikipedia.org/wiki/.properties), [`.yaml`](https://yaml.org/), [`.json`](https://www.json.org/). This way, if you need to read the file for your own purposes, you can have it in the format that is more accessible to you.

The suggested name for the file, when used, is `.nyx-state.<EXT>` (so `.nyx-state.properties`, `.nyx-state.yaml` or `.nyx-state.json`). It's highly recommended to let Git [ignore](https://git-scm.com/docs/gitignore) this file.
{: .notice--primary}

In order to comply with the *[immutable workspace]({{ site.baseurl }}/in-depth/design-principles/#immutable-workspace)* principle the state file is disabled by default. However, enabling its is highly recommended when using Nyx from the command line and invoking its [commands]({{ site.baseurl }}/reference/usage/#available-commands) individually, also to make sure you make the process *[incremental]({{ site.baseurl }}/in-depth/design-principles/#incremental-process)*.
{: .notice--info}

### Verbosity

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `verbosity`                                                                              |
| Type                      | string                                                                                   |
| Default                   | `quiet`                                                                                  |
| Command Line Option       | `--verbosity=<LEVEL>`                                                                    |
| Environment Variable      | `NYX_VERBOSITY=<LEVEL>`                                                                  |
| Configuration File Option | `verbosity`                                                                              |
| Related state attributes  |                                                                                          |

Controls the amount of output emitted by Nyx, where values are:

* `fatal`: only prints critical errors that prevent Nyx to complete. No output is printed when no fatal error is encountered
* `error`: only prints errors along with some essential informations
* `quiet`: only prints important essential output (like the generated version number)
* `warning`: only prints errors and warnings along with some essential informations
* `info` : prints high level informations about the internal action that Nyx runs
* `debug`: prints informations obout internals that can be useful to debug Nyx in case of anomalies
* `trace`: this is the most verbose option that prints lots of informations, including internals

The **Gradle** plugin infers the verbosity by the [Gradle log level](https://docs.gradle.org/current/userguide/command_line_interface.html#setting_log_level) by default, so once you run Gradle with a verbosity option that is automatically inherited by the Nyx plugin, unless it's overridden. When doing so, [Gradle log levels](https://docs.gradle.org/current/userguide/logging.html#logLevels) translate to Nyx verbosity levels as follows:

* `QUIET` > `quiet`
* `WARN` > `warning`
* `INFO` > `info`
* `DEBUG` > `debug`

You need to override the verbosity level if you want to set it to `fatal`, `error`, `trace` as Gradle has no corresponding value.

### Version

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `version`                                                                                |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `-v <VERSION>`, `--version=<VERSION>`                                                    |
| Environment Variable      | `NYX_VERSION=<VERSION>`                                                                  |
| Configuration File Option | `version`                                                                                |
| Related state attributes  | [`version`]({{ site.baseurl }}/reference/state/#global-attributes)                       |

Overrides the **current version** generated by Nyx. When overriding this value Nyx will not attempt to [detect the **latest release** or do the **scope analysis**]({{ site.baseurl }}/reference/how-nyx-works/) (unless it needs so for other tasks like [generating artifacts]({{ site.baseurl }}/reference/output/#artifacts)) so you take control of the version number generation instead of letting Nyx do that for you.

If you want Nyx to take control over the version number (as you should) just do not override this value.

When you override the [version](#version) you **never** need to [infer]({{ site.baseurl }}/reference/how-nyx-works/) the version number from the commit history and you use manual versioning instead of taking advantage of Nyx's *auto pilot*.
{: .notice--info}

#### The version in Gradle scripts

In **Gradle** build scripts this option is not available as an [extension configuration](#gradle) but the [standard Gradle project property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) is used instead. This grants consistency between the Gradle environment and Nyx. Example:

```groovy
version = 'v1.2.3'
nyx {
    // no 'version' option here
}
```

The entire Gradle script will be able to read the version number inferred by Nyx by reading the [version](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) property. In other words, the `version` property is both a configuration option and an [output value]({{ site.baseurl }}/reference/output/) to Nyx when used as a Gradle plugin.