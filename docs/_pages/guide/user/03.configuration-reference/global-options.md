---
title: Global Options
layout: single
toc: true
permalink: /guide/user/configuration-reference/global-options/
---

These are the top level options in the configuration:

| Name                                                      | Type    | Command Line Option                                       | Environment Variable                                          | Configuration File Option                                     | Default  |
| --------------------------------------------------------- | ------- | --------------------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------------------------- | -------- |
| [`bump`](#bump)                                           | string  | `-b <NAME>`, `--bump=<NAME>`                              | `NYX_BUMP=<NAME>`                                             | `bump`                                                        | N/A      |
| [`directory`](#directory)                                 | string  | `-d <PATH>`, `--directory=<PATH>`                         | `NYX_DIRECTORY=<PATH>`                                        | `directory`                                                   | Current working directory |
| [`dryRun`](#dry-run)                                      | boolean | `--dry-run`, `--dry-run=true|false`                       | `NYX_DRY_RUN=true|false`                                      | `dryRun`                                                      | `false`  |
| [`releasePrefix`](#release-prefix)                        | string  | `--release-prefix=<PREFIX>`                               | `NYX_RELEASE_PREFIX=<PREFIX>`                                 | `releasePrefix`                                               | `v`      |
| [`releasePrefixLenient`](#release-prefix-lenient)         | boolean | `--release-prefix-lenient`, `--release-prefix-lenient=true|false` | `NYX_RELEASE_PREFIX_LENIENT=true|false`               | `releasePrefixLenient`                                        | `true`   |
| [`scheme`](#scheme)                                       | string  | `--scheme=<NAME>`                                         | `NYX_SCHEME=<NAME>`                                           | `scheme`                                                      | `semver` |
| [`verbosity`](#verbosity)                                 | string  | `--verbosity=<LEVEL>`, `--fatal`, `--error`, `--warning`, `--info`, `--debug`, `--trace` | `NYX_VERBOSITY=<LEVEL>`        | `verbosity`                                                   | `warning`|
| [`version`](#version)                                     | string  | `-v <VERSION>`, `--version=<VERSION>`                     | `NYX_VERSION=<VERSION>`                                       | `version`                                                     | N/A      |

### Bump

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `bump`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `-b <NAME>`, `--bump=<NAME>`                                                             |
| Environment Variable      | `NYX_BUMP=<NAME>`                                                                        |
| Configuration File Option | `bump`                                                                                   |
| Related state attributes  | [TODO: put the link to the *version* state attribute here](){: .btn .btn--info .btn--small} |

Instructs Nyx on which identifier to bump on the past version in order to build the new version.

This option prevents Nyx to [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) the identifier to bump from the commit history.

Valid values for this options are the identifier names for the current version [`scheme`](#scheme).

Using [Semver]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) they are `core`, `major`, `minor`. Names other than these are interpreted as pre-release identifier names so, for example, if the past version is `1.2.3` and you set this option to `alpha`, the new [`version`](#version) will be `1.2.3-alpha.1` at the first run, `1.2.3-alpha.2` the second and so on.

This option has no effect if [`version`](#version) is also used.

### Directory

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `directory`                                                                              |
| Type                      | string                                                                                   |
| Default                   | Current working directory                                                                |
| Command Line Option       | `-d <PATH>`, `--directory=<PATH>`                                                        |
| Environment Variable      | `NYX_DIRECTORY=<PATH>`                                                                   |
| Configuration File Option | `directory`                                                                              |
| Related state attributes  | [TODO: put the link to the *directory* state attribute here](){: .btn .btn--info .btn--small} |

Sets the working directory for Nyx. The directory is where Nyx searches for the Git repository and is also used as the base path when relative paths to local files or directories.

By default Nyx uses the process' working directory for this.

The [**Gradle plugin**]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin) reads the current working directory by the [`projectDir`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) property by default, unless it's overridden by this configuration.

### Dry run

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `dryRun`                                                                                 |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--dry-run`, `--dry-run=true|false`                                                      |
| Environment Variable      | `NYX_DRY_RUN=true|false`                                                                 |
| Configuration File Option | `dryRun`                                                                                 |
| Related state attributes  |                                                                                          |

When this flag is set to `true` no action altering the repository state, either local or remote, is taken. Instead the actions that would be taken if this flag was not set are printed to the log.

When used with no value on the command line (i.e. `--dry-run` alone) `true` is assumed.

When enabling this flag you probably want to raise the [verbosity](#verbosity).

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
| Command Line Option       | `--release-prefix-lenient`, `--release-prefix-lenient=true|false`                        |
| Environment Variable      | `NYX_RELEASE_PREFIX_LENIENT=true|false`                                                  |
| Configuration File Option | `releasePrefixLenient`                                                                   |
| Related state attributes  |                                                                                          |

When this option is enabled (it is by default), Nyx will attempt to tolerate prefixes when **reading** Git tags from the commit history. When `true`, tags like `vx.y.x`, `relx.y.x` etc will be detected as release tags (for *version* `x.y.x`), regardless of the [prefix Nyx uses to **generate** release names](#release-prefix). On the other hand, when this is `false`, Nyx will only detect tags strictly complying with the [release prefix](#release-prefix) so, for instance, when `releasePrefix` is set to `v`, Nyx will only detect tags of the form `vx.y.x` while if the `releasePrefix` is not used, only `x.y.x` tags are detected.

The prefixes of tags parsed by Nyx with this option enabled don't need to use the same prefix as `releasePrefix` so, for instance, if `releasePrefix` is `v`, Nyx can parse tags like `x.y.x`, `vx.y.x`, `relx.y.x` etc.

When used with no value on the command line (i.e. `--release-prefix-lenient` alone) `true` is assumed.

### Scheme

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `scheme`                                                                                 |
| Type                      | string                                                                                   |
| Default                   | `semver`                                                                                 |
| Command Line Option       | `--scheme=<NAME>`                                                                        |
| Environment Variable      | `NYX_SCHEME=<NAME>`                                                                      |
| Configuration File Option | `scheme`                                                                                 |
| Related state attributes  | [TODO: put the link to the *scheme* state attribute here](){: .btn .btn--info .btn--small} |

Selects the [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) to use. Defaults to [`semver`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

### Verbosity

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `verbosity`                                                                              |
| Type                      | string                                                                                   |
| Default                   | `warning`                                                                                |
| Command Line Option       | `--verbosity=<LEVEL>`, `--fatal`, `--error`, `--warning`, `--info`, `--debug`, `--trace` |
| Environment Variable      | `NYX_VERBOSITY=<LEVEL>`                                                                  |
| Configuration File Option | `verbosity`                                                                              |
| Related state attributes  |                                                                                          |

Controls the amount of output emitted by Nyx, where values are:

* `fatal`: only prints critical errors that prevent Nyx to complete. No output is printed when no fatal error is encountered
* `error`: only prints errors along with some essential informations
* `warning`: only prints errors and warnings along with some essential informations (like the generated version number)
* `info` : prints high level informations about the internal action that Nyx runs
* `debug`: prints informations obout internals that can be useful to debug Nyx in case of anomalies
* `trace`: this is the most verbose option that prints lots of informations, including internals

The [**command line**]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-command-line) supports a shorthand option for each verbosity level so you can also use `--fatal`, `--error`, `--warning`, `--info`, `--debug`, `--trace` instead of `--verbosity=<LEVEL>`. When multiple verbosity levels are given, the most verbose one is used.

The [Java library]({{ site.baseurl }}{% link _pages/guide/developer/java/nyx-core.md %}) ignores this setting as it is supposed to be managed by the application using the library, as explained [here]({{ site.baseurl }}{% link _pages/guide/developer/java/nyx-core.md %}#logging).

The [**Gradle plugin**]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#using-the-gradle-plugin) manages its own [log level](https://docs.gradle.org/current/userguide/command_line_interface.html#setting_log_level) and being the Nyx plugin a [library]({{ site.baseurl }}{% link _pages/guide/developer/java/nyx-core.md %}) used by Gradle, it just conforms to Gradle's verbosity. In other words, when using the Gradle plugin you just have to set Gradle's verbosity.

### Version

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `version`                                                                                |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `-v <VERSION>`, `--version=<VERSION>`                                                    |
| Environment Variable      | `NYX_VERSION=<VERSION>`                                                                  |
| Configuration File Option | `version`                                                                                |
| Related state attributes  | [TODO: put the link to the *version* state attribute here](){: .btn .btn--info .btn--small} |

Overrides the version and prevents Nyx to [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer). When overriding this value you take over the tool and go the *manual versioning* way so Nyx won't try to read past versions from the commit history nor determine which identifiers to bump.

#### The version in Gradle scripts

In **Gradle** build scripts the [`version` standard Gradle project property](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) is used instead of an [extension configuration](#gradle) to grant consistency between the Gradle environment and Nyx. Example:

```groovy
version = 'v1.2.3'
nyx {
    // no 'version' option here
}
```

The entire Gradle script will be able to read the version number inferred by Nyx by reading the [`version`](https://docs.gradle.org/current/userguide/writing_build_scripts.html#sec:standard_project_properties) property. In other words, the `version` property is both a configuration option and an output value to Nyx when used as a Gradle plugin.

In order to [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) the version you should not assign any value to the `version` property in the Gradle script (which is to say you need to remove any `version = ...` definition) but you can still read it.
