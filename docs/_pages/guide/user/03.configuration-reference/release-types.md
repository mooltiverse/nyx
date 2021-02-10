---
title: Release Types
layout: single
toc: true
permalink: /guide/user/configuration-reference/release-types/
---

Release types are definitions that allow conditional settings to be used depending on *what* Nyx is releasing.

[Release types]({{ site.baseurl }}/reference/release-types/) are configured within the `releaseTypes` *section*. The section allows one sub-section for each release type and some overall options.

You can have as many release types as you want. You can use [default ones]({{ site.baseurl }}/reference/release-types/#preset-release-types) that come bundled with Nyx, override them completely or for just a few attributes or define your own from scratch.

### Release types overall options

| Name                                             | Type   | Command Line Option                    | Environment Variable                | Configuration File Option              | Default                                |
| ------------------------------------------------ | -------| -------------------------------------- | ----------------------------------- | -------------------------------------- | -------------------------------------- |
| [`enabled`](#enabled-release-types)              | list   | `--release-types-enabled=<NAMES>`      | `NYX_RELEASE_TYPES_ENABLED=<NAMES>` | `releaseTypes/enabled`                 | `main,maturity,stage,release,maintenance,internal` |

#### Enabled release types

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `enabled`                                                                                |
| Type                      | list                                                                                     |
| Default                   | `main,maturity,stage,release,maintenance,internal`                                       |
| Command Line Option       | `--release-types-enabled=<NAMES>`                                                        |
| Environment Variable      | `NYX_RELEASE_TYPES_ENABLED=<NAMES>`                                                      |
| Configuration File Option | `releaseTypes/enabled`                                                                   |
| Related state attributes  | [`releaseType`]({{ site.baseurl }}/reference/state/#release-type-attributes)             |

The comma separated list of release types that are enabled for the project. Here you can enable or disable the various release types, either custom or default. By default all [preset release types](#release-type-presets) are enabled.

Each item in the list must correspond to a release type `name` attribute. Each named release type must exist, but not all defined release types must be enabled here. Release types not listed here will just be ignored by Nyx as if they were not even defined.

The order in which release types are listed matters. The types listed first are evaluated first, so in case of ambiguous matches the order disambiguates.
{: .notice--info}

### Release type definition

Within the `releaseTypes` block you can define as many types as you want, each in its own separate block. The `name` identifies the type so to define a brand new release type make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a release type (given the [evaluation order](#evaluation-order)) then you are **overriding** an existing type and when you do, you can override a single attribute or all of them.

Configuring release types gives Nyx informations about:

* how to assume which type to select given a certain set of facts that are automatically inferred or overridden by user. The rules defining how to match a release type are [`onBranches`](#on-branches), [`onEnvironments`](#on-environments) and [`onWorkspaceStatus`](#on-workspace-status) and they are evaluated by an `AND` logic so **they must all evaluate `true` to make a succesful match**
* the actions to take for each release type, which is basically a workflow that can be different for each type

Each release type has the following attributes:

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                     | Configuration File Option                | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | -------------------------------------------------------- | ---------------------------------------- | ------------------------------------------ |
| [`collapseVersions`](#collapse-versions)       | boolean | `--release-types-<INDEX>-collapse-versions=true|false`     | `NYX_RELEASE_TYPES_<INDEX>_COLLAPSE_VERSIONS=true|false` | `releaseTypes/<INDEX>/collapseVersions`  | `false`                                    |
| [`enabledIdentifiers`](#enabled-identifiers)   | list    | `--release-types-<INDEX>-enabled-identifiers=<NAMES>`      | `NYX_RELEASE_TYPES_<INDEX>_ENABLED_IDENTIFIERS=<NAMES>`  | `releaseTypes/<INDEX>/enabledIdentifiers`| Empty                                      |
| [`gitCommit`](#git-commit)                     | boolean | `--release-types-<INDEX>-git-commit=<EXPR>`                | `NYX_RELEASE_TYPES_<INDEX>_GIT_COMMIT=<EXPR>`            | `releaseTypes/<INDEX>/gitCommit`         | `false`                                    |
| [`gitCommitMessage`](#git-commit-message)      | string  | `--release-types-<INDEX>-git-commit-message=<TEMPLATE>`    | `NYX_RELEASE_TYPES_<INDEX>_GIT_COMMIT_MESSAGE=<TEMPLATE>`| `releaseTypes/<INDEX>/gitCommitMessage`  | `{% raw %}Release version {{version}}{% endraw %}`|
| [`gitPush`](#git-push)                         | boolean | `--release-types-<INDEX>-git-push=<EXPR>`                  | `NYX_RELEASE_TYPES_<INDEX>_GIT_PUSH=<EXPR>`              | `releaseTypes/<INDEX>/gitPush`           | `false`                                    |
| [`gitTag`](#git-tag)                           | boolean | `--release-types-<INDEX>-git-tag=<EXPR>`                   | `NYX_RELEASE_TYPES_<INDEX>_GIT_TAG=<EXPR>`               | `releaseTypes/<INDEX>/gitTag`            | `false`                                    |
| [`gitTagMessage`](#git-tag-message)            | string  | `--release-types-<INDEX>-git-tag-message=<TEMPLATE>`       | `NYX_RELEASE_TYPES_<INDEX>_GIT_TAG_MESSAGE=<TEMPLATE>`   | `releaseTypes/<INDEX>/gitTagMessage`     | Empty                                      |
| [`name`](#name)                                | string  | `--release-types-<INDEX>-name=<NAME>`                      | `NYX_RELEASE_TYPES_<INDEX>_NAME=<NAME>`                  | `releaseTypes/<INDEX>/name`              | N/A                                        |
| [`onBranches`](#on-branches)                   | string  | `--release-types-<INDEX>-on-branches=<REGEX>`              | `NYX_RELEASE_TYPES_<INDEX>_ON_BRANCHES=<REGEX>`          | `releaseTypes/<INDEX>/onBranches`        | Empty                                      |
| [`onEnvironments`](#on-environments)           | string  | `--release-types-<INDEX>-on-environments=<REGEX>`          | `NYX_RELEASE_TYPES_<INDEX>_ON_ENVIRONMENTS=<REGEX>`      | `releaseTypes/<INDEX>/onEnvironments`    | Empty                                      |
| [`onWorkspaceStatus`](#on-workspace-status)    | list    | `--release-types-<INDEX>-on-workspace-status=<VALUES>`     | `NYX_RELEASE_TYPES_<INDEX>_ON_WORKSPACE_STATUS=<VALUES>` | `releaseTypes/<INDEX>/onWorkspaceStatus` | Empty                                      |
| [`publish`](#publish)                          | boolean | `--release-types-<INDEX>-publish=<EXPR>`                   | `NYX_RELEASE_TYPES_<INDEX>_PUBLISH=<EXPR>`               | `releaseTypes/<INDEX>/publish`           | `false`                                    |
| [`type`](#type)                                | string  | `--release-types-<INDEX>-type=<NAME>`                      | `NYX_RELEASE_TYPES_<INDEX>_TYPE=<NAME>`                  | `releaseTypes/<INDEX>/type`              | Empty                                      |
| [`versionRange`](#version-range)               | string  | `--release-types-<INDEX>-version-range=<REGEX>`            | `NYX_RELEASE_TYPES_<INDEX>_VERSION_RANGE=<REGEX>`        | `releaseTypes/<INDEX>/versionRange`      | Empty (no constrained range)               |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | boolean | `--release-types-<INDEX>-version-range-from-branch-name=true|false` | `NYX_RELEASE_TYPES_<INDEX>_VERSION_RANGE_FROM_BRANCH_NAME=true|false` | `releaseTypes/<INDEX>/versionRangeFromBranchName` | `false` |

#### Collapse versions

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `collapseVersions`                                                                       |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-collapse-versions=true|false`                                   |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_COLLAPSE_VERSIONS=true|false`                                 |
| Configuration File Option | `releaseTypes/<INDEX>/collapseVersions`                                                  |
| Related state attributes  | [`collapseVersions`]({{ site.baseurl }}/reference/state/#release-type-attributes)        |

This flag, which defaults to `false`, should be `true` on **all and only** the [pre-release]({{ site.baseurl }}/reference/release-types/#pre-release) types as it's what actually makes them different from others.

When this flag is `false` version numbers are bumped linearly starting from the [previous version]({{ site.baseurl }}/reference/state/#release-scope-attributes) and the number to bump is determined based on changes since then, bumping the most significant one when more are affected by the changes.

When `true`, instead, version numbers are bumped starting from the [prime version]({{ site.baseurl }}/reference/state/#release-scope-attributes) and the number to bump is inferred by cumulating all changes since then, bumping the most significant number based on the changes within the scope. This way of bumping versions could easily drive to overlappings in *core* version numbers so in order to avoid conflicts you should also use an [extra identifier](#extra-identifiers) to disambiguate them, with the [preset pre-release identifier](#pre-release-identifier) being the most suitable, also according to [Semantic Versioning](https://semver.org/).

This concept is better explained by [this example]({{ site.baseurl }}/example/version-history-for-different-release-types/) and [this FAQ]({{ site.baseurl }}/faq/#whats-the-difference-between-the-prime-version-and-the-previous-version) but to make the long story short, **enabling this flag tells Nyx to use the *pre-release* version numbering for this release type**.

TODO: link the commit message convention configuration and state attributes from this section
{: .notice--warning}

#### Enabled identifiers

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `enabledIdentifiers`                                                                     |
| Type                      | list                                                                                     |
| Default                   | Empty (no enabled identifier)                                                            |
| Command Line Option       | `--release-types-<INDEX>-enabled-identifiers=<NAMES>`                                    |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_ENABLED_IDENTIFIERS=<NAMES>`                                  |
| Configuration File Option | `releaseTypes/<INDEX>/enabledIdentifiers`                                                |
| Related state attributes  |                                                                                          |

The comma separated list of [extra identifiers](#extra-identifier-definition) that are enabled for the release type.

Each item in the list must correspond to an identifier `name`.

The order in which identifiers are listed matters. The identifiers listed first appear first in the version number, so in case of ambiguous matches the order disambiguates (among those with the same [`where`](#where-extra-identifier) attribute).
{: .notice--info}

In order to be actually used, identifiers also need their [`when`](#when-extra-identifier) expression to evaluate `true`. While this seems a fairly complex way of configuring extra attributes it is also a powerful method to implement version schemes matrices without duplicating configurations.
{: .notice--info}

#### Git commit

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `gitCommit`                                                                              |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-git-commit=<EXPR>`                                              |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_GIT_COMMIT=<EXPR>`                                            |
| Configuration File Option | `releaseTypes/<INDEX>/gitCommit`                                                         |
| Related state attributes  |                                                                                          |

When `true` and Nyx is configured to generate artifacts a new commit is created to add the new artifacts to the repository. The new commit becomes the current commit.

Here you can define an [expression]({{ site.baseurl }}/reference/expressions/) that is evaluated at runtime to make this decision dynamic.

Please note that enabling this option violates the [immutable workspace]({{ site.baseurl }}/in-depth/design-principles/#immutable-workspace) principle so use it with care.
{: .notice--warning}

#### Git commit message

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `gitCommitMessage`                                                                       |
| Type                      | string                                                                                   |
| Default                   | `{% raw %}Release version {{version}}{% endraw %}`                                       |
| Command Line Option       | `--release-types-<INDEX>-git-commit-message=<TEMPLATE>`                                  |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_GIT_COMMIT_MESSAGE=<TEMPLATE>`                                |
| Configuration File Option | `releaseTypes/<INDEX>/gitCommitMessage`                                                  |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}/reference/templates/) that, once rendered, is used as the commit message of commits when [`gitCommit`](#git-commit) is `true`.

This option is ignored when [`gitCommit`](#git-commit) is `false`.

#### Git push

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `gitPush`                                                                                |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-git-push=<EXPR>`                                                |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_GIT_PUSH=<EXPR>`                                              |
| Configuration File Option | `releaseTypes/<INDEX>/gitPush`                                                           |
| Related state attributes  |                                                                                          |

When `true` Nyx will push changes (including tags) to the remote repository upon release. When `false` all local changes, including [tags](#git-tag), will stay local and are not propagated remotely.

Here you can define an [expression]({{ site.baseurl }}/reference/expressions/) that is evaluated at runtime to make this decision dynamic.

#### Git tag

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `gitTag`                                                                                 |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-git-tag=<EXPR>`                                                 |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_GIT_TAG=<EXPR>`                                               |
| Configuration File Option | `releaseTypes/<INDEX>/gitTag`                                                            |
| Related state attributes  |                                                                                          |

When `true` Nyx will tag the current commit with the new release version.

Here you can define an [expression]({{ site.baseurl }}/reference/expressions/) that is evaluated at runtime to make this decision dynamic.

Whether the tag is [lightweight or annotated](https://git-scm.com/book/en/v2/Git-Basics-Tagging) depends on the [`gitTagMessage`](#git-tag-message).

#### Git tag message

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `gitTagMessage`                                                                          |
| Type                      | string                                                                                   |
| Default                   | Empty (no message)                                                                       |
| Command Line Option       | `--release-types-<INDEX>-git-tag-message=<TEMPLATE>`                                     |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_GIT_TAG_MESSAGE=<TEMPLATE>`                                   |
| Configuration File Option | `releaseTypes/<INDEX>/gitTagMessage`                                                     |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}/reference/templates/) that, once rendered, is used as the message of annotated tags when [`gitTag`](#git-tag) is `true`.

If this template is null or empty (the default) then Nyx will create [lightweight](https://git-scm.com/book/en/v2/Git-Basics-Tagging) tags instead of [annotated](https://git-scm.com/book/en/v2/Git-Basics-Tagging).

This option is ignored when [`gitTag`](#git-tag) is `false`.

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `name`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-types-<INDEX>-name=<NAME>`                                                    |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_NAME=<NAME>`                                                  |
| Configuration File Option | `releaseTypes/<INDEX>/name`                                                              |
| Related state attributes  | [`releaseType`]({{ site.baseurl }}/reference/state/#release-type-attributes)             |

The short name that identifies this release type. This is also the value you can use in the [enabled release types](#enabled-release-types).

This option is **mandatory**.

Do not use `auto` for the environment name as it overlaps with the `auto` keyword used in the [`releaseType`](#release-type) global option.
{: .notice--warning}

#### On branches

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `onBranches`                                                                             |
| Type                      | string                                                                                   |
| Default                   | Empty (matches any branch)                                                               |
| Command Line Option       | `--release-types-<INDEX>-on-branches=<REGEX>`                                            |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_ON_BRANCHES=<REGEX>`                                          |
| Configuration File Option | `releaseTypes/<INDEX>/onBranches`                                                        |
| Related state attributes  | [`branch`]({{ site.baseurl }}/reference/state/#release-scope-attributes)                  |

A regular expression that evaluates `true` when it *matches* the [current Git branch](https://git-scm.com/docs/git-branch). A few examples:

* `^master$`: matches only the whole word `master`. This is useful if you need to specify a single branch name. Note that if you use the simple `master` regular expression (without the `^` and `$` assertions) you still match the `master` branch but you'd also match values like `futuremaster`, `masterclass` as they just *contain* the `master string`
* `^(master|main)$`: matches only the whole words `master` or `main`. This is useful if you need to specify a list of exact branch names. As for the previous examples, removing the `^` and `$` assertions would also match partial words
* `^feature\/[[:alnum:]]+$`: matches only the words starting with `feature/` with any non empty alphanumeric string after the `/`. Note the slash `/` needs to be escaped by a backslash `\` to be interpreded as literal
* `^(release[-\/](0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)|feature[-\/][[:alnum:]]+)$`: matches the words like `release/1.2.3` **or** `feature/XYZ`, where the delimiter can be either `/` or `-` (so it can adapt to [flat or hierarchical naming conventions]({{ site.baseurl }}best-practice/branching-models/#branching-models)). The `release/` must be followed by a [valid SemVer identifier](https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string) (with the *major*.*minor*.*patch* values only and without any prefix like `v`), while the `feature/` can be followed by any non empty alphanumeric string

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

#### On environments

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `onEnvironments`                                                                         |
| Type                      | string                                                                                   |
| Default                   | Empty (matches any environment)                                                          |
| Command Line Option       | `--release-types-<INDEX>-on-environments=<REGEX>`                                        |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_ON_ENVIRONMENTS=<REGEX>`                                      |
| Configuration File Option | `releaseTypes/<INDEX>/onEnvironments`                                                    |
| Related state attributes  | [`environment`]({{ site.baseurl }}/reference/state/#environment-attributes)              |

A regular expression that evaluates `true` when it *matches* the [current environment name]({{ site.baseurl }}/reference/state/#global-attributes). A few examples:

* `^server$`: matches the environment named `server`
* `^(server|github)$`: matches the environments named `server` and `github`
* `^((?!local).)*$`: matches all environment names **but** `local`

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

This option allows to enable or disable the release type based on the environment. For example you may want to limit issuing [official]({{ site.baseurl }}/reference/release-types/#main-releases) releases on [authoritative](#authoritative-vs-non-authoritative-environments) environments only (like [`server`](#server)) and, when Nyx runs on a [`local`](#local) environment, only produce [internal]({{ site.baseurl }}/reference/release-types/#internal) releases.

The default is the empty string, which matches any environment, making the release type independent from the environment.

#### On workspace status

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `onWorkspaceStatus`                                                                      |
| Type                      | string                                                                                   |
| Default                   | Empty list (no toggles)                                                                  |
| Command Line Option       | `--release-types-<INDEX>-on-workspace-status=<VALUES>`                                   |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_ON_WORKSPACE_STATUS=<VALUES>`                                 |
| Configuration File Option | `releaseTypes/<INDEX>/onWorkspaceStatus`                                                 |
| Related state attributes  | [`workspaceStatus`]({{ site.baseurl }}/reference/state/#global-attributes)               |

These are toggles about a certain state of the repository. When one of these toggles is specified the workspace needs to match that state to evaluate `true`. Available toggles are:

* `clean|dirty`: you can specify one **or** the other, or none. `dirty` means evaluates `true` when the workspace has uncommitted changes, while `clean` means there are no uncommitted changes in the repository

#### Publish

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `publish`                                                                                |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-publish=<EXPR>`                                                 |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_PUBLISH=<EXPR>`                                               |
| Configuration File Option | `releaseTypes/<INDEX>/publish`                                                           |
| Related state attributes  |                                                                                          |

When `true` Nyx will run the [publish]({{ site.baseurl }}/reference/usage/#publish) steps, when needed, otherwise will skip the step.

Here you can define an [expression]({{ site.baseurl }}/reference/expressions/) that is evaluated at runtime to make this decision dynamic.

Please note that whether or not a certain release is published also depends on the [services](#services) configuration.

#### Type

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `type`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | `internal`                                                                               |
| Command Line Option       | `--release-types-<INDEX>-type=<NAME>`                                                    |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_TYPE=<NAME>`                                                  |
| Configuration File Option | `releaseTypes/<INDEX>/type`                                                              |
| Related state attributes  |                                                                                          |

This identifier defines the [class]({{ site.baseurl }}/reference/release-types/) this release belongs to. You can define multiple items within the `releaseTypes` block with the same `type`.

Allowed values are:

* `main` for [main release types]({{ site.baseurl }}/reference/release-types/#main-releases) (a.k.a. *official* releases)
* `prerelease` for [pre-releases]({{ site.baseurl }}/reference/release-types/#pre-release)
* `postrelease` for [post-releases]({{ site.baseurl }}/reference/release-types/#post-release) (a.k.a. *maintenance* releases)
* `internal` for [internal releases]({{ site.baseurl }}/reference/release-types/#internal-release) and custom release types

When defining a `prerelease` type you should also enable the [`collapseVersions`](#collapse-versions) flag and [enable](#enabled-identifiers) the [pre-release extra identifier](#pre-release-identifier) to disambiguate *collapsed versions*.

When defining a `postrelease` type you should also enable the [`versionRange`](#version-range) constraint or the [`versionRangeFromBranchName`](#version-range-from-branch-name) flag to make sure only a certain range of releases is issued.

When defining an `internal` type you should disable the [`publish`](#publish) flag to avoid releases to be issued to the pubblic and [enable some extra identifier](#enabled-identifiers) to name releases including some extra attributes and distinguish them from regular releases.

This option is **mandatory**.

#### Version range

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `versionRange`                                                                           |
| Type                      | string                                                                                   |
| Default                   | Empty (no constrained range)                                                             |
| Command Line Option       | `--release-types-<INDEX>-version-range=<REGEX>`                                          |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_VERSION_RANGE=<REGEX>`                                        |
| Configuration File Option | `releaseTypes/<INDEX>/versionRange`                                                      |
| Related state attributes  | [`versionRange`]({{ site.baseurl }}/reference/state/#release-type-attributes)            |

This option allows to define a constraint on the versions issued by this release type. When defined it is a regular expression which is evaluated against the generated version number. If the version being issued matches the regular expression the release process can proceed, otherwise it's interrupted by an error.

A typical case where these constraints are used is for [post-release]({{ site.baseurl }}/reference/release-types/#post-release) (a.k.a. *maintenance* or *release*) release types, when you need to put boundaries to issued releases to avoid conflicts.

If you use a standard naming for your [release branches]({{ site.baseurl }}/best-practice/branching-models/#release-branches) and [maintenance branches]({{ site.baseurl }}/best-practice/branching-models/#maintenance-branches) consider using the [`versionRangeFromBranchName`](#version-range-from-branch-name) instead of this option to let Nyx infer the range directly from the branch name. This option has prority over [`versionRangeFromBranchName`](#version-range-from-branch-name) so declaring both makes this one come first.

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.
#### Version range from branch name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `versionRangeFromBranchName`                                                             |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<INDEX>-version-range-from-branch-name=true|false`                      |
| Environment Variable      | `NYX_RELEASE_TYPES_<INDEX>_VERSION_RANGE_FROM_BRANCH_NAME=true|false`                    |
| Configuration File Option | `releaseTypes/<INDEX>/versionRange`                                                      |
| Related state attributes  | [`versionRange`]({{ site.baseurl }}/reference/state/#release-type-attributes)            |

This option (disabled by default) lets you make this release type constrained on the versions that can be issued but allowing you not to define a specific regular expression for defining the allowed releases.

This is much more powerful than [`versionRange`](#version-range) as it can be defined only once for one release type, while if you use `versionRange` you may need to update your configuration as the release numbers increase for your project.

To use this option you just need to use a standard naming convention for your [release branches]({{ site.baseurl }}/best-practice/branching-models/#release-branches) or [maintenance branches]({{ site.baseurl }}/best-practice/branching-models/#maintenance-branches) (or any other type of branch you want to constrain) and let Nyx infer the regular expression from the branch name. For example, when this option is `true`, the following branch names would yield to these regular expression ranges:

* `1`, `v1`, `1.x`, `v1.x`, `1.x.x`, `v1.x.x` yield the regular expression `^1\.(0|[1-9]\d*)\.(0|[1-9]\d*)?$`, which constrains the *major* number to be `1` but lets the *minor* and *patch* numbers to be anything
* `1.2`, `v1.2`, `1.2.x`, `v1.2.x` yield the regular expression `^1\.2\.(0|[1-9]\d*)?$`, which constrains the *major* and *minor* numbers to be `1` and `2`, respectively, *patch* number to be anything

As you can see you can use the prefix in branch names if it matches the [`prefix`](#release-prefix) option, while the generated regular expression doesn't need the prefix.

What this feature does, actually, is populating the [`versionRange`](#version-range) regular expression automatically, if it's not already defined. Using both options is not an error but since the [`versionRange`](#version-range) comes first, it wouldn't make sense.
