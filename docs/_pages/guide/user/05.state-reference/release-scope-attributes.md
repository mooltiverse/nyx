---
title: Release Scope Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/release-scope-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`branch`](#branch)                       | simple  | A branch name                        |
| [`finalCommit`](#final-commit)            | string  | A commit SHA                         |
| [`initialCommit`](#initial-commit)        | string  | A commit SHA                         |
| [`newVersion`](#new-version)              | boolean | A boolean flag                       |
| [`previousVersion`](#previous-version)    | string  | A version identifier                 |
| [`primeVersion`](#prime-version)          | string  | A version identifier                 |
| [`release`](#release)                     | string  | A release identifier                 |
| [`ticker`](#ticker)                       | integer | A positive integer, starting from 0  |
| [`version`](#version)                     | string  | A version identifier                 |

### Branch

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `branch`                                                                                 |
| Type                          | string                                                                                   |
| Related configuration options | [amendBranch]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#amend-branch){: .btn .btn--success .btn--small} [branch]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#branch){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) branch name (by default is the one referenced by `HEAD`), unless overridden by the [`branch`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#branch) configuration option.

### Final commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `finalCommit`                                                                            |
| Type                          | string                                                                                   |
| Related configuration options | [finalCommit]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) final commit SHA. By default is the commit referenced by `HEAD`, unless overridden by the [`finalCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit) configuration option.

### Initial commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `initialCommit`                                                                            |
| Type                          | string                                                                                   |
| Related configuration options | [initialCommit]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) initial commit SHA, unless overridden by the [`initialCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit) configuration option. It may be empty if there is no initial commit reachable from the current commit.

### New version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `newVersion`                                                                             |
| Type                          | boolean                                                                                  |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

This flag becomes `true` when a new version has been detected.

When using [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) this means that there is new contents to be released under a new version. Please note that new commits alone within the scope do not necessarily imply a new version as they may not require to bump the version number.

When the [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option is used to force the version number this flag may be true if the given version is different from any version that was previously tagged.

As a rule of thumb this flag is `true` when [`version`](#version) and [`primeVersion`](#prime-version) are different.

### Previous version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `previousVersion`                                                                        |
| Type                          | string                                                                                   |
| Related configuration options | [initialCommit]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

The previous version, [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) from the tags applied to the [`initialCommit`](#initial-commit). Unless the [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) is configured to [collapse versions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) this value is the same as the [prime version](#prime-version).

This value may be empty if the [version](#version) is overridden by the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option.

See also [this post]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) about the *prime* and *previous* versions.

### Prime version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `primeVersion`                                                                           |
| Type                          | string                                                                                   |
| Related configuration options | [releaseTypes/\<ID\>/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [initialCommit]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-and-final-commit){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) latest regular version identifier used to compute the [current version](#version). It might be out of the release scope (before the [`previousVersion`](#previous-version)) if the [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) is configured to [collapse versions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions).

This value may be empty if the [version](#version) is overridden by the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option.

See also [this post]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) about the *prime* and *previous* versions.

### Release

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `release`                                                                                |
| Type                          | string                                                                                   |
| Related configuration options | [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) release identifier of the current release, according to the selected [version scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme), unless overridden by the [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option.

This is the same as the [version](#version) attribute but also brings the (optional) [prefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix), when configured. See [this post]({{ site.baseurl }}{% link _posts/2020-01-01-what-is-the-difference-between-version-and-release.md %}) for more.

### Ticker

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `ticker`                                                                                 |
| Type                          | integer                                                                                  |
| Related configuration options | [releaseTypes/\<ID\>/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} |

A local counter used when the core identifiers of a version don't change (i.e. because version numbers are [collapsed]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions)). This may be used as a local build number or a [pre-release]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#pre-release) number. This number is reset to 0 whenever any of the core version numbers change.

### Version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `version`                                                                                |
| Type                          | string                                                                                   |
| Related configuration options | [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) version identifier of the current release, according to the selected [version scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme), unless overridden by the [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option.

This is the same as the [release](#release) attribute but without the (optional) [prefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix), if configured. See [this post]({{ site.baseurl }}{% link _posts/2020-01-01-what-is-the-difference-between-version-and-release.md %}) for more.
