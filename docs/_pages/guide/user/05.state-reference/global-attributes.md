---
title: Global Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/global-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                      | Type    | Values                                      |
| ----------------------------------------- | ------- | ------------------------------------------- |
| [`bump`](#bump)                           | string  | The bumped version identifier               |
| [`configuration`](#configuration)         | object  | The resolved configuration                  |
| [`directory`](#directory)                 | string  | Directory path                              |
| [`internals`](#internals)                 | map     | Name-Value pairs                            |
| [`newRelease`](#new-release)              | boolean | `true` if a new release has to be issued    |
| [`newVersion`](#new-version)              | boolean | `true` if a new version has been generated  |
| [`releaseScope`](#release-scope)          | object  | The release scope attributes                |
| [`scheme`](#scheme)                       | string  | `semver`                                    |
| [`timestamp`](#timestamp)                 | integer | A positive integer                          |
| [`version`](#version)                     | string  | The current version                         |

### Bump

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `bump`                                                                                   |
| Type                          | string                                                                                   |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

This string contains the name of the identifier that has been dumped to make the new [`version`](#version) starting from the [previous one]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version). Version identifiers depend on the selected version [scheme](#scheme).

This attribute is not initialized if the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference or the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) does not contain [significant changes]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant) to be released.

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Configuration

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `configuration`                                                                          |
| Type                          | object                                                                                   |
| Related configuration options | The [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) |
| Initialized by task           | *any*                                                                                    |

This object holds a copy of the [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) **resolved** according to the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order).

### Directory

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `directory`                                                                              |
| Type                          | string                                                                                   |
| Related configuration options | [directory]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#directory){: .btn .btn--success .btn--small} |
| Initialized by task           | *any*                                                                                    |

The path of current working directory.

### Internals

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `internals`                                                                              |
| Type                          | map                                                                                      |
| Related configuration options | N/A                                                                                      |
| Initialized by task           | *any*                                                                                    |

A map of attributes for internal use only.

Using the attributes in this section is not supported. You should never rely on the attributes in this block as they may change at any time without any notice.
{: .notice--warning}


### New release

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `newRelease`                                                                             |
| Type                          | boolean                                                                                  |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`newVersion`](#version) is `true` and a new release with the current [`version`](#version) has to be [issued](TODO: link to the releaseType state attribute telling if the release type is configured to publish new releases).

This is basically a shorthand to testing if [`version`](#version) is different than the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) and [publishing](TODO: link to the releaseType state attribute telling if the release type is configured to publish new releases) is enabled for the current [release type](TODO: link to the releaseType state block here).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### New version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `newVersion`                                                                             |
| Type                          | boolean                                                                                  |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`version`](#version) is new and is basically a shorthand to testing if [`version`](#version) is different than the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Release scope

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope`                                                                           |
| Type                          | object                                                                                   |
| Related configuration options | See the [details]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) |
| Initialized by task           | See the [details]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) |

This object collects several attributes defining the release scope, documented [here]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}).

### Scheme

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `scheme`                                                                                 |
| Type                          | string                                                                                   |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | *any*                                                                                    |

The configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}).

### Timestamp

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `timestamp`                                                                              |
| Type                          | integer                                                                                  |
| Related configuration options |                                                                                          |
| Initialized by task           | *any*                                                                                    |

The timestamp in the Unix format (seconds since Jan 01 1970. (UTC). Example: `1591802533`. See [here](https://www.unixtimestamp.com/) for examples.

In order to grant consistency, whenever a timestamp is needed within the build process, this value should be used instead of reading it from the underlying system. This is to ensure that all timestamps coming from the same release are homogeneous.

Nyx sets this value once at the beginning of every execution.

If this is not used as the sole timestamp you may see a skew due to when the system timestamp is taken. Suppose that a task that needs the timestamp reads it 2 seconds after another: you have two different timestamps there, and you may have artifacts belonging to the same release not matching this field. Instead, using this sole source for time and date ensures the same value within the same release process. For sure this may not split the millisecond overall, but when you release you need consistency over this kind of precision.

### Version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `version`                                                                                |
| Type                          | string                                                                                   |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The version that was [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer), unless the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference. When the version is not overridden or inferred the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) is used.

Please note that this version might be the same as the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) found in the Git commit history (or the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) if the commit history does not contain any previous version) if the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) has no [significant changes]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant). To know whether this is a new version or not you should check [`newVersion`](#new-version).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.
