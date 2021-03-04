---
title: Global Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/global-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`configuration`](#configuration)         | object  | The resolved configuration           |
| [`directory`](#directory)                 | string  | Directory path                       |
| [`internals`](#internals)                 | map     | Name-Value pairs                     |
| [`scheme`](#scheme)                       | string  | `semver`                             |
| [`timestamp`](#timestamp)                 | integer | A positive integer                   |
| [`version`](#version)                     | string  | Valid version, when available        |

### Configuration

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `configuration`                                                                          |
| Type                          | object                                                                                   |
| Related configuration options | The [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) |

This object holds a copy of the [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) **resolved** according to the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order).

### Directory

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `directory`                                                                              |
| Type                          | string                                                                                   |
| Related configuration options | [directory]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#directory){: .btn .btn--success .btn--small} |

The path of current working directory.

### Internals

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `internals`                                                                              |
| Type                          | map                                                                                      |
| Related configuration options |                                                                                          |

A map of attributes for internal use only.

Using the attributes in this section is not supported. You should never rely on the attributes in this block as they may change at any time without any notice.
{: .notice--warning}

### Scheme

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `scheme`                                                                                 |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} |

The configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}).

### Timestamp

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `timestamp`                                                                              |
| Type                          | integer                                                                                  |
| Related configuration options |                                                                                          |

The timestamp in the Unix format (seconds since Jan 01 1970. (UTC). Example: `1591802533`. See [here](https://www.unixtimestamp.com/) for examples.

In order to grant consistency, whenever a timestamp is needed within the build process, this value should be used instead of reading it from the underlying system. This is to ensure that all timestamps coming from the same release are homogeneous.

Nyx sets this value once at the beginning of every execution.

If this is not used as the sole timestamp you may see a skew due to when the system timestamp is taken. Suppose that a task that needs the timestamp reads it 2 seconds after another: you have two different timestamps there, and you may have artifacts belonging to the same release not matching this field. Instead, using this sole source for time and date ensures the same value within the same release process. For sure this may not split the millisecond overall, but when you release you need consistency over this kind of precision.

### Version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `version`                                                                                |
| Type                          | string                                                                                   |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |

The version that was [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer), unless the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference. When the version is not overridden or inferred the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) is used.

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.
