---
title: Global Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/global-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                                             | Type    | Values                                      |
| ---------------------------------------------------------------- | ------- | ------------------------------------------- |
| [`branch`](#branch)                                              | string  | The current Git branch                      |
| [`bump`](#bump)                                                  | string  | The bumped version identifier               |
| [`changelog`](#changelog)                                        | object  | The changelog data model                    |
| [`configuration`](#configuration)                                | object  | The resolved configuration                  |
| [`coreVersion`](#core-version)                                   | boolean | `true` if version is *core*                 |
| [`directory`](#directory)                                        | string  | Directory path                              |
| [`internals`](#internals)                                        | map     | Name-Value pairs                            |
| [`latestVersion`](#latest-version)                               | boolean | `true` if version is the latest             |
| [`newRelease`](#new-release)                                     | boolean | `true` if a new release has to be issued    |
| [`newVersion`](#new-version)                                     | boolean | `true` if a new version has been generated  |
| [`releaseScope`](#release-scope)                                 | object  | The release scope attributes                |
| [`releaseType`](#release-type)                                   | string  | The selected release type                   |
| [`scheme`](#scheme)                                              | string  | `SEMVER`                                    |
| [`timestamp`](#timestamp)                                        | integer | A positive integer                          |
| [`version`](#version)                                            | string  | The current version                         |
| [`versionBuildMetadata`](#version-build-metadata)                | string  | The version build metadata                  |
| [`versionMajorNumber`](#version-major-number)                    | string  | The version major number                    |
| [`versionMinorNumber`](#version-minor-number)                    | string  | The version minor number                    |
| [`versionPatchNumber`](#version-patch-number)                    | string  | The version patch number                    |
| [`versionPreReleaseIdentifier`](#version-pre-release-identifier) | string  | The version pre-release identifier          |
| [`versionRange`](#version-range)                                 | string  | The version range regular expression        |

### Branch

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `branch`                                                                                 |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This string contains the current Git branch name.

This attribute is not initialized if the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference.

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Bump

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `bump`                                                                                   |
| Type                          | string                                                                                   |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseTypes/ID/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This string contains the name of the identifier that has been bumped to create the new [`version`](#version). Version identifiers depend on the selected version [scheme](#scheme).

This attribute is not initialized if the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference or the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) does not contain [significant changes]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits) to be released.

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

When using [collapsed versioning]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) this attribute contains the value of the bumped *core* identifier only (if any was bumped), not the [collapsed version qualifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier). Broadly speaking, no [extra identifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) is ever used to set this value. This might be misleading as sometimes you don't see the *core* identifier actually bumped on the [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) as it might have already been bumped on the [prime version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version).
{: .notice--info}

### Changelog

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `changelog`                                                                           |
| Type                          | object                                                                                   |
| Related configuration options | See the [details]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}) |
| Initialized by task           | [make]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#make){: .btn .btn--small} |

This object holds the data used to render the changelog, documented [here]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/changelog.md %}).

Please note that the `changelog` object is only present when the changelog generation has been enabled by setting the changelog [`path`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#path) option.
{: .notice--info}

### Configuration

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `configuration`                                                                          |
| Type                          | object                                                                                   |
| Related configuration options | The [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) |
| Initialized by task           | *any*                                                                                    |

This object holds a copy of the [entire configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) **resolved** according to the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order).

### Core version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `coreVersion`                                                                            |
| Type                          | boolean                                                                                  |
| Related configuration options | [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`version`](#version) only uses *core* identifiers (i.e. is not a pre-release) according to the [scheme](#scheme).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

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

### Latest version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `latestVersion`                                                                            |
| Type                          | boolean                                                                                  |
| Related configuration options | [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`version`](#version) is the latest in the repository, meaning that, according to the [scheme](#scheme), there are no other tags in the Git repository representing any version greater than [`version`](#version).

Remember that when inspecting all the tags in te current repository Nyx needs to have access to all of them. It's a common practice for CI/CD platform to only check out the latest commit or, in general, reduce the size of the local repository by not fetching all informations. If that's the case, this attribute may give incorrect results just because Nyx just can't *see* all the tags in the repository. See [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/ci-cd.md %}) for more on how to make sure the whole repository is available when cloned.
{: .notice--info}

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### New release

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `newRelease`                                                                             |
| Type                          | boolean                                                                                  |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [releaseTypes/ID/publish]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`newVersion`](#version) is `true` and a new release with the current [`version`](#version) has to be [issued]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish).

This is basically a shorthand to testing if [`version`](#version) is different than the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) and [publishing]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) is enabled for the current [release type](#release-type).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### New version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `newVersion`                                                                             |
| Type                          | boolean                                                                                  |
| Related configuration options | [bump]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump){: .btn .btn--success .btn--small} [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This value is `true` when the [`version`](#version) is new and is basically a shorthand to testing if [`version`](#version) is different than the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Release scope

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope`                                                                           |
| Type                          | object                                                                                   |
| Related configuration options |                                                                                          |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This object collects several attributes defining the release scope, documented [here]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}).

### Release type

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseType`                                                                            |
| Type                          | object                                                                                   |
| Related configuration options | [releaseType]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#release-type-definition){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

This object references the release type that has been selected among the [enabled ones]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled) by [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer).

The referenced object has all the attributes modelled as configuration options of a [`releaseType`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#release-type-definition) and all templates are evaluated.

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
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The version that was [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer), unless the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) configuration option was passed to override inference. When the version is not overridden or inferred the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) is used.

Please note that this version might be the same as the [`releaseScope/previousVersion`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) found in the Git commit history (or the [`initialVersion`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) if the commit history does not contain any previous version) if the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) has no [significant changes]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits). To know whether this is a new version or not you should check [`newVersion`](#new-version).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version build metadata

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionBuildMetadata`                                                                   |
| Type                          | string                                                                                   |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The [`version`](#version) [*build* metadata](https://semver.org/), if present in the [`version`](#version). This attribute is only available when the configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) is [`SEMVER`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version major number

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionMajorNumber`                                                                     |
| Type                          | string                                                                                   |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The [`version`](#version) [*major* number](https://semver.org/). This attribute is only available when the configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) is [`SEMVER`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version minor number

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionMinorNumber`                                                                     |
| Type                          | string                                                                                   |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The [`version`](#version) [*minor* number](https://semver.org/). This attribute is only available when the configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) is [`SEMVER`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version patch number

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionPatchNumber`                                                                     |
| Type                          | string                                                                                   |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The [`version`](#version) [*patch* number](https://semver.org/). This attribute is only available when the configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) is [`SEMVER`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version pre-release identifier

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionPreReleaseIdentifier`                                                            |
| Type                          | string                                                                                   |
| Related configuration options | [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The [`version`](#version) [*pre-release* identifier](https://semver.org/), if present in the [`version`](#version). This attribute is only available when the configured [version scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) is [`SEMVER`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver).

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.

### Version range

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionRange`                                                                           |
| Type                          | string                                                                                   |
| Related configuration options | [versionRange]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range){: .btn .btn--success .btn--small} [versionRangeFromBranchName]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer){: .btn .btn--small} |

The regular expression matched against the [`version`](#version) to make sure it's within a specific range.

This constraint is optional and depends on the [`versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) and [`versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) configuration options. When the check is not enabled by this option this reference attribute remains undefined, otherwise it's populated with a regular expression that is dynamically generated to match the current [`version`](#version). To be more specific, when [`versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) is configured this value has the same value configured in [`versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) after the template has been evaluated, otherwise if [`versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) is enabled this value has the dynamically generated regular expression as it's inferred by the branch name.

This attribute is not available until [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) has run.
