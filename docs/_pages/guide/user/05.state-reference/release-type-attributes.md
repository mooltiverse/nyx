---
title: Release Type Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/release-type-attributes/
---

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`collapseVersions`](#collapse-versions)  | boolean | `true` or `false`                    |
| [`releaseTypeName`](#release-type-name)   | string  | A release type name                  |
| [`versionRange`](#version-range)          | string  | A regular expression                 |

### Collapse versions

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `collapseVersions`                                                                       |
| Type                          | boolean                                                                                  |
| Related configuration options | [releaseType]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-type){: .btn .btn--success .btn--small} [releaseTypes/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled-release-types){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} |

The flag telling if the [pre-release]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#pre-release) version increment logic is used (`true`) or not (`false`).

### Release type name

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseTypeName`                                                                        |
| Type                          | string                                                                                   |
| Related configuration options | [releaseType]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-type){: .btn .btn--success .btn--small} [releaseTypes/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled-release-types){: .btn .btn--success .btn--small} [releaseTypes/\<ID\>/name]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#name){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) name of the current [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}).

### Version range

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `versionRange`                                                                           |
| Type                          | string                                                                                   |
| Related configuration options | [releaseType]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-type){: .btn .btn--success .btn--small} [releaseTypes/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled-release-types){: .btn .btn--success .btn--small} [releaseTypes/versionRange]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range){: .btn .btn--success .btn--small} [releaseTypes/versionRangeFromBranchName]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name){: .btn .btn--success .btn--small} |

This option, when defined, is a regular expression that prevents releasing versions not matching this expression. It can be manually defined or [inferred by the branch name]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name).