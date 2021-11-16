---
title: Release Types
layout: single
toc: true
permalink: /guide/user/configuration-reference/release-types/
---

Release types are definitions that allow conditional settings to be used depending on *what* Nyx is releasing and *how*. The configuration allows using dynamic values that can also be used as conditionals so you can have multiple degrees of control over the released contents.

Release types are configured within the `releaseTypes` *section*. The section allows one sub-section for each release type and some overall options.

You can have as many release types as you want. You can use [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) that come bundled with Nyx, override them or define your own from scratch.

### Release types overall options

| Name                                             | Type   | Command Line Option                    | Environment Variable                | Configuration File Option              | Default                                |
| ------------------------------------------------ | -------| -------------------------------------- | ----------------------------------- | -------------------------------------- | -------------------------------------- |
| [`releaseTypes/enabled`](#enabled)               | list   | `--release-types-enabled=<NAMES>`      | `NYX_RELEASE_TYPES_ENABLED=<NAMES>` | `releaseTypes/enabled`                 | [ ["`default`"](#default-release-type) ] |

#### Enabled

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/enabled`                                                                   |
| Type                      | list                                                                                     |
| Default                   | [ ["`default`"](#default-release-type) ]                                                 |
| Command Line Option       | `--release-types-enabled=<NAMES>`                                                        |
| Environment Variable      | `NYX_RELEASE_TYPES_ENABLED=<NAMES>`                                                      |
| Configuration File Option | `releaseTypes/enabled`                                                                   |
| Related state attributes  | [`releaseType`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-type) |

The comma separated list of release types that are enabled for the project. Here you can enable or disable the various release types, either custom or default.

Each item in the list must correspond to a release type [`name`](#name) attribute. Each named release type must exist, but not all defined release types must be enabled here. Release types not listed here will just be ignored by Nyx as if they were not even defined.

The order in which release types are listed matters. The types listed first are evaluated first, so in case of ambiguous matches the order disambiguates.
{: .notice--info}

### Default release type

The default release type is named `default` and brings all the default values that you can find in the following sections.

This release type should not be used in production configurations and you should always define your own types or use [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}).
{: .notice--info}

### Release type definition

Within the `releaseTypes` block you can define as many types as you want, each in its own separate block. The `name` identifies the type so to define a brand new release type make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a release type (given the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order)) then you are **overriding** an existing type. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single type.

Configuring release types gives Nyx informations about:

* how to assume which type to select given a certain set of facts that are automatically inferred or overridden by user. The rules defining how to match a release type are [`matchBranches`](#match-branches), [`matchEnvironmentVariables`](#match-environment-variables) and [`matchWorkspaceStatus`](#match-workspace-status) and they are evaluated by an `AND` logic so **they must all evaluate `true` to make a successful match**
* which tags in the Git history must be considered for the release type so that the commit history can be consistently parsed. The match is done using the regular expression configured as the [`filterTags`](#filter-tags)
* the actions to take for each release type

Each release type has the following attributes:

| Name                                                                                       | Type    | Command Line Option                                                   | Environment Variable                                                    | Default                                              |
| ------------------------------------------------------------------------------------------ | ------- | --------------------------------------------------------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------- |
| [`releaseTypes/<NAME>/collapseVersions`](#collapse-versions)                               | boolean | `--release-types-<NAME>-collapse-versions=true|false`                 | `NYX_RELEASE_TYPES_<NAME>_COLLAPSE_VERSIONS=true|false`                 | `false`                                              |
| [`releaseTypes/<NAME>/collapsedVersionQualifier`](#collapsed-version-qualifier)            | string  | `--release-types-<NAME>-collapsed-version-qualifier=<TEMPLATE>`       | `NYX_RELEASE_TYPES_<NAME>_COLLAPSED_VERSION_QUALIFIER=<TEMPLATE>`       | Empty                                                |
| [`releaseTypes/<NAME>/filterTags`](#filter-tags)                                           | string  | `--release-types-<NAME>-filter-tags`                                  | `NYX_RELEASE_TYPES_<NAME>_FILTER_TAGS=<TEMPLATE>`                       | Empty                                                |
| [`releaseTypes/<NAME>/gitCommit`](#git-commit)                                             | string  | `--release-types-<NAME>-git-commit=<TEMPLATE>`                        | `NYX_RELEASE_TYPES_<NAME>_GIT_COMMIT=<TEMPLATE>`                        | `false`                                              |
| [`releaseTypes/<NAME>/gitCommitMessage`](#git-commit-message)                              | string  | `--release-types-<NAME>-git-commit-message=<TEMPLATE>`                | `NYX_RELEASE_TYPES_<NAME>_GIT_COMMIT_MESSAGE=<TEMPLATE>`                | `{% raw %}Release version {{version}}{% endraw %}` |
| [`releaseTypes/<NAME>/gitPush`](#git-push)                                                 | string  | `--release-types-<NAME>-git-push=<TEMPLATE>`                          | `NYX_RELEASE_TYPES_<NAME>_GIT_PUSH=<TEMPLATE>`                          | `false`                                              |
| [`releaseTypes/<NAME>/gitTag`](#git-tag)                                                   | string  | `--release-types-<NAME>-git-tag=<TEMPLATE>`                           | `NYX_RELEASE_TYPES_<NAME>_GIT_TAG=<TEMPLATE>`                           | `false`                                              |
| [`releaseTypes/<NAME>/gitTagMessage`](#git-tag-message)                                    | string  | `--release-types-<NAME>-git-tag-message=<TEMPLATE>`                   | `NYX_RELEASE_TYPES_<NAME>_GIT_TAG_MESSAGE=<TEMPLATE>`                   | Empty                                                |
| [`releaseTypes/<NAME>/identifiers`](#identifiers)                                          | list    | `--release-types-<NAME>-identifiers=<LIST>`                           | `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS=<LIST>`                           | Empty                                                |
| [`releaseTypes/<NAME>/matchBranches`](#match-branches)                                     | string  | `--release-types-<NAME>-match-branches=<TEMPLATE>`                    | `NYX_RELEASE_TYPES_<NAME>_MATCH_BRANCHES=<TEMPLATE>`                    | Empty                                                |
| [`releaseTypes/<NAME>/matchEnvironmentVariables`](#match-environment-variables)            | map     | `--release-types-<NAME>-match-environment-variables=<MAP>`            | `NYX_RELEASE_TYPES_<NAME>_MATCH_ENVIRONMENT_VARIABLES=<MAP>`            | Empty                                                |
| [`releaseTypes/<NAME>/matchWorkspaceStatus`](#match-workspace-status)                      | string  | `--release-types-<NAME>-match-workspace-status`                       | `NYX_RELEASE_TYPES_<NAME>_MATCH_WORKSPACE_STATUS=<STATUS>`              | Empty                                                |
| [`releaseTypes/<NAME>/name`](#name)                                                        | string  | `--release-types-<NAME>-name=<NAME>`                                  | `NYX_RELEASE_TYPES_<NAME>_NAME=<NAME>`                                  | N/A                                                  |
| [`releaseTypes/<NAME>/publish`](#publish)                                                  | string  | `--release-types-<NAME>-publish=<TEMPLATE>`                           | `NYX_RELEASE_TYPES_<NAME>_PUBLISH=<TEMPLATE>`                           | `false`                                              |
| [`releaseTypes/<NAME>/versionRange`](#version-range)                                       | string  | `--release-types-<NAME>-version-range=<TEMPLATE>`                     | `NYX_RELEASE_TYPES_<NAME>_VERSION_RANGE=<TEMPLATE>`                     | Empty (no constrained range)                         |
| [`releaseTypes/<NAME>/versionRangeFromBranchName`](#version-range-from-branch-name)        | boolean | `--release-types-<NAME>-version-range-from-branch-name=true|false`    | `NYX_RELEASE_TYPES_<NAME>_VERSION_RANGE_FROM_BRANCH_NAME=true|false`    | `false`                                              |

#### Collapse versions

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/collapseVersions`                                                   |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-collapse-versions=true|false`                                    |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_COLLAPSE_VERSIONS=true|false`                                  |
| Configuration File Option | `releaseTypes/items/<NAME>/collapseVersions`                                             |
| Related state attributes  | [bump]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#bump){: .btn .btn--info .btn--small} [releaseScope/previousVersion]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version){: .btn .btn--info .btn--small} [releaseScope/previousVersionCommit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version-commit){: .btn .btn--info .btn--small} [releaseScope/primeVersion]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version){: .btn .btn--info .btn--small} [releaseScope/primeVersionCommit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version-commit){: .btn .btn--info .btn--small} |

This flag, which defaults to `false`, should be `true` on **all and only** the pre-release branch types as it's what actually makes them different from others.

When this flag is `false` version numbers are bumped linearly starting from the [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) and the number to bump is determined based on changes since then, bumping the most significant one when more are affected by the changes.

When `true`, instead, version numbers are bumped starting from the [prime version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version) and the number to bump is inferred by cumulating all changes since then, bumping the most significant number based on the changes within the scope.

This concept is better explained by [this article]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-collapsed-versioning-work.md %}) and [this FAQ]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) but to make the long story short, **enabling this flag tells Nyx to use the *pre-release* version numbering for this release type**.

This way of bumping versions drives to overlappings in *core* version numbers so in order to avoid conflicts an additional identifier is used to distinguish among versions with the same *core* identifiers. When using [SemVer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) as the versioning [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme), the identifier is created in the [*pre-release*](https://semver.org/) block.

The extra identifier has a value that is linearly incremented among versions with the same *core* identifiers and is reset when they change. You can also give it a label using the [`collapsedVersionQualifier`](#collapsed-version-qualifier).

Examples of versions generated over time:

| Core identifiers  | Extra identifier    | Generated version |
| ----------------- | ------------------- | ----------------- |
| 1.2.3             | 1                   | 1.2.3-1           |
| 1.2.3             | 2                   | 1.2.3-2           |
| 1.2.3             | 3                   | 1.2.3-3           |
| 1.2.4             | 1                   | 1.2.4-1           |

As you can see by the example, as long as the *core* version is `1.2.3`, the extra identifiers `1`, `2` and `3` are appended in the *pre-release* block. It is reset to `1` when the *core* identifiers change to `1.2.4`.

Additional extra identifiers can be configured by means of the [identifiers](#identifiers) configuration block. The extra identifier herein described is completely independent by those in the [identifiers](#identifiers) section.
{: .notice--info}

#### Collapsed version qualifier

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/collapsedVersionQualifier`                                          |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--release-types-<NAME>-collapsed-version-qualifier=<TEMPLATE>`                          |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_COLLAPSED_VERSION_QUALIFIER=<TEMPLATE>`                        |
| Configuration File Option | `releaseTypes/items/<NAME>/collapsedVersionQualifier`                                    |
| Related state attributes  | [releaseScope/previousVersion]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version){: .btn .btn--info .btn--small} [releaseScope/previousVersionCommit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version-commit){: .btn .btn--info .btn--small} [releaseScope/primeVersion]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version){: .btn .btn--info .btn--small} [releaseScope/primeVersionCommit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version-commit){: .btn .btn--info .btn--small} |

This option is only evaluated when [`collapseVersions`](#collapse-versions) is `true` and allows you to define an optional name for the extra identifier used to disambiguate collapsed versions. The qualifier is actually an additional identifier preceding the number that disambiguates among *core* versions.

This option can be defined as a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) or an hardcoded string. It's a common practice to use the [name of the current branch]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#branch) for this option.

Examples (assuming [`collapseVersions`](#collapse-versions) is `true`):

| Generated version with no collapsed version qualifier | Generated version with collapsed version qualifier = `{% raw %}{{branch}}{% endraw %}` |
| ----------------------------------------------------- | ------------------------------------------------------------------------- |
| 1.2.3-1                                               | 1.2.3-alpha.1                                                             |
| 1.2.3-2                                               | 1.2.3-alpha.2                                                             |
| 1.2.3-3                                               | 1.2.3-alpha.3                                                             |
| 1.2.4-1                                               | 1.2.4-alpha.1                                                             |

In the above example the `collapsedVersionQualifier` defines a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that evaluates to the current branch name (in this case `alpha`).

This option is **mandatory** when [`collapseVersions`](#collapse-versions) is `true` and if a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) is used it must resolve to a legal, non empty string.

#### Filter tags

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/filterTags`                                                         |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--release-types-<NAME>-filter-tags=<TEMPLATE>`                                          |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_FILTER_TAGS=<TEMPLATE>`                                        |
| Configuration File Option | `releaseTypes/items/<NAME>/filterTags`                                                   |
| Related state attributes  | [releaseScope/previousVersion]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version){: .btn .btn--info .btn--small} [releaseScope/previousVersionCommit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version-commit){: .btn .btn--info .btn--small} |

A [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, produces a regular expression that filter tags in the commit history so that the [*prime* and *previous* version]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) can be correctly inferred, ignoring non relevant tags that may be found.

By default this is empty so all tags are matched.

Please note that this string value is rendered as a template in the first place and then the outcome is used as a regular expression. For example, `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` is first rendered to `{% raw %}"^(v)?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` (assuming the [`releasePrefix`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix) is set to `"v"`), then this regular expression is used to match actual values.
{: .notice--info}

A few examples:

* `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}`: matches all the tags representing a [valid SemVer identifier](https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string) (with *core* identifiers only), tolerating the optional  [`releasePrefix`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix) when configured
* `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(develop|development|integration|latest)(\.([0-9]\d*))?)?$"{% endraw %}`: matches a [valid SemVer identifier](https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string) (with *core* identifiers only), tolerating the optional  [`releasePrefix`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix) when configured that may optionally have an extra identifier in the *pre-release* part, like `1.2.3`, `1.2.3-develop`, `1.2.3-develop.2` etc

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

When extra [identifiers](#identifiers) are used and [tagging](#git-tag) is enabled the regular expression defined here must take into account all the extra identifiers or tagging may become inconsistent.
{: .notice--info}

#### Git commit

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/gitCommit`                                                          |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-git-commit=<TEMPLATE>`                                           |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_GIT_COMMIT=<TEMPLATE>`                                         |
| Configuration File Option | `releaseTypes/items/<NAME>/gitCommit`                                                    |
| Related state attributes  |                                                                                          |

When `true` and Nyx is configured to generate artifacts a new commit is created to add the new artifacts to the repository. The new commit becomes the new [final commit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#final-commit).

Here you can define a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that is [evaluated as a boolean]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#type-conversions) at runtime to make this configuration dynamic.

#### Git commit message

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/gitCommitMessage`                                                   |
| Type                      | string                                                                                   |
| Default                   | `{% raw %}Release version {{version}}{% endraw %}`                                       |
| Command Line Option       | `--release-types-<NAME>-git-commit-message=<TEMPLATE>`                                   |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_GIT_COMMIT_MESSAGE=<TEMPLATE>`                                 |
| Configuration File Option | `releaseTypes/items/<NAME>/gitCommitMessage`                                             |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, is used as the commit message of commits when [`gitCommit`](#git-commit) is `true`.

This option is ignored when [`gitCommit`](#git-commit) is `false`.

#### Git push

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/gitPush`                                                            |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-git-push=<TEMPLATE>`                                             |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_GIT_PUSH=<TEMPLATE>`                                           |
| Configuration File Option | `releaseTypes/items/<NAME>/gitPush`                                                      |
| Related state attributes  |                                                                                          |

When `true` Nyx will push changes (including tags) to the remote repository upon release. When `false` all local changes, including [tags](#git-tag), will stay local and are not propagated remotely.

Here you can define a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that is [evaluated as a boolean]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#type-conversions) at runtime to make this decision dynamic.

#### Git tag

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/gitTag`                                                             |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-git-tag=<TEMPLATE>`                                              |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_GIT_TAG=<TEMPLATE>`                                            |
| Configuration File Option | `releaseTypes/items/<NAME>/gitTag`                                                       |
| Related state attributes  |                                                                                          |

When `true` Nyx will tag the current commit with the new release version.

Here you can define a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that is [evaluated as a boolean]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#type-conversions) at runtime to make this decision dynamic.

#### Git tag message

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/gitTagMessage`                                                      |
| Type                      | string                                                                                   |
| Default                   | Empty (no message)                                                                       |
| Command Line Option       | `--release-types-<NAME>-git-tag-message=<TEMPLATE>`                                      |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_GIT_TAG_MESSAGE=<TEMPLATE>`                                    |
| Configuration File Option | `releaseTypes/items/<NAME>/gitTagMessage`                                                |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, is used as the message of annotated tags when [`gitTag`](#git-tag) is `true`.

If this template is null or empty (the default) then Nyx will create [lightweight](https://git-scm.com/book/en/v2/Git-Basics-Tagging) tags instead of [annotated](https://git-scm.com/book/en/v2/Git-Basics-Tagging).

This option is ignored when [`gitTag`](#git-tag) is `false`.

#### Identifiers

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/identifiers`                                                        |
| Type                      | list                                                                                     |
| Default                   | Empty (no custom identifiers)                                                            |
| Command Line Option       | `--release-types-<NAME>-identifiers=<LIST>`                                              |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS=<LIST>`                                            |
| Configuration File Option | `releaseTypes/items/<NAME>/identifiers`                                                  |
| Related state attributes  |                                                                                          |

A nested configuration block with the map of additional identifiers that are defined for the release type. Additional identifiers are meant to further qualify releases by adding attributes to version names. Examples of extra identifiers are branch names, timestamps, user names, environment names etc, as follows:

* version `1.2.3` has no extra identifiers
* version `1.2.3-alpha.12` has the [collapsed version identifier](#collapse-versions) along with the [qualifier](#collapsed-version-qualifier) (`alpha`) configured. This identifier is configured using the [`collapseVersions`](#collapse-versions) and the [`collapsedVersionQualifier`](#collapsed-version-qualifier) options, not using this configuration block
* version `1.2.3-alpha.12+johndoe` also adds another identifier in the *build* part with the local [user]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentuser)
* version `1.2.3-alpha.12.local+johndoe.20201217130636` adds another identifier in the *pre-release* part showing the environment name (`local`) (which might come from an [environment variable]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable)), and the [timestamp]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#timestampyyyymmddhhmmss) in the *build* part

These identifiers are made of a qualifier and a value. The qualifier is mandatory while the value is optional. Both may have arbitrary values as long as they comply with the [versioning scheme]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}) being used. When both the qualifier and value are present, they will be represented with the qualifier coming first and then the value, separated by a dot.

Do not confuse the identifier *name* and its *qualifier*. The *name* is used internally for configuration purposes only and is never rendered. What is actually rendered to versions is the *qualifier*.
{: .notice--info}

This nested configuration block allows one element for each extra identifier. You can have as many extra identifiers as you want.

The order in which identifiers are listed matters. The identifiers listed first are evaluated and applied first, according to the configured [position](#identifier-position). The identifier [position](#identifier-position) has higher priority over the *enabled* list.
{: .notice--info}

When configuring this map using command line options or environment variables you need to pass flattened values as documented [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects). In this case you can pass each identifier as a set of command line options like `--release-types-<NAME>-identifiers-<ORDINAL>-position=PRE_RELEASE|BUILD`, `--release-types-<NAME>-identifiers-<ORDINAL>-qualifier=<TEMPLATE>`, `--release-types-<NAME>-identifiers-<ORDINAL>-value=<TEMPLATE>` or as a set of environment variables like `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<ORDINAL>_POSITION=PRE_RELEASE|BUILD`, `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<ORDINAL>_QUALIFIER=<TEMPLATE>`, `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<ORDINAL>_VALUE=<TEMPLATE>`.
{: .notice--info}

When applying extra identifiers to a [previous]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) or [prime]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version) version that already has extra identifiers, existing identifiers are left in their position and the new ones are appended at the end. You never notice this difference when previous versions have been generated by Nyx using the same configuration but in case the commit history contains existing version tags where extra identifiers have a different order than the one configured here, you need to know this.

When using [collapsed versions](#collapse-versions) an extra identifier is also added to disambiguate versions with the same *core* values. This identifier is not affected by the configuration in this block, regardless of whether or not the [collapsed version qualifier](#collapsed-version-qualifier) is also configured.
{: .notice--info}

When extra identifiers are used and [tagging](#git-tag) is enabled the [regular expression that filters tags](#filter-tags) must take into account all the extra identifiers or tagging may become inconsistent.
{: .notice--info}

Each item in the `identifiers` block is made of a `qualifier`, a `value` and a `position`, as follows.
##### Identifier definition

Within the `releaseTypes/<NAME>/identifiers` block you can define as many identifiers as you want, each in its own separate block.

###### Identifier position

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/identifiers/<#>/position`                                           |
| Type                      | string                                                                                   |
| Default                   | `build`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-identifiers-<#>-position=PRE_RELEASE|BUILD`                      |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<#>_POSITION=PRE_RELEASE|BUILD`                    |
| Configuration File Option | `releaseTypes/items/<NAME>/identifiers/items/<#>/position`                               |
| Related state attributes  |                                                                                          |

This is only used with the [SemVer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) version [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme). Available values are:

* `PRE_RELEASE`: the identifier appears in the [pre-release part](https://semver.org/)
* `BUILD`: the identifier appears in the [build part](https://semver.org/)

Defaults is `BUILD`.

The position has higher priority than the order in which identifiers are listed in the [`enabled`](#enabled-identifiers) identifiers.
{: .notice--info}

###### Identifier qualifier

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/identifiers/<#>/qualifier`                                          |
| Type                      | string                                                                                   |
| Default                   | Empty (no qualifier)                                                                     |
| Command Line Option       | `--release-types-<NAME>-identifiers-<#>-qualifier=<TEMPLATE>`                            |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<#>_QUALIFIER=<TEMPLATE>`                          |
| Configuration File Option | `releaseTypes/items/<NAME>/identifiers/items/<#>/qualifier`                              |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, is used as the leftmost part of the extra qualifier. This string is mandatory and must evaluate to non empty and will be used for the leftmost part of the identifier in the version and if the value is non empty, the two will be separated by a dot.

Defaults to the empty string (no qualifier).

###### Identifier value

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/identifiers/<#>/value`                                              |
| Type                      | string                                                                                   |
| Default                   | Empty (no value)                                                                         |
| Command Line Option       | `--release-types-<NAME>-identifiers-<#>-value=<TEMPLATE>`                                |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_IDENTIFIERS_<#>_VALUE=<TEMPLATE>`                              |
| Configuration File Option | `releaseTypes/items/<NAME>/identifiers/items/<#>/value`                                  |
| Related state attributes  |                                                                                          |

This is a short optional [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, is used as the rightmost part of the extra qualifier. If empty the version will only show the identifier [qualifier](#identifier-qualifier). If not empty the result of the template evaluation will be used for the rightmost part (after the [qualifier](#identifier-qualifier) and the dot) of the identifier in the version.

Defaults to the empty string (no qualifier).

When the value evaluates to non empty it will overwrite the value of the same identifier in the [previous]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) or [prime]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#prime-version) version, if any. If, instead, the value evaluates to empty or is not defined and the previous identifier had a value, then the previous identifier will have the value unchanged. This is because Nyx has no means to know if the **next** identifier is a value or a standalone identifier. You might incur in this caveat only if you have previous tags in the commit history generated by other means or different Nyx configurations.

When using the [SemVer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) version [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme) and you want to use the identifier in the [pre release](#identifier-position) part, the value must be a valid integer al leading zeroes will be removed as part of the conversion to an integer. If you need values other than valid integers you need to use an additional identifier using the value as the [qualifier](#identifier-qualifier) or simply put the identifier in the [build](#identifier-position) part. These constraints are needed to comply with [Semantic Versioning](https://semver.org/);
{: .notice--info}

#### Match branches

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/matchBranches`                                                      |
| Type                      | string                                                                                   |
| Default                   | Empty (matches any branch)                                                               |
| Command Line Option       | `--release-types-<NAME>-match-branches=<TEMPLATE>`                                       |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_MATCH_BRANCHES=<TEMPLATE>`                                     |
| Configuration File Option | `releaseTypes/items/<NAME>/matchBranches`                                                |
| Related state attributes  |                                                                                          |

A [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, produces a regular expression that evaluates `true` when it *matches* the [current Git branch](https://git-scm.com/docs/git-branch).

By default this is empty so any branch is matched.

Please note that this string value is rendered as a template in the first place and then the outcome is used as a regular expression. For example, `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` is first rendered to `{% raw %}"^(v)?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` (assuming the [`releasePrefix`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix) is set to `"v"`), then this regular expression is used to match actual values.
{: .notice--info}

A few examples:

* `^master$`: matches only the whole word `master`. This is useful if you need to specify a single branch name. Note that if you use the simple `master` regular expression (without the `^` and `$` assertions) you still match the `master` branch but you'd also match values like `futuremaster`, `masterclass` as they just *contain* the `master string`
* `^(master|main)$`: matches only the whole words `master` or `main`. This is useful if you need to specify a list of exact branch names. As for the previous examples, removing the `^` and `$` assertions would also match partial words
* `^feature\/[[:alnum:]]+$`: matches only the words starting with `feature/` with any non empty alphanumeric string after the `/`. Note the slash `/` needs to be escaped by a backslash `\` to be interpreded as literal
* `^(release[-\/](0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)|feature[-\/][[:alnum:]]+)$`: matches the words like `release/1.2.3` **or** `feature/XYZ`, where the delimiter can be either `/` or `-` (so it can adapt to flat or hierarchical naming conventions). The `release/` must be followed by a [valid SemVer identifier](https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string) (with the *major*.*minor*.*patch* values only and without any prefix like `v`), while the `feature/` can be followed by any non empty alphanumeric string

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

When using this option, the release type is only evaluated when the current branch name is matched by the regular expression, otherwise it is ignored. You can use this option to constrain certain release types to be issued by a specific set of branches only.

#### Match environment variables

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/matchEnvironmentVariables`                                          |
| Type                      | map                                                                                      |
| Default                   | Empty (matches anything)                                                                 |
| Command Line Option       | `--release-types-<NAME>-match-environment-variables=<MAP>`                               |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_MATCH_ENVIRONMENT_VARIABLES=<MAP>`                             |
| Configuration File Option | `releaseTypes/items/<NAME>/matchEnvironmentVariables`                                    |
| Related state attributes  |                                                                                          |

A map where each entry is an environment variable to match. In order for the overall matching to succeed all environment variables must match.

The key of each entry is the name of an environment variable to match, while the value is a regular expression that needs to match the current environment variable value.

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

This option can be used to enable or disable a release type based on the environment. For example you may want to limit issuing official releases on authoritative CI servers only (where some specific environment variable is defined) and disable it on all other environments (like developer workstations).

The default is the empty map, which matches any environment, making the release type independent from the environment.

When configuring this map using command line options or environment variables you need to pass flattened values as documented [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects). In this case you can pass each environment variable to be matched as a command line option like `--release-types-<NAME>-match-environment-variables-<VARIABLE_NAME>=<REGEX>` or as an environment variable like `NYX_RELEASE_TYPES_<NAME>_MATCH_ENVIRONMENT_VARIABLES_<VARIABLE_NAME>=<REGEX>`.
{: .notice--info}

#### Match workspace status

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/matchWorkspaceStatus`                                               |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--release-types-<NAME>-match-workspace-status=CLEAN|DIRTY`                              |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_MATCH_WORKSPACE_STATUS=CLEAN|DIRTY`                            |
| Configuration File Option | `releaseTypes/items/<NAME>/matchWorkspaceStatus`                                         |
| Related state attributes  |                                                                                          |

This option lets you select a specific Git workspace status for the release type to be selected. Allowed values are `CLEAN` and `DIRTY`. When using `CLEAN` the release type is matched only when the workspace has no uncommitted changes. When using `DIRTY` the release type is matched only when the workspace does have uncommitted changes. Leaving this value empty or undefined matches any workspace status.

You may use this option to issue certain releases only when the workspace is *clean* or have multiple release types with similar configuration but a few details, and having one or the other selected based on the workspace status.

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>`                                                                    |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--release-types-<NAME>=<NAME>`                                                          |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>=<NAME>`                                                        |
| Configuration File Option | `releaseTypes/items/<NAME>`                                                              |
| Related state attributes  | [releaseType]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-type){: .btn .btn--info .btn--small} |

The short name that identifies this release type. This is also the value you can use in the [enabled](#enabled) release types. This is actually not a field to be set within a release type section but instead the key of the map element.

This option is **mandatory**.

#### Publish

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/publish`                                                            |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-publish=<TEMPLATE>`                                              |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_PUBLISH=<TEMPLATE>`                                            |
| Configuration File Option | `releaseTypes/items/<NAME>/publish`                                                      |
| Related state attributes  |                                                                                          |

When `true` Nyx will run the [publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#publish) steps, when needed, otherwise will skip the step.

Here you can define a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that is evaluated at runtime to make this decision dynamic.

TODO: Check this statement and the link when Services are implemented: Please note that whether or not a certain release is published also depends on the [services](#services) configuration.
{: .notice--warning}

#### Version range

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/versionRange`                                                       |
| Type                      | string                                                                                   |
| Default                   | Empty (no constrained range)                                                             |
| Command Line Option       | `--release-types-<NAME>-version-range=<TEMPLATE>`                                        |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_VERSION_RANGE=<TEMPLATE>`                                      |
| Configuration File Option | `releaseTypes/items/<NAME>/versionRange`                                                 |
| Related state attributes  | [version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#version){: .btn .btn--info .btn--small} [versionRange]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#version-range){: .btn .btn--info .btn--small} |

This option allows to define a constraint on the versions issued by this release type. When defined it is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) that, once rendered, produces a regular expression which is evaluated against the generated version number. If the version being issued matches the regular expression the release process can proceed, otherwise it's interrupted by an error.

Please note that this string value is rendered as a template in the first place and then the outcome is used as a regular expression. For example, `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` is first rendered to `{% raw %}"^(v)?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` (assuming the [`releasePrefix`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix) is set to `"v"`), then this regular expression is used to match actual values.
{: .notice--info}

A typical case where these constraints are used is for [post-release]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}) ([*maintenance*]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches) or [*release*]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches)) release types, when you need to put boundaries to issued releases to avoid conflicts. See [this example]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}#version-constrained-branch) for more on this.

If you use a standard naming for your [release]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches) and [maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches) branches consider using the [`versionRangeFromBranchName`](#version-range-from-branch-name) instead of this option to let Nyx infer the range directly from the branch name. This option has prority over [`versionRangeFromBranchName`](#version-range-from-branch-name) so declaring both makes this one come first.

You can use tools like [https://regex101.com/](https://regex101.com/) to write and test your regular expressions.

#### Version range from branch name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releaseTypes/<NAME>/versionRangeFromBranchName`                                         |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--release-types-<NAME>-version-range-from-branch-name=true|false`                       |
| Environment Variable      | `NYX_RELEASE_TYPES_<NAME>_VERSION_RANGE_FROM_BRANCH_NAME=true|false`                     |
| Configuration File Option | `releaseTypes/items/<NAME>/versionRange`                                                 |
| Related state attributes  | [version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#version){: .btn .btn--info .btn--small} [versionRange]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#version-range){: .btn .btn--info .btn--small} |

This option (disabled by default) lets you make this release type constrained on the versions that can be issued but allowing you not to define a specific regular expression for defining the allowed releases.

This is much more powerful than [`versionRange`](#version-range) as it can be defined only once for one release type, while if you use `versionRange` you may need to update your configuration as the release numbers increase for your project.

To use this option you just need to use a standard naming convention for your [release]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches) and [maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches) branches (or any other type of branch you want to constrain) and let Nyx infer the regular expression from the branch name. For example, when this option is `true`, the following branch names would yield to these regular expression ranges:

* `1`, `v1`, `1.x`, `v1.x`, `1.x.x`, `v1.x.x` yield the regular expression `^1\.(0|[1-9]\d*)\.(0|[1-9]\d*)?$`, which constrains the *major* number to be `1` but lets the *minor* and *patch* numbers to be anything
* `1.2`, `v1.2`, `1.2.x`, `v1.2.x` yield the regular expression `^1\.2\.(0|[1-9]\d*)?$`, which constrains the *major* and *minor* numbers to be `1` and `2`, respectively, *patch* number to be anything

The rule of thumb is that the `x` is used as a wildcard for the identifier in the position it appears. You don't need to specify all identifiers as the missing ones are just assumed to be wildcarded. On the other hand, the identifiers specifying a specific number are assumed to be matched exactly. Wildcards may appear in any place, also in more significant identifiers when less significant ones have fixed values (i.e. `x.2.x`).

Only *core* version identifiers can be constrained while extra identifiers are tolerated but not checked.

Nyx tries to be as tolerant as it can when parsing branch names trying to infer the constraints and as long as it's able to detect a *version like* pattern in **any** position it just ignores the rest. While the above examples are the simplest (and most frequently used) some more examples are:

* `1`: interpreted as `1.x.x`
* `x.2.x`: interpreted as `x.2.x` (note the wildcard may also be followed by fixed values)
* `vx`: interpreted as `x.x.x` (a simple `x` is interpreted as a *major* wildcard)
* `rel/1.x`: interpreted as `1.x.x`
* `release-v1.2`: interpreted as `1.2.x`
* `relv1.2.x-alpha`: interpreted as `1.2.x` (`alpha` is also ignored)
* `release-vx.2.3-abc.123+def.456`: interpreted as `x.23x`

What this feature does, actually, is similar to populating the [`versionRange`](#version-range) regular expression automatically, if it's not already defined. Using both options is not an error but since the [`versionRange`](#version-range) comes first, it wouldn't make sense.

See [this example]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}#version-constrained-branch) for more on this.
