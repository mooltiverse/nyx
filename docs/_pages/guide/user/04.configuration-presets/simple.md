---
title: Simple
layout: single
toc: true
permalink: /guide/user/configuration-presets/simple/
---

This configuration preset is for teams following standard best practices using a simple [release strategy]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}) with:

* official releases issued by the [mainline]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#mainline-only) (`master` or `main`)
* pre-releases issued by [maturity]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maturity-branches) (`alpha`, `beta`, `gamma` etc), [integration]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#integration-branches) (`develop`, `integration`) and stage (`acceptance`, `candidate`, `development`, `latest`, `pre-production`, `production`, `qa`, `quality-assurance`, `rc`, `release-candidate`, `stable`, `stage`, `test`) branches
* maintenance releases issued by [maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches) following the `vX.Y.Z` nomenclature
* internal releases for all others

[Conventional Commits](https://www.conventionalcommits.org/) is the commit message convention in use and a wide range of [environments](#environments) is supported.

The following is the list of configuration settings defined by this preset.

### Commit message conventions

| Attribute                                                                                                                                                           | Value                                     | Notes                                     |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------- | ----------------------------------------- |
| [`enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#enabled-commit-message-conventions)             | `conventionalCommits`                     | Enables the [Conventional Commits](#conventional-commits) convention |

#### Conventional commits



















### Extra identifiers

#### Branch name identifier

This identifier is used to add the sanitized branch name to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `branchName`                              | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ #sanitizeLower }}{{ branch }}{{ /sanitizeLower }}{% endraw %}` | Use the [sanitized lower case]({{ site.baseurl }}/reference/templates/#sanitizelower) branch name as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `pre-release`                             | Use the identifier in the [pre-release part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Commit SHA identifier

This identifier is used to add the commit SHA to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `commitSHA`                               | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ commit }}{% endraw %}`       | Use the commit SHA name as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Commit (short) SHA identifier

This identifier is used to add the short (5 characters) commit SHA to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `commitShortSHA`                          | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ #short5 }}{{ commit }}{{ /short5 }}{% endraw %}` | Use the [short]({{ site.baseurl }}/reference/templates/#short5) commit SHA name as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Environment name identifier

This identifier is used to add the environment name to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `environmentName`                         | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ environment }}{% endraw %}`  | Use the environment name as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Pre-release identifier

This identifier is the one used in [pre-releases]({{ site.baseurl }}/reference/release-types/#pre-release) and uses the branch name as the qualifier and the [ticker]({{ site.baseurl }}/reference/state/#global-attributes) for the value. Example: `1.2.3-alpha.4`.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `semverPrerelease`                        | |
| [`qualifier`](#qualifier-extra-identifier)   | `{% raw %}{{ #sanitizeLower }}{{ branch }}{{ /sanitizeLower }}{% endraw %}`| Use the [sanitized lower case]({{ site.baseurl }}/reference/templates/#sanitizelower) branch name as the qualifier |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ ticker }}{% endraw %}`       | Use the [ticker]({{ site.baseurl }}/reference/state/#global-attributes) as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `pre-release`                             | Use the identifier in the [pre-release part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Timestamp ISO8601 identifier

This identifier is used to add the timestamp ([ISO 8601 format](https://en.wikipedia.org/wiki/ISO_8601)) to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `timestampISO8601`                        | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ #timestampISO8601 }}{{ timestamp }}{{ /timestampISO8601 }}{% endraw %}` | Use the timestamp as the value, formatted using the [`timestampISO8601` template function]({{ site.baseurl }}/reference/templates/#timestampiso8601) |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Timestamp Unix identifier

This identifier is used to add the timestamp (Unix format) to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `timestampUnix`                           | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ timestamp }}{% endraw %}`    | Use the timestamp as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### Timestamp YYYYMMDDHHMMSS identifier

This identifier is used to add the timestamp (`YYYYMMDDHHMMSS` format) to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `timestampYYYYMMDDHHMMSS`                 | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ #timestampYYYYMMDDHHMMSS }}{{ timestamp }}{{ /timestampYYYYMMDDHHMMSS }}{% endraw %}` | Use the timestamp as the value, formatted using the [`timestampYYYYMMDDHHMMSS` template function]({{ site.baseurl }}/reference/templates/#timestampyyyymmddhhmmss) |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

#### User name identifier

This identifier is used to add the local user to the version.

| Attribute                                    | Value                                     | Notes                                                                                                                 |
| -------------------------------------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-extra-identifier)             | `userName`                                | |
| [`qualifier`](#qualifier-extra-identifier)   | Empty                                     | Do not use the qualifier for this identifier, just the value |
| [`value`](#value-extra-identifier)           | `{% raw %}{{ user }}{% endraw %}`         | Use the local user name as the value |
| [`when`](#when-extra-identifier)             | `true`                                    | Always enable using this identifier |
| [`where`](#where-extra-identifier)           | `build`                                   | Use the identifier in the [build part](https://semver.org/) (when using the [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme)) |

### Release types

The following are the release type presets that come pre-configured with Nyx. Remember you can override, [enable or disable](#enabled-release-types) them at your need.

As a rule of thumb, all release types are considered *internal* when not working on a *clean* repository and a *server* environment. Many branch nomenclatures are available as presets for [pre-releases]({{ site.baseurl }}/reference/release-types/#pre-release) and a fallback *internal* release type for all other branches is available. [Maintenance]({{ site.baseurl }}/reference/release-types/#maintenance-branches) and [release]({{ site.baseurl }}/reference/release-types/#release-branches) are also supported out of the box.

#### Main release type

The default release type for *official* releases (those issued from the [mainline]({{ site.baseurl }}/best-practice/branching-models/#mainline-only)). For a detailed description on this release type see [here]({{ site.baseurl }}/reference/release-types/#main-releases).

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `main`                                                                     | |
| [`collapseVersions`](#collapse-versions)     | `false`                                                                    | Use linear versions |
| [`enabledIdentifiers`](#enabled-identifiers) | Empty                                                                      | Do not use extra identifiers |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | `^(master|main)$`                                                          | Releases from branches named `master` or `main` belong to this release type |
| [`onEnvironments`](#on-environments)         | `^((?!local).)*$`                                                          | Do not use this release type on [`local`]({{ site.baseurl }}/reference/state/#environment-attributes) environments (but do on all others) |
| [`onWorkspaceStatus`](#on-workspace-status)  | `clean`                                                                    | Require the workspace to have no uncommitted changes for this release type |
| [`publish`](#publish)                        | `{% raw %}${ server }{% endraw %}`                                         | Publish the release when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`type`](#type-release)                      | `main`                                                                     | Qualifies this as a [main]({{ site.baseurl }}/reference/release-types/#main-releases) release type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `false`                                                 | No version range constraint is applied |

#### Maturity

The [pre-releases]({{ site.baseurl }}/reference/release-types/#pre-release) type following the [maturity branches]({{ site.baseurl }}/best-practice/branching-models/#maturity-branches) nomenclature based on the widely used [greek alphabet](https://en.wikipedia.org/wiki/Greek_alphabet). This release type uses the collapsed versioning to keep the core version identifiers aligned with the future official release number and an extra qualifier, inferred dynamically by the branch name, to use for the pre-release identifier.

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `maturity`                                                                 | |
| [`collapseVersions`](#collapse-versions)     | `true`                                                                     | Use collapsed versions |
| [`enabledIdentifiers`](#enabled-identifiers) | `semverPrerelease`                                                         | Use the [pre-release extra identifier](#pre-release-identifier) |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | `^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$` | Releases from branches named after the [greek alphabet](https://en.wikipedia.org/wiki/Greek_alphabet) |
| [`onEnvironments`](#on-environments)         | `^((?!local).)*$`                                                          | Do not use this release type on [`local`]({{ site.baseurl }}/reference/state/#environment-attributes) environments (but do on all others) |
| [`onWorkspaceStatus`](#on-workspace-status)  | `clean`                                                                    | Require the workspace to have no uncommitted changes for this release type |
| [`publish`](#publish)                        | `{% raw %}${ server }{% endraw %}`                                         | Publish the release when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`type`](#type-release)                      | `prerelease`                                                               | Qualifies this as a [pre-release]({{ site.baseurl }}/reference/release-types/#pre-release) type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `false`                                                 | No version range constraint is applied |

#### Stage

The [pre-releases]({{ site.baseurl }}/reference/release-types/#pre-release) type following the [maturity]({{ site.baseurl }}/best-practice/branching-models/#maturity-branches)/[integration]({{ site.baseurl }}/best-practice/branching-models/#integration-branches)/[environment]({{ site.baseurl }}/best-practice/branching-models/#environment-branches) branches nomenclature. This release type uses the collapsed versioning to keep the core version identifiers aligned with the future official release number and an extra qualifier, inferred dynamically by the branch name, to use for the pre-release identifier.

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `stage`                                                                    | |
| [`collapseVersions`](#collapse-versions)     | `true`                                                                     | Use collapsed versions |
| [`enabledIdentifiers`](#enabled-identifiers) | `semverPrerelease`                                                         | Use the [pre-release extra identifier](#pre-release-identifier) |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | `^(acceptance|candidate|develop|development|integration|latest|pre-production|production|qa|quality-assurance|rc|release-candidate|stable|stage|test)$` | Releases from branches named after environments |
| [`onEnvironments`](#on-environments)         | `^((?!local).)*$`                                                          | Do not use this release type on [`local`]({{ site.baseurl }}/reference/state/#environment-attributes) environments (but do on all others) |
| [`onWorkspaceStatus`](#on-workspace-status)  | `clean`                                                                    | Require the workspace to have no uncommitted changes for this release type |
| [`publish`](#publish)                        | `{% raw %}${ server }{% endraw %}`                                         | Publish the release when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`type`](#type-release)                      | `prerelease`                                                               | Qualifies this as a [pre-release]({{ site.baseurl }}/reference/release-types/#pre-release) type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `false`                                                 | No version range constraint is applied |

#### Release

The *future* release type following the [release branches]({{ site.baseurl }}/best-practice/branching-models/#release-branches) branches nomenclature. This release type has a version range constraint inferred by the branch name.

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `release`                                                                  | |
| [`collapseVersions`](#collapse-versions)     | `false`                                                                    | Use linear versions |
| [`enabledIdentifiers`](#enabled-identifiers) | Empty                                                                      | Do not use extra identifiers |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | `^(rel(ease)?[-\/](v)?(0|[1-9]\d*)(\.(0|[1-9]\d*)(\.(0|[1-9]\d*))?)?)$`    | Releases from branches with names starting with `rel` or `release` followed by a `/` or `-`, an optional `v` and between 1 and 3 integers separated by dots. I.e. `release-1.2`, `release/1.2.3`, `rel-v1.2` etc |
| [`onEnvironments`](#on-environments)         | `^((?!local).)*$`                                                          | Do not use this release type on [`local`]({{ site.baseurl }}/reference/state/#environment-attributes) environments (but do on all others) |
| [`onWorkspaceStatus`](#on-workspace-status)  | `clean`                                                                    | Require the workspace to have no uncommitted changes for this release type |
| [`publish`](#publish)                        | `{% raw %}${ server }{% endraw %}`                                         | Publish the release when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`type`](#type-release)                      | `main`                                                                     | Qualifies this as a [main]({{ site.baseurl }}/reference/release-types/#main-releases) release type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `true`                                                  | Version range is inferred by the branch name |

#### Maintenance

The *maintenance* release type following the [maintenance branches]({{ site.baseurl }}/best-practice/branching-models/#maintenance-branches) branches nomenclature. This release type has a version range constraint inferred by the branch name.

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `maintenance`                                                              | |
| [`collapseVersions`](#collapse-versions)     | `false`                                                                    | Use linear versions |
| [`enabledIdentifiers`](#enabled-identifiers) | Empty                                                                      | Do not use extra identifiers |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | `^((v)?(0|[1-9]\d*)(\.(x|0|[1-9]\d*)(\.(x|0|[1-9]\d*))?)?)$`               | Releases from branches with names starting with an optional `v` and between 1 and 3 integers separated by dots. The second and/or third integer may be replaced by `x` as a wildcard. I.e. `1.2`, `1.2.3`, `v1.2` etc |
| [`onEnvironments`](#on-environments)         | `^((?!local).)*$`                                                          | Do not use this release type on [`local`]({{ site.baseurl }}/reference/state/#environment-attributes) environments (but do on all others) |
| [`onWorkspaceStatus`](#on-workspace-status)  | `clean`                                                                    | Require the workspace to have no uncommitted changes for this release type |
| [`publish`](#publish)                        | `{% raw %}${ server }{% endraw %}`                                         | Publish the release when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`type`](#type-release)                      | `postrelease`                                                              | Qualifies this as a [post-release]({{ site.baseurl }}/reference/release-types/#post-release) type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `true`                                                  | Version range is inferred by the branch name |

#### Internal

The default release type that catches everything and manages it as an internal release, with several extra identifiers. This release type also includes other ones when running in *dirty* workspaces or running on environments other than `server`.

| Attribute                                    | Value                                                                      | Notes                                                                                                                 |
| -------------------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-release-type)                 | `internal`                                                                 | |
| [`collapseVersions`](#collapse-versions)     | `false`                                                                    | Use linear versions |
| [`enabledIdentifiers`](#enabled-identifiers) | `branchName,commitShortSHA,environmentName,timestampYYYYMMDDHHMMSS`        | Use the [`branchName`](#branch-name-identifier), [`commitShortSHA`](#commit-short-sha-identifier), [`environmentName`](#environment-name-identifier) and [`timestampYYYYMMDDHHMMSS`](#timestamp-yyyymmddhhmmss-identifier) extra identifiers |
| [`gitCommit`](#git-commit)                   | `false`                                                                    | Do not commit any generated artifact |
| [`gitCommitMessage`](#git-commit-message)    | `{% raw %}Release {{ version }}{% endraw %}`                               | In case [`gitCommit`](#git-commit) is overridden, commit artifacts with a message like `Release v.1.2.3` |
| [`gitPush`](#git-push)                       | `{% raw %}${ server }{% endraw %}`                                         | Push tags to the remote repository when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTag`](#git-tag)                         | `{% raw %}${ server }{% endraw %}`                                         | Tag the commit with the release tag when running on a [`server` environment]({{ site.baseurl }}/reference/state/#environment-attributes) |
| [`gitTagMessage`](#git-tag-message)          | Empty                                                                      | Do not add a message to tags (and use [lightweight tags](https://git-scm.com/book/en/v2/Git-Basics-Tagging) for releases) |
| [`onBranches`](#on-branches)                 | Empty                                                                      | Matches any branch name |
| [`onEnvironments`](#on-environments)         | Empty                                                                      | Matches any environment |
| [`onWorkspaceStatus`](#on-workspace-status)  | Empty                                                                      | Matches any workspace status |
| [`publish`](#publish)                        | `false`                                                                    | Never publish internal releases |
| [`type`](#type-release)                      | `internal`                                                                 | Qualifies this as an [internal]({{ site.baseurl }}/reference/release-types/#internal-release) release type |
| [`versionRange`](#version-range)             | Empty                                                                      | No version range constraint is applied |
| [`versionRangeFromBranchName`](#version-range-from-branch-name) | `false`                                                 | No version range constraint is applied |

### Environments

The following are the environment presets that come pre-configured with Nyx. Remember you can override, [enable or disable](#enabled-environments) them at your need.

#### Local (generic environment)

This environment is a generic local that should be used for all local environments like developer workstations or other [non authoritative](#authoritative-vs-non-authoritative-environments) environments. This can be used as the default to use as a fallback when all other detection attempts fail.

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `local`                                                                    |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `false`                                                                    | Auto detection is disabled                                                                                            |
| [`default`](#default-environment)  | `true`                                                                     | This is the last resort environment that should be used whenever no other more specific environment is detected       |
| [`local`](#local)                  | `true`                                                                     | This is a [non authoritative](#authoritative-vs-non-authoritative-environments) environment                           |
| [`server`](#local)                 | `false`                                                                    |                                                                                                                       |
| [`user`](#user)                    | `{% raw %}${environment.getUser()}{% endraw %}`                            | Read the user name from the standard [`USER` or `USERNAME`]({{ site.baseurl }}/reference/expressions/#environment) environment variable |

#### Server (generic environment)

This environment is a generic server one that should be defined after all other, more specific, server definitions as it's for custom server environments or used as a fallback option when no other can be detected.

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `server`                                                                   |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('CI')}{% endraw %}`                    | Assume that the custom server environment defines the `CI` environment variable. Please note that the `CI` variable is often used by other CI/CD platforms like [CircleCI](https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables), [GitHub](https://docs.github.com/en/free-pro-team@latest/actions/reference/environment-variables) and [GitLab](https://docs.gitlab.com/ee/ci/variables/predefined_variables.html) so it's important for this environment to be evaluated after those in the [enabled environments](#enabled-environments) |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `{% raw %}${environment.getUser()}{% endraw %}`                            | Read the user name from the standard [`USER` or `USERNAME`]({{ site.baseurl }}/reference/expressions/#environment) environment variable |

#### Bamboo (environment)

This is the [Bamboo](https://www.atlassian.com/software/bamboo) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `bamboo`                                                                   |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('bamboo_agentId')}{% endraw %}`        | The [`bamboo.agentId`](https://confluence.atlassian.com/bamboo/bamboo-variables-289277087.html) variable is unique to Bamboo and, when defined, we can assume the current environment. There is no generic *Bamboo* variable so we picked one among the most generic. Dots are translated to underscores according to [the docs](https://confluence.atlassian.com/bamboo/bamboo-variables-289277087.html#Bamboovariables-Systemvariables) |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `bamboo`                                                                   | Hardcoded as Jenkins does not expose an variable suitable for this                                                    |

#### CircleCI (environment)

This is the [CircleCI](https://circleci.com/) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `circleci`                                                                 |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('CIRCLECI')}{% endraw %}`              | The [`CIRCLECI`](https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables) variable is unique to CircleCI and, when defined, we can assume the current environment |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `{% raw %}${environment.hasVariable('CIRCLE_USERNAME')}{% endraw %}`       | Use the value of the [`CIRCLE_USERNAME`](https://circleci.com/docs/2.0/env-vars/#built-in-environment-variables) variable |

#### GitHub (environment)

This is the [GitHub Actions](https://docs.github.com/en/free-pro-team@latest/actions/guides/about-continuous-integration) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `github`                                                                   |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('GITHUB_ACTIONS')}{% endraw %}`        | The [`GITHUB_ACTIONS`](https://docs.github.com/en/free-pro-team@latest/actions/reference/environment-variables) variable is unique to GitHub and, when defined, we can assume the current environment |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `{% raw %}${environment.hasVariable('GITHUB_ACTOR')}{% endraw %}`          | Use the value of the [`GITHUB_ACTOR`](https://docs.github.com/en/free-pro-team@latest/actions/reference/environment-variables) variable (may yield to values like `octocat`) |

#### GitLab (environment)

This is the [GitLab CI](https://docs.gitlab.com/ee/ci/) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `gitlab`                                                                   |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('GITLAB_CI')}{% endraw %}`             | The [`GITLAB_CI`](https://docs.gitlab.com/ee/ci/variables/predefined_variables.html) variable is unique to GitLab and, when defined, we can assume the current environment |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `{% raw %}${environment.hasVariable('GITLAB_USER_LOGIN')}{% endraw %}`     | Use the value of the [`GITLAB_USER_LOGIN`](https://docs.gitlab.com/ee/ci/variables/predefined_variables.html) variable|

#### Jenkins (environment)

This is the [Jenkins](https://www.jenkins.io/) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `jenkins`                                                                  |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('JENKINS_URL')}{% endraw %}`           | The [`JENKINS_URL`](https://www.jenkins.io/doc/book/pipeline/jenkinsfile/#using-environment-variables) variable is unique to Jenkins and, when defined, we can assume the current environment |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `jenkins`                                                                  | Hardcoded as Jenkins does not expose an variable suitable for this                                                    |

#### TeamCity (environment)

This is the [TeamCity](https://www.jetbrains.com/teamcity/) environment, defined as follows:

| Attribute                          | Value                                                                      | Notes                                                                                                                 |
| ---------------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`name`](#name-environment)        | `teamcity`                                                                 |                                                                                                                       |
| [`autoDetect`](#auto-detect)       | `{% raw %}${environment.hasVariable('TEAMCITY_VERSION')}{% endraw %}`      | The [`TEAMCITY_VERSION`](https://www.jetbrains.com/help/teamcity/predefined-build-parameters.html#Server+Build+Properties) variable is unique to TeamCity and, when defined, we can assume the current environment |
| [`default`](#default-environment)  | `false`                                                                    |                                                                                                                       |
| [`local`](#local)                  | `false`                                                                    |                                                                                                                       |
| [`server`](#local)                 | `true`                                                                     | This is an [authoritative](#authoritative-vs-non-authoritative-environments) environment                              |
| [`user`](#user)                    | `teamcity`                                                                 | Hardcoded as TeamCity does not expose an variable suitable for this                                                   |

