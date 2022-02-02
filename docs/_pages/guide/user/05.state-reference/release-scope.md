---
title: Release Scope
layout: single
toc: true
permalink: /guide/user/state-reference/release-scope/
---

## Release scope attributes

The following attributes are children of the [`releaseScope`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-scope) element:

| Name                                                                | Type    | Values                                                    |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------- |
| [`releaseScope/commits`](#commits)                                  | list    | The [commits](#commit-objects) in the release scope       |
| [`releaseScope/finalCommit`](#final-commit)                         | string  | The last [commit](#commit-objects) in the release scope   |
| [`releaseScope/initialCommit`](#initial-commit)                     | string  | The first [commit](#commit-objects) in the release scope  |
| [`releaseScope/previousVersion`](#previous-version)                 | string  | The previous version                                      |
| [`releaseScope/previousVersionCommit`](#previous-version-commit)    | string  | The previous version [commit](#commit-objects)            |
| [`releaseScope/primeVersion`](#prime-version)                       | string  | The prime version                                         |
| [`releaseScope/primeVersionCommit`](#prime-version-commit)          | string  | The prime version [commit](#commit-objects)               |
| [`releaseScope/significantCommits`](#significant-commits)           | list    | The significant [commits](#commit-objects)                |

### Commits

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/commits`                                                                   |
| Type                          | list                                                                                     |
| Related configuration options |                                                                                          |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark){: .btn .btn--small} |

The ordered list of all [commits](#commit-objects) in the release scope. The list is reverse ordered, so the [newest commit](#final-commit) appears as the first element in the list, while the [oldest](#initial-commit) is the last in the list.

This value may remain undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

Furthermore this attribute may be changed by the [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark) task in case a new commit is added (i.e. to include generated release artifacts).

### Final commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/finalCommit`                                                               |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark){: .btn .btn--small} |

The last [commit](#commit-objects) in the release scope, which is to say the commit to be tagged upon release, the latest commit in the current branch (`HEAD`). This is the first element of the [`commits`](#commits) list.

This value may remain undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) or if there is no further commits after the [previous version](#previous-version-commit).

Furthermore this attribute may be changed by the [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark) task in case a new commit is added (i.e. to include generated release artifacts).

### Initial commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/initialCommit`                                                             |
| Type                          | string                                                                                   |
| Related configuration options | [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark){: .btn .btn--small} |

The [commit](#commit-objects) that became right after the [`releaseScope/previousVersionCommit`](#previous-version-commit), so the first commit in the range of commits belonging to the release being prepared. This is the last element of the [`commits`](#commits) list.

This value may remain undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) or if there is no further commits after the [previous version](#previous-version-commit).

If no [`releaseScope/previousVersion`](#previous-version) is detected this value will be the root commit in the Git repository.

### Previous version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/previousVersion`                                                           |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [releaseTypes/ID/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [releaseTypes/ID/collapsedVersionQualifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier){: .btn .btn--success .btn--small} [releaseTypes/ID/filterTags]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#filter-tags){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The version that was released before the one being created. This is the value of the version tag applied to the [`releaseScope/previousVersionCommit`](#previous-version-commit) and is found by browsing the commit history backward (in Git's natural order) until this tag that successfully matches the [release types filter]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#filter-tags) is found. Upon merge commits only the [first parent]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-nyx-deal-with-merge-commits-when-scanning-the-commit-history.md %}) is considered. The search stops at the first tag that is valid according to:

* the version [`scheme`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme)
* optional prefixes, according to [`releaseLenient`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient)
* the tag filter defined using the [`filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#filter-tags) option in the [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}).

This value remains undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

### Previous version commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/previousVersionCommit`                                                     |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [releaseTypes/ID/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [releaseTypes/ID/collapsedVersionQualifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier){: .btn .btn--success .btn--small} [releaseTypes/ID/filterTags]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#filter-tags){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The [commit](#commit-objects) that the [`previousVersion`](#previous-version) tag points to.

This value remains undefined when no previous version can be found in the commit history or [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

### Prime version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/primeVersion`                                                              |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [releaseTypes/ID/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [releaseTypes/ID/collapsedVersionQualifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The version that is used as the baseline when bumping version numbers when the release type uses [collapsed versioning]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) (the *pre-release* versioning).

This is the first version tag that is encountered browsing the commit history backwards that only has *core* identifiers. It is also the value of the version tag applied to the [`releaseScope/primeVersionCommit`](#prime-version-commit). Upon merge commits only the [first parent]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-nyx-deal-with-merge-commits-when-scanning-the-commit-history.md %}) is considered. The search stops at the first tag that is valid according to:

* the version [`scheme`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme)
* optional prefixes, according to [`releaseLenient`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient)

This value remains undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version). It also remains undefined when not using collapsed versioning.

Also see [this F.A.Q.]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-collapsed-versioning-work.md %}) and [this post]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) for more on the *previous* and *prime* versions.

### Prime version commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/primeVersionCommit`                                                        |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [releaseTypes/ID/collapseVersions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions){: .btn .btn--success .btn--small} [releaseTypes/ID/collapsedVersionQualifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier){: .btn .btn--success .btn--small} [releaseTypes/ID/filterTags]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#filter-tags){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The [commit](#commit-objects) that the [`primeVersion`](#prime-version) tag points to.

This value remains undefined when no previous or prime version can be found in the commit history or [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version). It also remains undefined when not using collapsed versioning.

### Significant commits

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/significantCommits`                                                        |
| Type                          | list                                                                                     |
| Related configuration options | [commitMessageConventions/ID/bumpExpressions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The ordered list of all significant [commits](#commit-objects) in the release scope, considering commits to be *significant* when they bring informations about some version identifier to bump, according to [`commitMessageConventions/<ID>/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions). The list is reverse ordered, so the [newest commit](#final-commit) appears as the first element in the list, while the [oldest](#initial-commit) is the last in the list.

When the [current release type]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-type) uses [collapsed versioning]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) significant commits may also include commits up to the [prime version commit](#prime-version-commit).

When this list is empty the scope has no new commits at all or the commits that have been found do not yield to a new version.

This value may remain undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

## Commit objects

Each commit in the release scope has the following properties:

| Name                                                                | Type    | Values                                                          |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------------------- |
| `sha`                                                               | string  | The SHA-1 identifier of the commit                              |
| `date`                                                              | integer | The integer representing the commit timestamp                   |
| `parents`                                                           | list    | The list of SHA-1 identifiers of parent commits                 |
| `authorAction`                                                      | object  | The container for the commit author fields (see below)          |
| `authorAction/identity`                                             | object  | The container for the commit author identity fields (see below) |
| `authorAction/identity/name`                                        | string  | The commit author name                                          |
| `authorAction/identity/email`                                       | string  | The commit author email (optional)                              |
| `authorAction/timeStamp`                                            | object  | The commit author timestamp (optional)                          |
| `authorAction/timeStamp/timeStamp`                                  | date    | The actual commit author timestamp                              |
| `authorAction/timeStamp/timeZone`                                   | string  | The commit author time zone (optional)                          |
| `commitAction`                                                      | object  | The container for the committer fields (see below)              |
| `commitAction/identity`                                             | object  | The container for the committer identity fields (see below)     |
| `commitAction/identity/name`                                        | string  | The committer name                                              |
| `commitAction/identity/email`                                       | string  | The committer email (optional)                                  |
| `commitAction/timeStamp`                                            | object  | The committer timestamp (optional)                              |
| `commitAction/timeStamp/timeStamp`                                  | date    | The actual committer timestamp                                  |
| `commitAction/timeStamp/timeZone`                                   | string  | The committer time zone (optional)                              |
| `message`                                                           | object  | The container for the commit message (see below)                |
| `message/fullMessage`                                               | string  | The entire commit message                                       |
| `message/shortMessage`                                              | string  | The first line of the commit message                            |
| `message/footers`                                                   | map     | The commit footers, each modelled as a name and value pair      |
| `tags`                                                              | list    | The list of tags applied to the commit                          |
