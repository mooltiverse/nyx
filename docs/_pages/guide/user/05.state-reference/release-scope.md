---
title: Release Scope
layout: single
toc: true
permalink: /guide/user/state-reference/release-scope/
---

## Release scope attributes

The following attributes are children of the [`releaseScope`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-scope) element:

| Name                                                                | Type    | Values                                              |
| ------------------------------------------------------------------- | ------- | --------------------------------------------------- |
| [`releaseScope/finalCommit`](#final-commit)                         | string  | The SHA-1 of the last commit in the release scope   |
| [`releaseScope/initialCommit`](#initial-commit)                     | string  | The SHA-1 of the first commit in the release scope  |
| [`releaseScope/previousVersion`](#previous-version)                 | string  | The previous version                                |
| [`releaseScope/previousVersionCommit`](#previous-version-commit)    | string  | The SHA-1 of the previous version commit            |
| [`releaseScope/significant`](#significant)                          | boolean | Whether or not the scope brings significant changes |

### Final commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/finalCommit`                                                             |
| Type                          | string                                                                                   |
| Related configuration options |                                                                                          |
| Initialized by task           | [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark){: .btn .btn--small} |

The SHA-1 of the last commit in the release scope, which is to say the commit to be tagged upon release, the latest commit in the current branch (`HEAD`).

Please note that this attribute is undefined until the [mark]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#mark) task runs as some extra commits may need to be included (i.e. for new release artifacts).

### Initial commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/initialCommit`                                                             |
| Type                          | string                                                                                   |
| Related configuration options | [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The SHA-1 of the commit that became right after the [`releaseScope/previousVersionCommit`](#previous-version-commit), so the first commit in the range of commits belonging to the release being prepared.

This value may remain undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version) or if there is no further commits after the [previous version](#previous-version-commit).

If no [`releaseScope/previousVersion`](#previous-version) is detected this value will be the SHA-1 of the root commit in the Git repository.

### Previous version

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/previousVersion`                                                           |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The version that was released before the one being created. This is the value of the version tag applied to the [`releaseScope/previousVersionCommit`](#previous-version-commit) and is found by browsing the commit history backward (in Git's natural order) until this tag is found. Upon merge commits only the [first parent]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-nyx-deal-with-merge-commits-when-scanning-the-commit-history.md %}) is considered. The search stops at the first tag that is valid according to the version [`scheme`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme), also considering prefixes, according to [`releaseLenient`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient).

This value remains undefined when no previous version can be found or [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

### Previous version commit

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/previousVersionCommit`                                                     |
| Type                          | string                                                                                   |
| Related configuration options | [initialVersion]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version){: .btn .btn--success .btn--small} [releaseLenient]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-lenient){: .btn .btn--success .btn--small} [releasePrefix]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#release-prefix){: .btn .btn--success .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme){: .btn .btn--success .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

The SHA-1 of the commit that was released before the one being created, which is to say, the commit that the [`previousVersion`](#previous-version) tag points to.

This value remains undefined when no previous version can be found or [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

### Significant

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `releaseScope/significant`                                                               |
| Type                          | boolean                                                                                  |
| Related configuration options | [conventions](TODO: add the link to the commit message conventions here){: .btn .btn--success .btn--small} |
| Initialized by task           | [infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer){: .btn .btn--small} |

This boolean tells if the [`version`](#version) release scope contains significant commits to be released or not. When this is `true` then significant changes have been detected and they are worth a new release. When `false` the scope has no new commits at all or the commits that have been found do not yield to a new release.

This value remains undefined when [inference]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) is skipped because the user overrides the [`version`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#version).

TODO: add links to the commit message conventions here (both in the configuration and other state attributes) and the behavior of other tasks like Make, Mark and Publish.
{: .notice--warning}
