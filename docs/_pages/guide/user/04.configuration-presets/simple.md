---
title: The Simple preset
layout: single
toc: true
permalink: /guide/user/configuration-presets/simple/
---

This preset can be used by setting the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) global configuration option value to `simple` and brings standard configurations well suited to get started with common items, as follows.

### Commit message conventions

The [Conventional Commits](https://www.conventionalcommits.org/) convention comes with this preset. This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`commitMessageConventions/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#enabled) | ["`conventionalCommits`"] |
| [`commitMessageConventions/conventionalCommits/expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) | "`(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\((?<scope>[a-z ]+)\))?:( (?<title>.+))$(?s).*`" |
| [`commitMessageConventions/conventionalCommits/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions) | "`major`" = "`(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*`", "`minor`" = "`(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*`", "`patch`" = "`(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*`" |

### Release types

This preset comes with a few release types, suitable for a [mainline only]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#mainline-only) branching model. An additional release type called *internal* is configured as a fallback when the mainline isn't matched.

Please note that even if a release type has its [`publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) flag enabled, publication to remote services doesn't happen because no [service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) is configured and none is listed in the [`publicationServices`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) list. If you want to publish releases using this preset as a starting point you need to configure some [services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) and override the [`releaseTypes/publicationServices`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services).
{: .notice--info}

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`releaseTypes/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled) | ["`mainline`", "`internal`"] |
| [`releaseTypes/publicationServices`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) | Empty |
| [`releaseTypes/mainline/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `false` |
| [`releaseTypes/mainline/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | Empty |
| [`releaseTypes/mainline/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/mainline/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` |
| [`releaseTypes/mainline/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/mainline/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/mainline/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/mainline/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/mainline/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/mainline/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/mainline/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(master|main)$"{% endraw %}` |
| [`releaseTypes/mainline/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/mainline/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/mainline/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/mainline/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/mainline/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/internal/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/internal/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `"internal"` |
| [`releaseTypes/internal/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/internal/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | Empty |
| [`releaseTypes/internal/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/internal/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/internal/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"false"` |
| [`releaseTypes/internal/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"false"` |
| [`releaseTypes/internal/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/internal/identifiers/0/position`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-position) | `"BUILD"` |
| [`releaseTypes/internal/identifiers/0/qualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-qualifier) | "`timestamp`" |
| [`releaseTypes/internal/identifiers/0/value`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-value) | `{% raw %}"{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}"{% endraw %}` |
| [`releaseTypes/internal/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | Empty |
| [`releaseTypes/internal/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/internal/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/internal/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/internal/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/internal/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
