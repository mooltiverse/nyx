---
title: The Extended preset
layout: single
toc: true
permalink: /guide/user/configuration-presets/extended/
---

This preset can be used by setting the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) global configuration option value to `extended` and brings rich standard configurations suitable for projects with several options in place.

### Changelog

A standard changelog configuration is included in this preset. The destination file is `CHANGELOG.md` (in the current working directory) and is rendered using the default template.

The rendered changelog uses the default template so its content is flat and no links are produced to any specific hosting service. Sections include commits matched by both the [Conventional Commits](https://www.conventionalcommits.org/) and [gitmoji](https://gitmoji.dev/) conventions.

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`changelog/path`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#path) | `CHANGELOG.md` |
| [`changelog/sections/Added`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) | `"^(feat|:boom:|:sparkles:)$"` |
| [`changelog/sections/Fixed`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) | "`^(fix|:bug:|:ambulance:)$`" |
| [`changelog/sections/Removed`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) | "`^:fire:$`" |
| [`changelog/sections/Security`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}#sections) | "`^:lock:$`" |

### Commit message conventions

The [Conventional Commits](https://www.conventionalcommits.org/) and [gitmoji](https://gitmoji.dev/) conventions come with this preset. This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`commitMessageConventions/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#enabled) | ["`conventionalCommits`", "`gitmoji`"] |
| [`commitMessageConventions/conventionalCommits/expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) | "`(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\((?<scope>[a-z ]+)\))?:( (?<title>.+))$(?s).*`" |
| [`commitMessageConventions/conventionalCommits/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions) | "`major`" = "`(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*`", "`minor`" = "`(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*`", "`patch`" = "`(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*`" |
| [`commitMessageConventions/gitmoji/expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) | "`(?m)^(:(?<type>[a-zA-Z0-9_]+):)( (?<title>.+))?$(?s).*`" |
| [`commitMessageConventions/gitmoji/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions) | "`major`" = "`(?m)^:boom:(?s).*`", "`minor`" = "`(?m)^:sparkles:(?s).*`", "`patch`" = "`(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*`" |

Please note that *gitmoji* is listed **after** *Conventional Commits* so when a commit message is evaluated, *gitmoji* is only taken into account if matching it against *Conventional Commit* does not yield to a positive match.

### Release types

This preset comes with a set of release types, suitable for a [mainline]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#mainline) branching model plus [integration]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#integration-branches), [maturity]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maturity-branches), [feature]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#feature-branches), fix, [hotfix]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#hotfix-branches), [release]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches) and [maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches). An additional release type called *internal* is configured as a fallback when the mainline isn't matched.

You may notice that *fix* and *hotfix* are different types in that *hotfix* is meant to deliver (and publish) a new release while the (simple) *fix* is supposed to happen under regular conditions, with no need to issue a new release, and behaves pretty much like a *feature* release type with just a different prefix in branches and tags.

Please note that even if a release type has its [`publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) flag enabled, publication to remote services doesn't happen because no [service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}) is listed in the [`publicationServices`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) list. If you want to publish releases using this preset as a starting point you need to override the [`releaseTypes/publicationServices`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) with the services you want to use as target (which may be the ones configured in the preset [services](#services) or some custom one).
{: .notice--info}

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`releaseTypes/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled) | ["`mainline`", "`integration`", "`maturity`", "`feature`", "`fix`", "`hotfix`", "`release`", "`maintenance`", "`internal`"] |
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
| [`releaseTypes/integration/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/integration/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/integration/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/integration/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(develop|development|integration|latest)(\.([0-9]\d*))?)$"{% endraw %}` |
| [`releaseTypes/integration/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/integration/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/integration/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/integration/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/integration/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/integration/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/integration/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(develop|development|integration|latest)$"{% endraw %}` |
| [`releaseTypes/integration/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/integration/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/integration/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/integration/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/integration/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/maturity/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/maturity/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/maturity/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/maturity/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\.([0-9]\d*))?)?$"{% endraw %}` |
| [`releaseTypes/maturity/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/maturity/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/maturity/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/maturity/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/maturity/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/maturity/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/maturity/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)$"{% endraw %}` |
| [`releaseTypes/maturity/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/maturity/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/maturity/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/maturity/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/maturity/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/feature/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/feature/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/feature/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/feature/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/feature/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/feature/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/feature/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"false"` |
| [`releaseTypes/feature/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"false"` |
| [`releaseTypes/feature/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/feature/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/feature/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(feat|feature)((-|\\/)[0-9a-zA-Z-_]+)?$"{% endraw %}` |
| [`releaseTypes/feature/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/feature/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/feature/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/feature/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/feature/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/fix/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/fix/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/fix/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/fix/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-fix(([0-9a-zA-Z]*)(\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/fix/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/fix/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/fix/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/fix/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"false"` |
| [`releaseTypes/fix/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/fix/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/fix/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^fix((-|\/)[0-9a-zA-Z-_]+)?$"{% endraw %}` |
| [`releaseTypes/fix/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/fix/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/fix/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/fix/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/fix/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/hotfix/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/hotfix/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{branch}}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/hotfix/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/hotfix/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-hotfix(([0-9a-zA-Z]*)(\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/hotfix/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/hotfix/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/hotfix/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/hotfix/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/hotfix/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/hotfix/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/hotfix/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^hotfix((-|\/)[0-9a-zA-Z-_]+)?$"{% endraw %}` |
| [`releaseTypes/hotfix/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/hotfix/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/hotfix/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/hotfix/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/hotfix/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |

| [`releaseTypes/release/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/release/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#firstLower}}{{branch}}{{/firstLower}}"{% endraw %}` |
| [`releaseTypes/release/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/release/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(rel|release)((\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/release/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/release/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/release/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/release/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/release/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/release/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/release/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(rel|release)(-|\/)({{configuration.releasePrefix}})?([0-9|x]\d*)(\.([0-9|x]\d*)(\.([0-9|x]\d*))?)?$"{% endraw %}` |
| [`releaseTypes/release/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/release/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/release/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/release/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/release/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `true` |
| [`releaseTypes/maintenance/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `false` |
| [`releaseTypes/maintenance/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | Empty |
| [`releaseTypes/maintenance/description`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#description) | Empty |
| [`releaseTypes/maintenance/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{configuration.releasePrefix}})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` |
| [`releaseTypes/maintenance/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/maintenance/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/maintenance/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/maintenance/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/maintenance/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/maintenance/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/maintenance/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^[a-zA-Z]*([0-9|x]\d*)(\.([0-9|x]\d*)(\.([0-9|x]\d*))?)?$"{% endraw %}` |
| [`releaseTypes/maintenance/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/maintenance/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/maintenance/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/maintenance/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/maintenance/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `true` |
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

### Services

The [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) services are available with this preset. They both use standard remote URIs so unless you use a privately hosted instance you don't need to change it. Authentication tokens are read from environment variables (`GITHUB_TOKEN` and `GITLAB_TOKEN`, respectively) so you can just set those variables to a valid Personal Access Token and you're set.

You still need to set the remaining options.

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`services/github/type`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#type) | `GITHUB` |
| [`services/github/options/AUTHENTICATION_TOKEN`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | `{% raw %}"{{#environmentVariable}}GITHUB_TOKEN{{/environmentVariable}}"{% endraw %}` |
| [`services/github/options/REPOSITORY_NAME`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | Empty |
| [`services/github/options/REPOSITORY_OWNER`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | Empty |
| [`services/gitlab/type`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#type) | `GITLAB` |
| [`services/gitlab/options/AUTHENTICATION_TOKEN`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | `{% raw %}"{{#environmentVariable}}GITLAB_TOKEN{{/environmentVariable}}"{% endraw %}` |
| [`services/gitlab/options/REPOSITORY_NAME`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | Empty |
| [`services/gitlab/options/REPOSITORY_OWNER`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#options) | Empty |

### Substitutions

Substitutions for a commn set of technologies is provided with this preset (names are the rule names you can use to [enable]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#enabled) the rules):

* `cargo_version`: sets the version attribute in [`Cargo.toml`](https://doc.rust-lang.org/cargo/reference/manifest.html) files in the project directory, useful for Rust projects
* `composer_version`: sets the version attribute in [`composer.json`](https://getcomposer.org/doc/01-basic-usage.md) files in the project directory, useful for PHP projects
* `dart_version`: sets the version attribute in [`pubspec.yaml`](https://dart.dev/tools/pub/pubspec) files in the project directory, useful for Flutter and Dart projects
* `elixir_version`: sets the version attribute in [`mix.exs`](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html) files in the project directory, useful for Elixir projects
* `expo_version`: sets the version attribute in [`app.json` and `app.config.json`](https://docs.expo.dev/versions/latest/config/app/) files in the project directory, useful for React projects
* `helm_version`: sets the version attribute in [`Chart.yaml`](https://v2.helm.sh/docs/developing_charts/#the-chart-yaml-file) files in the project directory, useful for Rust projects
* `node_version`: sets the version attribute in [`package.json`](https://docs.npmjs.com/cli/v9/configuring-npm/package-json) files in the project directory, useful for Node projects
* `text_version`: writes the version in `version.txt` files in the project directory, useful in many project automations

All these presets replace the version number in files at any depth in the project layout so, for example, you can  have multiple `package.json` file in your project tree and they will all get the updated version number. If this behavior doesn't fit your needs you can just override the substitution rule and make the `files` attribute more specific (i.e. use `./package.json` instead of `package.json` to just replace the version number in the `package.json` file in the root directory of your project).

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`substitutions/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#enabled) | none |
| [`substitutions/cargo_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}Cargo.toml{% endraw %}` |
| [`substitutions/cargo_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}version(\\s)*=(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?{% endraw %}` |
| [`substitutions/cargo_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}version = \"{{version}}\"{% endraw %}` |

| [`substitutions/composer_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}composer.json{% endraw %}` |
| [`substitutions/composer_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"{% endraw %}` |
| [`substitutions/composer_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}\"version\": \"{{version}}\"{% endraw %}` |

| [`substitutions/dart_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}pubspec.yaml{% endraw %}` |
| [`substitutions/dart_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?{% endraw %}` |
| [`substitutions/dart_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}version: \"{{version}}\"{% endraw %}` |

| [`substitutions/elixir_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}mix.exs{% endraw %}` |
| [`substitutions/elixir_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}version(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"{% endraw %}` |
| [`substitutions/elixir_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}version: \"{{version}}\"{% endraw %}` |

| [`substitutions/expo_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}@(app.json|app.config.json){% endraw %}` |
| [`substitutions/expo_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"{% endraw %}` |
| [`substitutions/expo_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}\"version\": \"{{version}}\"{% endraw %}` |

| [`substitutions/helm_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}Chart.yaml{% endraw %}` |
| [`substitutions/helm_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}(\"|')?version(\"|')?(\\s)*:(\\s)*(\"|')?(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?(\"|')?{% endraw %}` |
| [`substitutions/helm_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}version: \"{{version}}\"{% endraw %}` |

| [`substitutions/node_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}package.json{% endraw %}` |
| [`substitutions/node_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}\"version\"(\\s)*:(\\s)*\"(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?\"{% endraw %}` |
| [`substitutions/node_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}\"version\": \"{{version}}\"{% endraw %}` |

| [`substitutions/text_version/files`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#files) | `{% raw %}version.txt{% endraw %}` |
| [`substitutions/text_version/match`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#match) | `{% raw %}(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?{% endraw %}` |
| [`substitutions/text_version/replace`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}#replace) | `{% raw %}{{version}}{% endraw %}` |
