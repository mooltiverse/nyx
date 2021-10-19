---
title: The Extended preset
layout: single
toc: true
permalink: /guide/user/configuration-presets/extended/
---

This preset can be used by setting the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) global configuration option value to `extended` and brings rich standard configurations suitable for projects with several options in place.

### Commit message conventions

The [Conventional Commits](https://www.conventionalcommits.org/) and [gitmoji](https://gitmoji.dev/) conventions come with this preset. This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`commitMessageConventions/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#enabled) | ["`conventionalCommits`", "`gitmoji`"] |
| [`commitMessageConventions/conventionalCommits/expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) | "`(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\((?<scope>[a-z ]+)\))?:( (?<title>.+))$(?s).*`" |
| [`commitMessageConventions/conventionalCommits/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions) | "`major`" = "`(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*`", "`minor`" = "`(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*`", "`patch`" = "`(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*`" |
| [`commitMessageConventions/gitmoji/expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) | "`(?m)^(:(?<type>[a-zA-Z0-9_]+):)( (?<title>.+))?$(?s).*`" |
| [`commitMessageConventions/gitmoji/bumpExpressions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#bump-expressions) | "`major`" = "`(?m)^:boom:(?s).*`", "`minor`" = "`(?m)^:sparkles:(?s).*`", "`patch`" = "`(?m)^:(zap|bug|ambulance|lipstick|lock|arrow_down|arrow_up|pushpin|chart_with_upwards_trend|heavy_plus_sign|heavy_minus_sign|wrench|globe_with_meridians|pencil2|rewind|package|alien|bento|wheelchair|speech_balloon|card_file_box|children_crossing|iphone|egg|alembic|mag|label|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage):(?s).*`" |

Please note that *gitmoji* is listed **after** *Conventional Commits* so when a commit message is evaluated, *gitmoji* is only taken into account if matching it against *Conventiona Commit* does not yield to a positive match.

### Release types

This preset comes with a set of release types, suitable for a [mainline]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#mainline) branching model plus [integration]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#integration-branches), [maturity]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maturity-branches), [feature]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#feature-branches), [hotfix]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#hotfix-branches), [release]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#release-branches) and [maintenance]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches). An additional release type called *internal* is configured as a fallback when the mainline isn't matched.

This corresponds to the following configuration options:

| Name                                            | Value                                                                                    |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`releaseTypes/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled) | ["`mainline`", "`integration`", "`maturity`", "`feature`", "`hotfix`", "`release`", "`maintenance`", "`internal`"] |
| [`releaseTypes/mainline/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `false` |
| [`releaseTypes/mainline/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | Empty |
| [`releaseTypes/mainline/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` |
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
| [`releaseTypes/integration/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{ branch }}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/integration/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(develop|development|integration|latest)(\.([0-9]\d*))?)$"{% endraw %}` |
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
| [`releaseTypes/maturity/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{ branch }}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/maturity/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega)(\.([0-9]\d*))?)?$"{% endraw %}` |
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
| [`releaseTypes/feature/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{ branch }}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/feature/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(feat|feature)(([0-9a-zA-Z]*)(\.([0-9]\d*))?)?)$"{% endraw %}` |
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
| [`releaseTypes/hotfix/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/hotfix/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#sanitizeLower}}{{ branch }}{{/sanitizeLower}}"{% endraw %}` |
| [`releaseTypes/hotfix/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(fix|hotfix)(([0-9a-zA-Z]*)(\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/hotfix/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/hotfix/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/hotfix/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/hotfix/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/hotfix/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/hotfix/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/hotfix/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(fix|hotfix)((-|\/)[0-9a-zA-Z-_]+)?$"{% endraw %}` |
| [`releaseTypes/hotfix/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/hotfix/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/hotfix/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"true"` |
| [`releaseTypes/hotfix/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/hotfix/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
| [`releaseTypes/release/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `true` |
| [`releaseTypes/release/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | `{% raw %}"{{#firstLower}}{{ branch }}{{/firstLower}}"{% endraw %}` |
| [`releaseTypes/release/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)(-(rel|release)((\.([0-9]\d*))?)?)$"{% endraw %}` |
| [`releaseTypes/release/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/release/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/release/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"true"` |
| [`releaseTypes/release/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"true"` |
| [`releaseTypes/release/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/release/identifiers`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifiers) | Empty |
| [`releaseTypes/release/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | `{% raw %}"^(rel|release)(-|\/)({{ configuration.releasePrefix }})?([0-9|x]\d*)(\.([0-9|x]\d*)(\.([0-9|x]\d*))?)?$"{% endraw %}` |
| [`releaseTypes/release/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/release/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | `"CLEAN"` |
| [`releaseTypes/release/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/release/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/release/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `true` |
| [`releaseTypes/maintenance/collapseVersions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) | `false` |
| [`releaseTypes/maintenance/collapsedVersionQualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapsed-version-qualifier) | Empty |
| [`releaseTypes/maintenance/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | `{% raw %}"^({{ configuration.releasePrefix }})?([0-9]\d*)\.([0-9]\d*)\.([0-9]\d*)$"{% endraw %}` |
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
| [`releaseTypes/internal/filterTags`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-tags) | Empty |
| [`releaseTypes/internal/gitCommit`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) | `"false"` |
| [`releaseTypes/internal/gitCommitMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit-message) | Empty (use default) |
| [`releaseTypes/internal/gitPush`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) | `"false"` |
| [`releaseTypes/internal/gitTag`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag) | `"false"` |
| [`releaseTypes/internal/gitTagMessage`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message) | Empty (use default) |
| [`releaseTypes/internal/identifiers/enabled`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#enabled-identifiers) | ["`timestamp`"] |
| [`releaseTypes/internal/identifiers/timestamp/position`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-position) | `"BUILD"` |
| [`releaseTypes/internal/identifiers/timestamp/qualifier`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-qualifier) | "`timestamp`" |
| [`releaseTypes/internal/identifiers/timestamp/value`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#identifier-value) | `{% raw %}"{{#timestampYYYYMMDDHHMMSS}}{{ timestamp }}{{/timestampYYYYMMDDHHMMSS}}"{% endraw %}` |
| [`releaseTypes/internal/matchBranches`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-branches) | Empty |
| [`releaseTypes/internal/matchEnvironmentVariables`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-environment-variables) | Empty |
| [`releaseTypes/internal/matchWorkspaceStatus`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#match-workspace-status) | Empty |
| [`releaseTypes/internal/publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) | `"false"` |
| [`releaseTypes/internal/versionRange`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range) | Empty |
| [`releaseTypes/internal/versionRangeFromBranchName`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#version-range-from-branch-name) | `false` |
