---
title: Changelog
layout: single
toc: true
permalink: /guide/user/configuration-reference/changelog/
---

The `changelog` *section* is where you configure the (optional) changelog generation.

There is only one `changelog` *section* per configuration.

The changelog scope includes only those changes belonging to the current release plus, optionally, [unreleased](#include-unreleased) changes. What appears in the changelog is the first line of the commit message and, optionally, some decorators that may have been configured here or added by a custom template.

### Changelog options

| Name                                                 | Type    | Command Line Option                                                           | Environment Variable                             | Default                                |
| ---------------------------------------------------- | ------- | ----------------------------------------------------------------------------- | ------------------------------------------------ | -------------------------------------- |
| [`changelog/commitLink`](#commit-link)               | string  | `--changelog-commit-link=<EXPR>`                                              | `NYX_CHANGELOG_COMMIT_LINK=<EXPR>`               | N/A                                    |
| [`changelog/contributorLink`](#contributor-link)     | string  | `--changelog-contributor-link=<EXPR>`                                         | `NYX_CHANGELOG_CONTRIBUTOR_LINK=<EXPR>`          | N/A                                    |
| [`changelog/includeUnreleased`](#include-unreleased) | boolean | `--changelog-include-unreleased`, `--changelog-include-unreleased=true|false` | `NYX_CHANGELOG_INCLUDE_UNRELEASED=true|false`    | N/A                                    |
| [`changelog/issueId`](#issue-id)                     | string  | `--changelog-issue-id=<REGEX>`                                                | `NYX_CHANGELOG_ISSUE_ID=<REGEX>`                 | N/A                                    |
| [`changelog/issueLink`](#issue-link)                 | string  | `--changelog-issue-link=<EXPR>`                                               | `NYX_CHANGELOG_ISSUE_LINK=<EXPR>`                | N/A                                    |
| [`changelog/path`](#path)                            | string  | `--changelog-path=<PATH>`                                                     | `NYX_CHANGELOG_PATH=<PATH>`                      | N/A                                    |
| [`changelog/sections`](#sections)                    | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) | `--changelog-sections-<NAME>=<REGEX>` | `NYX_CHANGELOG_SECTIONS_<NAME>=<REGEX>` | N/A                                    |
| [`changelog/template`](#template)                    | string  | `--changelog-template=<PATH>`                                                 | `NYX_CHANGELOG_TEMPLATE=<PATH>`                  | N/A                                    |

#### Commit link

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/commitLink`                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-commit-link=<EXPR>`                                                         |
| Environment Variable      | `NYX_CHANGELOG_COMMIT_LINK=<EXPR>`                                                       |
| Configuration File Option | `changelog/commitLink`                                                                   |
| Related state attributes  |                                                                                          |

A format string used to generate working links to commits using the commit SHA ID. This format expression must have one `%s` parameter that will be replaced by the commit ID.

If this option is not specified commits will not be linked.

For example, for a commit `cc6ab7319a606a65f5be4b683045ba3cd052aa4d`, using the `https://example.com/commits/%s` link expression will produce the `https://example.com/commits/cc6ab7319a606a65f5be4b683045ba3cd052aa4d` link.

The expression you use here depends on the hosting platform you use for hosting the Git repository.

In order for links to work you need to define the project and/or team name in the URL.
{: .notice--info}

#### Contributor link

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/contributorLink`                                                              |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-contributor-link=<EXPR>`                                                    |
| Environment Variable      | `NYX_CHANGELOG_CONTRIBUTOR_LINK=<EXPR>`                                                  |
| Configuration File Option | `changelog/contributorLink`                                                              |
| Related state attributes  |                                                                                          |

A format string used to generate working links to contributors using the commit contributor ID. This format expression must have one `%s` parameter that will be replaced by the committer ID.

If this option is not specified contributors will not be linked.

For example, for a committer `jdoe`, using the `https://example.com/people/%s` link expression will produce the `https://example.com/people/jdoe` link.

The expression you use here depends on the hosting platform you use for hosting the Git repository.

In order for links to work you may need to define the project and/or team name in the URL.
{: .notice--info}

#### Include unreleased

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/includeUnreleased`                                                            |
| Type                      | boolean                                                                                  |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-include-unreleased`, `--changelog-include-unreleased=true|false`            |
| Environment Variable      | `NYX_CHANGELOG_INCLUDE_UNRELEASED=true|false`                                            |
| Configuration File Option | `changelog/includeUnreleased`                                                            |
| Related state attributes  |                                                                                          |

When this option is enabled, an *Unlereased* section is added on top of the regular releases with all the changes that have not been released yet (not even with the current Nyx run). This may happen when only when non [significant]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits) changes have been committed since the last tagged release.

When used with no value on the command line (i.e. `--changelog-include-unreleased` alone) `true` is assumed.

#### Issue ID

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/issueId`                                                                      |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-issue-id=<REGEX>`                                                           |
| Environment Variable      | `NYX_CHANGELOG_ISSUE_ID=<REGEX>`                                                         |
| Configuration File Option | `changelog/issueId`                                                                      |
| Related state attributes  |                                                                                          |

A regular expression used to find references to issues in commit messages. Those references are then used to generate working links to issues in the changelog by using the [issue link](#issue-link).

The expression must define a *named capturing group* `id` which only captures the issue ID without delimiters. The ID captured this way is used as the value to format using the [issue link](#issue-link) expression.

If this option is not specified issue references will not be transformed.

A common value used for this option is `(?m)#(?<id>[0-9]+)(?s).*` which captures all IDs starting with the `#` character followed by the issue ID (as [GitHub](https://github.com/) and [GitLab](https://gitlab.com/) do). The `id` captuning group only captures the digits after the `#` delimiter so only the following digits will be used as ID in the [issue link](#issue-link) expression.

For example, if a commit message contains the `#123` string at some point, the issue ID `123` will be used in [issue link](#issue-link).

#### Issue link

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/issueLink`                                                                    |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-issue-link=<EXPR>`                                                          |
| Environment Variable      | `NYX_CHANGELOG_ISSUE_LINK=<EXPR>`                                                        |
| Configuration File Option | `changelog/issueLink`                                                                    |
| Related state attributes  |                                                                                          |

A format string used to generate working links to issues whose ID has been detected by the [issue ID](#issue-id) regular expression. This format expression must have one `%s` parameter that will be replaced by the issue ID.

If this option is not specified issue references will not be transformed.

For example, for an issue ID `123`, using the `https://example.com/issues/%s` link expression will produce the `https://example.com/issues/123` link.

The expression you use here depends on the hosting platform you use for issues.

In order for links to work you need to define the project and/or team name in the URL.
{: .notice--info}

#### Path

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/path`                                                                         |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-path=<PATH>`                                                                |
| Environment Variable      | `NYX_CHANGELOG_PATH=<PATH>`                                                              |
| Configuration File Option | `changelog/path`                                                                         |
| Related state attributes  |                                                                                          |

The absolute or relative path to a local file where the changelog is saved when generated. If a file already exists at the given location it is overwritten. Parent directories are created if needed.

Setting this value also enables the changelog creation, which is to say, when this option is not defined no changelog is generated.

A common value used for this option is `CHANGELOG.md`.

#### Sections

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/sections`                                                                     |
| Type                      | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-sections-<NAME>=<REGEX>`                                                    |
| Environment Variable      | `NYX_CHANGELOG_SECTIONS_<NAME>=<REGEX>`                                                  |
| Configuration File Option | `changelog/sections`                                                                     |
| Related state attributes  |                                                                                          |

The `sections` option lets you define the sections that will appear inside a single release in the changelog, each one grouping changes of the same *type*. Each section is defined by a *Name*, which is the name of the section to show in the changelog, and a regular expression that matches zero or more commit *types*, as they are inferred by the [`expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) option of the [commit message convention]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) in use. If a commit *type* is not matched by any itemp in this map then it will not appear in the changelog, and this might be useful to skim all the unrelevant changes.

But let's resume the process to make things clearer:

1. the [Infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) task scans the commit history and, for each commit, it uses the configured [`expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) to determine various attributes of the commit based on its message
2. among the various attributes that are inferred, the commit `type` tells the type of changes contributed by the commit
3. when generating the changelog commits are grouped by their `type` so that attribute is used to qualify the commit
4. since the development team may want the layout of the changelog to be organized in a more readable manner, the map defined for this `sections` option allows to rename sections, sort them differently, and change the way commits are grouped by their `type`
5. if the commit `type` is not matched by any section it is ignored by the changelog generator

For example, when using the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) convention you get commit of these types: `fix`, `feat`, `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test` and more. But using [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) as the changelog standard you need to map the above commit types to: `Added`, `Changed`, `Deprecated`, `Removed`, `Fixed`, `Security`. This mapping is exactly what is done by the `sections` option here. A starting point may be:

* `Added` = `^feat$`
* `Fixed` = `^fix$`

to ignore all changes not bringing bug fixes or new features.

As you can guess the sections you define here strongly depend on the configured [commit message convention]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}).

#### Template

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/template`                                                                     |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-template=<PATH>`                                                            |
| Environment Variable      | `NYX_CHANGELOG_TEMPLATE=<PATH>`                                                          |
| Configuration File Option | `changelog/template`                                                                     |
| Related state attributes  |                                                                                          |

The absolute or relative path to a local file to use as a template instead of the Nyx built-in. The file must contain a valid [Mustache](https://mustache.github.io/) template.
