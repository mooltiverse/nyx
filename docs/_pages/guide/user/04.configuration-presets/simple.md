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

