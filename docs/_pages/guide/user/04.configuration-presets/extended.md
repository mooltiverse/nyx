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
