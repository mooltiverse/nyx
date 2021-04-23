---
layout: single
title:  "What's the difference between the *prime version* and the *previous version*?"
date:   2020-01-01 00:00:00 +0000
categories: faq user concepts
tags: git version
---

The *previous version* is, broadly speaking, the version that came before the current one (*current* the one Nyx is in the process of releasing).

The *prime version* is a concept introduced by Nyx for [*collapsed*](TODO: add the link to the collapsed versioning configuration option) versioning (also see [this F.A.Q.]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-collapsed-versioning-work.md %})), where the *previous version* alone is not enough in order to generate a new version number. The *prime version* is only used for this kind of versioning and you can see an example [here]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}#pre-release-branch) with a broad explanation.

The distinction between *previous* and *prime* version is specific to Nyx and does not apply out of this scope.
{: .notice--info}

The *prime version* is the version tag of the first reachable parent commit that doesn't use extra identifiers (so using only *core* identifiers). When evaluating the *parent* commit of a merge commit only the [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) (the commit that *received* the merge) is followed. The *prime* and *previous* versions may or may not match.

For example `1.2.3` may be a *prime version* but `1.2.3-alpha.1` may not as it uses additional identifiers.
{: .notice--info}

For an example let's compare these two commit histories side by side. On the left side you have a regular commit history using standard versioning only, while on the right side you have an `alpha` *pre-release* branch (on the right-side lane) using the *collapsed* versioning. These two examples are widely illustrated [here]({{ site.baseurl }}{% link _posts/2020-01-01-git-history-examples.md %}).

| Regular       |  Collapsed     |
|:-------------:|:--------------:|
| [![]({{site.baseurl}}/assets/git-history-regular-branch.svg)]({{site.baseurl}}/assets/git-history-regular-branch.svg) | [![]({{site.baseurl}}/assets/git-history-pre-release-branch.svg)]({{site.baseurl}}/assets/git-history-pre-release-branch.svg) |

*Prime versions* are those on the commits highlighted with orange circles while the path from any commit in the *pre-release* branch to (the commit tagged with) its *prime version* is highlighted in orange.

A couple of things to note:

* *prime versions* are only in the main line branch as they must not have extra identifiers
* the path from any *pre-release* commit to its *prime version* commit is interrupted whenever the *pre-release* branch is closed or deleted (like at commits `c8` and `c12`)
* a simple merge into the *pre-release* branch doesn't change the path to the *prime version* commit as you can see by commit `m18`, which still has `1.1.0` as its *prime version* (from commit `m13`), not `1.3.0` (from commit `m17`), because `m18`'s *first parent* is `c16` (which leads back to `m13`), not `m17`

If you want to know more about how the *prime version* is used when computing *collapsed* versions see [here]({{ site.baseurl }}{% link _posts/2020-01-01-how-does-collapsed-versioning-work.md %}).
