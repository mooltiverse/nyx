---
layout: single
title:  "Version history for different release types"
date:   2020-01-01 00:00:00 +0000
categories: example user concepts
tags: git release version semver configuration
---

Understanding how releases are generated depending on their [configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) might be confusing at times so here are a few examples.

## Semantic Versioning

This example uses the [Semantic Versioning]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver) version scheme and for simplicity we are ignoring the way changes are detected so that for each commit we just mark it with a `MAJOR`, `MINOR`, `PATCH` or `NONE` change (where `NONE` means that commit doesn't signal any relevant change).

We compare several branches. On the left you see `master` (of type [main]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#main-releases)) used as a reference line, while others are:

* `release` of type [main]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#main-releases) is another *[mainline]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#mainline-only)*, just used to compare how version numbers progress compared to other release types
* `v1.x` of type [post-release]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#post-release) is a [maintenance branch]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maintenance-branches)
* `alpha` of type [pre-release]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#pre-release) is a [maturity branch]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#maturity-branches)
* `wip` of type [internal]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#internal-release) is a generic [integration branch]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/branching-models.md %}#integration-branches)

On each column we simulate what the release would look for each type.

```text
                        master          release         v1.x            alpha           wip
*   9566e24 Initial     0.1.0
|\
| * 105c3fe PATCH                       0.1.1           -error-         0.1.1-alpha.1   0.1.1-wip.1+20200101103000
| * a07c495 PATCH                       0.1.2           -error-         0.1.1-alpha.2   0.1.1-wip.2+20200101103000
| * 6a11ee1 MINOR                       0.2.0           -error-         0.2.0-alpha.1   0.2.0-wip.1+20200101103000
| * 53ebdb0 MINOR                       0.3.0           -error-         0.2.0-alpha.2   0.2.0-wip.2+20200101103000
| * 0676e36 NONE                                                        0.2.0-alpha.3   0.2.0-wip.3+20200101103000
| * 12cd339 MAJOR                       1.0.0           -error-         1.0.0-alpha.1   1.0.0-wip.1+20200101103000
| * 6ccbb87 PATCH                       1.0.1           -error-         1.0.0-alpha.2   1.0.0-wip.2+20200101103000
| * 4dfc8a2 MINOR                       1.1.0           -error-         1.0.0-alpha.3   1.0.0-wip.3+20200101103000
| * e87b851 PATCH                       1.1.1           -error-         1.0.0-alpha.4   1.0.0-wip.4+20200101103000
| * 5eec056 MAJOR                       2.0.0           -error-         1.0.0-alpha.5   1.0.0-wip.5+20200101103000
|/
* 245c5fb   MAJOR       1.0.0           (conflict with v1.x/12cd339)(*)
* 597002b   PATCH       1.0.1           (conflict with v1.x/6ccbb87)(*)
* 6e2f3bf   MINOR       1.1.0           (conflict with v1.x/4dfc8a2)(*)
* f97787b   PATCH       1.1.1           (conflict with v1.x/e87b851)(*)
|\
| * 105c3fe PATCH                       1.1.2           1.1.2           1.1.2-alpha.1   1.1.2-wip.1+20200101103000
| * ef38f7d PATCH                       1.1.3           1.1.3           1.1.2-alpha.2   1.1.2-wip.2+20200101103000
| * 97cd1ae MINOR                       1.2.0           1.2.0           1.2.0-alpha.1   1.2.0-wip.1+20200101103000
| * 3720cb3 MINOR                       1.3.0           1.3.0           1.2.0-alpha.2   1.2.0-wip.2+20200101103000
| * 960decb NONE                                                        1.2.0-alpha.3   1.2.0-wip.3+20200101103000
| * 4628247 MAJOR                       2.0.0           -error-         2.0.0-alpha.1   2.0.0-wip.1+20200101103000
| * 1e9531c PATCH                       2.0.1           -error-         2.0.0-alpha.2   2.0.0-wip.2+20200101103000
| * c182722 MINOR                       2.1.0           -error-         2.0.0-alpha.3   2.0.0-wip.3+20200101103000
| * 4de7675 PATCH                       2.1.1           -error-         2.0.0-alpha.4   2.0.0-wip.4+20200101103000
| * 966e713 MAJOR                       3.0.0           -error-         2.0.0-alpha.5   2.0.0-wip.5+20200101103000
|/
* f9c211b   MAJOR       2.0.0           (conflict with v1.x/4628247)(*)
```

As you can see, the `release` column shows a linear version increment, where each commit causes the [previous]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) version number to be incremented by bumping the most significant number, based on the changes introduced by the commits. Fairly intuitive.

The `v.1.x` column follows the same version increment logic as `release` but since it's a maintenance branch (for version `1.x`) it is constrained to generate version numbers that start with `1.` only so any version violating this rule generates an error.

The `alpha` column uses a [collapsed]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#collapse-versions) increment logic because it's a *pre-release*. Here every version is computed starting from the [prime]({{ site.baseurl }}{% link _posts/2020-01-01-whats-the-difference-between-the-prime-version-and-the-previous-version.md %}) version instead of the previous one and the number bumped is the most significant by including all changes since the *prime* version. When the selected version identifier has already been bumped, the [ticker]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope-attributes.md %}#ticker) (`N` within `alpha.N` in the example) is used to disambiguate between versions, and it's incremented linearly, until a new, more significant change, is detected.

The `wip` column is very similar to `alpha` but is considered an internal release type so an [extra identifier]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/extra-identifiers.md %}) (`wip`) is added to releases to disambiguate. In addition, a [timestamp]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/simple.md %}#timestamp-yyyymmddhhmmss-identifier) is also used to help developers distinguish between different builds. These extra identifiers are not relevant to this example and they are just meant to outline the different release types.
