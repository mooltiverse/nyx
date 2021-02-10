---
layout: single
title:  "What's the difference between the *prime version* and the *previous version*?"
date:   2020-01-01 00:00:00 +0000
categories: faq user concepts
tags: version
---

The *previous version* is, broadly speaking, the version that came before the current one (*current* the one Nyx is in the process of releasing). The *prime version* is the one Nyx starts from when bumping numbers in order to generate the new version number.

The distinction between *previous* and *prime* version is specific to Nyx and does not apply out of this scope.
{: .notice--info}

Most of the times the *prime* and *previous* versions are the same but under some circumstances they may not. When they differ is because of the [release type configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}). Let's take this commit history by example, in which we compare what happens on a [regular]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#main-releases) branch or on a [pre-release branch]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/release-strategy.md %}#pre-release):

```text
                         |         regular        |               pre-release               |
                         | current previous prime | current        previous           prime |
*   9566e24 0.1.0        |                        |                                         |
|\                       |                        |                                         |
| * 105c3fe PATCH change | 0.1.1   0.1.0 == 0.1.0 | 0.1.1-alpha.1  0.1.0          ==  0.1.0 |
| * 6a11ee1 MINOR change | 0.2.0   0.1.1 == 0.1.1 | 0.2.0-alpha.1  0.1.1-alpha.1  !=  0.1.0 |
| * 12cd339 MAJOR change | 1.0.0   0.2.0 == 0.2.0 | 1.0.0-alpha.1  0.2.0-alpha.1  !=  0.1.0 |
| * 6ccbb87 PATCH change | 1.0.1   1.0.0 == 1.0.0 | 1.0.0-alpha.2  1.0.0-alpha.1  !=  0.1.0 |
| * 4dfc8a2 MINOR change | 1.1.0   1.0.1 == 1.0.1 | 1.0.0-alpha.3  1.0.0-alpha.2  !=  0.1.0 |
|/                       |                        |                                         |
* 245c5fb   1.0.0        |                        |                                         |
* ...other commits...    |                        |                                         |
* f97787b   1.1.1        |                        |                                         |
|\                       |                        |                                         |
| * 105c3fe PATCH change | 1.1.2   1.1.0 == 1.1.0 | 1.1.2-alpha.1  1.1.1          ==  1.1.1 |
| * 97cd1ae MINOR change | 1.2.0   1.1.2 == 1.1.2 | 1.2.0-alpha.1  1.1.2-alpha.1  !=  1.1.1 |
| * 4628247 MAJOR change | 2.0.0   1.2.0 == 1.2.0 | 2.0.0-alpha.1  1.2.0-alpha.1  !=  1.1.1 |
| * 1e9531c PATCH change | 2.0.1   2.0.0 == 2.0.0 | 2.0.0-alpha.2  2.0.0-alpha.1  !=  1.1.1 |
| * c182722 MINOR change | 2.1.0   2.0.1 == 2.0.1 | 2.0.0-alpha.3  2.0.0-alpha.2  !=  1.1.1 |
```

In this example we assume that the pre-release branch is using `alpha` for the pre-release qualifier.
{: .notice--info}

As you can see, when working with regular branches the *previous* and the *prime* version are always the same. On the other hand, in pre-release branches they are often different and that's because pre-release branches need to **collapse the bumps** as they all happened in the same commit, starting from the last *mark* in version history. For them, the *mark* is the last *regular version* (the first encountered going backward in the history) that was released, so this is the *prime* version.

A different way to put this is that the *prime version* in pre-release branches is always the last *regular* release (first going backward), while the *previous* versions is simply the previous.

The direct implication of this is that in pre-release branches, the *prime version* never changes unless you merge from a regular branch, while the *previous* version changes for every release.
