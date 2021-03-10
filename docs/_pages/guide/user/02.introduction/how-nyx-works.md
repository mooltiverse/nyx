---
title: How Nyx Works
layout: single
toc: true
permalink: /guide/user/introduction/how-nyx-works/
---

TODO: write the introductory section here
{: .notice--warning}

## Arrange

TODO: write this section
{: .notice--warning}

## Clean

TODO: write this section
{: .notice--warning}

## Infer

This phase is where a bunch of information is collected from the Git repository and elsewhere, if required. No change is applied so you may consider this a *read only* step.

Let's break down the actions taken:

1. **release scope definition**: the Git commit history is browsed using Git natural order (from newest to older commits) starting from the current commit in the current branch (`HEAD`). Each commit is inspected along with its tags and the search stops at commit `C` where `C` is the first commit with a valid release tag applied (according to the [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme)). The release tag becomes the [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) and commit `C+1` (the commit after `C`, which was encountered before `C` when stepping backward) becomes the [initial commit](TODO: link the state attribute here).

TODO: summarize the INFER steps here
{: .notice--warning}

Some of the above steps may be skipped when the user overrides their outcomes by configuration. In general, a *minimal effort* approach is used so these steps are only triggered when required.

When merge commits are encountered scanning the commit history Nyx follows the [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) commit to stay on the main branch.
{: .notice--info}

## Make

TODO: write this section
{: .notice--warning}

## Mark

TODO: write this section
{: .notice--warning}

## Publish

TODO: write this section
{: .notice--warning}
