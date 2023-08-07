---
title: How Nyx Works
layout: single
toc: true
permalink: /guide/user/introduction/how-nyx-works/
---

In this page you can find the overall principles of the various Nyx phases and how they work in order to take full advantage.

## Clean

In this phase, which must be invoked explicitly, the repository state is reverted to its initial state by removing all the files created during the other stages, if any.

Nyx will only clean the artifacts created by its own release process (like the changelog or the summary file) while all others are ignored.

## Infer

This phase is where information is collected from the Git repository, if required. No change is applied so you may consider this a *read only* step.

Let's break down the actions taken:

1. **release scope definition**: the Git commit history is browsed using Git natural order (from newest to older commits) starting from the current commit in the current branch (`HEAD`). Each commit is inspected along with its tags and the search stops at commit `C` where `C` is the first commit with a valid release tag applied (according to the [scheme]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme) in use). The release tag becomes the [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) and commit `C+1` (the commit after `C`, which was encountered before `C` when stepping backward) becomes the [initial commit]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#initial-commit).
2. **scope analysis**: messages of commits within the release scope are analyzed against the configured [conventions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) in order to detect whether or not there are [significant]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant-commits) changes and, if so, which version identifier must be bumped.
3. **generate version**: creates the current version identifier. If, during the scope analysis, the release scope has beed detected as containing some significant changes, the previous version is increased by bumping the identifier that was inferred during the analysis (depending on the commit message convention). Otherwise the previous version is left intact. If the above steps didn't find any [previous version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#previous-version) then the [initial version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) is used.

By the end of this task, Nyx has all the information it needs to proceed to further steps. This information is also made available through the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}).

Some of the above steps may be skipped when the user overrides their outcomes by configuration. In general, a *minimal effort* approach is used so these steps are only triggered when required.

When merge commits are encountered scanning the commit history Nyx follows the [first parent](https://git-scm.com/docs/git-log#Documentation/git-log.txt---first-parent) commit to stay on the main branch.
{: .notice--info}

## Make

This is where the configured artifacts are built or modified by Nyx, when needed. More specifically:

* [`substitutions`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/substitutions.md %}) are evaluated and text tokens are replaced in the configured files
* the [`changelog`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/changelog.md %}) is built and rendered

## Mark

This phase is where:

1. new or changed release artifacts are **committed**, if any and if the [configured release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-commit) instructs the tool to do so
2. a new release **tag** is created in the Git repository. The tag anatomy (i.e. whether it is [lightweight or annotated](https://git-scm.com/book/en/v2/Git-Basics-Tagging)) is driven by the [release type configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-tag-message)
3. the new commit and tag are **pushed** to the [remote](https://git-scm.com/docs/git-remote) repositories, if required by the [release type configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push)

These steps are only taken if there is a [new version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-version) resulting from the commit history after [inference](#infer), otherwise no action is taken.

## Publish

If the [matched release type]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#release-type) configuration has the [`publish`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publish) flag enabled the new release, [if any]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-version), is published to the configured [services]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services).

These steps are only taken if there is a [new version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-version) resulting from the commit history after [inference](#infer), otherwise no action is taken.
