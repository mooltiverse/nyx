---
sidebar_position: 2
tags:
  - user
  - guide
  - introduction
---

# Introduction

[Release management](https://en.wikipedia.org/wiki/Release_management) entails many tasks and responsibilities and can be daunting when the size of the team and the project grows. Examples of these tasks are:

* tracking released versions
* managing merge policies between branches
* fetching information from the commit history
* granting a consistent versioning policy
* assigning consistent release numbers to new releases
* generating artifacts such as the *changelog*
* publishing the released artifacts
* sending notification upon new releases

Some of these tasks can be automated and here is where Nyx comes into play. Once set up is complete, either by detailed configuration or by using presets, you can just forget all of the above and make your release process a no brainer.

## Things you need before you get started

To get started you need a [Git](https://git-scm.com/) repository, which can be local only or hosted on a central repository like [GitHub](https://github.com/) and [GitLab](https://gitlab.com/).

You also need to make a few decisions such as:

* the [version scheme](version-schemes.mdx) to use (chances are you want to use [SemVer](version-schemes.mdx#semantic-versioning-semver))
* the [branching strategy](../best-practice/branching-models.mdx) to use. If you don't have one you can assume you're using the [mainline only](../best-practice/branching-models.mdx#mainline-only), which is the simplest one to start with
* the [release types](../configuration-reference/release-types.mdx) you want to manage and what is your desired workflow for each
* the [commit message conventions](../configuration-reference/commit-message-conventions.mdx) you have adopted to provide structured information along with commits
