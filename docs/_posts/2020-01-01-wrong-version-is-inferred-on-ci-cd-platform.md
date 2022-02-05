---
layout: single
title:  "Wrong version is inferred on CI/CD platforms"
date:   2020-01-01 00:00:00 +0000
categories: troubleshooting user
tags: support configuration version ci-cd github actions
---

One of the very first steps when running a pipeline on a CI/CD platfom is checking out the Git repository and this is usually done by the platform without the user to define any specific configuration or workflow.

In order to optimize the proces, especially for very large repositories, the underlying CI/CD platform often tries not  to checkout the entire repository but just the bare minimum contents. Common strategies often check out just the single last commit (which sometimes results in a [*detached HEAD*](https://git-scm.com/docs/git-checkout#_detached_head)).

While this works fine in most cases where you don't need the entire Git history, it may seriously affect Nyx's behavior as it needs the repository history in order to infer the required information and take consequent actions. What you see in this situation is that Nyx always start from the [initial version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) (i.e. `0.1.0` when using [semver]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver)), regardless of the repository history.

The solution to this issue is simple and is about instructing the CI/CD platform to check out the **entire** repository history when running pipelines.

The [CI/CD]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/ci-cd.md %}) can give you directions on how to get this done with tested CI/CD platforms.
