---
title: CI/CD
layout: single
toc: true
permalink: /guide/user/introduction/ci-cd/
---

Nyx is perfectly suited for use in CI/CD platforms and leverage [automation]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/build-and-automation.md %}). This page gives you additional information to set up your build pipelines using common CI/CD platforms effectively and work around some caveats.

## [GitHub Actions](https://docs.github.com/en/actions)

### Check out the entire repository when running jobs

If you're running your pipelines on GitHub Actions you probably start your build jobs with the [checkout](https://github.com/actions/checkout) action, which, by default, only checks out the latest commit as the `fetch-depth` parameter defaults to `1`.

This prevents Nyx from inferring information from the commit history and you likely end up with the inferred version to always be the [initial version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) (i.e. `0.1.0`) as further illustrated [here]({{ site.baseurl }}{% link _posts/2020-01-01-wrong-version-is-inferred-on-ci-cd-platform.md %}).

To work around this you just have to configure the checkout action to always fetch the entire commit history by setting the `fetch-depth` parameter to `0` as documented [here](https://github.com/actions/checkout#Fetch-all-history-for-all-tags-and-branches):

```yaml
- uses: actions/checkout@v2
  with:
    fetch-depth: 0
```