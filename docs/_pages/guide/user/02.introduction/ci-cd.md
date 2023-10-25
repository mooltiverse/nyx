---
title: CI/CD
layout: single
toc: true
permalink: /guide/user/introduction/ci-cd/
---

Nyx is perfectly suited for use in CI/CD platforms and leverage [automation]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/build-and-automation.md %}). This page gives you additional information to set up your build pipelines using common CI/CD platforms effectively and work around some caveats.

## [GitHub Actions](https://docs.github.com/en/actions)

### Using the official Nyx GitHub Action

Please refer to the [GitHub Action home page]({{ site.data.project.home }}-github-action).

### Check out the entire repository when running jobs

If you're running your pipelines on GitHub Actions you probably start your build jobs with the [checkout](https://github.com/actions/checkout) action, which, by default, only checks out the latest commit as the `fetch-depth` parameter defaults to `1`.

This prevents Nyx from inferring information from the commit history and you likely end up with the inferred version to always be the [initial version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) (i.e. `0.1.0`) as further illustrated [here]({{ site.baseurl }}{% link _posts/2020-01-01-wrong-version-is-inferred-on-ci-cd-platform.md %}).

To work around this you just have to configure the checkout action to always fetch the entire commit history by setting the `fetch-depth` parameter to `0` as documented [here](https://github.com/actions/checkout#Fetch-all-history-for-all-tags-and-branches):

```yaml
- uses: actions/checkout@v3
  with:
    fetch-depth: 0
```

### Credentials

When configuring the [GitHub service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#github) to [publish releases]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) you need to pass credentials to Nyx.

When running GitHub Actions pipelines you can take advantage of the [automatic token authentication](https://docs.github.com/en/actions/security-guides/automatic-token-authentication) that provides the `GITHUB_TOKEN` environment variable to GitHub Actions jobs so you don't need to generate a new [OAuth or Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) for this specific purpose. The same token is also available from within the [`secrets` context](https://docs.github.com/en/actions/learn-github-actions/contexts#github-context).

As an example, to read the token and set it as the `GH_TOKEN` environment variable you can define your GitHub Actions job like in the following snippets.

Running Nyx using the [GitHub Action]({{ site.data.project.home }}-github-action#usage):

```yaml
release:
  steps:
    - name: Release
      uses: mooltiverse/nyx-github-action@main
      env:
        GH_TOKEN: {% raw %}${{ secrets.GITHUB_TOKEN }}{% endraw %}
      with:
        command: publish
```

Running Nyx using the Gradle plugin:

```yaml
release:
  steps:
    - name: Publish
      env:
        GH_TOKEN: {% raw %}${{ secrets.GITHUB_TOKEN }}{% endraw %}
      run: ./gradlew nyxPublish
```

## [GitLab CI](https://docs.gitlab.com/ee/ci/)

When running pipelines on GitLab CI you need to override some defaults to make Nyx work correctly. In particular you need to set the values for the [`GIT_STRATEGY`](https://docs.gitlab.com/ee/ci/runners/configure_runners.html#git-strategy) and [`GIT_DEPTH`](https://docs.gitlab.com/ee/ci/runners/configure_runners.html#shallow-cloning) variables to `clone` and `0` respectively,  so that the entire repository is checked out, like:

```yaml
variables:
  GIT_STRATEGY: clone
  GIT_DEPTH: "0"
```

If you don't override these variables the local repository will only have a shallow copy, which does not contain the information Nyx requires in order to infer the version. In other words, it's very likely that the inferred version will always be the [initial version]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#initial-version) (i.e. `0.1.0`) as further illustrated [here]({{ site.baseurl }}{% link _posts/2020-01-01-wrong-version-is-inferred-on-ci-cd-platform.md %}).

### Credentials

When configuring the [GitLab service]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#gitlab) to [publish releases]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#publication-services) you need to pass credentials to Nyx.

When running GitLab pipelines and you want Nyx to push Git changes or publish releases on your behalf you need to pass a [token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) with `api`, `read_user`, `read_api`, `write_repository` and `write_registry` scope as a [variable](https://docs.gitlab.com/ee/ci/variables/) (say `GITLAB_TOKEN`) and use this environment variable in Nyx configuration in [GitLab configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/services.md %}#gitlab-configuration-options).
