---
title: Artifacts and Publications
layout: single
toc: true
permalink: /guide/user/introduction/artifacts-and-publications/
---

TODO: write this section about generating artifacts like changelogs or other local files, publishing releases and sending out notifications
{: .notice--warning}

### State file

The state file is where Nyx optionally stores its [internal state]({{ site.baseurl }}/reference/state/) to make its behavior [incremental]({{ site.baseurl }}/in-depth/design-principles/#incremental-process) even in stateless environments (like when using the command line with separate [commands]({{ site.baseurl }}/reference/usage/#available-commands)).

The state file is disabled by default but you can enable it by mean of the [`stateFile`]({{ site.baseurl }}/configuration/global-options/#state-file) option. The format is inferred by the extension of the file name you specify.

When enabled, the file is written and updated by the [infer]({{ site.baseurl }}/reference/usage/#infer), [make]({{ site.baseurl }}/reference/usage/#make) and [publish]({{ site.baseurl }}/reference/usage/#publish) commands and is deleted by [clean]({{ site.baseurl }}/reference/usage/#clean).

### Changelog

TODO: write this section
{: .notice--warning}







A very powerful feature provided by Nyx is the integration with external services to let you configure and orchestrate the release process in one place.

## Git hosting services

### Bitbucket

[Bitbucket](https://bitbucket.org/) is not fully supported yet. [Stay tuned](https://github.com/mooltiverse/nyx/releases) for upcoming releases.
{: .notice--warning}

### GitHub

Nyx comes with a GitHub API client that extends functionalities in order to integrate some GitHub features. With this service you can publish your releases to [GitHub Releases](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github).

In order to [authenticate](https://docs.github.com/en/rest/guides/getting-started-with-the-rest-api#authentication) for the GitHub API Nyx uses a [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token) that you need to create yourself in advance. You can pass the token to the service by several means, either by hardcoding it into the configuration (discouraged) or by setting an environment variable that Nyx looks up at run time. By default Nyx looks for the `GITHUB_TOKEN` environment variable, which is also used by [GitHub Actions](https://docs.github.com/en/actions/reference/authentication-in-a-workflow).

By default, the GitHub service uses the GitHub hosted service instance at [https://api.github.com/](https://api.github.com/) but you can also set a custom base API URL in case you're running your own GitHub instance.

To know more about the GitHub service configuration see the [reference]({{ site.baseurl }}/configuration/services/#github-service).

### GitLab

Nyx comes with a GitLab API client that extends functionalities in order to integrate some GitLab features. With this service you can publish your releases to [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/).

In order to [authenticate](https://docs.gitlab.com/ee/api/#authentication) for the GitLab API Nyx uses a [personal access token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) or [project access token](https://docs.gitlab.com/ee/user/project/settings/project_access_tokens.html) that you need to create yourself in advance. You can pass the token to the service by several means, either by hardcoding it into the configuration (discouraged) or by setting an environment variable that Nyx looks up at run time. By default Nyx looks for the `GITLAB_TOKEN` environment variable.

By default, the GitLab service uses the GitLab hosted service instance at [https://gitlab.com/api/v4/](https://gitlab.com/api/v4/) but you can also set a custom base API URL in case you're running your own GitLab instance.

To know more about the GitLab service configuration see the [reference]({{ site.baseurl }}/configuration/services/#gitlab-service).
