---
title: Services
layout: single
toc: true
permalink: /guide/user/configuration-reference/services/
---

Services allow you to use additional features provided by external entities like, for example, publishing releases on Git hosting service like GitHub and GitLab.

This option has little or nothing to deal with the [environments](#environments) although some names like `github` or `gitlab` match. The *environment* is where Nyx is running while multiple *services* might be used for publishing releases. In other words, when names match between *environments* and *services* it's because the same platform offers both CI/CD and Git services.
{: .notice--info}

[Services]({{ site.baseurl }}/reference/services/) are configured within the `services` section. The section allows one sub-section for each service and some overall options. Using a certain service is enabled only when you define the service block in the configuration file.

You can configure more than one service for your project although in most cases you just need the one your project is hosted to.

### Available services

#### Bitbucket (service)

[Bitbucket](https://bitbucket.org/) is not fully supported yet. [Stay tuned](https://github.com/mooltiverse/nyx/releases) for upcoming releases.
{: .notice--warning}

See also the [Bitbucket service section]({{ site.baseurl }}/reference/services/#bitbucket).

#### GitHub (service)

If your project is hosted on GitHub (cloud or hosted) you can configure this service to let Nyx use GitHub's features like [Releases](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github).

You can configure the service within the `services/github` section, using the following options:

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                     | Configuration File Option                | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | -------------------------------------------------------- | ---------------------------------------- | ------------------------------------------ |
| [`accessToken`](#github-access-token)          | string  | `--services-github-access-token=<TOKEN>`                   | `NYX_SERVICES_GITHUB_ACCESS_TOKEN=<TOKEN>` | `services/github/accessToken`  | `{% raw %}${ environment.getVariable('GITHUB_TOKEN') }{% endraw %}` |
| [`apiURL`](#github-api-url)                    | string  | `--services-github-api-url=<URL>`                          | `NYX_SERVICES_GITHUB_API_URL=<URL>`        | `services/github/apiURL`       | `https://api.github.com/`                                           |
| [`releases`](#github-releases)                 | boolean | `--services-github-releases=true|false`                    | `NYX_SERVICES_GITHUB_RELEASES=true|false`  | `services/github/releases`     | `true`                                                              |

See also the [GitHub service section]({{ site.baseurl }}/reference/services/#github).

##### GitHub access token

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `accessToken`                                                                            |
| Type                      | string                                                                                   |
| Default                   | `{% raw %}${ environment.getVariable('GITHUB_TOKEN') }{% endraw %}`                      |
| Command Line Option       | `--services-github-access-token=<TOKEN>`                                                 |
| Environment Variable      | `NYX_SERVICES_GITHUB_ACCESS_TOKEN=<TOKEN>`                                               |
| Configuration File Option | `services/github/accessToken`                                                            |
| Related state attributes  |                                                                                          |

Here you can pass the [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token) to use for authentication. You can hardcode the token (discouraged) or define an [expression]({{ site.baseurl }}/reference/expressions/) to be evaluated at runtime to get the value from an environment variable (usually named `GITHUB_TOKEN`).

By default the `GITHUB_TOKEN` environment variable is looked up, which is also compatible with [GitHub Actions](https://docs.github.com/en/actions/reference/authentication-in-a-workflow) environments.

This option is **required** to use the remote API.

##### GitHub API URL

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `apiURL`                                                                                 |
| Type                      | string                                                                                   |
| Default                   | `https://api.github.com/`                                                                |
| Command Line Option       | `--services-github-api-url=<URL>`                                                        |
| Environment Variable      | `NYX_SERVICES_GITHUB_API_URL=<URL>`                                                      |
| Configuration File Option | `services/github/accessToken`                                                            |
| Related state attributes  |                                                                                          |

If using a hosted GitHub instance, here you need to set the URL to the API.

You should only set this value to a non default value when using an hosted GitHub instance.

##### GitHub releases

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releases`                                                                               |
| Type                      | boolean                                                                                  |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--services-github-releases=true|false`                                                  |
| Environment Variable      | `NYX_SERVICES_GITHUB_RELEASES=true|false`                                                |
| Configuration File Option | `services/github/releases`                                                               |
| Related state attributes  |                                                                                          |

This flag enables or disables the publication of new releases to [GitHub Releases](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github). Whether or not a specific release is published also depends on the [`publish`](#publish) flag set on the [release type definition](#release-types).

By default this flag is enabled.

#### GitLab (service)

If your project is hosted on GitLab (cloud or hosted) you can configure this service to let Nyx use GitLab's features like [Releases](https://docs.gitlab.com/ee/user/project/releases/).

You can configure the service within the `services/gitlab` section, using the following options:

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                     | Configuration File Option                | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | -------------------------------------------------------- | ---------------------------------------- | ------------------------------------------ |
| [`accessToken`](#gitlab-access-token)          | string  | `--services-gitlab-access-token=<TOKEN>`                   | `NYX_SERVICES_GITLAB_ACCESS_TOKEN=<TOKEN>` | `services/gitlab/accessToken`  | `{% raw %}${ environment.getVariable('GITLAB_TOKEN') }{% endraw %}` |
| [`apiURL`](#gitlab-api-url)                    | string  | `--services-gitlab-api-url=<URL>`                          | `NYX_SERVICES_GITLAB_API_URL=<URL>`        | `services/gitlab/apiURL`       | `https://gitlab.com/api/v4/`                                        |
| [`releases`](#gitlab-releases)                 | boolean | `--services-gitlab-releases=true|false`                    | `NYX_SERVICES_GITLAB_RELEASES=true|false`  | `services/gitlab/releases`     | `true`                                                              |

See also the [GitLab service section]({{ site.baseurl }}/reference/services/#gitlab).

##### GitLab access token

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `accessToken`                                                                            |
| Type                      | string                                                                                   |
| Default                   | `{% raw %}${ environment.getVariable('GITLAB_TOKEN') }{% endraw %}`                      |
| Command Line Option       | `--services-gitlab-access-token=<TOKEN>`                                                 |
| Environment Variable      | `NYX_SERVICES_GITLAB_ACCESS_TOKEN=<TOKEN>`                                               |
| Configuration File Option | `services/gitlab/accessToken`                                                            |
| Related state attributes  |                                                                                          |

Here you can pass the [personal access token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) to use for authentication. You can hardcode the token (discouraged) or define an [expression]({{ site.baseurl }}/reference/expressions/) to be evaluated at runtime to get the value from an environment variable (usually named `GITLAB_TOKEN`).

By default the `GITLAB_TOKEN` environment variable is looked up.

If using an hosted service you may also use a [project access token](https://docs.gitlab.com/ee/user/project/settings/project_access_tokens.html) here.

This option is **required** to use the remote API.

##### GitLab API URL

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `apiURL`                                                                                 |
| Type                      | string                                                                                   |
| Default                   | `https://gitlab.com/api/v4/`                                                             |
| Command Line Option       | `--services-gitlab-api-url=<URL>`                                                        |
| Environment Variable      | `NYX_SERVICES_GITLAB_API_URL=<URL>`                                                      |
| Configuration File Option | `services/gitlab/accessToken`                                                            |
| Related state attributes  |                                                                                          |

If using a hosted GitLab instance, here you need to set the URL to the API.

You should only set this value to a non default value when using an hosted GitLab instance.

##### GitLab releases

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `releases`                                                                               |
| Type                      | boolean                                                                                  |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--services-gitlab-releases=true|false`                                                  |
| Environment Variable      | `NYX_SERVICES_GITLAB_RELEASES=true|false`                                                |
| Configuration File Option | `services/gitlab/releases`                                                               |
| Related state attributes  |                                                                                          |

This flag enables or disables the publication of new releases to [GitLab Releases](https://docs.gitlab.com/ee/user/project/releases/). Whether or not a specific release is published also depends on the [`publish`](#publish) flag set on the [release type definition](#release-types).

By default this flag is enabled.