---
title: Services
layout: single
toc: true
permalink: /guide/user/configuration-reference/services/
---

Services are objects performing additional operations to be plugged in the overall workflow. They may support one or more [features](#service-features) and they are invoked by specific configurations. The points where a service can be invoked from depend on the features they provide so for each invocation point the list of required features is outlined. Example services are those used to publish releases on Git hosting service like [GitHub](#github) and [GitLab](#gitlab) or produce artifacts using templates.

The configuration of services is somewhat different from other options as each service is associated to a [*type*](#service-types) (or *class*) which represents the implementation. This is because services are not purely declarative entities but they rely on some underlying custom implementation. There may be multiple *instances* of services with the same *type* to let users define different configurations depending on the specific use of a certain service for different purposes.

Services are configured within the `services` section. The section is a map of objects where entry names are symbolic names chosen by the user and objects are service definitions. Furthermore, each service accepts a number of options, also defined in a nested map, with available map values documented for each service *type*. See the [examples]({{ site.baseurl }}/examples/) section for hints on how to configure services.

### Service definition

Within the `services` block you can define as many services as you want, each in its own separate block. The `name` identifies the service configuration so to define a brand new one make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a service (given the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order)) then you are **overriding** an existing service. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single service.

Each service configuration has the following attributes:

| Name                                                                                       | Type    | Command Line Option                                                   | Environment Variable                                                    | Default                                              |
| ------------------------------------------------------------------------------------------ | ------- | --------------------------------------------------------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------- |
| [`services/<NAME>/name`](#name)                                                            | string  | `--services-<NAME>-name=<NAME>`                                       | `NYX_SERVICES_<NAME>_NAME=<NAME>`                                       | N/A                                                  |
| [`services/<NAME>/options`](#options)                                                      | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) | `--services-<NAME>-options-<OPTION>=<VALUE>`                          | `NYX_SERVICES_<NAME>_OPTIONS-<OPTION>=<VALUE>`                          | Empty                                                |
| [`services/<NAME>/type`](#type)                                                            | string  | `--services-<NAME>-type=<TYPE>`                                       | `NYX_SERVICES_<NAME>_TYPE=<TYPE>`                                       | N/A                                                  |

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `services/<NAME>`                                                                        |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--services-<NAME>=<NAME>`                                                               |
| Environment Variable      | `NYX_SERVICES_<NAME>=<NAME>`                                                             |
| Configuration File Option | `services/<NAME>`                                                                        |
| Related state attributes  |                                                                                          |

The short name that identifies this service configuration. This is actually not a field to be set within a service configuration section but instead the key of the map element.

This option is **mandatory**.

#### Options

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `services/<NAME>/options`                                                                |
| Type                      | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--services-<NAME>-options-<OPTION>=<VALUE>`                                             |
| Environment Variable      | `NYX_SERVICES_<NAME>_OPTIONS-<OPTION>=<VALUE>`                                           |
| Configuration File Option | `services/<NAME>/options`                                                                |
| Related state attributes  |                                                                                          |

A map where each entry defines a configuration option for the service. Available options depend on the service [type](#type).

The values specified here can be [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to be evaluated at runtime to make the configuration dynamic.

The default is the empty map.

When configuring this map using command line options or environment variables you need to pass flattened values as documented [here]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects). In this case you can pass each option as a command line option like `--services-<NAME>-options-<OPTION_NAME>=<VALUE>` or as an environment variable like `NYX_SERVICES_<NAME>_OPTIONS_<OPTION_NAME>=<VALUE>`.
{: .notice--info}

#### Type

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `services/<NAME>/type`                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/S                                                                                      |
| Command Line Option       | `--services-<NAME>-type=<TYPE>`                                                          |
| Environment Variable      | `NYX_SERVICES_<NAME>_TYPE=<TYPE>`                                                        |
| Configuration File Option | `services/<NAME>/type`                                                                   |
| Related state attributes  |                                                                                          |

The type (class) of the service being configured. Available types are:

* [`GIT`](#git)
* [`GITHUB`](#github)
* [`GITLAB`](#gitlab)

This option is **mandatory**.

### Service types

#### Git

The service of `GIT` [type](#type) giving you access to generic Git features. This service type supports:

* the `GIT_REMOTE` [feature](#service-features) to serve the credentials required for the local Git to use the remote repository to push changes

This service should be used as a fall back when there is no other service more specific for your needs. For example, if your remote repository is hosted on GitHub or GitLab you should use the [GitHub](#github) or [GitLab](#gitlab) service instead. On the other hand, this generic yet feature-limited service can be used for generic remotes.

##### Git configuration options

This service type supports the following [options](#options):

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                       | Configuration File Option                        | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------ | ------------------------------------------ |
| `REMOTES`                                      | list    | `--services-<NAME>-options-REMOTES=<NAMES>`                | `NYX_SERVICES_<NAME>_REMOTES=<NAMES>`                      | `services/<NAME>/options/REMOTES`                | N/A                                        |
| `USER`                                         | string  | `--services-<NAME>-options-USER=<NAME>`                    | `NYX_SERVICES_<NAME>_OPTIONS_USER=<NAME>`                  | `services/<NAME>/options/USER`                   | N/A                                        |
| `PASSWORD`                                     | string  | `--services-<NAME>-options-PASSWORD=<VALUE>`               | `NYX_SERVICES_<NAME>_OPTIONS_PASSWORD=<VALUE>`             | `services/<NAME>/options/PASSWORD`               | N/A                                        |

`REMOTES` is the optional comma separated list of remote names supported by the local Git. The names in this list are the simple names that are returned by running `git remote` on the command line and this list is a filter telling which remotes are supported by this service, or better, if the credentials configured for this service are suitable for remote operations like [pushing]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) changes (if any) to this service if it is among [enabled remotes]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#remote-repositories). If you have configured multiple [`GIT_REMOTE`](#service-features) services like this, this option allows you to match specific service credentials to specific remotes. Explicit matches (where a remote is explicitly listed among the ones in this option) have priority over implicit matches (where the service does notspecify any filter). Use this option only when you have configured multiple services providing the `GIT_REMOTE` [feature](#service-features), otherwise ignore it.

`USER` and `PASSWORD` are the credentials to use when authenticating to the remote repository to perform access controlled operations. You can [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable) that read environment variables here.

#### GitHub

The service of `GITHUB` [type](#type) giving you access to [GitHub](https://github.com/) extra features. This service type supports:

* the `GIT_REMOTE` [feature](#service-features) to serve the credentials required for the local Git to use the remote repository to push changes
* the `RELEASES` [feature](#service-features) to publish a [GitHub Release](https://help.github.com/en/github/administering-a-repository/releasing-projects-on-github) when a new release is produced

##### GitHub configuration options

This service type supports the following [options](#options):

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                       | Configuration File Option                        | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------ | ------------------------------------------ |
| `BASE_URI`                                     | string  | `--services-<NAME>-options-BASE_URI=<URI>`                 | `NYX_SERVICES_<NAME>_OPTIONS_BASE_URI=<URI>`               | `services/<NAME>/options/BASE_URI`               | `https://api.github.com`                   |
| `AUTHENTICATION_TOKEN`                         | string  | `--services-<NAME>-options-AUTHENTICATION_TOKEN=<TOKEN>`   | `NYX_SERVICES_<NAME>_OPTIONS_AUTHENTICATION_TOKEN=<TOKEN>` | `services/<NAME>/options/AUTHENTICATION_TOKEN`   | N/A                                        |
| `REMOTES`                                      | list    | `--services-<NAME>-options-REMOTES=<NAMES>`                | `NYX_SERVICES_<NAME>_REMOTES=<NAMES>`                      | `services/<NAME>/options/REMOTES`                | N/A                                        |
| `REPOSITORY_NAME`                              | string  | `--services-<NAME>-options-REPOSITORY_NAME=<TOKEN>`        | `NYX_SERVICES_<NAME>_OPTIONS_REPOSITORY_NAME=<TOKEN>`      | `services/<NAME>/options/REPOSITORY_NAME`        | N/A                                        |
| `REPOSITORY_OWNER`                             | string  | `--services-<NAME>-options-REPOSITORY_OWNER=<TOKEN>`       | `NYX_SERVICES_<NAME>_OPTIONS_REPOSITORY_OWNER=<TOKEN>`     | `services/<NAME>/options/REPOSITORY_OWNER`       | N/A                                        |

`BASE_URI` is meant to be used if you're using GitHub on a self hosted environment. If that's your case just pass the URI to your REST API endpoint here otherwise, if you're using the public service, do not pass any value.

`AUTHENTICATION_TOKEN` is a [OAuth or Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) that is used to authenticate and perform access controlled operations. This option is **mandatory** for the service in order to work. A suggested value is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable) that reads the [conventionally used](https://cli.github.com/manual/gh_help_environment) `GITHUB_TOKEN` environment variable so if you already have a token stored in `GITHUB_TOKEN` you don't have to configure anything else, otherwise just paste a PAT here. `GH_TOKEN` is another variable name often used for the same purpose.

As of now Personal Access Tokens are the only supported authentication method. The token you use for this purpose needs to have all the `repo` [permissions](https://docs.github.com/en/developers/apps/building-oauth-apps/scopes-for-oauth-apps#available-scopes).

You are encouraged to create a new token for this purpose.
{: .notice--info}

`REPOSITORY_NAME` is the name of the hosted repository. If your GitHub repository is `https://github.com/octocat/hello-world`, the value for this option is `hello-world`. This option is **mandatory** for the service in order to work.

`REPOSITORY_OWNER` is the name of the owner of hosted repository. If your GitHub repository is `https://github.com/octocat/hello-world`, the value for this option is `octocat`. If your repository is owned by an organization this is the organization name. This option is **mandatory** for the service in order to work.

`REMOTES` is the optional comma separated list of remote names supported by the local Git. The names in this list are the simple names that are returned by running `git remote` on the command line and this list is a filter telling which remotes are supported by this service, or better, if the credentials configured for this service are suitable for remote operations like [pushing]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) changes (if any) to this service if it is among [enabled remotes]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#remote-repositories). If you have configured multiple [`GIT_REMOTE`](#service-features) services like this, this option allows you to match specific service credentials to specific remotes. Explicit matches (where a remote is explicitly listed among the ones in this option) have priority over implicit matches (where the service does notspecify any filter). Use this option only when you have configured multiple services providing the `GIT_REMOTE` [feature](#service-features), otherwise ignore it.

#### GitLab

The service of `GITLAB` [type](#type) giving you access to [GitLab](https://gitlab.com/) extra features. This service type supports:

* the `GIT_REMOTE` [feature](#service-features) to serve the credentials required for the local Git to use the remote repository to push changes
* the `RELEASES` [feature](#service-features) to publish a [GitLab Release](https://docs.gitlab.com/ee/user/project/releases/) when a new release is produced

##### GitLab configuration options

This service type supports the following [options](#options):

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                       | Configuration File Option                        | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------ | ------------------------------------------ |
| `BASE_URI`                                     | string  | `--services-<NAME>-options-BASE_URI=<URI>`                 | `NYX_SERVICES_<NAME>_OPTIONS_BASE_URI=<URI>`               | `services/<NAME>/options/BASE_URI`               | `https://gitlab.com/api/v4`                |
| `AUTHENTICATION_TOKEN`                         | string  | `--services-<NAME>-options-AUTHENTICATION_TOKEN=<TOKEN>`   | `NYX_SERVICES_<NAME>_OPTIONS_AUTHENTICATION_TOKEN=<TOKEN>` | `services/<NAME>/options/AUTHENTICATION_TOKEN`   | N/A                                        |
| `REMOTES`                                      | list    | `--services-<NAME>-options-REMOTES=<NAMES>`                | `NYX_SERVICES_<NAME>_REMOTES=<NAMES>`                      | `services/<NAME>/options/REMOTES`                | N/A                                        |
| `REPOSITORY_NAME`                              | string  | `--services-<NAME>-options-REPOSITORY_NAME=<TOKEN>`        | `NYX_SERVICES_<NAME>_OPTIONS_REPOSITORY_NAME=<TOKEN>`      | `services/<NAME>/options/REPOSITORY_NAME`        | N/A                                        |
| `REPOSITORY_OWNER`                             | string  | `--services-<NAME>-options-REPOSITORY_OWNER=<TOKEN>`       | `NYX_SERVICES_<NAME>_OPTIONS_REPOSITORY_OWNER=<TOKEN>`     | `services/<NAME>/options/REPOSITORY_OWNER`       | N/A                                        |

`BASE_URI` is meant to be used if you're using GitLab on a self hosted environment. If that's your case just pass the URI to your REST API endpoint here otherwise, if you're using the public service, do not pass any value.

`AUTHENTICATION_TOKEN` is a [OAuth2, Personal or Project Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) that is used to authenticate and perform access controlled operations. This option is **mandatory** for the service in order to work. A suggested value is a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable) that reads the `GITLAB_TOKEN` environment variable so if you already have a token stored in `GITLAB_TOKEN` you don't have to configure anything else, otherwise just paste a PAT here. `GL_TOKEN` is another variable name often used for the same purpose.

As of now Personal Access Tokens are the only supported authentication method. The token you use for this purpose needs to have the `api`, `read_api` and `write_repository` [permissions](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html#personal-access-token-scopes).

You are encouraged to create a new token for this purpose.
{: .notice--info}

`REPOSITORY_NAME` is the name of the hosted repository. If your GitLab repository is `https://gitlab.com/jdoe/project`, the value for this option is `project`. If you're using an [hierarchical organization](https://docs.gitlab.com/ee/user/group/subgroups/) remember to avoid passing the leading and trailing slashes here. This option is **mandatory** for the service in order to work.

`REPOSITORY_OWNER` is the name of the owner of hosted repository. If your GitLab repository is `https://gitlab.com/jdoe/project`, the value for this option is `jdoe`. If your repository is owned by an organization this is the organization name. If you're using an [hierarchical organization](https://docs.gitlab.com/ee/user/group/subgroups/) remember to avoid passing the leading and trailing slashes here. This option is **mandatory** for the service in order to work.

`REMOTES` is the optional comma separated list of remote names supported by the local Git. The names in this list are the simple names that are returned by running `git remote` on the command line and this list is a filter telling which remotes are supported by this service, or better, if the credentials configured for this service are suitable for remote operations like [pushing]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#git-push) changes (if any) to this service if it is among [enabled remotes]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}#remote-repositories). If you have configured multiple [`GIT_REMOTE`](#service-features) services like this, this option allows you to match specific service credentials to specific remotes. Explicit matches (where a remote is explicitly listed among the ones in this option) have priority over implicit matches (where the service does notspecify any filter). Use this option only when you have configured multiple services providing the `GIT_REMOTE` [feature](#service-features), otherwise ignore it.

### Service features

The list of possible service features is:

* `GIT_REMOTE`: services supporting this feature can be used to perform Git tasks on remotes
* `RELEASES`: services supporting this feature can be used to publish releases to hosting services

Please note that using a service for a feature that is not supported will result in an error.
{: .notice--info}
