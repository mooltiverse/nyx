---
title: Git
layout: single
toc: true
permalink: /guide/user/configuration-reference/git/
---

The `git` *section* is where you can pass Git parameters to Nyx.

## Remotes

### Credentials

#### Using tokens

Many Git hosting services encourage to use tokens (Personal Access Tokens, OAuth Tokens etc) to log in as they are more secure and they allow tokens to be issued and revoked independently from the user accounts. However Git does not support (so far) authentication using a single token and it still needs credentials to be passed as a user name and password pair. In order to cope with this you can map the token to user names and passwords by following known rules for remote services. Here is the list of known services and the mapping:

* when configuring a remote repository hosted on [GitHub](https://github.com/) pass the [OAuth or Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) as the [`user`](#user) name and an empty string as the [`password`](#password)
* when configuring a remote repository hosted on [GitLab](https://gitlab.com/) pass the fixed string `PRIVATE-TOKEN` as the [`user`](#user) name and the [OAuth2, Personal or Project Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) as the [`password`](#password)

A fallback rule for many remote services not listed here is to pass the token for both the user name and the password.

Hardcoding sensitive credentials into configuration files exposes your accounts at security risks so always consider using [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read them from environment variables]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable).
{: .notice--warning}

### Remote definition

Within the `remotes` block you can define as many remote repositories as you want, each in its own separate block. The `name` identifies the remote repository as it is returned by running [`git remote`](https://git-scm.com/docs/git-remote) locally. If you use a `name` that was already defined for a remote then you are **overriding** an existing remote configuration. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single remote.

Configuring remotes gives Nyx information about:

* the configured remotes
* the credentials to use when performing operations to and from remote repositories

Each remote has the following attributes:

| Name                                        | Type    | Command Line Option                        | Environment Variable                          | Default |
| ------------------------------------------- | ------- | ------------------------------------------ | --------------------------------------------- | ------- |
| [`git/remotes/<NAME>/password`](#password)  | string  | `--git-remotes-<NAME>-password=<TEMPLATE>` | `NYX_GIT_REMOTES_<NAME>_PASSWORD=<TEMPLATE>`  | N/A     |
| [`git/remotes/<NAME>/user`](#user)          | string  | `--git-remotes-<NAME>-user=<TEMPLATE>`     | `NYX_GIT_REMOTES_<NAME>_USER=<TEMPLATE>`      | N/A     |

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>`                                                                     |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>=<NAME>`                                                            |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>=<NAME>`                                                          |
| Configuration File Option | `git/remotes/items/<NAME>`                                                               |
| Related state attributes  |                                                                                          |

The name that identifies the remote repository as it is returned by running [`git remote`](https://git-scm.com/docs/git-remote) locally. This is actually not a field to be set within a remotes but instead the key of the map element.

This option is **mandatory**.

#### Password

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>/password`                                                            |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>-password=<TEMPLATE>`                                               |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>_PASSWORD=<TEMPLATE>`                                             |
| Configuration File Option | `git/remotes/items/<NAME>/password`                                                      |
| Related state attributes  |                                                                                          |

The password to use when connecting to the remote repository. Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read them from environment variables]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable).

#### User

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>/user`                                                                |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>-user=<TEMPLATE>`                                                   |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>_USER_=<TEMPLATE>`                                                |
| Configuration File Option | `git/remotes/items/<NAME>/user`                                                          |
| Related state attributes  |                                                                                          |

The password to use when connecting to the remote repository. Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read them from environment variables]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable).
