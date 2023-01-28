---
title: Git
layout: single
toc: true
permalink: /guide/user/configuration-reference/git/
---

The `git` *section* is where you can pass Git parameters to Nyx.

## Remotes

When using multiple [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) or customizing [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}), these values must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

### Limitations

Nyx uses external libraries to connect to remotes and so inherits their limitations. These libraries are:

* [JSch (mwiede's fork)](https://github.com/mwiede/jsch) and [JGit](https://www.eclipse.org/jgit/) for the Java (Gradle) version
* [go-git](https://github.com/go-git/go-git) and [ssh](https://pkg.go.dev/golang.org/x/crypto/ssh) for the Go (command line) version

The limitations coming from those libraries may show up, for example:

* in case of unsupported key algorithms and formats (when using SSH keys)
* in case of unsupported JVM versions (for the Java version)
* when files are not found at their usual locations (i.e. `~/.ssh`, when using SSH keys)

Other limitations may be due to remote end services.

In case you notice some errors or unexpected behavior please make sure it's not due to the above libraries.

### Credentials

#### Using tokens

Many Git hosting services encourage to use tokens (Personal Access Tokens, OAuth Tokens etc) to log in as they are more secure and they allow tokens to be issued and revoked independently from the user accounts. However Git does not support (so far) authentication using a single token and it still needs credentials to be passed as a user name and password pair. In order to cope with this you can map the token to user names and passwords by following known rules for remote services. Here is the list of known services and the mapping:

* when configuring a remote repository hosted on [GitHub](https://github.com/) pass the [OAuth or Personal Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) as the [`user`](#user) name and an empty string as the [`password`](#password)
* when configuring a remote repository hosted on [GitLab](https://gitlab.com/) pass the fixed string `PRIVATE-TOKEN` as the [`user`](#user) name and the [OAuth2, Personal or Project Access Token](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html) as the [`password`](#password)

A fallback rule for many remote services not listed here is to pass the token for both the user name and the password.

Hardcoding sensitive credentials into configuration files exposes your accounts at security risks so always consider using [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read them from environment variables]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable).
{: .notice--warning}

#### Using public key (SSH)

Using public keys is encouraged for security reasons although it introduces some complexity in handling credentials. Nyx supports public key authentication to access remote Git repositories. Keep in mind that:

* the remote URL must be in the SSH form, i.e. `git@github.com:mooltiverse/nyx.git`; you can check your remote URL with `git remote -v`
* although you can pass private keys as parameters along with optional passphrases, you should use keys from the standard locations (i.e. `~/.ssh` folder)
* when private keys are passed as parameters, remote key fingerprint check is not performed
* ssh-agent (including Pageant) support is **experimental** to avoid entering the passphrase to private keys, when used

Not all algorithms are supported on all platforms so make sure your keys are supported. A few handy references:

* [Adding a new SSH key to your GitHub account](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account) shows instructions on how to set up SSH keys on GitHub along with supported key types
* [Use SSH keys to communicate with GitLab](https://docs.gitlab.com/ee/user/ssh.html) shows instructions on how to set up SSH keys on GitLab along with supported key types

### Remote definition

Within the `remotes` block you can define as many remote repositories as you want, each in its own separate block. The `name` identifies the remote repository as it is returned by running [`git remote`](https://git-scm.com/docs/git-remote) locally. If you use a `name` that was already defined for a remote then you are **overriding** an existing remote configuration. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single remote.

Configuring remotes gives Nyx information about:

* the configured remotes
* the authentication method to use for remote repositories
* the credentials to use when performing operations to and from remote repositories

Each remote has the following attributes:

| Name                                                                | Type    | Command Line Option                                  | Environment Variable                                    | Default |
| ------------------------------------------------------------------- | ------- | ---------------------------------------------------- | ------------------------------------------------------- | ------- |
| [`git/remotes/<NAME>/authenticationMethod`](#authentication-method) | string  | `--git-remotes-<NAME>-authenticationMethod=<METHOD>` | `NYX_GIT_REMOTES_<NAME>_AUTHENTICATION_METHOD=<METHOD>` | N/A     |
| [`git/remotes/<NAME>/password`](#password)                          | string  | `--git-remotes-<NAME>-password=<TEMPLATE>`           | `NYX_GIT_REMOTES_<NAME>_PASSWORD=<TEMPLATE>`            | N/A     |
| [`git/remotes/<NAME>/user`](#user)                                  | string  | `--git-remotes-<NAME>-user=<TEMPLATE>`               | `NYX_GIT_REMOTES_<NAME>_USER=<TEMPLATE>`                | N/A     |
| [`git/remotes/<NAME>/privateKey`](#private-key)                     | string  | `--git-remotes-<NAME>-privateKey=<TEMPLATE>`         | `NYX_GIT_REMOTES_<NAME>_PRIVATE_KEY=<TEMPLATE>`         | N/A     |
| [`git/remotes/<NAME>/passphrase`](#passphrase)                      | string  | `--git-remotes-<NAME>-passphrase=<TEMPLATE>`         | `NYX_GIT_REMOTES_<NAME>_PASSPHRASE=<TEMPLATE>`          | N/A     |

#### Authentication method

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>/authenticationMethod`                                                |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>-authenticationMethod=<METHOD>`                                     |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>_AUTHENTICATION_METHOD=<METHOD>`                                  |
| Configuration File Option | `git/remotes/items/<NAME>/authenticationMethod`                                          |
| Related state attributes  |                                                                                          |

The authentication metod to use. Available values are `USER_PASSWORD` (for user name and password or token authentication, see [above](#using-tokens)) and `PUBLIC_KEY` (for [SSH authentication](#using-public-key-ssh)).

When not specified and at least one between the [user](#user) and [password](#password) is set, then `USER_PASSWORD` is assumed.

To use SSH keys, `PUBLIC_KEY` must be explicitly set. When using `PUBLIC_KEY` you can provide a [private key](#private-key) and an optional [passphrase](#passphrase) or, if you don't, public keys will be used from their standard locations (i.e. the `~/.ssh` folder) and if they require a passphrase Nyx can connect to the ssh-agent or Pageant (**experimental**).

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

This value is only considered when the [authentication method](#authentication-method) is `USER_PASSWORD` or is not set.

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

This value is only considered when the [authentication method](#authentication-method) is `USER_PASSWORD` or is not set.

#### Private key

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>/privateKey`                                                          |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>-privateKey=<TEMPLATE>`                                             |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>_PRIVATE_KEY=<TEMPLATE>`                                          |
| Configuration File Option | `git/remotes/items/<NAME>/privateKey`                                                    |
| Related state attributes  |                                                                                          |

The private key to use to connect to the remote repository using SSH authentication. Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read from a local file]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#filecontent).

This value is only considered when the [authentication method](#authentication-method) is `PUBLIC_KEY`. When [authentication method](#authentication-method) is `PUBLIC_KEY`, this value can pass a private key explicitly, otherwise, when not set, private keys will be loaded from their default locations (i.e. the `~/.ssh` folder).

#### Passphrase

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `git/remotes/<NAME>/passphrase`                                                          |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--git-remotes-<NAME>-passphrase=<TEMPLATE>`                                             |
| Environment Variable      | `NYX_GIT_REMOTES_<NAME>_PASSPHRASE=<TEMPLATE>`                                           |
| Configuration File Option | `git/remotes/items/<NAME>/passphrase`                                                    |
| Related state attributes  |                                                                                          |

The passphrase to decrypt the [private key](#private-key) to use to connect to the remote repository using SSH authentication. Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to [read from environment variables]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#environmentvariable).

This value is only considered when the [authentication method](#authentication-method) is `PUBLIC_KEY`. When [authentication method](#authentication-method) is `PUBLIC_KEY`, this value can pass a passphrase explicitly, otherwise, when not set, and in case the private key is passphrase-protected, Nyx will connect to the ssh-agent (or Pageant), if available.
