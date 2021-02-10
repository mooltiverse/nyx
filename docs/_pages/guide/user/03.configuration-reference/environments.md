---
title: Environments
layout: single
toc: true
permalink: /guide/user/configuration-reference/environments/
---

Environments are definitions that allow conditional settings to be used depending on *where* Nyx is running.

Environments are configured within the `environments` *section*. The section allows one sub-section for each defined environment and some overall options.

You can have as many environments as you want. You can use [default ones](#default-environments) that come bundled with Nyx, override them completely or for just a few attributes or define your own from scratch.

Environments have little or nothing to deal with the [configured services](#services) although some names like `github` or `gitlab` match. The *environment* is where Nyx is running while multiple *services* might be used for publishing releases. In other words, when names match between *environments* and *services* it's because the same platform offers both CI/CD and Git services.
{: .notice--info}

### Environments overall options

| Name                                             | Type   | Command Line Option                    | Environment Variable                | Configuration File Option              | Default                                |
| ------------------------------------------------ | -------| -------------------------------------- | ----------------------------------- | -------------------------------------- | -------------------------------------- |
| [`enabled`](#enabled-environments)               | list   | `--environments-enabled=<NAMES>`       | `NYX_ENVIRONMENTS_ENABLED=<NAMES>`  | `environments/enabled`                 | `github,gitlab,bamboo,circleci,jenkins,teamcity,server,local` |

#### Enabled environments

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `enabled`                                                                                |
| Type                      | list                                                                                     |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--environments-enabled=<NAMES>`                                                         |
| Environment Variable      | `NYX_ENVIRONMENTS_ENABLED=<NAMES>`                                                       |
| Configuration File Option | `environments/enabled`                                                                   |
| Related state attributes  | [`environment`]({{ site.baseurl }}/reference/state/#environment-attributes)              |

The comma separated list of environments that are enabled for the project. Here you can enable or disable the various environments, either custom or default.

Each item in the list must correspond to an environment `name`. Each named environment must exist, but not all defined environments must be enabled here. Environments not listed here will just be ignored by Nyx as if they were not even defined.

The order in which environments are listed matters. The environments listed first are evaluated first, so in case of ambiguous matches the order disambiguates.
{: .notice--info}

### Environment definition

Within the `environments` block you can define as many elements as you want, each in its own separate block. The `name` identifies the environment so to define a brand new environment make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for an environment (given the [evaluation order](#evaluation-order)) then you are **overriding** an existing environment and when you do, you can override a single attribute or all of them.

Configuring environments gives Nyx informations about:

* how to detect when running on a certain environment based on a certain set of facts that are evaluated at runtime. The rule defining how to detect the environment is [Auto Detect](#auto-detect).
* the attributes and flags to set for each environment, when detected, so they can be used somewhere else, like in [templates]({{ site.baseurl }}/reference/templates/)

Each environment definition has the following attributes:

| Name                                    | Type    | Command Line Option                                | Environment Variable                                 | Configuration File Option              | Default |
| --------------------------------------- | ------- | -------------------------------------------------- | ---------------------------------------------------- | -------------------------------------- | --------|
| [`autoDetect`](#auto-detect)            | boolean | `--environments-<INDEX>-auto-detect=<EXPR>`        | `NYX_ENVIRONMENTS_<INDEX>_AUTO_DETECT=<EXPR>`        | `environments/<INDEX>/autoDetect`      | `false` |
| [`default`](#default)                   | boolean | `--environments-<INDEX>-default=true|false`        | `NYX_ENVIRONMENTS_<INDEX>_DEFAULT=true|false`        | `environments/<INDEX>/default`         | `false` |
| [`local`](#local)                       | boolean | `--environments-<INDEX>-local=<EXPR>`              | `NYX_ENVIRONMENTS_<INDEX>_LOCAL=<EXPR>`              | `environments/<INDEX>/local`           | `true`  |
| [`name`](#name)                         | string  | `--environments-<INDEX>-name=<NAME>`               | `NYX_ENVIRONMENTS_<INDEX>_NAME=<NAME>`               | `environments/<INDEX>/name`            | N/A     |
| [`server`](#server)                     | boolean | `--environments-<INDEX>-server=<EXPR>`             | `NYX_ENVIRONMENTS_<INDEX>_SERVER=<EXPR>`             | `environments/<INDEX>/server`          | `false` |
| [`user`](#user)                         | string  | `--environments-<INDEX>-user=<EXPR>`               | `NYX_ENVIRONMENTS_<INDEX>_USER=<EXPR>`               | `environments/<INDEX>/user`            | N/A     |

#### Auto detect

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `autoDetect`                                                                             |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--environments-<INDEX>-auto-detect=<EXPR>`                                              |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_AUTO_DETECT=<EXPR>`                                            |
| Configuration File Option | `environments/<INDEX>/autoDetect`                                                        |
| Related state attributes  |                                                                                          |

Here you can define the [expression]({{ site.baseurl }}/reference/expressions/) that is evaluated at runtime and, when evaluates `true`, enables the environment.

If you set this to `false` (the default) auto detection is disabled and this means the environment can be used only when:

* it's flagged as the [default](#default) environment and no other environment has been detected before this
* it's forced to be used by the [environment](#environment) global option

Setting this option `true` is considered a mistake as the environment will **always** be positively detected by Nyx. This may not be an issue if this is the [last environment enabled](#enabled-environments) as others can be detected before this. As a rule of thumb, remember that an environment with this option set to `true` prevents all the next items in the [list of enabled environments](#enabled-environments) to be ever evaluated.
{: .notice--warning}

This option is effective when you use a dynamic [expression]({{ site.baseurl }}/reference/expressions/) here in order to check for a *fact* in the actual runtime environment. Examples:

* `{% raw %}${ environment.hasVariable('CI') }{% endraw %}` will positively detect the environment when an environment variable named `CI` has been defined in the current environment (which happens in many CI/CD platforms)
* `{% raw %}${ file.exists('/etc/acme.conf') }{% endraw %}` will positively detect the environment when the `/etc/acme.conf` file is found, which might be a configuration file available on certain environments only

#### Default

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `default`                                                                                |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--environments-<INDEX>-default=true|false`                                              |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_DEFAULT=true|false`                                            |
| Configuration File Option | `environments/<INDEX>/default`                                                           |
| Related state attributes  |                                                                                          |

If `true` marks this environment as the default one when [auto detection](#auto-detect) can't select an environment based on the configured rules.

It's always a good idea to have one (and only one) default environment configured to be used as a last resort and it should be the *safest* choice.

When multiple environments are marked as *default* then the first one in the list of [enabled environments](#enabled-environments) will be used.

#### Local

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `local`                                                                                  |
| Type                      | boolean                                                                                  |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--environments-<INDEX>-local=<EXPR>`                                                    |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_LOCAL=<EXPR>`                                                  |
| Configuration File Option | `environments/<INDEX>/local`                                                             |
| Related state attributes  | [`local`]({{ site.baseurl }}/reference/state/#environment-attributes)                    |

A flag indicating if this environment is local (i.e. a developer workstation) instead of a server (like a CI/CD server). Default is `true` as it's the safest option.

This can also be an [expression]({{ site.baseurl }}/reference/expressions/) to be evaluated at run time and needs to return a boolean value.

This value should always evaluate as the opposite of [server](#server).

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `name`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--environments-<INDEX>-name=<NAME>`                                                     |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_NAME=<NAME>`                                                   |
| Configuration File Option | `environments/<INDEX>/name`                                                              |
| Related state attributes  | [`environment`]({{ site.baseurl }}/reference/state/#environment-attributes)              |

The short name that identifies this environment definition. This is also the value you can use in the [Enabled environments](#enabled-environments).

This option is **mandatory**.

Do not use `auto` for the environment name as it overlaps with the `auto` keyword used in the [`environment`](#environment) global option.
{: .notice--warning}

#### Server

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `server`                                                                                 |
| Type                      | boolean                                                                                  |
| Default                   | `false`                                                                                  |
| Command Line Option       | `--environments-<INDEX>-server=<EXPR>`                                                   |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_SERVER=<EXPR>`                                                 |
| Configuration File Option | `environments/<INDEX>/server`                                                            |
| Related state attributes  | [`server`]({{ site.baseurl }}/reference/state/#environment-attributes)                   |

A flag indicating if this environment is a server (like a CI/CD server) instead of a local environment (i.e. a developer workstation). Default is `false` as it's the safest option.

This can also be an [expression]({{ site.baseurl }}/reference/expressions/) to be evaluated at run time and needs to return a boolean value.

This value should always evaluate as the opposite of [local](#local).

#### User

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `user`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--environments-<INDEX>-user=<EXPR>`                                                     |
| Environment Variable      | `NYX_ENVIRONMENTS_<INDEX>_USER=<EXPR>`                                                   |
| Configuration File Option | `environments/<INDEX>/user`                                                              |
| Related state attributes  | [`user`]({{ site.baseurl }}/reference/state/#global-attributes)                          |

The name of the user (may be a service user) that runs the process. This can be an [expression]({{ site.baseurl }}/reference/expressions/) to be evaluated at run time (i.e. to fetch an environment variable value) and needs to return a string value.

Some environments may not expose this value so consider hardcoding a value in case.

##### Authoritative vs non authoritative environments

While Nyx can run and create releases on all platforms using the same [build scripts]({{ site.baseurl }}/best-practice/automation/), attention must be paid to which environments are allowed to issue official releases and which are not. Only centralized environments (like CI/CD platforms) with controlled access should be considered *authoritative* as authorized to issue official releases while all others (like developer workstations) should only be allowed to create [internal](#internal) releases, not issued to the public.

The `server` flag, controlling the [`server` state attribute]({{ site.baseurl }}/reference/state/), can be used to distinguish authoritative environments to those that are not.

This is especially useful when configuring [release types](#release-types) in a way that is able to make different releases (with different identifiers in their names and different tasks enabled) based on the current environment.
