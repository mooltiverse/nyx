---
title: Substitutions
layout: single
toc: true
permalink: /guide/user/configuration-reference/substitutions/
---

Substitutions are used to replace arbitrary text tokens in any text file with static or dynamic contents.

Substitutions are configured with rules where each rule:

* defines the file paths where replacements must occur; file paths are defined as [globs](https://en.wikipedia.org/wiki/Glob_(programming)) so you can match multiple files with a single rule
* defines the token to be replaced using [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) to give you complete freedom on the selection
* defines the content to use when replacing the content as a static string or a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) so you can use any value from the internal [State]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %})

A common use case for substitutions is replacing the version attribute into platform-specific files so you can bridge the version detection and generation performed by Nyx with any language-specific artifact. For example, if you're woking on a Node project you can replace the value of the `version` attribute in the [`package.json`](https://docs.npmjs.com/cli/v9/configuring-npm/package-json#version) using the [`version`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md%}#version) attribute from the [State]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) to make sure the Node version is always in synch with Nyx.

Substitutions are configured within the `substitutions` *section*. The section allows one sub-section for each substitution and some overall options.

Substitutions are performed by the [Make]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#make) command.

You can have as many substitutions as you want. You can use [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) that come bundled with Nyx, override them or define your own from scratch.

### Substitutions overall options

| Name                                             | Type   | Command Line Option                            | Environment Variable                             | Default                                |
| ------------------------------------------------ | -------| ---------------------------------------------- | ------------------------------------------------ | -------------------------------------- |
| [`substitutions/enabled`](#enabled)              | list   | `--substitutions-enabled=<NAMES>`              | `NYX_SUBSTITUTIONS_ENABLED=<NAMES>`              | No substitution                        |

#### Enabled

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `substitutions/enabled`                                                                  |
| Type                      | list                                                                                     |
| Default                   | No substitution                                                                          |
| Command Line Option       | `--substitutions-enabled=<NAMES>`                                                        |
| Environment Variable      | `NYX_SUBSTITUTIONS_ENABLED=<NAMES>`                                                      |
| Configuration File Option | `substitutions/enabled`                                                                  |
| Related state attributes  |                                                                                          |

The comma separated list of substitution names that are enabled for the project. Here you can enable or disable the various substitutions, either custom or default.

Each item in the list must correspond to a substitution [`name`](#name) attribute. Each named substitution must exist, but not all defined substitutions must be enabled here. Substitutions not listed here will just be ignored by Nyx as if they were not even defined.

The order in which substitution are listed matters. The substitutions listed first are evaluated first, so consider this in case you are defining multiple substitutions that may replace the same secrions of a target file.
{: .notice--info}

### Substitution definition

Within the `substitutions` block you can define as many substitutions as you want, each in its own separate block. The `name` identifies the substitution so to define a brand new substitution make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a substitution then you are **overriding** an existing substitution. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single substitution.

Configuring substitution gives Nyx information about:

* which files to operate the substitutions on
* the text tokens to replace
* the content to use to replace the matching text tokens

Each substitution has the following attributes:

| Name                                                                   | Type    | Command Line Option                                         | Environment Variable                                           | Default                                    |
| ---------------------------------------------------------------------- | ------- | ----------------------------------------------------------- | -------------------------------------------------------------- | ------------------------------------------ |
| [`substitutions/<NAME>/files`](#files)                                 | string  | `--substitutions-<NAME>-files=<GLOB>`                       | `NYX_SUBSTITUTIONS_<NAME>_FILES=<GLOB>`                        | N/A                                        |
| [`substitutions/<NAME>/match`](#match)                                 | string  | `--substitutions-<NAME>-match=<REGEX>`                      | `NYX_SUBSTITUTIONS_<NAME>_MATCH=<REGEX>`                       | N/A                                        |
| [`substitutions/<NAME>/value`](#value)                                 | string  | `--substitutions-<NAME>-value=<TEMPLATE>`                   | `NYX_SUBSTITUTIONS_<NAME>_VALUE=<TEMPLATE>`                    | N/A                                        |

When using multiple [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) or customizing [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}), these values must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

#### Files

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `substitutions/<NAME>/files`                                                             |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--substitutions-<NAME>-files=<GLOB>`                                                    |
| Environment Variable      | `NYX_SUBSTITUTIONS_<NAME>_FILES=<GLOB>`                                                  |
| Configuration File Option | `substitutions/items/<NAME>/files`                                                       |
| Related state attributes  |                                                                                          |

The `files` is a glob that matches zero, one or multiple files to evaluate for substitutions. When relative paths are used they are interpreted as relative to the current working directory.

#### Match

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `substitutions/<NAME>/match`                                                             |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--substitutions-<NAME>-match=<REGEX>`                                                   |
| Environment Variable      | `NYX_SUBSTITUTIONS_<NAME>_MATCH=<REGEX>`                                                 |
| Configuration File Option | `substitutions/items/<NAME>/match`                                                       |
| Related state attributes  |                                                                                          |

The `match` regular expression defines the tokens to be replaced as a whole using the [`value`](#value) configured for the rule. This expression can match zero, one or more text tokens. When multiple tokens are matched they are all replaced.

Please note that the token is matched **as a whole** so, for example, if this expression matches `version: 1.2.3`, the [`value`](#value) must contain `{% raw %}version: {{version}}{% endraw %}`, not just `{% raw %}{{version}}{% endraw %}`, or the `version: ` part will disappear and subsequent substitutions won't work.
{: .notice--info}

Use tools like [regular expressions 101](https://regex101.com/) to write and test your regular expressions.
{: .notice--info}

#### Value

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `substitutions/<NAME>/value`                                                             |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--substitutions-<NAME>-value=<TEMPLATE>`                                                |
| Environment Variable      | `NYX_SUBSTITUTIONS_<NAME>_VALUE=<TEMPLATE>`                                              |
| Configuration File Option | `substitutions/items/<NAME>/value`                                                       |
| Related state attributes  | any                                                                                      |

The text to replace the [matched tokens](#match), if any.

Here you can pass a [template]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to generate this attribute dynamically at runtime.

#### Name

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `substitutions/<NAME>`                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--substitutions-<NAME>=<NAME>`                                                          |
| Environment Variable      | `NYX_SUBSTITUTIONS_<NAME>=<NAME>`                                                        |
| Configuration File Option | `substitutions/items/<NAME>`                                                             |
| Related state attributes  |                                                                                          |

The short name that identifies this substitution. This is also the value you can use in the [enabled](#enabled) substitutions. This is actually not a field to be set within a substitution section but instead the key of the map element.

This option is **mandatory**.
