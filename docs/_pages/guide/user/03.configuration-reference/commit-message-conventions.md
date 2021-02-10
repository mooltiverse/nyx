---
title: Commit Message Conventions
layout: single
toc: true
permalink: /guide/user/configuration-reference/commit-message-conventions/
---

Commit message conventions are definitions that allow Nyx to fetch informations directly from the commit messages and infer other informations like the version number to bump.

[Conventions]({{ site.baseurl }}reference/commit-message-conventions/) are configured within the `commitMessageConventions` *section*. The section allows one sub-section for each convention and some overall options.

You can have as many conventions as you want. You can use [default ones]({{ site.baseurl }}/reference/commit-message-conventions/#preset-commit-message-conventions) that come bundled with Nyx, override them completely or for just a few attributes or define your own from scratch.

### Commit message conventions overall options

| Name                                             | Type   | Command Line Option                            | Environment Variable                             | Configuration File Option              | Default                                |
| ------------------------------------------------ | -------| ---------------------------------------------- | ------------------------------------------------ | -------------------------------------- | -------------------------------------- |
| [`enabled`](#enabled-commit-message-conventions) | list   | `--commit-message-conventions-enabled=<NAMES>` | `NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=<NAMES>` | `commitMessageConventions/enabled`     | `conventionalCommits,gitmoji`          |

#### Enabled commit message conventions

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `enabled`                                                                                |
| Type                      | list                                                                                     |
| Default                   | `conventionalCommits,gitmoji`                                                            |
| Command Line Option       | `--commit-message-conventions-enabled=<NAMES>`                                           |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=<NAMES>`                                         |
| Configuration File Option | `commitMessageConventions/enabled`                                                       |
| Related state attributes  |                                                                                          |

The comma separated list of commit message conventions that are enabled for the project. Here you can enable or disable the various conventions, either custom or default. By default all [preset conventions](#commit-message-convention-presets) are enabled.

Each item in the list must correspond to a convention `name` attribute. Each named convention must exist, but not all defined convention must be enabled here. Convention not listed here will just be ignored by Nyx as if they were not even defined.

The order in which convention are listed matters. The conventions listed first are evaluated first, so when evaluating a commit message, the first convention that matches the [`type`](#type-commit-message) is used.
{: .notice--info}

### Commit message convention definition

Within the `commitMessageConventions` block you can define as many conventions as you want, each in its own separate block. The `name` identifies the convention so to define a brand new convention make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a convention (given the [evaluation order](#evaluation-order)) then you are **overriding** an existing convention and when you do, you can override a single attribute or all of them.

Configuring conventions gives Nyx informations about:

* how to parse commit messages
* which version number to bump

Each convention has the following attributes:

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                         | Configuration File Option                      | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | ------------------------------------------------------------ | ---------------------------------------------- | ------------------------------------------ |
| [`body`](#body-commit-message)                 | string  | `--commit-message-conventions-<INDEX>-body=<REGEX>`        | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_BODY=<REGEX>`        | `commitMessageConventions/<INDEX>/body`        | N/A                                        |
| [`bump`](#body-commit-message)                 | string  | `--commit-message-conventions-<INDEX>-bump=<REGEX>`        | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_BUMP=<REGEX>`        | `commitMessageConventions/<INDEX>/bump`        | N/A                                        |
| [`description`](#description-commit-message)   | string  | `--commit-message-conventions-<INDEX>-description=<REGEX>` | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_DESCRIPTION=<REGEX>` | `commitMessageConventions/<INDEX>/description` | N/A                                        |
| [`field`](#field-commit-message)               | string  | `--commit-message-conventions-<INDEX>-field=<REGEX>`       | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_FIELD=<REGEX>`       | `commitMessageConventions/<INDEX>/field`       | N/A                                        |
| [`name`](#name-commit-message-convention)      | string  | `--commit-message-conventions-<INDEX>-name=<NAME>`         | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_NAME=<NAME>`         | `commitMessageConventions/<INDEX>/name`        | N/A                                        |
| [`scope`](#scope-commit-message)               | string  | `--commit-message-conventions-<INDEX>-scope=<REGEX>`       | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_SCOPE=<REGEX>`       | `commitMessageConventions/<INDEX>/scope`       | N/A                                        |
| [`type`](#type-commit-message)                 | string  | `--commit-message-conventions-<INDEX>-type=<REGEX>`        | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_TYPE=<REGEX>`        | `commitMessageConventions/<INDEX>/type`        | N/A                                        |

#### Body (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `body`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-body=<NAME>`                                       |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_BODY=<NAME>`                                     |
| Configuration File Option | `commitMessageConventions/<INDEX>/body`                                                  |
| Related state attributes  |                                                                                          |

A regular expression that matches the `body` value within the commit message. If the `body` message is not matched it's assumed that the message does not comply with the convention.

#### Bump (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `bump`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-bump=<NAME>`                                       |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_BUMP=<NAME>`                                     |
| Configuration File Option | `commitMessageConventions/<INDEX>/bump`                                                  |
| Related state attributes  |                                                                                          |

A regular expression that tells the version number identifier to dump. If this is left empty the convention does not support automatic bumping based on commit messages. Please note that the returned identifier name must comply with the [selected versioning scheme](#scheme).

#### Description (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `description`                                                                            |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-description=<NAME>`                                |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_DESCRIPTION=<NAME>`                              |
| Configuration File Option | `commitMessageConventions/<INDEX>/description`                                           |
| Related state attributes  |                                                                                          |

A regular expression that matches the `description` value within the commit message. If nothing is matched then the message is assumed to define no description. If this is left empty then the convention doesn't handle the `description` value.

#### Field (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `field`                                                                                  |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-field=<NAME>`                                      |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_FIELD=<NAME>`                                    |
| Configuration File Option | `commitMessageConventions/<INDEX>/field`                                                 |
| Related state attributes  |                                                                                          |

A regular expression that matches the `field`s within the commit message. If nothing is matched then the message is assumed to define no fields. If this is left empty then the convention doesn't handle fields.

#### Name (commit message convention)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `name`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--commit-message-conventions-<INDEX>-name=<NAME>`                                       |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_NAME=<NAME>`                                     |
| Configuration File Option | `commitMessageConventions/<INDEX>/name`                                                  |
| Related state attributes  |                                                                                          |

The short name that identifies this convention. This is also the value you can use in the [enabled commit message conventions](#enabled-commit-message-conventions).

This option is **mandatory**.

#### Type (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `type`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-type=<NAME>`                                       |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_TYPE=<NAME>`                                     |
| Configuration File Option | `commitMessageConventions/<INDEX>/type`                                                  |
| Related state attributes  |                                                                                          |

A regular expression that matches the `type` value within the commit message. If the `type` message is not matched it's assumed that the message does not comply with the convention.

This option is **mandatory**.

#### Scope (commit message)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `scope`                                                                                  |
| Type                      | string                                                                                   |
| Default                   | Empty                                                                                    |
| Command Line Option       | `--commit-message-conventions-<INDEX>-scope=<NAME>`                                      |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<INDEX>_SCOPE=<NAME>`                                    |
| Configuration File Option | `commitMessageConventions/<INDEX>/scope`                                                 |
| Related state attributes  |                                                                                          |

A regular expression that matches the `scope` value within the commit message. If nothing is matched then the message is assumed to define no scope. If this is left empty then the convention doesn't handle the `scope` value.

### Commit message convention presets

The following are the commit message convention presets that come pre-configured with Nyx. Remember you can override, [enable or disable](#enabled-commit-message-conventions) them at your need.

#### Conventional Commits

This element configures the [Conventional Commits]({{ site.baseurl }}/reference/commit-message-conventions/#conventional-commits) convention.

| Attribute                                        | Value                                                                      | Notes                                                                                                                 |
| ------------------------------------------------ | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`body`](#body)                                  | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`bump`](#bump-commit-message)                   | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`description`](#description-commit-message)     | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`field`](#field-commit-message)                 | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`name`](#name-commit-message-convention)        | `conventionalCommits`                                                      |                                                                                                                       |
| [`type`](#type-commit-message)                   | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`scope`](#scope-commit-message)                 | TODO: enter the regular expression here                                    |                                                                                                                       |

#### gitmoji

This element configures the [Gitmoji]({{ site.baseurl }}/reference/commit-message-conventions/#gitmoji) convention.

| Attribute                                        | Value                                                                      | Notes                                                                                                                 |
| ------------------------------------------------ | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| [`body`](#body)                                  | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`bump`](#bump-commit-message)                   | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`description`](#description-commit-message)     | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`field`](#field-commit-message)                 | TODO: enter the regular expression here                                    |                                                                                                                       |
| [`name`](#name-commit-message-convention)        | `conventionalCommits`                                                      |                                                                                                                       |
| [`type`](#type-commit-message)                   | `^:art|zap|fire|bug|ambulance|sparkles|rocket|lipstick|tada|white_check_mark|lock|bookmark|rotating_light|construction|green_heart|arrow_down|arrow_up|pushpin|construction_worker|chart_with_upwards_trend|recycle|heavy_plus_sign|heavy_minus_sign|wrench|hammer|globe_with_meridians|pencil2|poop|rewind|twisted_rightwards_arrows|package|alien|truck|page_facing_up|boom|bento|wheelchair|bulb|beers|speech_balloon|card_file_box|loud_sound|mute|busts_in_silhouette|children_crossing|building_construction|iphone|clown_face|egg|see_no_evil|camera_flash|alembic|mag|label|seedling|triangular_flag_on_post|goal_net|dizzy|wastebasket|passport_control|adhesive_bandage|monocle_face:` | |
| [`scope`](#scope-commit-message)                 | TODO: enter the regular expression here                                    |                                                                                                                       |
