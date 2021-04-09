---
title: Commit Message Conventions
layout: single
toc: true
permalink: /guide/user/configuration-reference/commit-message-conventions/
---

Commit message conventions are patterns used for Git commit messages widely used in software development. To name a few: [Conventional Commits](https://www.conventionalcommits.org/), [Angular](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#-commit-message-format), [Karma](http://karma-runner.github.io/4.0/dev/git-commit-msg.html), [jQuery](https://contribute.jquery.org/commits-and-pull-requests/#commit-guidelines), [JSHint](https://github.com/jshint/jshint/blob/master/CONTRIBUTING.md#commit-message-guidelines), [Ember](https://github.com/emberjs/ember.js/blob/master/CONTRIBUTING.md#commit-tagging), [gitmoji](https://gitmoji.dev/). While discussing these conventions is out of scope for this page, what is important here is that you can configure Nyx with any convention in order to fetch information directly from the commit messages (like, for example, the version number to bump after a certain commit).

Conventions are configured within the `commitMessageConventions` *section*. The section allows one sub-section for each convention and some overall options.

You can have as many conventions as you want. You can use [presets](TODO: link the preset conventions here) that come bundled with Nyx, override them or define your own from scratch.

### Commit message conventions overall options

| Name                                             | Type   | Command Line Option                            | Environment Variable                             | Configuration File Option              | Default                                |
| ------------------------------------------------ | -------| ---------------------------------------------- | ------------------------------------------------ | -------------------------------------- | -------------------------------------- |
| [`enabled`](#enabled)                            | list   | `--commit-message-conventions-enabled=<NAMES>` | `NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=<NAMES>` | `commitMessageConventions/enabled`     | No convention                          |

#### Enabled

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `enabled`                                                                                |
| Type                      | list                                                                                     |
| Default                   | No convention                                                                            |
| Command Line Option       | `--commit-message-conventions-enabled=<NAMES>`                                           |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_ENABLED=<NAMES>`                                         |
| Configuration File Option | `commitMessageConventions/enabled`                                                       |
| Related state attributes  |                                                                                          |

The comma separated list of commit message convention names that are enabled for the project. Here you can enable or disable the various conventions, either custom or default.

Each item in the list must correspond to a convention `name` attribute. Each named convention must exist, but not all defined convention must be enabled here. Convention not listed here will just be ignored by Nyx as if they were not even defined.

The order in which convention are listed matters. The conventions listed first are evaluated first, so when evaluating a commit message, the first convention that matches is used.
{: .notice--info}

### Commit message convention definition

Within the `commitMessageConventions` block you can define as many conventions as you want, each in its own separate block. The `name` identifies the convention so to define a brand new convention make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for a convention then you are **overriding** an existing convention. Depending on the [configuration method]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) you use the `name` property might be defined inside or outside the block that configures a single convention.

Configuring conventions gives Nyx informations about:

* which structured information to extract from commit messages
* which version number to bump according to the commit message contents

Each convention has the following attributes:

| Name                                           | Type    | Command Line Option                                         | Environment Variable                                           | Configuration File Option                         | Default                                    |
| ---------------------------------------------- | ------- | ----------------------------------------------------------- | -------------------------------------------------------------- | ------------------------------------------------- | ------------------------------------------ |
| [`expression`](#expression)                    | string  | `--commit-message-conventions-<NAME>-expression=<REGEX>`    | `NYX_COMMIT_MESSAGE_CONVENTIONS_<NAME>_EXPRESSION=<REGEX>`     | `commitMessageConventions/<NAME>/expression`      | N/A                                        |
| [`bumpExpressions`](#bump-expressions)         | map     | `--commit-message-conventions-<NAME>-bumpExpressions=<MAP>` | `NYX_COMMIT_MESSAGE_CONVENTIONS_<NAME>_BUMP_EXPRESSIONS=<MAP>` | `commitMessageConventions/<NAME>/bumpExpressions` | N/A                                        |

#### Expression

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `commitMessageConventions/expression`                                                    |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--commit-message-conventions-<NAME>-expression=<REGEX>`                                 |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<NAME>_EXPRESSION=<REGEX>`                               |
| Configuration File Option | `commitMessageConventions/expression`                                                    |
| Related state attributes  |                                                                                          |

The `expression` in a commit message convention is a regular expression that:

* is evaluated against the entire commit message
* needs to *match* the entire commit message in order to be selected
* uses a well defined set of *named capturing group* in order to return structured fields

The named capturing groups are:

* `type`: a short identifier of the type of changes brought by a commit
* `scope`: an optional short identifier of the scope of the commit contents
* `title`: a short, single line statement of the commit contents and purpose

All the capturing groups are optional as not all conventions support all of these fields. These groups are read by name when the regular expression is evaluated and their values are used as structured informations for various purposes by Nyx.

For example, the [Conventional Commits](https://www.conventionalcommits.org/) expression is `(?m)^(?<type>[a-zA-Z0-9_]+)(!)?(\((?<scope>[a-z ]+)\))?:( (?<title>.+))$(?s).*` and as you can see it has all the named capturing groups.

Commit messages are often made of multiple lines of text so, when authoring these expressions, consider using the *multi-line* and *single-line* flags (`(?m)` and `(?s)`, respectively).
{: .notice--info}

Use tools like [regular expressions 101](https://regex101.com/) to write and test your regular expressions.
{: .notice--info}

While conventions usually define a range of allowed values for `type` and `scope`, Nyx is permissive about them so you don't need to be specific on their values belonging to a well known range of values. For example, [Conventional Commits](https://www.conventionalcommits.org/) defines `feat` and `fix` as meaningful values for the commit `type` while other values are allowed (i.e. `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test` and others). Nyx doesn't make assumptions on these values (unless they're also used for the [`bumpExpressions`](#bump-expressions)) so, as you can see by the example expression above, any value can be used.

#### Bump expressions

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `commitMessageConventions/bumpExpressions`                                               |
| Type                      | map                                                                                      |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--commit-message-conventions-<NAME>-bumpExpressions=<MAP>`                              |
| Environment Variable      | `NYX_COMMIT_MESSAGE_CONVENTIONS_<NAME>_BUMP_EXPRESSIONS=<MAP>`                           |
| Configuration File Option | `commitMessageConventions/bumpExpressions`                                               |
| Related state attributes  | [releaseScope/significant]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant){: .btn .btn--info .btn--small} [scheme]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#scheme){: .btn .btn--info .btn--small} [version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#version){: .btn .btn--info .btn--small} |

The `bumpExpressions` map gives Nyx instructions about which version identifiers are expected to be bumped according to the commit message. This map can be empty for those conventions not addressing the version bumping.

When at least one commit in the release scope succesfully matches one of these expressions the release scope is considered to have [significant]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#significant) changes.

Each entry in this map is made of two strings: the name of a version identifier and a regular expression that, when matching the commit message, instructs Nyx to bump the identifier in the name of the map entry.

For example, the [Conventional Commits](https://www.conventionalcommits.org/) bump expressions are:

* `major` = `(?s)(?m)^[a-zA-Z0-9_]+(!|.*^(BREAKING( |-)CHANGE: )).*`
* `minor` = `(?s)(?m)^feat(?!!|.*^(BREAKING( |-)CHANGE: )).*`
* `patch` = `(?s)(?m)^fix(?!!|.*^(BREAKING( |-)CHANGE: )).*`

As you can see, when a commit message has an exlamation mark after the `type` or `BREAKING CHANGE: ` (or `BREAKING-CHANGE: `) appears in the footer, the `major` identifier is bumped, otherwise `minor` is bumped when the message `type` is `feat` or `patch` is bumped when the message `type` is `fix`. No bump is performed if none of these expressions is matched.

Bear in mind that:

* the identifier names must comply with the configured version [`scheme`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#scheme) (the above example is using [Semantic Versioning]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}#semantic-versioning-semver))
* each expression must match exactly one bump type and not others or the identifier that is dumped might be unpredictable
* capturing groups are ignored (named or not) and these expressions are just used to match or not a commit message