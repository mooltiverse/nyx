---
title: Changelog
layout: single
toc: true
permalink: /guide/user/configuration-reference/changelog/
---

The `changelog` *section* is where you configure the (optional) changelog generation.

There is only one `changelog` *section* per configuration.

The range of commits included in the changelog is limited to those in the current [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}#commits). What appears in the changelog is the first line of the commit message and, optionally, some decorators that may have been configured here or added by a custom template.

The changelog is generated only when a [new version]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#new-version) has been [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer).

### Changelog options

| Name                                                 | Type    | Command Line Option                                                           | Environment Variable                             | Default                                |
| ---------------------------------------------------- | ------- | ----------------------------------------------------------------------------- | ------------------------------------------------ | -------------------------------------- |
| [`changelog/append`](#append)                        | string  | `--changelog-append=head|tail`                                                | `NYX_CHANGELOG_APPEND=head|tail`                 | N/A                                    |
| [`changelog/path`](#path)                            | string  | `--changelog-path=<PATH>`                                                     | `NYX_CHANGELOG_PATH=<PATH>`                      | N/A                                    |
| [`changelog/sections`](#sections)                    | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) | `--changelog-sections-<NAME>=<REGEX>` | `NYX_CHANGELOG_SECTIONS_<NAME>=<REGEX>` | N/A                                    |
| [`changelog/substitutions`](#substitutions)          | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) | `--changelog-substitutions-<REGEX>=<FORMAT_STRING>` | `NYX_CHANGELOG_SUBSTITUTIONS_<REGEX>=<FORMAT_STRING>` | N/A                                    |
| [`changelog/template`](#template)                    | string  | `--changelog-template=<PATH>`                                                 | `NYX_CHANGELOG_TEMPLATE=<PATH>`                  | N/A                                    |

#### Append

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/append`                                                                       |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-append=head|tail`                                                           |
| Environment Variable      | `NYX_CHANGELOG_APPEND=head|tail`                                                         |
| Configuration File Option | `changelog/append`                                                                       |
| Related state attributes  | [changelog]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/changelog.md %}){: .btn .btn--info .btn--small} |

If a changelog file already exists at the given [path](#path), this flag allow to append the contents for the new release to the existing content.

When this value is `head` new contents are added on top of the file, shifting the precedent contents down, otherwise when this is set to `tail` new contents are appended at the end of the file. In other words, use `head` to have a changelog with most recent releases on top of the file, or `tail` to have them at the end (natural ordering).

When this option is not set or is emptty the previous contents of the changelog file are overwitten.

#### Path

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/path`                                                                         |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-path=<PATH>`                                                                |
| Environment Variable      | `NYX_CHANGELOG_PATH=<PATH>`                                                              |
| Configuration File Option | `changelog/path`                                                                         |
| Related state attributes  | [changelog]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/changelog.md %}){: .btn .btn--info .btn--small} |

The absolute or relative path to a local file where the changelog is saved when generated. If a file already exists at the given location it is overwritten.

Setting this value also enables the changelog creation, which is to say, when this option is not defined no changelog is generated.

A common value used for this option is `CHANGELOG.md`.

#### Sections

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/sections`                                                                     |
| Type                      | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-sections-<NAME>=<REGEX>`                                                    |
| Environment Variable      | `NYX_CHANGELOG_SECTIONS_<NAME>=<REGEX>`                                                  |
| Configuration File Option | `changelog/sections`                                                                     |
| Related state attributes  | [changelog/releases/ID/sections]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/changelog.md %}#sections){: .btn .btn--info .btn--small} |

The `sections` option lets you define the sections that will appear inside a single release in the changelog, each one grouping changes of the same *type*. Each section is defined by a *Name*, which is the name of the section to show in the changelog, and a regular expression that matches zero or more commit *types*, as they are inferred by the [`expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) option of the [commit message convention]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) in use. If a commit *type* is not matched by any itemp in this map then it will not appear in the changelog, and this might be useful to skim all the unrelevant changes.

But let's resume the process to make things clearer:

1. the [Infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}#infer) task scans the commit history and, for each commit, it uses the configured [`expression`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}#expression) to determine various attributes of the commit based on its message
2. among the various attributes that are inferred, the commit `type` tells the type of changes contributed by the commit
3. when generating the changelog commits are grouped by their `type` so that attribute is used to qualify the commit
4. since the development team may want the layout of the changelog to be organized in a more readable manner, the map defined for this `sections` option allows to rename sections, sort them differently, and change the way commits are grouped by their `type`
5. if the commit `type` is not matched by any section it is ignored by the changelog generator

For example, when using the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) convention you get commit of these types: `fix`, `feat`, `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test` and more. But using [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) as the changelog standard you need to map the above commit types to: `Added`, `Changed`, `Deprecated`, `Removed`, `Fixed`, `Security`. This mapping is exactly what is done by the `sections` option here. A starting point may be:

* `Added` = `^feat$`
* `Fixed` = `^fix$`

With this example definition, all changes not bringing bug fixes or new features are ignored.

As you can guess the sections you define here strongly depend on the configured [commit message convention]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}).

When using multiple [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) or customizing [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}), these values must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

#### Substitutions

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/substitutions`                                                                |
| Type                      | [map]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#collections-of-objects) |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-substitutions-<REGEX>=<FORMAT_STRING>`                                      |
| Environment Variable      | `NYX_CHANGELOG_SUBSTITUTIONS_<REGEX>=<FORMAT_STRING>`                                    |
| Configuration File Option | `changelog/substitutions`                                                                |
| Related state attributes  |                                                                                          |

The optional `substitutions` map lets you define string replacements to make to the rendered document. These are usually the issue IDs (i.e. `#123`) to be replaced with links (i.e. `{% raw %}[#123](https://github.com/example/prj/issues/123){% endraw %}`) but they can be used for anything.

The replacement string must be coherent with the type of the rendered document. In the above example it's a Markdown link.
{: .notice--info}

Each entry in the map is made of a pair of strings: the entry name is a regular expression used to match the strings to be replaced, while the value is a *printf* format string used to generate the new string.

The format string can thereby use the `%s` placeholder to insert the value captured by the regular expression in the new string.

The regular expression needs to have at least one (named or unnamed) capture group. In case more than one capture group is present, only the first one is considered (and used for the format string parameters).

Both the regular expression and the format string are required for each entry.

For example, to replace all occurrences of numeric IDs starting with the `#` character (like issue IDs) with a link like `{% raw %}[#XXX](https://github.com/example/prj/issues/XXX){% endraw %}`, an entry can be:

`{% raw %}(?m)#([0-9]+)(?s){% endraw %}` = `{% raw %}[#%s](https://github.com/example/prj/issues/%s){% endraw %}`

So if the changelog contains the `#123` string it will be replaced with `{% raw %}[#123](https://github.com/example/prj/issues/123){% endraw %}`.

As you can see by the example, the `%s` placeholder is used twice in the format string but the same value `123` is used for both replacements.

Note that all occurrences are treated as strings, so the `%s` placeholder is the only one allowed (no numbers, dates etc), which means that even numbers will be treated as strings.

When using multiple [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) or customizing [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}), these values must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

#### Template

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `changelog/template`                                                                     |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--changelog-template=<PATH>`                                                            |
| Environment Variable      | `NYX_CHANGELOG_TEMPLATE=<PATH>`                                                          |
| Configuration File Option | `changelog/template`                                                                     |
| Related state attributes  |                                                                                          |

The absolute or relative path to a local file or an URL to load a remote file to use as a template instead of the Nyx built-in. The file must contain a valid [Handlebars](https://handlebarsjs.com/) template ([Mustache](https://mustache.github.io/) templates are also supported). Template [functions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}#functions) can be used in custom templates.

If you need to know the object model available when customizing a see [this reference]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/changelog.md %}#functions)

You can find the default template [here](https://raw.githubusercontent.com/mooltiverse/nyx/main/modules/java/main/src/main/resources/changelog.tpl){:target="_blank"}.
