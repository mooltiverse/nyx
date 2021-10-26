---
title: Configuration Methods
layout: single
toc: true
permalink: /guide/user/introduction/configuration-methods/
---

Nyx offers a wide range of configuration options, including:

* one or more configuration files, available in [several grammars](#supported-file-grammars)
* command line options
* environment variables
* a mix of the above, following a clear [evaluation order](#evaluation-order)

All configuration options are available for any of the above methods to let you pick what best suits your needs.

[Examples]({{ site.baseurl }}/examples/) are available for any option and file grammar.

## Evaluation order

Options are evaluated in the following order:

1. command line option
2. environment variable
3. plugin settings ([Gradle](#gradle))
4. custom [configuration file](#supported-file-grammars) that can be set using the [`configurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file) global option
5. standard [configuration file](#supported-file-grammars), in the order:
   1. `.nyx.json`
   2. `.nyx.yaml` (or `.nyx.yml`)
6. custom [shared configuration file](#supported-file-grammars) that can be set using the [`sharedConfigurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#shared-configuration-file) global option
7. standard [shared configuration file](#supported-file-grammars), in the order:
   1. `.nyx-shared.json`
   2. `.nyx-shared.yaml` (or `.nyx-shared.yml`)
8. [preset]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) configuration
9. default value

This means that command line options have priority over all others while default values are only taken into account if no other mean is used for a certain option.

Please note that Nyx searches configuration files at standard locations using default names `.nyx.yaml` (or `.nyx.yml`), `.nyx.json`, `.nyx-shared.yaml` (or `.nyx-shared.yml`), `.nyx-shared.json` (pay attention to the leading `.`) so as long as you use one of these file names and the file is available in the current working directory you don't need to use the [`configurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file) option. The file extension reflects the grammar the file must be authored with and if none is available among the ones listed above JSON is assumed by default. If more than one default file is present, they will be evaluated following the above order (`.nyx.yaml` takes precedence over `.nyx.json`, while `.nyx-shared.yaml` takes precedence over `.nyx-shared.json`).

As a rule of thumb, consider using default values and [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) before customizing. Defaults are provided for almost all available options and they are engineered to suit the majority of cases. Moreover, there are many entities already modelled as presets (commit message conventions, environments, extra identifiers, release types, services) that you can enable by just using the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) option and may spare you a lot of work.
{: .notice--success}

Since there are so many different ways to configure Nyx, knowing how options are evaluated in the end might be cumbersome. A quick way to see how the final configuration is resolved is by [serializing the state file]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) and inspect the [`configuration`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#configuration) element.
{: .notice--info}

## Command line options

When you need to pass a configuration option on the command line you can use the option name listed in the *Command Line Option* column available in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) sections.

## Environment variables

When you need to pass a configuration option as an environment variable you can use the variable listed in the *Environment Variable* column available in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) sections.

## Supported file grammars

### YAML

```yaml
simpleOption: <VALUE>
section:
    simpleNestedOption: <VALUE>
listOption:
    - <VALUE1>
    - <VALUE2>
    - <VALUEN>
mapOption:
    <NAME>: "<VALUE>"
    <NAME>: "<VALUE>"
    <NAME>: "<VALUE>"
```

The grammar definition is available at [https://yaml.org/](https://yaml.org/) and you can check the correctness of your YAML files at [http://www.yamllint.com/](http://www.yamllint.com/).

### JSON

```json
{
    "simpleOption": "<VALUE>",
    "section": {
        "simpleNestedOption": "<VALUE>"
    },
    "listOption": ["<VALUE1>", "<VALUE2>", "<VALUEN>"],
    "mapOption" : {
        "<NAME>" : "<VALUE>",
        "<NAME>" : "<VALUE>",
        "<NAME>" : "<VALUE>"
    }
}
```

The grammar definition is available at [https://www.json.org/](https://www.json.org/) and you can check the correctness of your JSON files at [https://jsonlint.com/](https://jsonlint.com/).

### Gradle

[Gradle build scripts](https://docs.gradle.org/current/userguide/writing_build_scripts.html) can be used to pass options to Nyx by mean of extension configuration. To use it just define the extension configuration section in your `settings.gradle` or `build.gradle` script like:

```groovy
nyx {
    simpleOption = '<VALUE>'
    section {
        simpleNestedOption = '<VALUE>'
    }
    listOption = ['<VALUE1>', '<VALUE2>', '<VALUEN>']
    mapOption {
        "<NAME>" {
            objectOption = "<VALUE>"
            objectSetting = "<VALUE>"
        }
        "<NAME>" {
            objectOption = "<VALUE>"
            objectSetting = "<VALUE>"
        }
        "<NAME>" {
            objectOption = "<VALUE>"
            objectSetting = "<VALUE>"
        }
    }
}
```

As you can see the entire Nyx configuration is enclosed within the `nyx` block to isolate it from other options.

Gradle build scripts can be authored using the [Groovy](https://groovy-lang.org/syntax.html) or the [Kotlin](https://kotlinlang.org/docs/reference/grammar.html) grammar.

## List and objects

Passing configuration options as list and objects is fairly intuitive when using configuration files, as already [shown](#supported-file-grammars). On the other hand, passing complex values as command line options or environment variables needs a clear convention.

When passing objects belonging to a list we need to intruduce an identifier to refer to an element or another. This identifier can be a positive integer or, in all cases when list elements have a `name` attribute, the elemnt name. This identifier is represented in this guide as `<NAME>` or shortly `<ID>`.

For example, using the same example as per [configuration files](#supported-file-grammars), object lists can be passed as command line options as:

```bash
--objectListOption-first-name=first
--objectListOption-first-objectOption=<VALUE>
--objectListOption-first-objectSetting=<VALUE>
--objectListOption-second-name=second
--objectListOption-second-objectOption=<VALUE>
--objectListOption-second-objectSetting=<VALUE>
--objectListOption-third-name=third
--objectListOption-third-objectOption=<VALUE>
--objectListOption-third-objectSetting=<VALUE>
```

The same goes for environment variables, so:

```bash
NYX_OBJECT_LIST_OPTION_first_NAME=first
NYX_OBJECT_LIST_OPTION_first_OBJECT_OPTION=<VALUE>
NYX_OBJECT_LIST_OPTION_first_OBJECT_SETTING_=<VALUE>
NYX_OBJECT_LIST_OPTION_second_NAME=second
NYX_OBJECT_LIST_OPTION_second_OBJECT_OPTION=<VALUE>
NYX_OBJECT_LIST_OPTION_second_OBJECT_SETTING=<VALUE>
NYX_OBJECT_LIST_OPTION_third_NAME=third
NYX_OBJECT_LIST_OPTION_third_OBJECT_OPTION_=<VALUE>
NYX_OBJECT_LIST_OPTION_third_OBJECT_SETTING=<VALUE>
```

Names in identifiers must be considered case sensitive. Moreover, item names must not contain whitespaces or characters other than alphanumeric.
