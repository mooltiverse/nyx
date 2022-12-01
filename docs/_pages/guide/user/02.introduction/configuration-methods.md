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

This means that command line options have priority over all others while default values are only taken into account if no other means is used for a certain option.

Please note that Nyx searches configuration files at standard locations using default names `.nyx.yaml` (or `.nyx.yml`), `.nyx.json`, `.nyx-shared.yaml` (or `.nyx-shared.yml`), `.nyx-shared.json` (pay attention to the leading `.`) so as long as you use one of these file names and the file is available in the current working directory you don't need to use the [`configurationFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#configuration-file) option. The file extension reflects the grammar the file must be authored with and if none is available among the ones listed above JSON is assumed by default. If more than one default file is present, they will be evaluated following the above order (`.nyx.yaml` takes precedence over `.nyx.json`, while `.nyx-shared.yaml` takes precedence over `.nyx-shared.json`).

As a rule of thumb, consider using default values and [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) before customizing. Defaults are provided for almost all available options and they are engineered to suit the majority of cases. Moreover, there are many entities already modelled as presets (commit message conventions, environments, extra identifiers, release types, services) that you can enable by just using the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) option and may spare you a lot of work.
{: .notice--success}

Since there are so many different ways to configure Nyx, knowing how options are evaluated in the end might be cumbersome. A quick way to see how the final configuration is resolved is by [serializing the state file]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) and inspect the [`configuration`]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#configuration) element.
{: .notice--info}

When using multiple configuration methods or customizing [presets]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}), complex configuration options (like `commitMessageConventions`, `releaseTypes`, `services`, for example) must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

## Command line options

When you need to pass a configuration option on the command line you can use the option name listed in the *Command Line Option* column available in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) sections.

Some options are available in two forms, the regular one and the short one. For example, [`bump`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#bump) can be passed both as `--bump=<NAME>` or `-b=<NAME>`. In case both are used, the short one has priority.

Other options act as boolean flags so you can explicitly pass a value for them or just set the name and `true` is assumed. For example, if you want to pass [`dryRun`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#dry-run) you can either pass `--dry-run=true` or simply `--dry-run` and they will have the same effect. If you want to pass `false` you need to pass the value as `--dry-run=false`.

In order to support dynamic argument names, unsupported or malformed arguments are ignored and don't cause any error.
{: .notice--info}

## Environment variables

When you need to pass a configuration option as an environment variable you can use the variable listed in the *Environment Variable* column available in the [configuration reference]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) sections.

## Supported file grammars

### YAML

```yaml
simpleOption: "<VALUE>"
section:
    simpleNestedOption: "<VALUE>"
listOption:
    - "<VALUE1>"
    - "<VALUE2>"
    - "<VALUEN>"
objectsListOption:
    -
        option1: "<VALUE1>"
        option2: "<VALUE1>"
    -
        option1: "<VALUE2>"
        option2: "<VALUE2>"
    -
        option1: "<VALUE2>"
        option2: "<VALUE2>"
mapOption:
    <NAME1>: "<VALUE>"
    <NAME2>: "<VALUE>"
    <NAME3>: "<VALUE>"
objectsMapOption:
    <NAME1>:
        option1: "<VALUE2>"
        option2: "<VALUE2>"
    <NAME2>:
        option1: "<VALUE2>"
        option2: "<VALUE2>"
    <NAME3>:
        option1: "<VALUE2>"
        option2: "<VALUE2>"
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
    "objectsListOption": [
        {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        },
        {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        },
        {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        }
    ],
    "mapOption" : {
        "<NAME1>" : "<VALUE>",
        "<NAME2>" : "<VALUE>",
        "<NAME3>" : "<VALUE>"
    },
    "objectsMapOption" : {
        "<NAME1>" : {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        },
        "<NAME2>" : {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        },
        "<NAME3>" : {
            "option1": "<VALUE1>",
            "option2": "<VALUE2>"
        }
    }
}
```

The grammar definition is available at [https://www.json.org/](https://www.json.org/) and you can check the correctness of your JSON files at [https://jsonlint.com/](https://jsonlint.com/).

### Gradle

[Gradle build scripts](https://docs.gradle.org/current/userguide/writing_build_scripts.html) can be used to pass options to Nyx by means of extension configuration. To use it just define the extension configuration section in your `settings.gradle` or `build.gradle` script like:

```groovy
nyx {
    simpleOption = '<VALUE>'
    section {
        simpleNestedOption = '<VALUE>'
    }
    listOption = ['<VALUE1>', '<VALUE2>', '<VALUEN>']
    objectsListOption {
        "<NAME1>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
        "<NAME2>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
        "<NAME3>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
    }
    mapOption {
        "<NAME1>" = "<VALUE>"
        "<NAME2>" = "<VALUE>"
        "<NAME3>" = "<VALUE>"
    }
    objectsMapOption {
        "<NAME1>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
        "<NAME2>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
        "<NAME3>" {
            option1 = "<VALUE>"
            option2 = "<VALUE>"
        }
    }
}
```

As you can see the entire Nyx configuration is enclosed within the `nyx` block to isolate it from other options.

Please note that object lists in Gradle DSL are defined just like object maps. Which means you need to give each item a name, even if the object shouldn't. This is due to a limitation reported [here](https://github.com/mooltiverse/nyx/issues/77). As a rule of thumb, use the string representation (enclosed in quotes) of the item ordinal for the item name to make sure the order of items is safe.
{: .notice--info}

Gradle build scripts can be authored using the [Groovy](https://groovy-lang.org/syntax.html) or the [Kotlin](https://kotlinlang.org/docs/reference/grammar.html) grammar.

## Collections of objects

Passing configuration options as maps of objects is fairly intuitive when using configuration files, as already [shown](#supported-file-grammars). On the other hand, passing complex values as command line options or environment variables needs a clear convention.

When passing objects belonging to a list or map we need to intruduce an identifier to refer to an element or another. This identifier can be a simple string, also used as the `name` attribute. This identifier is represented in this guide as `<NAME>` (for maps), `<#>` (for lists) or `<ID>`.

For example, using the same example as per [configuration files](#supported-file-grammars), object maps can be passed as command line options as:

```bash
--objectMapOption-first-objectOption1=<VALUE>
--objectMapOption-first-objectOption2=<VALUE>
--objectMapOption-second-objectOption1=<VALUE>
--objectMapOption-second-objectOption2=<VALUE>
--objectMapOption-third-objectOption1=<VALUE>
--objectMapOption-third-objectOption2=<VALUE>
```

The same goes for environment variables, so:

```bash
NYX_OBJECT_MAP_OPTION_first_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_MAP_OPTION_first_OBJECT_OPTION2=<VALUE>
NYX_OBJECT_MAP_OPTION_second_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_MAP_OPTION_second_OBJECT_OPTION2=<VALUE>
NYX_OBJECT_MAP_OPTION_third_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_MAP_OPTION_third_OBJECT_OPTION2=<VALUE>
```

Names in identifiers must be considered case sensitive. Moreover, item names must not contain whitespaces or characters other than alphanumeric.

Object lists can be passed as command line options as:

```bash
--objectListOption-1-objectOption1=<VALUE>
--objectListOption-1-objectOption2=<VALUE>
--objectListOption-2-objectOption1=<VALUE>
--objectListOption-2-objectOption2=<VALUE>
--objectListOption-3-objectOption1=<VALUE>
--objectListOption-3-objectOption2=<VALUE>
```

The same goes for environment variables, so:

```bash
NYX_OBJECT_LIST_OPTION_1_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_LIST_OPTION_1_OBJECT_OPTION2=<VALUE>
NYX_OBJECT_LIST_OPTION_2_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_LIST_OPTION_2_OBJECT_OPTION2=<VALUE>
NYX_OBJECT_LIST_OPTION_2_OBJECT_OPTION1=<VALUE>
NYX_OBJECT_LIST_OPTION_2_OBJECT_OPTION2=<VALUE>
```

Ordinals can be positive integers only and must not contain whitespaces or characters other than numeric. The ordinal used determines the order of elements. The first ordinal is `0`.
