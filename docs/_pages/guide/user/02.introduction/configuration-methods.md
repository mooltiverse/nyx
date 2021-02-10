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

## Evaluation order

Options are evaluated in the following reverse order:

1. command line option
2. environment variable
3. plugin settings ([Gradle](#gradle))
4. custom [configuration file](#supported-file-grammars) that can be set using the [`configurationFile`]({{ site.baseurl }}/configuration/global-options/#configuration-file) global option
5. standard [configuration file](#supported-file-grammars), in the order:
   1. `.nyx.yaml`
   2. `.nyx.json`
   3. `.nyx.properties`
6. custom [shared configuration file](#supported-file-grammars) that can be set using the [`sharedConfigurationFile`]({{ site.baseurl }}/configuration/global-options/#shared-configuration-file) global option
7. standard [shared configuration file](#supported-file-grammars), in the order:
   1. `.nyx-shared.yaml`
   2. `.nyx-shared.json`
   3. `.nyx-shared.properties`
8. [preset]({{ site.baseurl }}/configuration/global-options/#preset) configuration
9. default value

This means that command line options have priority over all others while default values are only taken into account if no other mean is used for a certain option.

Please note that Nyx searches configuration files using default names `.nyx.yaml`, `.nyx.json`, `.nyx.properties`, `.nyx-shared.yaml`, `.nyx-shared.json`, `.nyx-shared.properties` (pay attention to the leading `.`) so as long as you use one of these file names and the file is available in the current working directory you don't need to use the [`configurationFile`]({{ site.baseurl }}/configuration/global-options/#configuration-file) option. The extension of the file reflects the grammar the file must be authored with. If more than one default file is present, they will be evaluated following the above order (`.nyx.yaml` overrides `.nyx.json` which overrides `.nyx.properties`, while `.nyx-shared.yaml` overrides `.nyx-shared.json` which overrides `.nyx-shared.properties`).

As a rule of thumb, consider using default values and [presets]({{ site.baseurl }}/reference/standard-configurations/) before customizing. Defaults are provided for almost all available options and they are engineered to suit the majority of cases. Moreover, there are many entities already modelled as [presets]({{ site.baseurl }}/configuration/presets/) (commit message conventions, environments, extra identifiers, release types, services) that you can enable by just using the [`preset`]({{ site.baseurl }}/configuration/global-options/#preset) option and may spare you a lot of work.
{: .notice--success}

Since there are so many different ways to configure Nyx, knowing how options are evaluated in the end might be cumbersome. A quick way to see how the final configuration is resolved is by [serializing the state file]({{ site.baseurl }}/configuration/global-options/#state-file) and inspect the [`configuration`]({{ site.baseurl }}/state/global-attributes/) element
{: .notice--info}

## Command line options

When you need to pass a configuration option on the command line you can use the option name listed in the *Command Line Option* column available in the [configuration reference]({{ site.baseurl }}/configuration/) sections.

## Environment variables

When you need to pass a configuration option as an environment variable you can use the variable listed in the *Environment Variable* column available in the [configuration reference]({{ site.baseurl }}/configuration/) sections.

## Supported file grammars

Nyx uses a consistent naming for each option across the different configuration file types. All tables in the [configuration reference]({{ site.baseurl }}/configuration/) sections have a *Configuration File Option* column with the name of the given option that is valid for each grammar. However, due to the different grammars, these names must be translated into the specific syntax in use so consider the *Configuration File Option* value a symbolic name that has to be transposed.

Let's takle this by example and assume this options to be represented:

| **Configuration File Option**                              | Type    | Description                                                                                           |
| ---------------------------------------------------------- | ------- | ----------------------------------------------------------------------------------------------------- |
| `simpleOption`                                             | simple  | Options wit a simple value                                                                            |
| `section/simpleNestedOption`                               | simple  | Options wit a simple value, appearing inside a section (configuration block)                          |
| `listOption`                                               | list    | Option to define a list of simple values, like names and numbers                                      |
| `objectListOption`                                         | list    | Option to define a list of objects (that encloses other options)                                      |

The following sections all represent the same options and values.

### Properties

```properties
simpleOption=<VALUE>
section.simpleNestedOption=<VALUE>
listOption=<VALUE1>,<VALUE2>,<VALUEN>
objectListOption[0].name=<VALUE>
objectListOption[0].objectOption=<VALUE>
objectListOption[0].objectSetting=<VALUE>
objectListOption[1].name=<VALUE>
objectListOption[1].objectOption=<VALUE>
objectListOption[1].objectSetting=<VALUE>
objectListOption[2].name=<VALUE>
objectListOption[2].objectOption=<VALUE>
objectListOption[2].objectSetting=<VALUE>
```

The grammar definition is available at [https://en.wikipedia.org/wiki/.properties](https://en.wikipedia.org/wiki/.properties).

### YAML

```yaml
simpleOption: <VALUE>
section:
    simpleNestedOption: <VALUE>
listOption:
    - <VALUE1>
    - <VALUE2>
    - <VALUEN>
objectListOption:
    -
        name: <VALUE>
        objectOption: <VALUE>
        objectSetting: <VALUE>
    -
        name: <VALUE>
        objectOption: <VALUE>
        objectSetting: <VALUE>
    -
        name: <VALUE>
        objectOption: <VALUE>
        objectSetting: <VALUE>
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
    "objectListOption": [
        { "name": "<VALUE>", "objectOption": "<VALUE>", "objectSetting": "<VALUE>" },
        { "name": "<VALUE>", "objectOption": "<VALUE>", "objectSetting": "<VALUE>" },
        { "name": "<VALUE>", "objectOption": "<VALUE>", "objectSetting": "<VALUE>" }
    ]
}
```

The grammar definition is available at [https://www.json.org/](https://www.json.org/) and you can check the correctness of your JSON files at [https://jsonlint.com/](https://jsonlint.com/).

### Gradle

[Gradle build scripts](https://docs.gradle.org/current/userguide/writing_build_scripts.html) can be used to pass options to Nyx by mean of extension configuration. To use it just define the extension configuration section in your `build.gradle` script like:

```groovy
nyx {
    simpleOption = '<VALUE>'
    section {
        simpleNestedOption = '<VALUE>'
    }
    listOption = ['<VALUE1>', '<VALUE2>', '<VALUEN>']
    objectListOption {
        {
            name = "<VALUE>"
            objectOption = "<VALUE>"
            objectSetting = "<VALUE>"
        }
        {
            name = "<VALUE>"
            objectOption = "<VALUE>"
            objectSetting = "<VALUE>"
        }
        {
            name = "<VALUE>"
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

When passing objects belonging to a list we need to intruduce an identifier to refer to an element or another. This identifier can be a positive integer or, in all cases when list elements have a `name` attribute, the elemnt name. This identifier is represented in this guide as `<INDEX>`, `<NAME>` or shortly `<ID>`.

With JSON and YAML files the `<INDEX>` identifier is implicit so is `<ID>`.
{: .notice--info}

For example, using the same example as per [configuration files](#supported-file-grammars), object lists can be passed as command line options as:

```bash
--objectListOption-0-name=first
--objectListOption-0-objectOption=<VALUE>
--objectListOption-0-objectSetting=<VALUE>
--objectListOption-1-name=second
--objectListOption-1-objectOption=<VALUE>
--objectListOption-1-objectSetting=<VALUE>
--objectListOption-2-name=third
--objectListOption-2-objectOption=<VALUE>
--objectListOption-2-objectSetting=<VALUE>
```

or

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

which is equivalent.

The same goes for environment variables, so::

```bash
NYX_OBJECT_LIST_OPTION_0_NAME=first
NYX_OBJECT_LIST_OPTION_0_OBJECT_OPTION=<VALUE>
NYX_OBJECT_LIST_OPTION_0_OBJECT_SETTING_=<VALUE>
NYX_OBJECT_LIST_OPTION_1_NAME=second
NYX_OBJECT_LIST_OPTION_1_OBJECT_OPTION=<VALUE>
NYX_OBJECT_LIST_OPTION_1_OBJECT_SETTING=<VALUE>
NYX_OBJECT_LIST_OPTION_2_NAME=third
NYX_OBJECT_LIST_OPTION_2_OBJECT_OPTION_=<VALUE>
NYX_OBJECT_LIST_OPTION_2_OBJECT_SETTING=<VALUE>
```

and

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

are equivalent.

This might be misleading as commd line options and environment variables usually have fixed names but as you can see the `<INDEX>` expands into the option or variable name, making them dynamic.
{: .notice--warning}

For completeness, in case you wonder, the same applies to [`.properties`](#properties) files. Again:

```properties
objectListOption[0].name=first
objectListOption[0].objectOption=<VALUE>
objectListOption[0].objectSetting=<VALUE>
objectListOption[1].name=second
objectListOption[1].objectOption=<VALUE>
objectListOption[1].objectSetting=<VALUE>
objectListOption[2].name=third
objectListOption[2].objectOption=<VALUE>
objectListOption[2].objectSetting=<VALUE>
```

and

```properties
objectListOption[first].name=first
objectListOption[first].objectOption=<VALUE>
objectListOption[first].objectSetting=<VALUE>
objectListOption[second].name=second
objectListOption[second].objectOption=<VALUE>
objectListOption[second].objectSetting=<VALUE>
objectListOption[third].name=third
objectListOption[third].objectOption=<VALUE>
objectListOption[third].objectSetting=<VALUE>
```

yield to the same result.

### Important notes about lists and identifiers

* you can use both approaches (numeric index or name) in the same context of a configuration file, environment or command line which means you can pass some options using one method and others using the other
* when using numeric identifiers only numbers between `0` and `100` (included) are available for command line options and environment variables. You don't need to use contiguous numeric identifiers
* numeric identifiers do not represent priority between elements. When priority matters other options are used (usually there is one `enabled...` option where you select the items to be used and the order they are declared also gives the priority)
* when using names as identifiers they must be considered case sensitive. Moreover, item names must not contain whitespaces or characters other than alphanumeric
* when overriding a value it's highly recommended to use the element name instead of the numeric identifier in order to select the right element