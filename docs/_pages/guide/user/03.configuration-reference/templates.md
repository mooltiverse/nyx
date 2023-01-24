---
title: Templates
layout: single
toc: true
permalink: /guide/user/configuration-reference/templates/
---

Templates can be used in several places to make configuration dynamic or parametrize text outputs, be them strings, messages or even whole text files.

Nyx uses [Handlebars](https://handlebarsjs.com/) templates passing the engine the [internal state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) for the template scope (the template input value) so it's easy to figure out which values are available. Moreover a few [functions](#functions) (lambdas) are available for common needs.

Handlebars is also compatible with [Mustache](https://mustache.github.io/) templates so you can reuse them.
{: .notice--info}

Tip: when writing templates you may find useful to [serialize the state to a file]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) for reference or troubleshooting.
{: .notice--info}

## Reference

### Render a static output

At times you just need to output a simple hardcoded string instead of rendering even the simplest template. Don't even bother with `{% raw %}{{ }}{% endraw %}` delimiters in this case and simply enter the value.

This means that wherever templates are allowed, you can still use static text, with no templates.

Let's assume you want to set the `Hello World` value for a configuration `option`, then your template is just the plain `Hello World`:

```
option = "Hello World"
```

### Render a simple string

Often times you just need to use a simple value from the [Nyx state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) to be the result of the template.

This is fairly simple as you just need to wrap the name of the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) attribute with double curly braces, like `{% raw %}{{attribute}}{% endraw %}`. This is the simplest form of a [Handlebars expression](https://handlebarsjs.com/guide/#simple-expressions) (and yes, the double curly brances are the *handlebars*, or *mustaches*).

So if we want to set our configuration `option` to the current value of the `version` attribute from the [state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) our definition will look like:

```
option = "{% raw %}{{version}}{% endraw %}"
```

When the value you need to render is nested into another (i.e. `section/attribute` or `section.attribute`, depending on the notation), the template looks like:

```
option = "{% raw %}{{#section}}{{attribute}}{{/section}}{% endraw %}"
```

As you can see this is a little cumbersome for a simple value but you will soon understand how powerful it can be. What is important to note here is that the `{% raw %}{{attribute}}{% endraw %}` expression is now enclosed between a `{% raw %}{{#section}}{% endraw %}` opening tag and a `{% raw %}{{/section}}{% endraw %}` closing tag (denoted by the `#` and `/` characters).

Alternatively you can simply use the dotted notation, like:

```
option = "{% raw %}{{section.attribute}}{% endraw %}"
```

### Type conversions

All templates return text values but ofter times the template output needs to be converted to other types. This may happen for configuration options, for example, when a dynamically computed value needs a boolean or a number. You can always tell if some conversion is to be performed by the *type* of the option.

This table gives you the overall rules used to convert text values to types other than strings:

| Expected type    | Translation criteria                                                                                          |
| -----------------| ------------------------------------------------------------------------------------------------------------- |
| boolean          | If the expression returns an empty or blank string translates to `false`, otherwise returns the boolean evaluation of the string value |
| number           | Translates to the number representation of the string when it contains a valid number, `0` in all other cases, including when the string does not contain a valid number. Different numeric types (i.e. integers and floats) require specific constraints to be met in order for the conversion to succeed, as per the standard number representation rules |

## Functions

Wherever templates are allowed you can also use functions to produce outputs or transform an input value. These functions are provided by lambdas and the syntax is like the one we've seen for nested values, like in this example:

```
option = "{% raw %}{{#upper}}{{ attribute }}{{/upper}}{% endraw %}"
```

Here `upper` is a function accepting one parameter (`attribute`) and returning the same output, with upper case. Below you can find the list of available lambdas.

The underlying [Handlebars](https://handlebarsjs.com/) template engine also provides an powerful set of built-in functions to handle conditionals, loops, lookups and log messages. Those functions (a.k.a. *helpers*) can be freely used in templates but they are not officially supported by Nyx. For more on those helpers see [Built-in Helpers](https://handlebarsjs.com/guide/builtin-helpers.html).
{: .notice--info}

### The functions library

#### `lower`

Transforms the input characters to lower case. Example:

```
output = "{% raw %}{{#lower}}{{input}}{{/lower}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `camelCase`                | `camelcase`                |
| `feature`                  | `feature`                  |
| `FEATURE`                  | `feature`                  |
| `feature/XX-12345`         | `feature/xx-12345`         |

#### `upper`

Transforms the input characters to upper case. Example:

```
output = "{% raw %}{{#upper}}{{input}}{{/upper}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `camelCase`                | `CAMELCASE`                |
| `feature`                  | `FEATURE`                  |
| `FEATURE`                  | `FEATURE`                  |
| `feature/XX-12345`         | `FEATURE/XX-12345`         |

#### `trim`

Removes the leading and trailing spaces from the input. Example:

```
output = "{% raw %}{{#trim}}{{input}}{{/trim}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `camelCase`                | `CAMELCASE`                |
| ` feature `                | `feature`                  |
| ` FEATURE `                | `FEATURE`                  |
| ` feature/XX-12345 `       | `feature/XX-12345`         |

#### `first`

Discards everything from the first occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{#first}}{{input}}{{/first}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `feature`                  |

#### `firstLower`

Discards everything from the first occurrence of a character other than letters and positive digits and transforms the remaining characters to lower case. Example:

```
output = "{% raw %}{{#firstLower}}{{input}}{{/firstLower}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `feature`                  |
| `FEATURE/XX-12345`         | `feature`                  |

#### `firstUpper`

Discards everything from the first occurrence of a character other than letters and positive digits and transforms the remaining characters to upper case. Example:

```
output = "{% raw %}{{#firstUpper}}{{input}}{{/firstUpper}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `FEATURE`                  |
| `FEATURE/XX-12345`         | `FEATURE`                  |

#### `last`

Discards everything before the last occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{#last}}{{input}}{{/last}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

#### `lastLower`

Discards everything before the last occurrence of a character other than letters and positive digits and transforms the remaining characters to lower case. Example:

```
output = "{% raw %}{{#lastLower}}{{input}}{{/lastLower}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `FEATURE`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

#### `lastUpper`

Discards everything before the last occurrence of a character other than letters and positive digits and transforms the remaining characters to upper case. Example:

```
output = "{% raw %}{{#lastUpper}}{{input}}{{/lastUpper}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `FEATURE`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

#### `sanitize`

Removes all characters other than letters and positive digits from the input string, leaving all other characters untouched. Example:

```
output = "{% raw %}{{#sanitize}}{{input}}{{/sanitize}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `featureXX12345`           |

#### `sanitizeLower`

Removes all characters other than letters and positive digits from the input string, and transforms all others to lower case. Example:

```
output = "{% raw %}{{#sanitizeLower}}{{input}}{{/sanitizeLower}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `featurexx12345`           |

#### `sanitizeUpper`

Removes all characters other than letters and positive digits from the input string, and transforms all others to upper case. Example:

```
output = "{% raw %}{{#sanitizeUpper}}{{input}}{{/sanitizeUpper}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `FEATUREXX12345`           |

#### `cutLeft`

Returns only the last N characters of the input, where N is an arbitrary positive integer represented by the `length` option. If the input is shorter than N characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{#cutLeft length="3"}}{{input}}{{/cutLeft}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Options    | Output     |
| ------------------------------------------ | ---------- | ---------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `length=3` | `606`      |
| `7b9da`                                    | `length=3` | `9da`      |
| `7b`                                       | `length=3` | `7b`       |

#### `cutRight`

Returns only the first N characters of the input, where N is an arbitrary positive integer represented by the `length` option. If the input is shorter than N characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{#cutRight length="3"}}{{input}}{{/cutRight}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Options    | Output      |
| ------------------------------------------ | ---------- | ----------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `length=3` | `7b9`       |
| `7b9da`                                    | `length=3` | `7b9`       |
| `7b`                                       | `length=3` | `7b`        |

#### `short5`

Returns only the first 5 characters of the input. If the input is shorter than 5 characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{#short5}}{{input}}{{/short5}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Output     |
| ------------------------------------------ | ---------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `7b9da`    |
| `7b9da`                                    | `7b9da`    |
| `7b`                                       | `7b`       |

For arbitrary length strings see [`cutLeft`](#cutleft) and [`cutRight`](#cutright).

#### `short6`

Returns only the first 6 characters of the input. If the input is shorter than 5 characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{#short6}}{{input}}{{/short6}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Output     |
| ------------------------------------------ | ---------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `7b9da5`   |
| `7b9da5`                                   | `7b9da5`   |
| `7b`                                       | `7b`       |

For arbitrary length strings see [`cutLeft`](#cutleft) and [`cutRight`](#cutright).

#### `short7`

Returns only the first 7 characters of the input. If the input is shorter than 5 characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{#short7}}{{input}}{{/short7}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Output     |
| ------------------------------------------ | ---------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `7b9da52`  |
| `7b9da52`                                  | `7b9da52`  |
| `7b`                                       | `7b`       |

For arbitrary length strings see [`cutLeft`](#cutleft) and [`cutRight`](#cutright).


#### `timestamp`

Returns a timestamp formatted according to the given format string.

The value for the timestamp is the current system time in milliseconds since January 1, 1970, 00:00:00 GMT unless a value is provided.

The returned string is the timestamp as an integer value unless the `format` option is passed with a valid format string.

Since the underlying frameworks have different behaviors, the output from this function may not be consistent between the command line version (written in Go) and the Gradle version (written in Java). The format string passd in the `format` option also depends on the implementation. Please see [here](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/text/SimpleDateFormat.html) for the available patterns available in Java and [here](https://pkg.go.dev/time#example-Time.Format) and [here](https://pkg.go.dev/time#pkg-constants) for those available in Go. Moreover, when invalid patterns are passed in the `format` option, the behavior is different between the two implementations: the Java implementation returns and empty string (and logs an error), while the Go implementation just returns the plain value passed in the `format` option. For consistent behavior functions see [`timestampISO8601`](#timestampISO8601) and [`timestampYYYYMMDDHHMMSS`](#timestampYYYYMMDDHHMMSS).
{: .notice--warning}

Example inputs and corresponding outputs:

```
output = "{% raw %}{{#timestamp}}{{/timestamp}}{% endraw %}"
output = "{% raw %}{{#timestamp}}{{timestamp}}{{/timestamp}}{% endraw %}"
output = "{% raw %}{{#timestamp format="yyyyMMdd"}}{{/timestamp}}{% endraw %}"
output = "{% raw %}{{#timestamp format="yyyyMMdd"}}{{timestamp}}{{/timestamp}}{% endraw %}"
output = "{% raw %}{{#timestamp format="20060102"}}{{/timestamp}}{% endraw %}"
output = "{% raw %}{{#timestamp format="20060102"}}{{timestamp}}{{/timestamp}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                                      | Options                                | Output                |
| ------------------------------------------ | -------------------------------------- | --------------------- |
|                                            |                                        | `1577880000000`       |
|                                            | `format=yyyyMMdd` or `format=yyyyMMdd` | `20200101`            |
| `1577880000000`                            |                                        | `1577880000000`       |
| `1577880000000`                            | `format=yyyyMMdd` or `format=yyyyMMdd` | `20200101`            |

#### `timestampISO8601`

Provided a timestamp in the [unix format](https://www.unixtimestamp.com/) returns it formatted as [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) UTC. If the input is not a Unix timestamp returns an empty string. Example:

```
output = "{% raw %}{{#timestampISO8601}}{{timestamp}}{{/timestampISO8601}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `1608210396`               | `2020-12-17T13:06:36`      |

For arbitrary timestamp formats see [`timestamp`](#timestamp).

#### `timestampYYYYMMDDHHMMSS`

Provided a timestamp in the [unix format](https://www.unixtimestamp.com/) returns it formatted as `YYYYMMDDHHMMSS` UTC. If the input is not a Unix timestamp returns an empty string. Example:

```
output = "{% raw %}{{#timestampYYYYMMDDHHMMSS}}{{timestamp}}{{/timestampYYYYMMDDHHMMSS}}{% endraw %}"
```

Example inputs and corresponding outputs:

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `1608210396 `              | `20201217130636`           |

For arbitrary timestamp formats see [`timestamp`](#timestamp).

#### `environmentUser`

Returns the current system user name. Example:

```
user = "{% raw %}{{environmentUser}}{% endraw %}"
```

or, if you prefer the open/close tags:

```
user = "{% raw %}{{#environmentUser}}{{/environmentUser}}{% endraw %}"
```

In case you pass a value to this function it is ignored.

#### `environmentVariable`

Returns the value of the environment variable used as parameter, if any. Example:

```
os = "{% raw %}{{#environmentVariable}}OS{{/environmentVariable}}{% endraw %}"
```

returns the value of the `OS` environment variable, if present, or an empty string.

#### `fileContent`

Returns content of the given file, if it exists, or an empty string. Example:

```
filecontent = "{% raw %}{{#fileContent}}example.txt{{/fileContent}}{% endraw %}"
```

returns returns the **entire** content of the `example.txt` file (if it exists).

The file name can be a relative or an absolute path. Please note that when a relative path is used, it's always resolved to the current working directory and other configured directories are ignored.

Be careful with the file content type, which must be text, and its size.

#### `fileExists`

Returns the string representation of a boolean, depending on whether the the given file exists. Example:

```
fileexists = "{% raw %}{{#fileExists}}example.txt{{/fileExists}}{% endraw %}"
```

returns `true` if the `example.txt` file exists, `false` otherwise.

The file name can be a relative or an absolute path. Please note that when a relative path is used, it's always resolved to the current working directory and other configured directories are ignored.

## Example

Here is a more complex example where we combine several state attributes to produce a multi-line text content. This example is only useful to show the use of templates and is not meant to be used anywhere.

Consider this template:

```
{% raw %}Version: {{version}} (bumping '{{bump}}' on {{configuration.initialVersion}} using lenient ({{configuration.releaseLenient}}))
Scheme: {{scheme}}
Timestamp: {{timestamp}}
OS: {{#environmentVariable}}OS{{/environmentVariable}}
User: {{environmentUser}}
Previous Version: {{releaseScope.previousVersion}} at {{#short5}}{{releaseScope.previousVersionCommit}}{{/short5}}

Commits:
{{#releaseScope.commits}}
  {{.}}
{{/releaseScope.commits}}{% endraw %}
```

When rendered, it yields to an output like:

```
Version: 9.8.6 (bumping 'theta' on 1.2.3 using lenient (true))
Scheme: SEMVER
Timestamp: 9223372036854775807
OS: Linux
Users: jdoe
Previous Version: 4.5.6 at 05cbf

Commits:
  d40fcded9e516158a2901f5657794931528af106
  9bed70fac8a27a4b14b6b12307d034bc59da85c3
  ef6a6481adb2df26bc7eebfde465e5c2f3e93539

```

As you can see there are state attributes like `version`, `bump`, `scheme` and `timestamp` used in the template and their usage should already be clear.

But since the resolved configuration is also available as a [nested state object]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#configuration), a couple of attributes are fetched from there, as you can see by `configuration.initialVersion` and `configuration.releaseLenient`.

Some values are retrieved from the current environment: the `OS` environment variable is fetched by the `{% raw %}{{#environmentVariable}}OS{{/environmentVariable}}{% endraw %}` block (where the function parameter `OS` is the name of the variable to retrieve) and also the system user name is retrieved by `{% raw %}{{{environmentUser}}{% endraw %}`.

Moreover, the [release scope]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/release-scope.md %}) is also used to get the `releaseScope.previousVersion` and `releaseScope.previousVersionCommit`. By pay attention here: the `previousVersionCommit` is not used as is but only the shortened SHA-1 is used, thanks to the [`short5`](#short5) function.

Finally, all the SHA-1 IDs of the release scope commits are printed, as you can see by the final block enclosed within `#releaseScope.commits` and `/releaseScope.commits`.