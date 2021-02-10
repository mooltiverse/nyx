---
title: Templates
layout: single
toc: true
permalink: /guide/user/configuration-reference/templates/
---

Templates are used in several places to parametrize text outputs, be them strings, messages or even whole text files.

Nyx uses [Mustache](https://mustache.github.io/) templates passing the engine the [internal state]({{ site.baseurl }}/reference/state/) for the input object so it's easy to figure out which values are available. Moreover a few functions ([lambdas](#using-lambdas)) are available for common needs.

Tip: when writing templates you may find useful to [serialize the state to a file]({{ site.baseurl }}/reference/configuration/#state-file) for reference or troubleshooting.
{: .notice--info}

## Reference

### Render a static output

At times you just need to output a simple hardcoded string instead of rendering even the simplest template. Don't even bother with `{% raw %}{{ }}{% endraw %}` delimiters in this case and simply enter the value.

Let's assume you want to set the `Hello World` value for a configuration `option`, then your template is just the plain `Hello World`:

```
option = "Hello World"
```

### Render a simple string

Often times you just need to use a simple value from the [Nyx state]({{ site.baseurl }}/reference/state/) to be the result of the template.

This is fairly simple as you just need to wrap the name of the [state]({{ site.baseurl }}/reference/state/) attribute with double curly braces, like `{% raw %}{{ attribute }}{% endraw %}`. This is the simplest form of a [Mustache tag](https://mustache.github.io/mustache.5.html) (and yes, the double curly brances are the *mustaches*).

So if we want to set our configuration `option` to the current value of the `version` attribute from the [Nyx state]({{ site.baseurl }}/reference/state/) our definition will look like:

```
option = "{% raw %}{{ version }}{% endraw %}"
```

When the value you need to render is nested into another (i.e. `section/attribute` or `section.attribute`, depending on the notation), the template looks like:

```
option = "{% raw %}{{ #section }}{{ attribute }}{{ /section }}{% endraw %}"
```

As you can see this is a little cumbersome for a simple value but you will soon understand how powerful it can be. What is important to note here is that the `{% raw %}{{ attribute }}{% endraw %}` expression is now enclosed between a `{% raw %}{{ #section }}{% endraw %}` opening tag and a `{% raw %}{{ /section }}{% endraw %}` closing tag (denoted by the `#` and `/` characters).

### Using lambdas

You can also use functions to produce outputs or transform an input value. These functions are provided by lambdas and the syntax is like the one we've seen for nested values, like in this example:

```
option = "{% raw %}{{ #uppercase }}{{ attribute }}{{ /uppercase }}{% endraw %}"
```

Here `uppercase` is a function accepting one parameter (`attribute`) and returning the same output, with upper case. Below you can find the list of available lambdas.

#### The functions library

##### `lower`

Transforms the input characters to lower case. Example:

```
output = "{% raw %}{{ #lower }}{{ input }}{{ /lower }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `camelCase`                | `camelcase`                |
| `feature`                  | `feature`                  |
| `FEATURE`                  | `feature`                  |
| `feature/XX-12345`         | `feature/xx-12345`         |

##### `upper`

Transforms the input characters to upper case. Example:

```
output = "{% raw %}{{ #upper }}{{ input }}{{ /upper }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `camelCase`                | `CAMELCASE`                |
| `feature`                  | `FEATURE`                  |
| `FEATURE`                  | `FEATURE`                  |
| `feature/XX-12345`         | `FEATURE/XX-12345`         |

##### `first`

Discards everything from the first occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{ #first }}{{ input }}{{ /first }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `feature`                  |

##### `last`

Discards everything before the last occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{ #last }}{{ input }}{{ /last }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

##### `sanitize`

Removes all characters other than letters and positive digits from the input string, leaving all other characters untouched. Example:

```
output = "{% raw %}{{ #sanitize }}{{ input }}{{ /sanitize }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `featureXX12345`           |

##### `sanitizeLower`

Removes all characters other than letters and positive digits from the input string, and transforms all others to lower case. Example:

```
output = "{% raw %}{{ #sanitizeLower }}{{ input }}{{ /sanitizeLower }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `featurexx12345`           |

##### `sanitizeUpper`

Removes all characters other than letters and positive digits from the input string, and transforms all others to upper case. Example:

```
output = "{% raw %}{{ #sanitizeUpper }}{{ input }}{{ /sanitizeUpper }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `FEATUREXX12345`           |

##### `sanitizeFirst`

Discards everything from the first occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{ #sanitizeFirst }}{{ input }}{{ /sanitizeFirst }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `feature`                  |

##### `sanitizeFirstLower`

Discards everything from the first occurrence of a character other than letters and positive digits and transforms the remaining characters to lower case. Example:

```
output = "{% raw %}{{ #sanitizeFirstLower }}{{ input }}{{ /sanitizeFirstLower }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `feature`                  |
| `FEATURE/XX-12345`         | `feature`                  |

##### `sanitizeFirstUpper`

Discards everything from the first occurrence of a character other than letters and positive digits and transforms the remaining characters to upper case. Example:

```
output = "{% raw %}{{ #sanitizeFirstUpper }}{{ input }}{{ /sanitizeFirstUpper }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `FEATURE`                  |
| `FEATURE/XX-12345`         | `FEATURE`                  |

##### `sanitizeLast`

Discards everything before the last occurrence of a character other than letters and positive digits. Example:

```
output = "{% raw %}{{ #sanitizeLast }}{{ input }}{{ /sanitizeLast }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

##### `sanitizeLastLower`

Discards everything before the last occurrence of a character other than letters and positive digits and transforms the remaining characters to lower case. Example:

```
output = "{% raw %}{{ #sanitizeLastLower }}{{ input }}{{ /sanitizeLastLower }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `feature`                  |
| `FEATURE`                  | `feature`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

##### `sanitizeLastUpper`

Discards everything before the last occurrence of a character other than letters and positive digits and transforms the remaining characters to upper case. Example:

```
output = "{% raw %}{{ #sanitizeLastUpper }}{{ input }}{{ /sanitizeLastUpper }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `feature`                  | `FEATURE`                  |
| `FEATURE`                  | `FEATURE`                  |
| `12345`                    | `12345`                    |
| `feature/XX-12345`         | `12345`                    |

##### `short5`

Returns only the first 5 characters of the input. If the input is shorter than 5 characters it's returned untouched. This is often useful to shorten SHAs. Example:

```
output = "{% raw %}{{ #short5 }}{{ input }}{{ /short5 }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `7b9da5286d4724dd7385bb80639a08841fa26606` | `7b9da`    |
| `7b9da`                                    | `7b9da`    |
| `7b`                                       | `7b`       |

##### `timestampISO8601`

Provided a timestamp in the [unix format](https://www.unixtimestamp.com/) returns it formatted as [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601). If the input is not a Unix timestamp returns an empty string. Example:

```
output = "{% raw %}{{ #timestampISO8601 }}{{ timestamp }}{{ /timestampISO8601 }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `1608210396 `              | `2020-12-17T13:06:36+00:00`|


##### `timestampYYYYMMDDHHMMSS`

Provided a timestamp in the [unix format](https://www.unixtimestamp.com/) returns it formatted as `YYYYMMDDHHMMSS`. If the input is not a Unix timestamp returns an empty string. Example:

```
output = "{% raw %}{{ #timestampYYYYMMDDHHMMSS }}{{ timestamp }}{{ /timestampYYYYMMDDHHMMSS }}{% endraw %}"
```

| Input                      | Output                     |
| -------------------------- | -------------------------- |
| `1608210396 `              | `20201217130636`           |
