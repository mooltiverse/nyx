---
title: Expressions
layout: single
toc: true
permalink: /guide/user/configuration-reference/expressions/
---

Nyx allows expressions to be used in the configuration so that it can dynamically change on some external factors like, for instance, environment variables or local files.

These expression must not be confused with *regular expressions* (used to *analyze* strings against known patterns) or [templates]({{ site.baseurl }}/reference/templates/) (used to produce text output). See [this FAQ]({{ site.baseurl }}/faq/#expressions-and-templates-when-should-i-use-which).
{: .notice--info}

You can use attributes from the [internal state]({{ site.baseurl }}/reference/state/) in expressions.

## Expression language

### Notation

Nyx uses a common notation for expressions that you may be already familiar with. All expressions are enclosed by a dollar sign and a left curly brace on the left side and a right curly brace on the right, like `{% raw %}${ ... }{% endraw %}`.

### Expression output

The output type of expressions is driven by the type of option or attribute that will receive the expression. When the expected type (the type of the option or attribute that receives the output) doesn't match the expression return type translation happens as follows:

| Expected type    | Expression type | Translation criteria                                                                                          |
| -----------------| --------------- | ------------------------------------------------------------------------------------------------------------- | 
| boolean          | string          | If the expression returns an empty string translates to `false`, otherwise `true` in all other cases          |
| boolean          | number          | If the expression returns `0` translates to `false`, otherwise `true` in all other cases                      |
| string           | boolean         | Translates to the literal `true` or `false`                                                                   |
| string           | number          | Translates to the literal representation of the number                                                        |
| number           | boolean         | Translates to `0` if the expression returns `false`, otherwise `true` in all other cases                      |
| number           | string          | Translates to the number representation of the string when it contains a valid number, `0` in all other cases |

### Static output

Sometimes an attribute can accept an expression to be evaluated dynamically but you just need to hardcode a fixed value.

In this case just set the fixed value (without the `{% raw %}${ }{% endraw %}` delimiters) in place of the expression and no evaluation happens (although the output is still [translated](#expression-output) according to the expected type).

### Reference

Here are the available objects and methods.

#### `environment`

The `environment` object has the following methods available:

| Name             | Type    | Parameters                                        | Description                                                                                             |
| -----------------| ------- | ------------------------------------------------- | ------------------------------------------------------------------------------------------------------- | 
| `getUser`        | string  |                                                   | Returns the current user name in a platform independent way, so it's able to read from the `USER` or `USERNAME` variables hinding the platform peculiarities |
| `getVariable`    | string  | `name:string`                                     | Returns the value of the environment variable with the given `name`, otherwise an empty string          |
| `hasVariable`    | boolean | `name:string`                                     | Returns `true` if the current environment has a variable with the given `name` defined, false otherwise |

Examples:

```text
# Returns the value of the PATH environment variable
{% raw %}${ environment.getVariable('PATH') }{% endraw %}

# Returns true when the PATH environment variable is defined, false otherwise
{% raw %}${ environment.hasVariable('PATH') }{% endraw %}
```

#### `file`

The `file` object has the following methods available:

| Name             | Type    | Parameters                                        | Description                                                                                             |
| -----------------| ------- | ------------------------------------------------- | ------------------------------------------------------------------------------------------------------- | 
| `content`        | string  | `path:string`                                     | Returns the content of the file with the given `path` if the file exists, otherwise an empty string     |
| `exists`         | boolean | `path:string`                                     | Returns `true` if the file with the given `path` exists, `false` otherwise                              |

Examples:

```text
# Returns the content of the 'my.txt' file
{% raw %}${ file.content('my.txt') }{% endraw %}

# Returns true when the 'my.txt' file exists, false otherwise
{% raw %}${ file.exists('my.txt') }{% endraw %}
```