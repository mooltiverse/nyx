---
title: Extra Identifiers
layout: single
toc: true
permalink: /guide/user/configuration-reference/extra-identifiers/
---

Extra identifiers are additional elements that may be used in the composition of a version identifier. Examples of extra identifiers are branch names, timestamps, user names, environment names etc, as follows:

* version `1.2.3` has no extra identifiers
* version `1.2.3-alpha.12` has one extra identifier whose qualifier is the branch name (`alpha`) and the value is the [ticker]({{ site.baseurl }}/reference/state/#global-attributes) with value `12`; this is the common usage of extra identifiers in [pre-release]({{ site.baseurl }}/reference/release-types/#pre-release)
* version `1.2.3-alpha.12+johndoe` also adds another identifier in the *build* part with the local [user]({{ site.baseurl }}/reference/state/#global-attributes)
* version `1.2.3-alpha.12.local+johndoe.build.543.202001010955` adds another identifier in the *pre-release* part showing the environment name (`local`), and other two identifiers in the *build* part: the build number and the [timestamp]({{ site.baseurl }}/reference/state/#global-attributes)

Extra identifiers are configured within the `extraIdentifiers` *section*. The section allows one sub-section for each extra identifier. In this section you just *define* the identifiers in some abstract way but you don't enable or apply these extra identifiers. Enabling them is done be reference in the [`enabled identifiers`](#enabled-identifiers) attribute of each [release type](#release-type-definition).

You can have as extra identifiers as you want. You can use default ones that come bundled with Nyx, override them completely or for just a few attributes or define your own from scratch.

### Extra identifier definition

Within the `extraIdentifiers` block you can define as many identifiers as you want, each in its own separate block. The `name` identifies the identifier so to define a brand new identifier make sure you give it a `name` that was not already in use. If you use a `name` that was already defined for an identifier (given the [evaluation order](#evaluation-order)) then you are **overriding** an existing identifier and when you do, you can override a single attribute or all of them.

When defining an extra identifier do not confuse the `name` with the `qualifier`. The `name` is the technical ID for the identifier definition and is the one you use in the [`enabled identifiers`](#enabled-identifiers) attribute in [release types](#release-type-definition). The `qualifier` is the (optional) *left* part of the extra identifier to appear in version names. For example, in version `1.2.3-alpha.12` you don't see the `name` (which is only in Nyx configuration), while `alpha` is the qualifier (and `12` the `value`).
{: .notice--warning}

With extra identifiers both the `qualifier` and the `value` are optional. If both are present, the `qualifier` appears on the left side of the separator (the dot) and the `value` on the right. If only one is present you just don't see the other nor the dot.
{: .notice--info}

Each extra identifier has the following attributes:

| Name                                           | Type    | Command Line Option                                        | Environment Variable                                     | Configuration File Option                | Default                                    |
| ---------------------------------------------- | ------- | ---------------------------------------------------------- | -------------------------------------------------------- | ---------------------------------------- | ------------------------------------------ |
| [`name`](#name-extra-identifier)               | string  | `--extra-identifiers-<INDEX>-name=<NAME>`                  | `NYX_EXTRA_IDENTIFIERS_<INDEX>_NAME=<NAME>`              | `extraIdentifiers/<INDEX>/name`          | N/A                                        |
| [`qualifier`](#qualifier-extra-identifier)     | string  | `--extra-identifiers-<INDEX>-qualifier=<TEMPLATE>`         | `NYX_EXTRA_IDENTIFIERS_<INDEX>_QUALIFIER=<TEMPLATE>`     | `extraIdentifiers/<INDEX>/qualifier`     | Empty (no qualifier)                       |
| [`value`](#value-extra-identifier)             | string  | `--extra-identifiers-<INDEX>-value=<TEMPLATE>`             | `NYX_EXTRA_IDENTIFIERS_<INDEX>_VALUE=<TEMPLATE>`         | `extraIdentifiers/<INDEX>/value`         | Empty (no value)                           |
| [`when`](#when-extra-identifier)               | boolean | `--extra-identifiers-<INDEX>-when=<EXPR>`                  | `NYX_EXTRA_IDENTIFIERS_<INDEX>_WHEN=<EXPR>`              | `extraIdentifiers/<INDEX>/when`          | `true`                                     |
| [`where`](#where-extra-identifier)             | string  | `--extra-identifiers-<INDEX>-where=<EXPR>`                 | `NYX_EXTRA_IDENTIFIERS_<INDEX>_WHERE=<VALUE>`            | `extraIdentifiers/<INDEX>/where`         | `build`                                    |

#### Name (extra identifier)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `name`                                                                                   |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--extra-identifiers-<INDEX>-name=<NAME>`                                                |
| Environment Variable      | `NYX_EXTRA_IDENTIFIERS_<INDEX>_NAME=<NAME>`                                              |
| Configuration File Option | `extraIdentifiers/<INDEX>/name`                                                          |
| Related state attributes  |                                                                                          |

The short name that identifies this identifier for configuration purpose. This is also the value you can use in the [`enabledIdentifiers`](#enabled-identifiers) attribute in [`releaseTypes`](#release-type-definition).

This option is **mandatory**.

#### Qualifier (extra identifier)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `qualifier`                                                                              |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--extra-identifiers-<INDEX>-qualifier=<TEMPLATE>`                                       |
| Environment Variable      | `NYX_EXTRA_IDENTIFIERS_<INDEX>_QUALIFIER=<TEMPLATE>`                                     |
| Configuration File Option | `extraIdentifiers/<INDEX>/qualifier`                                                     |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}/reference/templates/) that, once rendered, is used as the leftmost part of the extra qualifier in versions. If empty the version will only show the identifier [value](#value-extra-identifier). If not empty the result of the template evaluation will be used for the leftmost part of the identifier in the version and if the value is non empty, the two will be separated by a dot.

Defaults to the empty string (no qualifier).

#### Value (extra identifier)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `value`                                                                                  |
| Type                      | string                                                                                   |
| Default                   | N/A                                                                                      |
| Command Line Option       | `--extra-identifiers-<INDEX>-value=<TEMPLATE>`                                           |
| Environment Variable      | `NYX_EXTRA_IDENTIFIERS_<INDEX>_VALUE=<TEMPLATE>`                                         |
| Configuration File Option | `extraIdentifiers/<INDEX>/value`                                                         |
| Related state attributes  |                                                                                          |

This is a short [template]({{ site.baseurl }}/reference/templates/) that, once rendered, is used as the rightmost part of the extra qualifier in versions. If empty the version will only show the identifier [qualifier](#qualifier-extra-identifier). If not empty the result of the template evaluation will be used for the rightmost part of the identifier in the version and if the qualifier is non empty, the two will be separated by a dot.

Defaults to the empty string (no value).

#### When (extra identifier)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `when`                                                                                   |
| Type                      | boolean                                                                                  |
| Default                   | `true`                                                                                   |
| Command Line Option       | `--extra-identifiers-<INDEX>-when=<EXPR>`                                                |
| Environment Variable      | `NYX_EXTRA_IDENTIFIERS_<INDEX>_WHEN=<EXPR>`                                              |
| Configuration File Option | `extraIdentifiers/<INDEX>/when`                                                          |
| Related state attributes  |                                                                                          |

Here you can dynamically enable or disable the usage of the extra identifier based on some dynamic values. In other words the extra identifier may be used or not conditionally based on some context attribute.

By default this is `true` so once the identifier is listed within the [`enabled identifiers`](#enabled-identifiers) attribute in [release types](#release-type-definition), it is always used for that release type. If you set this to `false` it is equivalent to not declare it in the [`enabled identifiers`](#enabled-identifiers).

This option is effective when you use a dynamic [expression]({{ site.baseurl }}/reference/expressions/) here in order to check for a *fact* in the actual runtime environment. Examples:

* `{% raw %}${ environment.hasVariable('CI') }{% endraw %}` will positively detect the environment when an environment variable named `CI` has been defined in the current environment (which happens in many CI/CD platforms)
* `{% raw %}${ file.exists('/etc/acme.conf') }{% endraw %}` will positively detect the environment when the `/etc/acme.conf` file is found, which might be a configuration file available on certain environments only

#### Where (extra identifier)

| ------------------------- | ---------------------------------------------------------------------------------------- |
| Name                      | `where`                                                                                  |
| Type                      | string                                                                                   |
| Default                   | `build`                                                                                  |
| Command Line Option       | `--extra-identifiers-<INDEX>-where=<EXPR>`                                               |
| Environment Variable      | `NYX_EXTRA_IDENTIFIERS_<INDEX>_WHERE=<VALUE>`                                            |
| Configuration File Option | `extraIdentifiers/<INDEX>/where`                                                         |
| Related state attributes  |                                                                                          |

This is only used [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) [version scheme](#scheme). Available values are:

* `pre-release`: the identifier appears in the [pre-release part](https://semver.org/)
* `build`: the identifier appears in the [build part](https://semver.org/)

Defaults is `build`.

When multiple extra identifiers are used for the same part, their order is determined by the [`enabled identifiers`](#enabled-identifiers) attribute in [release types](#release-type-definition).
{: .notice--info}