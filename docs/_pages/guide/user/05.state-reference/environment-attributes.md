---
title: Environment Attributes
layout: single
toc: true
permalink: /guide/user/state-reference/environment-attributes/
---

## Top level attributes

The following attributes are at the top of the hierarchy:

| Name                                      | Type    | Values                               |
| ----------------------------------------- | ------- | ------------------------------------ |
| [`environmentName`](#environment-name)    | string  | An environment name                  |
| [`environmentUser`](#environment-user)    | string  | User name                            |
| [`localEnvironment`](#local-environment)  | boolean | `true` or `false`                    |
| [`serverEnvironment`](#server-environment)| boolean | `true` or `false`                    |

### Environment name

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `environmentName`                                                                        |
| Type                          | string                                                                                   |
| Related configuration options | [environment]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#environment){: .btn .btn--success .btn--small} [environments/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#enabled-environments){: .btn .btn--success .btn--small} [environments/\<ID\>/name]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#name){: .btn .btn--success .btn--small} |

The [inferred]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer) name of the current [environment]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}).

### Environment user

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `environmentUser`                                                                        |
| Type                          | string                                                                                   |
| Related configuration options | [environment]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#environment){: .btn .btn--success .btn--small} [environments/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#enabled-environments){: .btn .btn--success .btn--small} [environments/\<ID\>/user]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#user){: .btn .btn--success .btn--small} |

The name of the user running the process as it's inferred by the [environment configuration]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#user), which in turn may have it as a fixed string, read it from an environment variable or compute it by other means.

### Local environment

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `localEnvironment`                                                                       |
| Type                          | boolean                                                                                  |
| Related configuration options | [environment]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#environment){: .btn .btn--success .btn--small} [environments/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#enabled-environments){: .btn .btn--success .btn--small} [environments/\<ID\>/local]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#local){: .btn .btn--success .btn--small} |

The flag indicating if this is a *local* environment (i.e. a developer workstation) or a server (i.e. a CI/CD server). This is always the opposite (negation) of [`serverEnvironment`](#server-environment).

### Server environment

| ----------------------------- | ---------------------------------------------------------------------------------------- |
| Name                          | `serverEnvironment`                                                                      |
| Type                          | boolean                                                                                  |
| Related configuration options | [environment]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#environment){: .btn .btn--success .btn--small} [environments/enabled]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#enabled-environments){: .btn .btn--success .btn--small} [environments/\<ID\>/server]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/environments.md %}#server){: .btn .btn--success .btn--small} |

The flag indicating if this is a server (i.e. a CI/CD server) or a *local* environment (i.e. a developer workstation). This is always the opposite (negation) of [`localEnvironment`](#local-environment).
