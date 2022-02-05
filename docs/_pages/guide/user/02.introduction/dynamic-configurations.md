---
title: Dynamic Configurations
layout: single
toc: true
permalink: /guide/user/introduction/dynamic-configurations/
---

Although you can configure Nyx by means of static values, you also have the option to use [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) to make them dynamic.

Templates are allowed for many configuration options and they are evaluated at runtime based on a predefined set of expressions available.

As an example, let's suppose you want to make the publication of a release conditional, based on the value of some environment variable. This case is used when you want to issue *official* releases from continuous integration servers only, making sure the build environment runs in a controlled environment.

To do so you can inspect the value of the `CI` environment variable (it's commonly available on continuous environment platforms) and you want it to be `true`. So when you configure the *official* [release type]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}) you want it to be selected only if the `CI` environment variable is defined in the current environment and its value is `true`.

Assuming you're using a JSON configuration, your `releaseTypes/official/` configuration block will then have a declaration like:

```json
"releaseTypes":{
  "enabled":[ "official" ],
  "items":{
    "official":{
      matchEnvironmentVariables {
          CI = '^true$'
      }
    }
  }
}
```

Now your *official* release type is only matched when the `CI` environment variable is defined and its value is exactly `true`.

You can use templates to fetch and compute dynamic values from various sources and in several contexts. Please check out the [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) library for more.
