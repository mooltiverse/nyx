---
title: Configuration Presets
layout: single
toc: false
permalink: /guide/user/configuration-presets/
---

Configuration presets are the easiest way to get started with Nyx as they bring streamlined configurations ready to use with just one option to configure.

You can enable a preset by simply setting the [`preset`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#preset) global configuration option to its name.

Presets are not defaults. While defaults are simple configuration values used when not defined by user configurations, presets are higher level, organic and engineered configurations addressing standard worflows, patterns and best practices.

You can easily use presets as a baseline to start from when building up your own custom configurations as you can still add other options or override the ones coming from presets, according to the [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order).

When using multiple [configuration methods]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) or customizing presets, complex configuration options (like `commitMessageConventions`, `releaseTypes`, `services`, for example) must be inherited or overridden as a whole. Overriding single values and inheriting others is not supported for this type of configuration option so when they are re-declared at one configuration level, all inherited values from those configuration methods with lower precedence are suppressed.
{: .notice--warning}

This chapter lists all available presets and their built in configurations.
