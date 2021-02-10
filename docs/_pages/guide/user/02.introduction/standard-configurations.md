---
title: Standard Configurations
layout: single
toc: true
permalink: /guide/user/introduction/standard-configurations/
---

Just like any powerful tool Nyx has a wide range of configuration options. On the other hand, most teams, especially when following common best practices, end up with the same needs and this translates to the same configurations.

Nyx gets you covered in case you just want to get started quickly with standard configurations or when you want to get into the nuts and bolts by giving preset configuration to be enabled with just one flag for the formers, while letting the latters play with configuration details.

Don't be afraid to get stuck when using presets as you still have all options available for you to play with. Should you find yourself in between you can use presets and customize by overriding or adding what's missing for you.

If you like the hard way and want to have full control you can run Nyx once enabling presets then grab the [resolved configuration]({{ site.baseurl }}/state/global-attributes/#configuration) from the [state]({{ site.baseurl }}/reference/state/) and start authoring your own configuration using that as a reference baseline.
{: .notice--info}

Presets are [evaluated]({{ site.baseurl }}/reference/configuration-methods/#evaluation-order) with low priority, just above default values but below any configuration file, environment variable or command line option so they can be overridden by all available means.

Preset configurations are:

* extremely easy to enable with just one flag
* engineered to be consistent among many options
* maintained along with the tool so when you upgrade you don't need to worry about configuration changes
* streamlined for best practices or special use cases
* flexible and customizeable so you can use them as a baseline and just override, add or replace the options you need to

## Presets

To use a preset all you have to do is to set the [preset]({{ site.baseurl }}/configuration/global-options/#preset) option to its name. For example, to use the [`generic`](#generic) preset from the command line you just run Nyx like:

```bash
nyx --preset=generic
```

If you want to do the same by using environment variables just use the `NYX_PRESET` variable (`NYX_PRESET=generic`). Otherwise you can just set the `preset = generic` option in a configuration file.

If the option is set multiple times it is evaluated using the standard [evaluation order]({{ site.baseurl }}/reference/configuration-methods/#evaluation-order).

| ----------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Related configuration options | [preset]({{ site.baseurl }}/configuration/global-options/#preset){: .btn .btn--success .btn--small}          |
| Related state attributes      |                                                                                                              |
{: .notice--info}

### Generic

The *generic* preset configures Nyx for mainstream best practices and is suitable for teams of any size. You will probably find some items exceeding your needs but they don't hurt.

The detailed configuration is available in the [configuration section]({{ site.baseurl }}/configuration/presets/#generic).

At a glance:

* [SemVer]({{ site.baseurl }}/reference/version-schemes/#semantic-versioning-semver) is the version scheme
* [main]({{ site.baseurl }}/reference/release-strategy/#main-releases), [pre-release]({{ site.baseurl }}/reference/release-strategy/#pre-release), [post-release]({{ site.baseurl }}/reference/release-strategy/#post-release) and [internal]({{ site.baseurl }}/reference/release-strategy/#internal-release) release types are defined for common strategies

TODO: complete this section with summary of the generic preset
{: .notice--warning}