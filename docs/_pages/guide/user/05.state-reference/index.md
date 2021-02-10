---
title: State Reference
layout: single
toc: false
permalink: /guide/user/state-reference/
---

Nyx gives you the means to inspect its internals by mean of its *state*. The *state* is the internal representation of the facts that are used throughout the entire execution and is helpful in different cases, like:

* when authoring [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}), either to [parametrize the configuration]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/dynamic-configurations.md %}) or to generate custom [artifact]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/artifacts-and-publications.md %}). In both cases templates are rendered by an engine that feeds the data from a [JSON representation]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#json) of the entire internal state so you need to know the structure and contents of such data
* integrating other tools in your build process and passing them values generated or inferred by Nyx. Again you can just read a bunch of data from the *state* and hand it over to any number of other tools. The data is available in several [different formats]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars) so you can sure find what's easier for you to read
* debugging and [troubleshooting]({{ site.baseurl }}/categories/#troubleshooting), for example when you use a combination of [different configuration means]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) that gives you headache to figure out how the [configuration resolved]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#configuration) in the end
* you are just curious and want to know some internals

You can instruct Nyx to dump the *state* to a regular file by just setting the path to the [`stateFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) global option. Just like for configuration files, the state file can be rendered in [different formats]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars) (properties, YAML and JSON), inferred by the extension of the [`stateFile`]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) value.

You don't always need to dump the *state* to a file. For example, when authoring [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}), knowing the structure of the internal state from this reference guide is enough, provided that the *state* is passed to the template engine in memory.

The *state* contains a hierarchy of attributes, objects and collections represented using the same [rules used for configuration files]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#list-and-objects). Details about each value are provided in the next pages within this section.

[Examples]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/state-examples.md %}) are also available at the end.