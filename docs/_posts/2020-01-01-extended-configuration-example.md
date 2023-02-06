---
layout: single
toc: true
title:  "Extended configuration example"
date:   2020-01-01 00:00:00 +0000
categories: example user
tags: support configuration
---

Here you can find the example of a large configuration in the supported [`formats`]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#supported-file-grammars).

This configuration doesn't use any [preset]({{ site.baseurl }}{% link _pages/guide/user/04.configuration-presets/index.md %}) but sets many [global configuration options]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}) explicitly along with [commit message conventions]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/commit-message-conventions.md %}) and [release types]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/release-types.md %}). Where allowed, [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) are also used.

Configuration options using [templates]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/templates.md %}) may not be rendered correctly in this page so please refer to the links to source code provided for each file.
{: .notice--warning}

## JSON

```json
{% include .nyx-extended.json %}
```

Source code for this file is available [here](https://raw.githubusercontent.com/mooltiverse/nyx/main/docs/_includes/.nyx-extended.json){:target="_blank"}.

## YAML

```yaml
{% include .nyx-extended.yaml %}
```

Source code for this file is available [here](https://raw.githubusercontent.com/mooltiverse/nyx/main/docs/_includes/.nyx-extended.yaml){:target="_blank"}.

## Groovy (Gradle)

```groovy
{% include .nyx-extended.groovy %}
```

Source code for this file is available [here](https://raw.githubusercontent.com/mooltiverse/nyx/main/docs/_includes/.nyx-extended.groovy){:target="_blank"}.
