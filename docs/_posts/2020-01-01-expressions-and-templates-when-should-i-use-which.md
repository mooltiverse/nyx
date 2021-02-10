---
layout: single
title:  "Expressions and templates: when should I use which?"
date:   2020-01-01 00:00:00 +0000
categories: faq user
tags: configuration expressions templates
---

The short answer is that you can't chose. Some configuration options accept expressions while others accept templates, but they're not interchangeable. Using one where the other is expected will throw an error.

Don't be mislead by similar delimiters: expressions use `{% raw %}${ ... }{% endraw %}`, while templates use `{% raw %}{{ ... }}{% endraw %}` so mind the dollar sign!

Expressions are for internal use, like enabling or disabling certain features depending on external facts, while templates produce text output, either in simple attributes or entire files.

Templates can produce list outputs by looping over collections of values, while expressions always return a simple value. On the other hand, expressions have typed input and ouput values (they're often used to produce a boolean output) while with templates, everything is text.
