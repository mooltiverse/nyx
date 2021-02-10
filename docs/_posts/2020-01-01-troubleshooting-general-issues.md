---
layout: single
title:  "Troubleshooting general issues"
date:   2020-01-01 00:00:00 +0000
categories: troubleshooting user
tags: support configuration verbosity state
---

As a rule of thumb, when encountering issues or unexpected results from Nyx consider to:

* increase the log [verbosity]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#verbosity) (i.e. to `debug` or even `trace` if needed)
* [enable]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) the [state file]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) to be written on the file system to understand the values that Nyx infers or reads from the configuration and how the actual values are resolved

Also check out the [support]({{ site.baseurl }}{% link _pages/meta/support.md %}) page.
