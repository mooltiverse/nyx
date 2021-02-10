---
layout: single
title:  "Troubleshooting configuration issues"
date:   2020-01-01 00:00:00 +0000
categories: troubleshooting user
tags: support configuration state
---

If you encounter configuration issues:

1. check out the [troubleshooting general issues]({{ site.baseurl }}{% link _posts/2020-01-01-troubleshooting-general-issues.md %}) page first to gather more informations
2. inspect the [`configuration` element]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/global-attributes.md %}#configuration) inside the [state file]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) ([enable]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/global-options.md %}#state-file) it if you haven't done yet) to see how the actual configuration has been resolved
3. make sure you took the configuration [evaluation order]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}#evaluation-order) into account
