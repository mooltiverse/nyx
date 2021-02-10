---
layout: single
title:  "Does Nyx support plugins?"
date:   2020-01-01 00:00:00 +0000
categories: faq user
tags: misc architecture
---

No, Nyx does not support plugins by design. There are a few reasons behind this:

* there is actually no need for plugins as beside plenty of [configuration options]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/index.md %}) that let you control every details, you can control its [modular workflow]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/design-principles.md %}) to inject custom tasks at any stage. Also reading Nyx's [intermediate state]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}) gives you a wide range of extension possibilities by reading the internal state in a structured way and using it for other tasks
* plugins would be needed if Nyx had a monolythic workflow with no injection points but this is not the case
* we want Nyx to be idempotent across versions and platforms so it would be really hard to offer the same plugin support for different versions like Java and Go
