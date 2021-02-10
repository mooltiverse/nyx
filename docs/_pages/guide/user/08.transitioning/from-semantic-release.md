---
title: Transitioning to Nyx from Semantic Release
layout: single
toc: true
permalink: /guide/user/transitioning/from-semantic-release/
---

If you're considering to move to Nyx from [semantic-release](https://github.com/semantic-release/semantic-release) please consider a few things in advance.

First off, if your project is based on Node, think twice. Although *semantic-release* is not strictly tied to any technology, it's conceived for Node projects so before quitting you should consider that carefully.

Second, *semantic-release* is very opinionated and is already used in many projects so it's quite easy to find support.

In a few words, you should move to Nyx only when you find that *semantic-release* falls short in some areas that you need for your project.

The informations in this page are collected at the time of the writing. Please contact [support]({{ site.baseurl }}/meta/support/) in case you find outdated or incorrect informations in this page.
{: .notice--info}

## Main differences at a glance

When transitioning to Nyx from *semantic-release* you get some more features like:

* a portable and generic tool to give you more freedom from the technologies you use for your project
* support for [multiple version schemes]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/version-schemes.md %}), while *semantic-release* only supports [Semantic Versioning](https://semver.org/)
* early access to the current version number (i.e. to use it to build your project artifacts) while *semantic-release* only gives you that after an actual release (unless you do a *dry run* first, which clutters your build process a little)
* support for [extra identifiers]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/extra-identifiers.md %}) in version names like timestamps, commit SHA, pipeline ID, user name etc; these are useful for improved traceability in those organizations with complex release workflows and QA cycles
* no need to install Node tools like *npm* in your local environment
* native support for [common build tools]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}) (like [Gradle](https://gradle.org/))
* fine grained control over the entire [build process]({{ site.baseurl }}{% link _pages/guide/user/07.in-depth/design-principles.md %})
* a [multitude of configuration options]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/configuration-methods.md %}) to let you use Nyx even without any configuration file, while *semantic-release* doesn't let you control all of its options on the command line
* the option to use multiple [commit message conventions]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/commit-message-conventions.md %}) together
* the capability to [amend]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#amend) the workspace (i.e. when in the *detached HEAD state*)

On the other hand you lose:

* strong integration with Node development environments and tools
* native support for Node distribution channels like [npm dist-tags](https://docs.npmjs.com/cli/v6/commands/npm-dist-tag)
* the ability to use [custom plugins](https://github.com/semantic-release/semantic-release/blob/master/docs/usage/plugins.md), especially the ones you might have written for your own use case
