---
title: Design Principles
layout: single
toc: true
permalink: /guide/user/in-depth/design-principles/
---

## Transactional process

The release process needs to be *transactional* because *releasing* is, from a semantic point of view, an *atomic* operation that must entirely succeed or *roll back* once all the regular build tasks have succeeded.

### With tools other than Nyx

To cope with this principle, release tools other than Nyx usually apply a very simple restriction: they perform all of their tasks in one single shot (usually triggered by the final *release* step of build scripts) to mimic an *atomic* transaction.

To make this clearer, consider a typical build process with the following steps:

```
1. build      # preprocess, compile, assemble, package etc         [the version number is already needed here...]
2. test       # deploy to test environment, run tests
3. release    # tag, generate change log, notify                   [...but here is where the version number is generated]
```

This is acceptable in some circumstances but for many projects it's too simplistic and usually drives to a duplicate execution of the *release* task within the build process: the first occurrence is kind of a *dry run* with the sole purpose of generating a version number, while the last actually performs the release. The regular *build* tasks that need the version rumber run in between the two. Like this:

```
1. release    # dry run, just generate the version number          [here the version number is available]
2. build      # preprocess, compile, assemble, package etc
3. test       # deploy to test environment, run tests
4. release    # tag, generate change log, notify                   [repeats the process at step 1, but finalizing]
```

### With Nyx

Nyx splits the process in different phases: the *[infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#infer)* phase just gathers informations and makes available the new version number (*prepares* the transaction without altering the state of the repository), while the *[publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#publish)* phase *applies* the transaction atomically. This way you can rely on the version number since the early stages, so you can achieve full [*consistency*]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/automation.md %}#build-scripts) and avoid repetitions. It looks like:

```
1. infer      # analyze repo and generate the version number
2. build      # preprocess, compile, assemble, package etc
3. test       # deploy to test environment, run tests
4. publish    # tag, generate change log, notify
```

This looks like the previous one at a first sight but the processes at steps 1 and 4 do not overlap and they allow other build tasks to run in between *by design*.

Nyx actually has a few other phases in its [internal workflow]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}) but these are the ones useful for the sake of this concept.
{: .notice--warning}

## Incremental process

In order to be [efficient]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/automation.md %}#build-scripts) build processes need to be **incremental** so tools and plugins like Nyx must comply with such principle as well. Nyx's [workflow]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %}) fully supports the incremental process as it's conveniently split in different phases ([commands]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#available-commands)) than even when used individually do not repeat the tasks that were already taken.

While support for incremental processes is implicitly built in when using Nyx as a plugin for *stateful* environments (i.e. [Gradle]({{ site.baseurl }}{% link _pages/guide/user/01.quick-start/gradle-plugin.md %})), passing the state between phases in stateless scenarios may need a little more. This is the case of using the command line tool.

The easiest way is to achieve statefulness in stateless environmemts is using the [state file]({{ site.baseurl }}{% link _pages/guide/user/05.state-reference/index.md %}). This way Nyx stores the state incrementally between phases and doesn't need to repeat any task at any time.

## Immutable workspace

By default Nyx never alters the state of the local Git repository as it performs all of its core operations by just reading the local repository, inferring informations and making them available as output. This also helps in granting [transactionality](#transactional-process).

However there are some features that may violate this principle like when the *[make]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/usage.md %}#make)* command is configured to produce local [artifacts]({{ site.baseurl }}{% link _pages/guide/user/03.configuration-reference/artifacts.md %}) like the changelog.

These features are never enabled by default so you have to be aware when enabling them.
{: .notice--info}

## Idempotence

As long as [the local Git repository state doesn't change](#immutable-workspace) Nyx grants that:

* the version number generated during the *[infer]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %})* phase doesn't change until the end of the process so you can rely on it throughout the entire build
* interrupting the process before the *[publish]({{ site.baseurl }}{% link _pages/guide/user/02.introduction/how-nyx-works.md %})* phase (i.e. because the process fails some tests) and running Nyx again will generate the same version number, unless Nyx is configured to use timestamps in version numbers (i.e. in the *build* metadata)
* the conditions that Nyx uses to determine the version number (Git branches and tags, workspace status, configuration options) don't change across executions

This allows the build process to be [efficient]({{ site.baseurl }}{% link _pages/guide/user/06.best-practice/automation.md %}#build-scripts) and leverage [incremental builds](#incremental-process) without compromising consistency.
