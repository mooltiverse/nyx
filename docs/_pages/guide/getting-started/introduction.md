---
title: Introduction to Nyx
layout: single
toc: true
permalink: /getting-started/introduction/
---

## How Nyx works

### Nyx Internal Workflow

Nyx processing is done in two major steps:

1. release **inference**: in this phase all the necessary information are collected in order to detect the previous version and determine the next version number, based on different elements. During inference Nyx only reads information and doesn't apply any change or take any other action
2. release **publication**: in this phase Nyx actually publishes the release, which implies, depending on the configuration, tagging, committing and invoking a bunch of API

The reason behind this separation is that you may need to do other things in between. This is usually the case of using build tools like Gradle or Bazel or CI/CD platforms.

The *inference* and *publication* phases are further split into other steps but you only need to understand them when [extending Nyx with custom plugins]({{ site.baseurl }}/extending/).

### Transactional release process
The release process needs to be *transactional* to some extent because *releasing* is, from a semantic point of view, an *atomic* operation that must entirely succeed (or *roll back*), also because the other tasks that are usually executed by the build process before the *release* step must not be repeated.

In order to cope with this principle, most of the release tools available apply a very simple restriction: they generate the version number only during the final *release* step, within the same *atomic* transaction. While this is acceptable in some circumstances, for most of the projects it's too simplistic and usually drives to a duplicate execution of the release tool within the build process: the first occurrence is a *dry run* with the sole purpose of generating a version number to use when building and testing, while the last actually performs the release.

That's why Nyx splits the process in two phases: the *inference* phase just *prepares* the transaction without altering the state of the repository, while the *publication* phase runs the transaction atomically. On the other hand you can rely on the version number since the early stages, so you can achieve full [*consistency*]({{ site.baseurl }}/best-practice/workflow/#build-workflow) and avoid repetitions.

To make this clearer, consider a typical workflow with the following steps:

```
1. build      # preprocess, compile, assemble, package etc
2. test       # deploy to test environment, run tests
3. publish    # publish artifacts to repositories and registries
4. release    # tag, generate change log, notify
```

You need to know the version number that *will* be used to release before the *build* step in order to implement a [consistent process]({{ site.baseurl }}/best-practice/workflow/#build-workflow) that doesn't require building and packaging again from scratch when reaching the *publish* and *release* steps. If you don't know the version number since in the early stages you end up by duplicating the *release* task or building and testing with fake version numbers and when it's time to publish and release you rebuild with the right number, with a loss in build time and process consistency.

Nyx grants that:

* the version number generated during the *inference* phase doesn't change until the end of the process so you can rely on it throughout the entire build
* interrupting the process before the *publication* phase (i.e. because the process fails some tests) and running Nyx again will generate the same version number, provided some conditions are met:
    * the workspace state doesn't change (i.e. by adding or modifying content)
    * Nyx is not configured to use timestamps in version numbers (i.e. in the *build* metadata)
* the conditions that Nyx uses to determine the version number (Git branches and tags, workspace status, configuration options) don't change across executions

This allows the build process to be [efficient]({{ site.baseurl }}/best-practice/workflow/#build-workflow) and leverage **incremental builds** without compromising consistency.








## Branching model
Nyx relies on [Git branches](https://git-scm.com/book/it/v2/Git-Branching-Basic-Branching-and-Merging) as to determine the type of release and the version to generate.

## Release types
